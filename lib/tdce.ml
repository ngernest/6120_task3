open Helpers
open Syntax
open Core
open Cfg

(** A module of int sets *)
module IntSet = Stdlib.Set.Make(Int)

(** A module of maps from [string] to [int] *)
module StrMap = Stdlib.Map.Make(String)

(** Extracts the list of arguments from an instruction
    - If the instruction has no arguments, the empty list is returned *)
let get_args (instr : instr) : arg list =
  match instr with
  | Binop (_, _, arg1, arg2) -> [ arg1; arg2 ]
  | Unop (_, _, arg) | Br (arg, _, _) -> [ arg ]
  | Call (_, _, args) | Print args -> args
  | Ret (Some arg) -> [ arg ]
  | Ret None | Nop | Label _ | Const _ | Jmp _ -> []

(** Extracts the destination (if one exists) of an instruction *)
let get_dest (instr : instr) : dest option =
  match instr with
  | Binop (dest, _, _, _) | Unop (dest, _, _) | Const (dest, _) -> Some dest
  | Call (dest_opt, _, _) -> dest_opt
  | _ -> None

(** Determines whether [instr] may have an effect. Instructions
    without a destination may have an effect, as well as
    instructions that make a call to another function *)
let has_eff (instr : instr) : bool =
  match get_dest instr with
  | None -> true
  | Some _ ->
      match instr with
      | Call _ -> true
      | _ -> false

(** Removes instructions from [func] whose results are never used 
    as arguments to any other instruction. 
    
    This function returns a pair consisting of: 
    - a [bool] indicating whether the function changed 
    - a list of updated instructions (updated basic blocks) *)
let trivial_dce_pass (func : func) : bool * func =
  (* Build the basic blocks for this function *)
  let blocks : block list = form_blocks func.instrs in
  (* Mark all variables that appear as an argument to an instruction as used *)
  let used : arg list =
    List.fold_left blocks ~init:[] ~f:(fun acc block ->
        let args = List.concat_map ~f:get_args block in
        List.append acc args) in
  let changed = ref false in
  let updated_blocks =
    List.map blocks ~f:(fun block ->
        (* Keep all effect instructions and instructions whose
           destinations appear in the [used] set *)
        let new_block =
          List.filter block ~f:(fun instr ->
              let dst_used =
                Option.for_all (get_dest instr)
                  ~f:(fun (dst, _) -> List.mem used dst ~equal:equal_arg)
              in
              has_eff instr || dst_used
          )
        in
        (* Record whether we actually removed any instructions *)
        changed := !changed ||
          not (Int.equal (List.length new_block) (List.length block));
        new_block) in
  (!changed, { func with instrs = List.concat updated_blocks})

(** Iteratively removes dead instructions, stopping when there are no
    instructions left to remove *)
let rec trivial_dce (func : func) : func =
  let has_changed, updated_func = trivial_dce_pass func in
  (* Keep iterating trivial DCE until our function doesn't change *)
  if has_changed then trivial_dce updated_func
  else updated_func

(** Delete instructions in a single block whose result is unused
    before the next assignment. Return a bool indicating whether
    anything changed, along with the updated block *)
let drop_killed_local (block : block) : bool * block =
  let changed, to_delete, _ = 
   List.foldi
    block
    ~init:(false, IntSet.empty, StrMap.empty)
    ~f:(fun id (changed, to_delete, unused_assigned_at) instr ->
      let changed', to_delete', unused_assigned_at' =
        match get_dest instr with
        | Some (var, _) ->
          (* Add variable assigned to by instruction to set of unusued
             variables (keeping track of where the variable was last
             assigned at) -- as long as the instruction has no effect *)
          let unused_assigned_at' =
            if has_eff instr then
              unused_assigned_at
            else
              StrMap.add var id unused_assigned_at
          in
          (* If variable had been assigned to before, delete instruction
             that has been "killed" by this assignment *)
          begin match StrMap.find_opt var unused_assigned_at with 
          | Some id' -> true, IntSet.add id' to_delete, unused_assigned_at'
          | None -> changed, to_delete, unused_assigned_at'
          end
        (* If instruction does not write to a variable, move on *)
        | None -> changed, to_delete, unused_assigned_at 
      in
      let args = get_args instr in 
      let unused_assigned_at'' =
        List.fold
          args
          ~init:unused_assigned_at'
          ~f:(fun acc arg -> StrMap.filter (fun var _ -> not (String.equal var arg)) acc)
      in
      changed', to_delete', unused_assigned_at''
    )
  in
  if not changed then
    false, block
  else
    changed, List.filteri block ~f:(fun i _ -> not (IntSet.mem i to_delete))

(** Drop killed instructions from all blocks within a function *)    
let drop_killed_pass (func : func) : bool * func =
  let (changed, blocks') =
   List.fold
    (List.rev (form_blocks func.instrs))
      ~init:(false, [])
      ~f:(fun (changed, blocks) block ->
        let (blk_changed, block') = drop_killed_local block in
        changed || blk_changed, block' :: blocks
      )
  in 
  (changed, { func with instrs = List.concat blocks' })

(** Iteratively removes locally killed instructions, stopping when there
    are no instructions left to remove *)
let rec drop_killed (fn : func) : func =
  let (changed, fn') = drop_killed_pass fn in
  if changed then drop_killed fn' else fn'

(** Invokes both [trivial_dce_pass] & [drop_killed_pass] *)  
let tdce_plus (func : func) : func =
  let rec loop (fn : func) : bool -> func = function
    | false -> fn
    | true ->
      let (chgd, fn') = trivial_dce_pass fn in
      let (chgd', fn'') = drop_killed_pass fn' in
      loop fn'' (chgd || chgd') in
  loop func true

let tdce_pipeline () : unit =
  (* Load a Bril program (as JSON) from [stdin] *)
  let json = load_json () in
  (* Convert the JSON to our typed representation *)
  let functions =
    List.map ~f:func_of_json (list_of_json (json $! "functions")) in
  (* Apply both trivial DCE & dropping killed instructions *)
  let updated_prog = List.map ~f:drop_killed functions in
  (* Convert the optimization program to JSON & write to stdout *)
  Yojson.Safe.pretty_to_channel stdout (json_of_prog updated_prog)
