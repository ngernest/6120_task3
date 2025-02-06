open Syntax

(** Datatype that represents any Bril operation 
    (i.e. any instruction that isn't a label) *)
type op = 
  | Binop of binop 
  | Unop of unop
  | Ret 
  | Print 
  | Nop 
  | Call 
  | Const 
  | Jmp 
  | Br 

(** Converts an [instr] to its corresponding [op] *)  
let op_of_instr (instr : instr) : op = 
  match instr with 
  | Label _ -> failwith "Labels aren't ops"  
  | Binop (_, binop, _, _) -> Binop binop 
  | Unop (_, unop, _) -> Unop unop
  | Call _ -> Call 
  | Print _ -> Print 
  | Ret _ -> Ret
  | Nop -> Nop 
  | Jmp _ -> Jmp 
  | Br _ -> Br 
  | Const _ -> Const 
  
(** An argument to a value tuple is either the index of a row in a table,
    or a string containing the variable name (if the variable is a {i live-in},
    i.e. it comes from a different block) *)
type val_arg = Row of int | Var of string | Lit of literal

(** A {i value tuple} is a pair consisting of an operation & a list of 
   arguments, represented by their row in the table *)
type value = op * val_arg list

(** A module for maps whose keys are [int]s *)
module IntMap = Map.Make(Int)

(** A module for maps whose keys are [string]s *)
module StrMap = Map.Make(String)

(** A module for sets whose elements are [string]s *)
module StrSet = Set.Make(String)

(** The type of tables, mapping each row index to a pair consisting 
    of a value tuple and the canonical variable name *)
type tbl = (value * string) IntMap.t 

(** The type of environments, mapping variable names to their row index 
    in the table *)   
type env = int StrMap.t 

(** Looks up an [arg] in the environment, returning a [val_arg]
    (either a row index or the string containing the variable name) *)
let val_arg_of_arg (env : env) (arg : arg) : val_arg = 
  match StrMap.find_opt arg env with 
  | Some idx -> Row idx 
  | None -> Var arg

(** Makes a value tuple corresponding to an [instr], where the value tuple
    refers to the row indexes given by the current environment [env] *)  
let mk_value (instr : instr) (env : env) : value = 
  match instr with 
  | Label _ -> failwith "Can't make a value tuple for labels"
  | Const (_, lit) -> (Const, [Lit lit])
  | _ -> 
    let val_args = List.map (val_arg_of_arg env) (get_args instr) in 
    let op = op_of_instr instr in 
    (op, val_args)

(** Checks whether a value tuple exists in a table, returning the
    row index in the table containing that value if it exists *)    
let find_value (v : value) (tbl : tbl) : int option = 
  IntMap.fold (fun idx (v', _) -> function
    | None -> if v = v' then Some idx else None
    | Some idx' -> Some idx'
  ) tbl None

(** A module for sets of strings *)  
module StringSet = Set.Make(String)

(** Given a list of instructions, [last_writes] determines
    if that instruction is the last write for that variable,
    returning a new list which pairs each [instr] with a
    corresponding bool *)
let last_writes (instrs : instr list) : (instr * bool) list = 
  let seen = StringSet.empty in
  let (_, out) = List.fold_left (fun (seen, acc) instr ->
    let dest = get_dest instr in
    match dest with
    | Some (arg, _) ->
      begin match StringSet.find_opt arg seen with
      | None ->
        let seen' = StringSet.add arg seen in
        (seen', (instr, true) :: acc)
      | Some _ -> (seen, (instr, false) :: acc)
      end
    | None -> (seen, (instr, false) :: acc))
    (seen, [])
    (List.rev instrs) in
  out

(** Create a new instruction from an original instruction by modifying
    its [arg]s with the table of [value]s, and optionally updating its
    destination.
*)
let mk_instr ?dest:(dest_opt=None) (ins: instr) (env: env) (tbl: tbl) : instr =
  (* Search for argument's canonical variable name in table *)
  let from_tbl = fun arg ->
    match StrMap.find_opt arg env with
    | Some row -> IntMap.find row tbl |> snd
    | None -> arg
  in
  (* Prefer [dest_opt] over the instruction's original destination *)
  let or_ = fun dest -> Option.value dest_opt ~default:dest in
  match ins with
  | Const (dest, literal) -> Const (or_ dest, literal)
  | Binop (dest, bop, arg1, arg2) ->
      Binop (or_ dest, bop, from_tbl arg1, from_tbl arg2)
  | Unop (dest, uop, arg) -> Unop (or_ dest, uop, from_tbl arg)
  | Jmp lbl -> Jmp lbl
  | Br (arg, lbl1, lbl2) -> Br (from_tbl arg, lbl1, lbl2)
  | Ret arg_opt -> Ret (Option.map from_tbl arg_opt)
  | Print args -> Print (List.map from_tbl args)
  | Call (dest_opt, name, args) ->
      begin match dest_opt with
      | None -> Call (None, name, List.map from_tbl args)
      | Some dest -> Call (Some (or_ dest), name, List.map from_tbl args)
      end
  | Nop -> Nop
  | Label _ -> failwith "mk_instr called on label instruction"

(** Generates a fresh variable that is not in the existing set of [vars] *)
let mk_gen_fresh_var (vars: StrSet.t) () : unit -> string =
  let count = ref (-1) in
  fun () ->
    let rec loop () : string =
      count := !count + 1;
      let var = Printf.sprintf "v%d" !count in
      if StrSet.mem var vars then loop () else var
    in
    loop ()

(** Extracts the set of variables used in a function *)
let vars_of_func (fn : func) : StrSet.t =
  List.fold_left (fun vars instr ->
    get_dest instr
    |> Option.map (fun (var, _) -> StrSet.add var vars)
    |> Option.value ~default:vars
  ) StrSet.empty fn.instrs

let is_rewriteable (ins: instr) : bool =
  match get_dest ins with
  | None -> false
  | Some _ ->
      match ins with
      | Call _ -> false
      | _ -> true

(** Implements local value numbering for a basic block *)
let lvn_block (gen_fresh_var : unit -> string) (instrs: instr list) : instr list =
  let instrs', _, _ =
    List.fold_left
      (fun (instrs', env, tbl) (instr, is_last_write) ->
         (* If the [instr] isn't an [op], just copy it over to the new
           list of instructions *)
        if not (is_op instr) then
          (instr :: instrs', env, tbl)
        else begin match get_dest instr with
        | None ->
          (* If the [instr] is an effect operation (i.e. no [dest]), do
             not save its value in the table but still use the table to
             overwrite its args *)
          let instr' = mk_instr instr env tbl in
          (instr' :: instrs', env, tbl)
        | Some ((dst, dst_ty) as dest) ->
          (* Otherwise this is a constant/value operation so we
             search its value in the table *)
          let v = mk_value instr env in
          begin match find_value v tbl with
          | Some row when is_rewriteable instr ->
            (* The value already exists in the table,
               and the instruction is rewriteable. We
               rebuild the instruction as a [Id var]
               instruction *)
            let (_, var) = IntMap.find row tbl in
            Unop (dest, Id, var) :: instrs', StrMap.add dst row env, tbl
          | _ ->
            (* Compute a fresh value number, i.e. 1 greater than
               the current size of the table *)
            let row = IntMap.cardinal tbl + 1 in

            (* Figure out the actual destination of the instruction:
               - if it's not the last_write (i.e. the instruction will
                 be overwritten later), we generate a fresh variable name
               - otherwise we can just keep [dst] *)
            let dst' = if not is_last_write then gen_fresh_var () else dst in

            (* Since we are adding a new row and having the
               variable [dst] now map to this row, we remove
               any rows in the table that previously had [dst]
               as the canonical name for a given value *)
            let tbl' =
              IntMap.filter (fun _ (_, var) -> var <> dst) tbl
              |> IntMap.add row (v, dst')
            in

            let instr' = mk_instr instr env tbl ~dest:(Some (dst', dst_ty)) in

            instr' :: instrs', StrMap.add dst row env, tbl'
          end
        end
      )
      ([], StrMap.empty, IntMap.empty)
      (last_writes instrs)
  in
  List.rev instrs'
      
(** Implements local value numbering for a function *)
let lvn (fn: func) : func =
  let gen_fresh_var = mk_gen_fresh_var (vars_of_func fn) () in

  { fn with instrs =
    Cfg.form_blocks (fn.instrs)
    |> List.map (lvn_block gen_fresh_var)
    |> List.concat
  }
