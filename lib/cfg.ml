open Helpers
open Syntax
open Core

(** Determines if an instruction is a terminator ([Jmp], [Br], [Ret]) *)
let is_terminator : instr -> bool = function
  | Jmp _ | Br _ | Ret _ -> true
  | _ -> false

(** Determines whether an instruction is an operation 
    (all instructions are operations except labels) *)
let is_op : instr -> bool = function
  | Label _ -> false
  | _ -> true

(** The type of {i basic blocks} (an ordered list of instructions) *)
type block = instr list [@@deriving sexp]

(** Forms basic blocks containing the instructions in [body] *)
let form_blocks (body : instr list) : block list =
  let curr_block : block ref = ref [] in
  (* NB: [blocks] is the list of basic blocks that this function outputs *)
  let blocks =
    List.fold
      ~f:(fun blocks instr ->
        if is_op instr then (
          (* Add the current instruction to the block *)
          curr_block := List.append !curr_block [ instr ];
          (* A terminator terminates [curr_block], so we need to add it to
             [blocks] *)
          if is_terminator instr then (
            let new_blocks = List.append blocks [ !curr_block ] in
            curr_block := [];
            new_blocks)
          else blocks)
        else
          (* We have a label *)
          let new_blocks =
            if not (List.is_empty !curr_block) then
              List.append blocks [ !curr_block ]
            else blocks in
          curr_block := [ instr ];
          new_blocks)
      ~init:[] body in
  (* Need to add a final block to [blocks] if we have any [instr]s remaining *)
  if not (List.is_empty !curr_block) then List.append blocks [ !curr_block ]
  else blocks

(** Creates labels for a list of blocks 
    (generating fresh labels when necessary) *)
let mk_block_map (blocks : block list) : (label * block) list =
  List.rev
  @@ List.fold
       ~f:(fun acc block ->
         let fresh_name = spf "b%d" (List.length acc) in
         match block with
         | [] -> (fresh_name, block) :: acc
         | hd :: tl ->
           (* If [hd = Label lbl], then we want [lbl] to only point to [tl] (the
              remaining [instr]s in the [block]).

              Otherwise, we produce a fresh name and make it point to the
              entirety of the block. *)
           let name, block =
             match hd with
             | Label lbl -> (lbl, tl)
             | _ -> (fresh_name, block) in
           (name, block) :: acc)
       ~init:[] blocks

(** Given a name-to-block map [name2block], [get_cfg] 
    produces a map from block names to a list of successor block names *)
let get_cfg (name2block : (label * block) list) : (label * label list) list =
  List.rev
  @@ List.foldi
       ~f:(fun i acc (name, block) ->
         if List.is_empty block then (name, []) :: acc
         else
           let last = List.last_exn block in
           let succ =
             match last with
             | Jmp label -> [ label ]
             | Br (_, lbl1, lbl2) -> [ lbl1; lbl2 ]
             | Ret _ -> []
             | _ ->
               if Int.equal i (List.length name2block - 1) then
                 (* We've reached the block, so there is no successor *)
                 []
               else
                 (* Fall through to the next block in [name2block] *)
                 let keys = List.map ~f:fst name2block in
                 [ List.nth_exn keys (i + 1) ] in
           (name, succ) :: acc)
       ~init:[] name2block

(** Constructs a CFG for a Bril program *)
let build_cfg () : unit =
  (* Load a Bril program (as JSON) from [stdin] *)
  let json = load_json () in
  let functions = list_of_json (json $! "functions") in
  List.iter functions ~f:(fun func ->
      (* Convert Bril programs from JSON to a typed representation *)
      let instrs = List.map ~f:instr_of_json (list_of_json (func $! "instrs")) in

      let blocks = form_blocks instrs in

      (* Fetch labelled basic blocks *)
      let name2block = mk_block_map blocks in

      (* Build the CFG *)
      let cfg = get_cfg name2block in

      (* Produce GraphViz visualization *)
      printf "digraph %s {\n" (Yojson.Basic.Util.to_string (func $! "name"));
      List.iter name2block ~f:(fun (name, _) -> printf "  %s;\n" name);
      List.iter cfg ~f:(fun (name, succs) ->
          List.iter succs ~f:(fun succ -> printf "  %s -> %s\n" name succ));
      printf "}\n")
