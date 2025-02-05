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
type val_arg = Row of int | Var of string 

(** A {i value tuple} is a pair consisting of an operation & a list of 
   arguments, represented by their row in the table *)
type value = op * val_arg list

(** A module for maps whose keys are [int]s *)
module IntMap = Map.Make(Int)

(** A module for maps whose keys are [string]s *)
module StrMap = Map.Make(String)

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

(** Given a list of instructions, [is_last_write] determines 
    if that instruction is the last write for that variable, 
    returning a new list which pairs each [instr] with a corresponding bool *)  
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

(** Implements local value numbering *)  
let lvn (fn: func) : func =
  let instrs', _, _ =
    List.fold_left
      (fun (instrs', env, tbl) (instr, is_last_write) -> 
        (* If the [instr] isn't an [op], just copy it over to the new
           list of instructions *)
        if not (is_op instr) then 
          (instr :: instrs', env, tbl)
        else if has_eff instr then
          failwith "TODO"
        else (
          let v = mk_value instr env in
          let dst, _ as dest = get_dest instr |> Option.get in
          begin match find_value v tbl with
          | None ->
            (* Compute a fresh value number, i.e. 1 greater than 
               the current size of the table *)
            let row = IntMap.cardinal tbl + 1 in 
            (* Figure out the actual destination of the instruction:
               - if it's not the last_write (i.e. the instr will be 
                overwritten later), we need to generate a fresh variable name
               - otherwise we can just keep [dst] *)
            let actual_dest = 
              if not is_last_write then 
                failwith "TODO: need to generate fresh variable name"
              else 
                dst
            in 
            let updated_tbl = IntMap.add row (v, actual_dest) tbl in 
            ([], StrMap.add dst row env, updated_tbl)
          | Some row ->
            (* The value already exists in the table, 
               so we can just rebuild the instruction as a 
               [Id var] instruction *)
            let (_, var) = IntMap.find row tbl in
            let ins : instr = Unop (dest, Id, var) in 
            (ins :: instrs', StrMap.add dst row env, tbl)
          end
        )
      )
      ([], StrMap.empty, IntMap.empty)
      (last_writes fn.instrs)
  in
  
  let instrs'' = List.rev instrs' in 
      
  (* TODO: implement *)
  { fn with instrs = instrs'' }
