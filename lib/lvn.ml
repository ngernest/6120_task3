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

(** Checks whether a value tuple exists in a table, returning a bool *)    
let is_value_in_table (value : value) (tbl : tbl) : bool = 
  IntMap.exists (fun _ (v, _) -> v = value) tbl
    
let lvn (fn: func) : func =
  let instrs', _, _ =
    List.fold_left
      (fun (instrs', env, tbl) instr -> 
        (* If the [instr] isn't an [op], just copy it over to the new
           list of instructions *)
        if not (is_op instr) then 
          (instr :: instrs', env, tbl)
        else 
          failwith "TODO: use the helper functions we defined")
      ([], StrMap.empty, IntMap.empty)
      fn.instrs
  in
  
  let instrs'' = List.rev instrs' in 
      
  (* TODO: implement *)
  { fn with instrs = instrs'' }
