open Syntax
open Helpers

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

(** Create a new instruction from an original instruction by modifying
    its [args] with the table of [value]s.
*)
let mk_instr (ins: instr) (dest: dest) (op: op) (env: env) (tbl: tbl) : instr =
  let labels = get_lbls ins in
  let args =
    List.map (fun arg ->
      match StrMap.find_opt arg env with
      | Some row -> IntMap.find row tbl |> snd
      | None -> arg) 
      (get_args ins)
  in
  match op with
  | Binop binop -> 
    begin match args with 
    | [arg1; arg2] -> Binop (dest, binop, arg1, arg2) 
    | _ -> failwith (spf "Binop %s has incorrect no. of args" 
        (Base.Sexp.to_string_hum ([%sexp_of: binop] binop)))
    end
  | Unop unop -> 
    begin match args with 
    | [arg] -> Unop (dest, unop, arg) 
    | _ -> failwith (spf "Unop %s has incorrect no. of args" 
      (Base.Sexp.to_string_hum ([%sexp_of: unop] unop)))
    end 
  | Ret -> 
    begin match args with 
    | [] -> Ret None 
    | [arg] -> Ret (Some arg)
    | _ -> failwith "Ret has too many args"
    end
  | Print -> Print args 
  | Nop -> Nop
  | Call -> 
    begin match ins with 
    | Call (None, func_name, _) -> Call (None, func_name, args)
    | Call (Some _, func_name, _) -> Call (Some dest, func_name, args)
    | _ -> failwith "mk_instr was called with op = Call but instr != Call"
    end
  | Const -> 
    begin match ins with 
    | Const (_, literal) -> Const (dest, literal)
    | _ -> failwith "mk_instr was called with op = Const but instr != Const"
    end
  | Jmp -> 
    begin match labels with 
    | [lbl] -> Jmp lbl 
    | _ -> failwith "Jmp has an incorrect no. of labels"
    end
  | Br -> 
    begin match labels, args with 
    | [lbl1; lbl2], [arg] -> Br (arg, lbl1, lbl2)
    | _ -> failwith "Br has an incorrect no. of labels / args"
    end

(** Generates a fresh variable that is not in the existing set of [vars] *)
let mk_gen_fresh_var (vars: StrSet.t) () : unit -> string =
  let count = ref (-1) in
  fun () ->
    let rec loop () : string =
      count := !count + 1;
      let var = Printf.sprintf "v%d" !count in
      if StrSet.mem var vars then loop () else var
    in
    loop () |> (^) "v"

(** Extracts the set of variables used in a function *)
let vars_of_func (fn : func) : StrSet.t =
  List.fold_left (fun vars instr ->
    get_dest instr
    |> Option.map (fun (var, _) -> StrSet.add var vars)
    |> Option.value ~default:vars
  ) StrSet.empty fn.instrs

(** Implements local value numbering *)  
let lvn (fn: func) : func =
  let gen_fresh_var = mk_gen_fresh_var (vars_of_func fn) () in

  let instrs, _, _ =
    List.fold_left
      (fun (instrs, env, tbl) (instr, is_last_write) ->
         (* If the [instr] isn't an [op], just copy it over to the new
           list of instructions *)
        if not (is_op instr) then
          (instr :: instrs, env, tbl)
        else if has_eff instr then
          failwith "TODO"
        else (
          let (op, _) as v = mk_value instr env in
          let dst, dst_ty as dest = get_dest instr |> Option.get in
          begin match find_value v tbl with
          | None ->
            (* Compute a fresh value number, i.e. 1 greater than
               the current size of the table *)
            let row = IntMap.cardinal tbl + 1 in
            (* Figure out the actual destination of the instruction:
               - if it's not the last_write (i.e. the instruction will
                 be overwritten later), we generate a fresh variable name
               - otherwise we can just keep [dst] *)
            let dst' = if not is_last_write then gen_fresh_var () else dst in

            let instr' = mk_instr instr (dst', dst_ty) op env tbl in

            instr' :: instrs, StrMap.add dst' row env, IntMap.add row (v, dst') tbl
          | Some row ->
            (* The value already exists in the table, 
               so we can just rebuild the instruction as a 
               [Id var] instruction *)
            let (_, var) = IntMap.find row tbl in
            Unop (dest, Id, var) :: instrs, StrMap.add dst row env, tbl
          end
        )
      )
      ([], StrMap.empty, IntMap.empty)
      (last_writes fn.instrs)
  in
      
  { fn with instrs = List.rev instrs }
