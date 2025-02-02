open Helpers
open Core

(* -------------------------------------------------------------------------- *)
(*                            Labels and arguments                            *)
(* -------------------------------------------------------------------------- *)

(** All [label]s are just strings *)
type label = string [@@deriving sexp, equal, quickcheck]

(** All arguments are strings *)
type arg = string [@@deriving sexp, equal, quickcheck]

(* -------------------------------------------------------------------------- *)
(*                                    Types                                   *)
(* -------------------------------------------------------------------------- *)

(** Primitive types (int or bools) *)
type ty =
  | TyInt
  | TyBool
[@@deriving sexp, equal, quickcheck]

(** Converts a string to a [ty] *)
let ty_of_string (str : string) : ty =
  match str with
  | "int" -> TyInt
  | "bool" -> TyBool
  | _ -> failwith (spf "invalid string: %s\n" str)

(** Converts a [ty] to its string representation *)
let string_of_ty : ty -> string = function
  | TyInt -> "int"
  | TyBool -> "bool"

(* -------------------------------------------------------------------------- *)
(*                                  Literals                                  *)
(* -------------------------------------------------------------------------- *)

(** Literal values (int & bool values) *)
type literal =
  | LitInt of int
  | LitBool of bool
[@@deriving sexp, equal, quickcheck]

(** Converts a [literal] to its equivalent Yojson representation *)
let json_of_literal : literal -> Yojson.Basic.t = function
  | LitInt i -> `Int i
  | LitBool b -> `Bool b

(* -------------------------------------------------------------------------- *)
(*                            Destination variables                           *)
(* -------------------------------------------------------------------------- *)

(** A {i destination variable} is a pair consisting of 
    the variable name & the variable's type *)
type dest = string * ty [@@deriving sexp, equal, quickcheck]

(** Converts a destination variable to an association list
    mapping JSON field names to Yojson JSON objects *)
let json_of_dest ((varname, ty) : dest) : (string * Yojson.Basic.t) list =
  [ ("dest", `String varname); ("type", `String (string_of_ty ty)) ]

(* -------------------------------------------------------------------------- *)
(*                              Binary Operators                              *)
(* -------------------------------------------------------------------------- *)

(** Binary operators *)
type binop =
  | Add
  | Mul
  | Sub
  | Div
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
[@@deriving sexp, equal, quickcheck]

let binop_opcode_map : (string * binop) list =
  [
    ("add", Add);
    ("mul", Mul);
    ("sub", Sub);
    ("div", Div);
    ("eq", Eq);
    ("lt", Lt);
    ("gt", Gt);
    ("le", Le);
    ("ge", Ge);
    ("and", And);
    ("or", Or);
  ]

(** Determines if an opcode represents a binary operator *)
let is_binop (opcode : string) : bool =
  let binop_opcodes : string list = List.map ~f:fst binop_opcode_map in
  List.mem binop_opcodes opcode ~equal:String.equal

(** Converts a string to a [binop] *)
let binop_of_string (opcode : string) : binop =
  List.Assoc.find_exn binop_opcode_map opcode ~equal:String.equal

(** Converts a [binop] to a string *)
let string_of_binop (binop : binop) : string =
  let open List.Assoc in
  let inverse_map = inverse binop_opcode_map in
  find_exn inverse_map binop ~equal:equal_binop

(* -------------------------------------------------------------------------- *)
(*                               Unary operators                              *)
(* -------------------------------------------------------------------------- *)

(** Unary operators *)
type unop =
  | Not
  | Id
[@@deriving sexp, equal, quickcheck]

(** Maps each unary operator's opcode to the corresponding [unop] *)
let unop_opcode_map : (string * unop) list = [ ("not", Not); ("id", Id) ]

(** Determines if an opcode represents a binary operator *)
let is_unop (opcode : string) : bool =
  let unop_opcodes = List.map ~f:fst unop_opcode_map in
  List.mem unop_opcodes opcode ~equal:String.equal

(** Converts a string to an [unop] *)
let unop_of_string (opcode : string) : unop =
  List.Assoc.find_exn unop_opcode_map opcode ~equal:String.equal

(** Converts an [unop] to a string *)
let string_of_unop (unop : unop) : string =
  let open List.Assoc in
  let inverse_map = inverse unop_opcode_map in
  find_exn inverse_map unop ~equal:equal_unop

(* -------------------------------------------------------------------------- *)
(*                                Other opcodes                               *)
(* -------------------------------------------------------------------------- *)

(* -------------------- Functions for determining opcodes ------------------- *)

(** Determines if an opcode is ["const"] *)
let is_const (opcode : string) : bool = String.equal opcode "const"

(** Determines if an opcode is ["nop"] *)
let is_nop (opcode : string) : bool = String.equal opcode "nop"

(** Determines if an opcode is ["jmp"] *)
let is_jmp (opcode : string) : bool = String.equal opcode "jmp"

(** Determines if an opcode is ["br"] *)
let is_br (opcode : string) : bool = String.equal opcode "br"

(** Determines if an opcode is ["call"] *)
let is_call (opcode : string) : bool = String.equal opcode "call"

(** Determines if an opcode is ["print"] *)
let is_print (opcode : string) : bool = String.equal opcode "print"

(** Determines if an opcode is ["ret"] *)
let is_ret (opcode : string) : bool = String.equal opcode "ret"

(* -------------------------------------------------------------------------- *)
(*                                Instructions                                *)
(* -------------------------------------------------------------------------- *)

(** The type of Bril instructions *)
type instr =
  | Label of label
  | Const of dest * literal
  | Binop of dest * binop * arg * arg
  | Unop of dest * unop * arg
  | Jmp of label
  | Br of arg * label * label
  | Ret of arg option
  | Print of arg list [@sexp.list]
  | Call of dest option * string * arg list
  | Nop
[@@deriving sexp, equal, quickcheck]

(* -------------------------------------------------------------------------- *)
(*                                  Functions                                 *)
(* -------------------------------------------------------------------------- *)

(** The type of a Bril function: 
{[
  {
    "name": "<string>",
    "args": [{"name": "<string>", "type": <Type>}, ...]?,
    "type": <Type>?,
    "instrs": [<Instruction>, ...]
  } 
]} *)
type func = {
  name : string;
  args : (arg * ty) list; [@sexp.list]
  ret_type : ty option; [@sexp.option]
  instrs : instr list; [@sexp.list]
}
[@@deriving sexp, equal, quickcheck]

(* -------------------------------------------------------------------------- *)
(*                           Extracting JSON fields                           *)
(* -------------------------------------------------------------------------- *)

open Yojson.Basic.Util

(* ---------------------------- For instructions ---------------------------- *)

(** Retrieves the contents of the ["args"] field in a JSON object 
    as a list of strings  *)
let get_instr_args (json : Yojson.Basic.t) : arg list =
  let args_list = Helpers.list_of_json (json $! "args") in
  List.map ~f:to_string args_list

(** Retrieves the contents of the ["fields"] field in a JSON object 
    as a list of strings  *)
let get_labels (json : Yojson.Basic.t) : arg list =
  let labels_list = Helpers.list_of_json (json $! "labels") in
  List.map ~f:to_string labels_list

(** Retrieves the contents of the ["value"] field in a JSON object
    as a Bril literal (either an int or a bool) *)
let get_value (json : Yojson.Basic.t) : literal =
  let value = json $! "value" in
  match value with
  | `Int i -> LitInt i
  | `Bool b -> LitBool b
  | _ -> failwith (spf "Invalid value %s" (to_string value))

(** Retrieves the name and type of the destination variable
    from a JSON object *)
let get_dest (json : Yojson.Basic.t) : dest =
  let dest_string = to_string (json $! "dest") in
  let ty = ty_of_string @@ to_string (json $! "type") in
  (dest_string, ty)

(** Retrieves the contents of the ["funcs"] field in a JSON object
    as a string list (list of function names) *)
let get_funcs (json : Yojson.Basic.t) : string list =
  List.map ~f:to_string (Helpers.list_of_json (json $! "funcs"))

(* ------------------------------ For functions ----------------------------- *)

(** Extracts the ["name"] field in a JSON object 
    (this is used for Bril functions only) *)
let get_name (json : Yojson.Basic.t) : string =
  let open Yojson.Basic in
  match json $! "name" with
  | `String name -> name
  | `Null -> failwith (spf "Missing name field in %s\n" (pretty_to_string json))
  | _ -> failwith (spf "Invalid json %s\n" (pretty_to_string json))

(** Retrieves the optional ["type"] field in a JSON object as a [ty option] 
    (this is used for Bril functions only) *)
let get_type_option (json : Yojson.Basic.t) : ty option =
  let open Yojson.Basic in
  match json $! "type" with
  | `String ty_name -> Some (ty_of_string ty_name)
  | `Null -> None
  | _ -> failwith (spf "Invalid json %s\n" (pretty_to_string json))

(** Retrieves the list of arguments in a function JSON object
    as an association list of type [(arg * ty) list] 
    - If the function has no arguments, the empty list is returned
    *)
let get_func_args (json : Yojson.Basic.t) : (arg * ty) list =
  let open Yojson.Basic in
  match json $! "args" with
  | `List arg_objs ->
    List.map arg_objs ~f:(fun arg_obj ->
        match (arg_obj $! "name", arg_obj $! "type") with
        | `String name, `String ty_name -> (name, ty_of_string ty_name)
        | _ ->
          failwith
            (spf "Malformed argument json %s\n" (pretty_to_string arg_obj)))
  | `Null -> []
  | _ -> failwith (spf "Invalid json %s\n" (pretty_to_string json))

(* -------------------------------------------------------------------------- *)
(*                  Converting from JSON to Bril instructions                 *)
(* -------------------------------------------------------------------------- *)

(** Converts a JSON object to an [instr] *)
let instr_of_json (json : Yojson.Basic.t) : instr =
  match json $! "label" with
  | `String label -> Label label
  | `Null ->
    let opcode : string = to_string (json $! "op") in
    if is_const opcode then
      (* Constants *)
      let dest = get_dest json in
      let literal = get_value json in
      Const (dest, literal)
    else if is_binop opcode then
      (* Binary operators *)
      let binop = binop_of_string opcode in
      let dest = get_dest json in
      let args = get_instr_args json in
      let arg1 = List.nth_exn args 0 in
      let arg2 = List.nth_exn args 1 in
      Binop (dest, binop, arg1, arg2)
    else if is_unop opcode then
      (* Unary operators *)
      let unop = unop_of_string opcode in
      let dest = get_dest json in
      let arg = List.hd_exn (get_instr_args json) in
      Unop (dest, unop, arg)
    else if is_jmp opcode then
      (* Jmp *)
      let label = List.hd_exn (get_labels json) in
      Jmp label
    else if is_br opcode then
      (* Br *)
      let arg = List.hd_exn (get_instr_args json) in
      let labels = get_labels json in
      let true_lbl = List.nth_exn labels 0 in
      let false_lbl = List.nth_exn labels 1 in
      Br (arg, true_lbl, false_lbl)
    else if is_ret opcode then
      (* Ret *)
      let args = get_instr_args json in
      match args with
      | [] -> Ret None
      | _ -> Ret (Some (List.hd_exn args))
    else if is_print opcode then
      (* Print *)
      let args = get_instr_args json in
      Print args
    else if is_call opcode then
      (* Call *)
      let args = get_instr_args json in
      let func_name = List.hd_exn (get_funcs json) in
      if contains_key json "dest" then
        let dest = get_dest json in
        Call (Some dest, func_name, args)
      else Call (None, func_name, args)
    else if is_nop opcode then Nop
    else failwith (spf "Invalid opcode : %s" opcode)
  | _ -> failwith (spf "Invalid JSON : %s" (Yojson.Basic.pretty_to_string json))

(** Retrieves the ["instrs"] field in a JSON object as a [instr list]
    (this is used for Bril functions only) *)
let get_instrs (json : Yojson.Basic.t) : instr list =
  let open Yojson.Basic in
  match json $! "instrs" with
  | `List instrs -> List.map ~f:instr_of_json instrs
  | `Null ->
    failwith (spf "Missing instrs field in %s\n" (pretty_to_string json))
  | _ -> failwith (spf "Invalid json %s\n" (pretty_to_string json))

(** Converts a Bril function JSON object to the [func] type *)
let func_of_json (json : Yojson.Basic.t) : func =
  let name = get_name json in
  let args = get_func_args json in
  let ret_type = get_type_option json in
  let instrs = get_instrs json in
  { name; args; ret_type; instrs }

(* -------------------------------------------------------------------------- *)
(*                  Converting from Bril instructions to JSON                 *)
(* -------------------------------------------------------------------------- *)

(** Converts a list of OCaml strings to a JSON string list *)
let mk_json_string_list (xs : string list) : Yojson.Basic.t =
  `List (List.map ~f:(fun x -> `String x) xs)

(** Converts a Bril instruction to its Yojson JSON representation *)
let json_of_instr (instr : instr) : Yojson.Basic.t =
  match instr with
  | Label label -> `Assoc [ ("label", `String label) ]
  | Const (dest, literal) ->
    let dest_json = json_of_dest dest in
    `Assoc
      (("op", `String "const")
      :: ("value", json_of_literal literal)
      :: dest_json)
  | Jmp lbl ->
    `Assoc [ ("op", `String "jmp"); ("labels", mk_json_string_list [ lbl ]) ]
  | Print args ->
    `Assoc [ ("op", `String "print"); ("args", mk_json_string_list args) ]
  | Nop -> `Assoc [ ("op", `String "nop") ]
  | Br (arg, true_lbl, false_lbl) ->
    `Assoc
      [
        ("op", `String "br");
        ("args", mk_json_string_list [ arg ]);
        ("labels", mk_json_string_list [ true_lbl; false_lbl ]);
      ]
  | Binop (dest, binop, arg1, arg2) ->
    let dest_json = json_of_dest dest in
    `Assoc
      (("op", `String (string_of_binop binop))
      :: ("args", mk_json_string_list [ arg1; arg2 ])
      :: dest_json)
  | Unop (dest, unop, arg) ->
    let dest_json = json_of_dest dest in
    `Assoc
      (("op", `String (string_of_unop unop))
      :: ("args", mk_json_string_list [ arg ])
      :: dest_json)
  | Ret None -> `Assoc [ ("op", `String "ret") ]
  | Ret (Some arg) ->
    `Assoc [ ("op", `String "ret"); ("args", mk_json_string_list [ arg ]) ]
  | Call (dest_opt, func_name, args) ->
    let dest_json =
      match dest_opt with
      | None -> []
      | Some dest -> json_of_dest dest in
    `Assoc
      ([
         ("op", `String "call");
         ("funcs", mk_json_string_list [ func_name ]);
         ("args", mk_json_string_list args);
       ]
      @ dest_json)

(** Converts a Bril function to its Yojson JSON representation *)
let json_of_func (func : func) : Yojson.Basic.t =
  let json_args =
    List.map
      ~f:(fun (name, ty) ->
        `Assoc [ ("name", `String name); ("type", `String (string_of_ty ty)) ])
      func.args in
  let return_type_json =
    match func.ret_type with
    | None -> []
    | Some ty -> [ ("type", `String (string_of_ty ty)) ] in
  `Assoc
    ([
       ("name", `String func.name);
       ("args", `List json_args);
       ("instrs", `List (List.map ~f:json_of_instr func.instrs));
     ]
    @ return_type_json)

(* -------------------------------------------------------------------------- *)
(*                                  Programs                                  *)
(* -------------------------------------------------------------------------- *)

(** A top-level Bril program is a list of functions, 
    each of which have type [func] *)
type prog = func list

(** Converts a Bril program to its JSON representation *)
let json_of_prog (prog : prog) : Yojson.Basic.t =
  `Assoc [ ("functions", `List (List.map ~f:json_of_func prog)) ]
