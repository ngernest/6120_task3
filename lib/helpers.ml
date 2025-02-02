open Yojson.Basic.Util

let spf = Printf.sprintf

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Basic.t = Yojson.Basic.from_channel stdin

(** Infix operator for looking up a key in a JSON object, 
    where [json $! key] means [json["key"]] 
    - This returns [`Null] if [json] doesn't contain [key] *)
let ( $! ) (json : Yojson.Basic.t) (key : string) : Yojson.Basic.t =
  member key json

(** [contains_key json key] returns [true] if the [json] object contains the key,
    and false otherwise *)
let contains_key (json : Yojson.Basic.t) (key : string) : bool =
  match json $! key with
  | `Null -> false
  | _ -> true

(** A total function which converts a Yojson JSON array 
    to a list of JSON objects
    - The [`Null] JSON object is converted to the empty list *)
let list_of_json (json : Yojson.Basic.t) : Yojson.Basic.t list =
  match json with
  | `Null -> []
  | _ -> Yojson.Basic.Util.to_list json
