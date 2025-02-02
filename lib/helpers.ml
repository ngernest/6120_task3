open Yojson.Safe.Util

let spf = Printf.sprintf

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Safe.t = Yojson.Safe.from_channel stdin

(** Infix operator for looking up a key in a JSON object, 
    where [json $! key] means [json["key"]] 
    - This returns [`Null] if [json] doesn't contain [key] *)
let ( $! ) (json : Yojson.Safe.t) (key : string) : Yojson.Safe.t =
  member key json

(** [contains_key json key] returns [true] if the [json] object contains the key,
    and false otherwise *)
let contains_key (json : Yojson.Safe.t) (key : string) : bool =
  match json $! key with
  | `Null -> false
  | _ -> true

(** A total function which converts a Yojson JSON array 
    to a list of JSON objects
    - The [`Null] JSON object is converted to the empty list *)
let list_of_json (json : Yojson.Safe.t) : Yojson.Safe.t list =
  match json with
  | `Null -> []
  | _ -> Yojson.Safe.Util.to_list json
