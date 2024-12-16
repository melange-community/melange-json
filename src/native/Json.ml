type t = Yojson.Basic.t
(** The type of a JSON data structure *)

type json = t
(** Defined for convenience. *)

let to_string t = Yojson.Basic.to_string t

exception Of_string_error of string

let of_string s =
  try Yojson.Basic.from_string s
  with Yojson.Json_error msg -> raise (Of_string_error msg)

type 'a to_json = 'a -> json
(** Describe how to encode a value into JSON. *)

let to_json : json to_json = fun x -> x

type error = Json_error of string | Unexpected_variant of string

exception Of_json_error of error

let of_json_error msg = raise (Of_json_error (Json_error msg))

let show_json_type = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"

let of_json_error_type_mismatch json expected =
  of_json_error ("Expected " ^ expected ^ ", got " ^ show_json_type json)

type 'a of_json = json -> 'a
(** Describe how to decode a value from JSON. *)

let of_json : 'a of_json = fun x -> x

module Of_json = struct
  let typeof = function
    | `Assoc _ -> "object"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `Int _ -> "int"
    | `List _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"

  let string = function
    | `String s -> s
    | json -> of_json_error_type_mismatch json "string"

  let bool = function
    | `Bool b -> b
    | json -> of_json_error_type_mismatch json "bool"

  let int = function
    | `Int i -> i
    | json -> of_json_error_type_mismatch json "int"

  let int64 = function
    | `String i as json -> (
        match Int64.of_string_opt i with
        | Some v -> v
        | None -> of_json_error_type_mismatch json "int64 as string")
    | json -> of_json_error_type_mismatch json "int64 as string"

  let float = function
    | `Float f -> f
    | `Int i -> float_of_int i
    | json -> of_json_error_type_mismatch json "float"

  let unit = function `Null -> () | _ -> of_json_error "expected null"

  let option v_of_json = function
    | `Null -> None
    | json -> Some (v_of_json json)

  let list v_of_json = function
    | `List l -> List.map v_of_json l
    | json -> of_json_error_type_mismatch json "array"

  let array v_of_json = function
    | `List l -> Array.map v_of_json (Array.of_list l)
    | json -> of_json_error_type_mismatch json "array"

  let result ok_of_json err_of_json json =
    match json with
    | `List [ `String "Ok"; x ] -> Ok (ok_of_json x)
    | `List [ `String "Error"; x ] -> Error (err_of_json x)
    | _ -> of_json_error "invalid JSON"
end

module To_json = struct
  let string v = `String v
  let bool v = `Bool v
  let int v = `Int v
  let int64 v = `String (Int64.to_string v)
  let float v = `Float v
  let unit () = `Null
  let list v_to_json vs = `List (List.map v_to_json vs)
  let array v_to_json vs = `List (Array.to_list (Array.map v_to_json vs))
  let option v_to_json = function None -> `Null | Some v -> v_to_json v

  let result a_to_json b_to_json v =
    match v with
    | Ok x -> `List [ `String "Ok"; a_to_json x ]
    | Error x -> `List [ `String "Error"; b_to_json x ]
end

module Primitives = struct
  let string_of_json = Of_json.string
  let bool_of_json = Of_json.bool
  let float_of_json = Of_json.float
  let int_of_json = Of_json.int
  let int64_of_json = Of_json.int64
  let option_of_json = Of_json.option
  let unit_of_json = Of_json.unit
  let result_of_json = Of_json.result
  let list_of_json = Of_json.list
  let array_of_json = Of_json.array
  let string_to_json = To_json.string
  let bool_to_json = To_json.bool
  let float_to_json = To_json.float
  let int_to_json = To_json.int
  let int64_to_json = To_json.int64
  let option_to_json = To_json.option
  let unit_to_json = To_json.unit
  let result_to_json = To_json.result
  let list_to_json = To_json.list
  let array_to_json = To_json.array
end

let classify :
    t ->
    [ `Null
    | `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `List of t list
    | `Assoc of (string * t) list ] =
 fun x -> x
