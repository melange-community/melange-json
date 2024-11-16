type t = Yojson.Basic.t

let to_json t = t
let of_json t = t
let to_string t = Yojson.Basic.to_string t

exception Of_string_error of string

let of_string s =
  try Yojson.Basic.from_string s
  with Yojson.Json_error msg -> raise (Of_string_error msg)

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
  of_json_error
    ("expected " ^ expected ^ " but got " ^ show_json_type json)

module To_json = struct
  let string_to_json v = `String v
  let bool_to_json v = `Bool v
  let int_to_json v = `Int v
  let int64_to_json v = `String (Int64.to_string v)
  let float_to_json v = `Float v
  let unit_to_json () = `Null
  let list_to_json v_to_json vs = `List (List.map v_to_json vs)

  let array_to_json v_to_json vs =
    `List (Array.to_list (Array.map v_to_json vs))

  let option_to_json v_to_json = function
    | None -> `Null
    | Some v -> v_to_json v

  let result_to_json a_to_json b_to_json v =
    match v with
    | Ok x -> `List [ `String "Ok"; a_to_json x ]
    | Error x -> `List [ `String "Error"; b_to_json x ]
end

module Of_json = struct
  let typeof = function
    | `Assoc _ -> "object"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `Int _ -> "int"
    | `List _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"

  let string_of_json = function
    | `String s -> s
    | json -> of_json_error_type_mismatch json "string"

  let bool_of_json = function
    | `Bool b -> b
    | json -> of_json_error_type_mismatch json "bool"

  let int_of_json = function
    | `Int i -> i
    | json -> of_json_error_type_mismatch json "int"

  let int64_of_json = function
    | `String i as json -> (
        match Int64.of_string_opt i with
        | Some v -> v
        | None -> of_json_error_type_mismatch json "int64 as string")
    | json -> of_json_error_type_mismatch json "int64 as string"

  let float_of_json = function
    | `Float f -> f
    | `Int i -> float_of_int i
    | json -> of_json_error_type_mismatch json "float"

  let unit_of_json = function
    | `Null -> ()
    | _ -> of_json_error "expected null"

  let option_of_json v_of_json = function
    | `Null -> None
    | json -> Some (v_of_json json)

  let list_of_json v_of_json = function
    | `List l -> List.map v_of_json l
    | json -> of_json_error_type_mismatch json "array"

  let array_of_json v_of_json = function
    | `List l -> Array.map v_of_json (Array.of_list l)
    | json -> of_json_error_type_mismatch json "array"

  let result_of_json ok_of_json err_of_json json =
    match json with
    | `List [ `String "Ok"; x ] -> Ok (ok_of_json x)
    | `List [ `String "Error"; x ] -> Error (err_of_json x)
    | _ -> of_json_error "invalid JSON"
end

module Primitives = struct
  include To_json
  include Of_json
end

module Classify = struct
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
end
