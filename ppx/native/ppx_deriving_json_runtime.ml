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

let () =
  Printexc.register_printer (function
    | Of_json_error (Json_error str) ->
        Some
          (Printf.sprintf
             " exception \
              Ppx_deriving_json_runtime.Of_json_error(Json_error %S)"
             str)
    | Of_json_error (Unexpected_variant str) ->
        Some
          (Printf.sprintf
             " exception \
              Ppx_deriving_json_runtime.Of_json_error(Unexpected_variant \
              %S)"
             str)
    | _ -> None)

let show_json_type = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"

let show_json_error ?depth json =
  let fprintf = Format.fprintf in
  let rec loop ?depth fmt json =
    let depth = Option.map (fun i -> i - 1) depth in
    match depth with
    | Some 0 -> fprintf fmt "_"
    | _ -> (
        match json with
        | `Any ->
            (* Special non-json variant to represent parts where anything is allowed in expected json. *)
            fprintf fmt "_"
        | `Assoc assoc ->
            fprintf fmt "{";
            List.iter
              (fun (k, v) -> fprintf fmt {|"%s": %a, |} k (loop ?depth) v)
              assoc;
            fprintf fmt "}"
        | `Bool bool -> fprintf fmt (if bool then "true" else "false")
        | `Float float -> fprintf fmt "%f" float
        | `Int int -> fprintf fmt "%i" int
        | `List li ->
            fprintf fmt "[";
            List.iter (fun elt -> fprintf fmt "%a, " (loop ?depth) elt) li;
            fprintf fmt "]"
        | `Null -> fprintf fmt "null"
        | `String str -> fprintf fmt "%S" str)
  in
  Format.asprintf "%a"
    (loop ?depth:(Option.map (fun i -> i + 1) depth))
    json

let of_json_msg_error msg = raise (Of_json_error (Json_error msg))

let of_json_error_mismatch ~depth ~expected ~json =
  of_json_msg_error
    ("expected "
    ^ (expected |> List.map show_json_error |> String.concat "or ")
    ^ "but got"
    ^ show_json_error ~depth json)

let of_json_error ?(depth = 2) ~json msg =
  of_json_msg_error
    (Printf.sprintf "%s. Json received: %s" msg
       (show_json_error ~depth json))

let of_json_error_type_mismatch json expected =
  of_json_msg_error
    ("expected "
    ^ expected
    ^ " but got "
    ^ show_json_type json
    ^ ": "
    ^ show_json_error ~depth:1 json)

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
    | json -> of_json_error_type_mismatch json "expected null"

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
    | _ ->
        of_json_error_mismatch
          ~expected:
            [
              `List [ `String "Ok"; `Any ];
              `List [ `String "Error"; `Any ];
            ]
          ~depth:2 ~json
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
