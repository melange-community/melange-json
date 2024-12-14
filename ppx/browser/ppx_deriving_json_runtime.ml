[@@@alert "-deprecated"]
type t = Js.Json.t

let to_json t = t
let of_json t = t
let to_string t = Js.Json.stringify t

exception Of_string_error of string

let of_string s =
  try Js.Json.parseExn s
  with exn ->
    let msg =
      match Js.Exn.asJsExn exn with
      | Some jsexn -> Js.Exn.message jsexn
      | None -> None
    in
    let msg =
      (* msg really cannot be None in browser or any sane JS runtime *)
      Option.value msg ~default:"JSON error"
    in
    raise (Of_string_error msg)

type error = Json.Decode.error =
  | Json_error of string
  | Unexpected_variant of string

exception Of_json_error = Json.Decode.DecodeError

let of_json_error msg = raise (Of_json_error (Json_error msg))

let unexpected_variant_error tag =
  raise (Of_json_error (Unexpected_variant tag))

module To_json = struct
  external string_to_json : string -> t = "%identity"
  external bool_to_json : bool -> t = "%identity"
  external int_to_json : int -> t = "%identity"

  let int64_to_json : int64 -> t = fun v -> Obj.magic (Int64.to_string v)

  external float_to_json : float -> t = "%identity"

  let unit_to_json () : t = Obj.magic Js.null

  let array_to_json v_to_json vs : t =
    let vs : Js.Json.t array = Js.Array.map ~f:v_to_json vs in
    Obj.magic vs

  let list_to_json v_to_json vs : t =
    let vs = Array.of_list vs in
    array_to_json v_to_json vs

  let option_to_json v_to_json v : t =
    match v with None -> Obj.magic Js.null | Some v -> v_to_json v

  let result_to_json a_to_json b_to_json v : t =
    match v with
    | Ok x -> Obj.magic [| string_to_json "Ok"; a_to_json x |]
    | Error x -> Obj.magic [| string_to_json "Error"; b_to_json x |]
end

module Of_json = struct
  let string_of_json (json : t) : string =
    if Js.typeof json = "string" then (Obj.magic json : string)
    else of_json_error "expected a string"

  let bool_of_json (json : t) : bool =
    if Js.typeof json = "boolean" then (Obj.magic json : bool)
    else of_json_error "expected a boolean"

  let is_int value =
    Js.Float.isFinite value && Js.Math.floor_float value == value

  let int_of_json (json : t) : int =
    if Js.typeof json = "number" then
      let v = (Obj.magic json : float) in
      if is_int v then (Obj.magic v : int)
      else of_json_error "expected an integer"
    else of_json_error "expected an integer"

  let int64_of_json (json : t) : int64 =
    if Js.typeof json = "string" then
      let v = (Obj.magic json : string) in
      match Int64.of_string_opt v with
      | Some v -> v
      | None -> of_json_error "expected int64 as string"
    else of_json_error "expected int64 as string"

  let float_of_json (json : t) : float =
    if Js.typeof json = "number" then (Obj.magic json : float)
    else of_json_error "expected a float"

  let unit_of_json (json : t) =
    if (Obj.magic json : 'a Js.null) == Js.null then ()
    else of_json_error "expected null"

  let array_of_json v_of_json (json : t) =
    if Js.Array.isArray json then
      let json = (Obj.magic json : Js.Json.t array) in
      Js.Array.map ~f:v_of_json json
    else of_json_error "expected a JSON array"

  let list_of_json v_of_json (json : t) =
    array_of_json v_of_json json |> Array.to_list

  let option_of_json v_of_json (json : t) =
    if (Obj.magic json : 'a Js.null) == Js.null then None
    else Some (v_of_json json)

  let result_of_json ok_of_json err_of_json (json : t) =
    if Js.Array.isArray json then
      let array = (Obj.magic json : Js.Json.t array) in
      let len = Js.Array.length array in
      if Stdlib.( > ) len 0 then
        let tag = Js.Array.unsafe_get array 0 in
        if Stdlib.( = ) (Js.typeof tag) "string" then
          let tag = (Obj.magic tag : string) in
          if Stdlib.( = ) tag "Ok" then (
            if Stdlib.( <> ) len 2 then
              of_json_error "expected a JSON array of length 2";
            Ok (ok_of_json (Js.Array.unsafe_get array 1)))
          else if Stdlib.( = ) tag "Error" then (
            if Stdlib.( <> ) len 2 then
              of_json_error "expected a JSON array of length 2";
            Error (err_of_json (Js.Array.unsafe_get array 1)))
          else of_json_error "invalid JSON"
        else
          of_json_error
            "expected a non empty JSON array with element being a string"
      else of_json_error "expected a non empty JSON array"
    else of_json_error "expected a non empty JSON array"
end

module Primitives = struct
  include Of_json
  include To_json
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
   fun json ->
    if (Obj.magic json : 'a Js.null) == Js.null then `Null
    else
      match Js.typeof json with
      | "string" -> `String (Obj.magic json : string)
      | "number" ->
          let v = (Obj.magic json : float) in
          if Of_json.is_int v then `Int (Obj.magic v : int) else `Float v
      | "boolean" -> `Bool (Obj.magic json : bool)
      | "object" ->
          if Js.Array.isArray json then
            let xs = Array.to_list (Obj.magic json : t array) in
            `List xs
          else
            let xs = Js.Dict.entries (Obj.magic json : t Js.Dict.t) in
            `Assoc (Array.to_list xs)
      | typ -> failwith ("unknown JSON value type: " ^ typ)
end
