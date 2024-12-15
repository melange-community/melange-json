[@@@alert "-deprecated"]

type t = Js.Json.t

let to_json t = t
let of_json t = t
let to_string t = Js.Json.stringify t

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
    raise (Json.Of_string_error msg)

type error = Json.of_json_error =
  | Json_error of string
  | Unexpected_variant of string

exception Of_json_error = Json.Of_json_error

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
  let string_of_json = Json.Of_json.string
  let bool_of_json = Json.Of_json.bool
  let int_of_json = Json.Of_json.int
  let int64_of_json = Json.Of_json.int64
  let float_of_json = Json.Of_json.float
  let unit_of_json = Json.Of_json.unit
  let array_of_json = Json.Of_json.array
  let list_of_json = Json.Of_json.list
  let option_of_json = Json.Of_json.option
  let result_of_json = Json.Of_json.result
end

module Primitives = struct
  include Of_json
  include To_json
end

module Classify = struct
  (* This function is also defined in `Json` module, but not exposed on its mli *)
  let is_int value =
    Js.Float.isFinite value && Js.Math.floor_float value == value

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
          if is_int v then `Int (Obj.magic v : int) else `Float v
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
