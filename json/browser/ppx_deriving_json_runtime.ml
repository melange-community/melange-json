type t = Js.Json.t

let to_json t = t
let of_json t = t
let of_json_error msg = Js.Exn.raiseError msg

module To_json = struct
  external string_to_json : string -> t = "%identity"
  external bool_to_json : bool -> t = "%identity"
  external int_to_json : int -> t = "%identity"
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
end

module Primitives = struct
  include Of_json
  include To_json
end
