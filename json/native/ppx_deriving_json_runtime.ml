type t = Yojson.Basic.t

let to_json t = t
let of_json t = t

exception Of_json_error of string

let of_json_error msg = raise (Of_json_error msg)

module To_json = struct
  let string_to_json v = `String v
  let bool_to_json v = `Bool v
  let int_to_json v = `Int v
  let float_to_json v = `Float v
  let unit_to_json () = `Null
  let list_to_json v_to_json vs = `List (List.map v_to_json vs)

  let option_to_json v_to_json = function
    | None -> `Null
    | Some v -> v_to_json v
end

module Of_json = struct
  let string_of_json = Yojson.Basic.Util.to_string
  let bool_of_json = Yojson.Basic.Util.to_bool
  let int_of_json = Yojson.Basic.Util.to_int
  let float_of_json = Yojson.Basic.Util.to_float

  let unit_of_json = function
    | `Null -> ()
    | _ -> of_json_error "expected null"

  let option_of_json v_of_json = Yojson.Basic.Util.to_option v_of_json

  let list_of_json v_of_json json =
    List.map v_of_json (Yojson.Basic.Util.to_list json)
end

module Primitives = struct
  include To_json
  include Of_json
end
