external _unsafeCreateUninitializedArray : int -> 'a array = "Array"
[@@mel.new]

external _stringify : Js.Json.t -> string = "JSON.stringify"

let _isInteger value =
  Js.Float.isFinite value && Js.Math.floor_float value == value

type 'a decoder = Js.Json.t -> 'a
type error = Json_error of string | Unexpected_variant of string

let error_to_string = function
  | Json_error msg -> msg
  | Unexpected_variant tag -> "unexpected variant: " ^ tag

exception DecodeError of error

let error msg = raise (DecodeError (Json_error msg))
let id json = json

let bool json =
  if Js.typeof json = "boolean" then (Obj.magic (json : Js.Json.t) : bool)
  else error ("Expected boolean, got " ^ _stringify json)

let float json =
  if Js.typeof json = "number" then (Obj.magic (json : Js.Json.t) : float)
  else error ("Expected number, got " ^ _stringify json)

let int json =
  let f = float json in
  if _isInteger f then (Obj.magic (f : float) : int)
  else error ("Expected integer, got " ^ _stringify json)

let string json =
  if Js.typeof json = "string" then
    (Obj.magic (json : Js.Json.t) : string)
  else error ("Expected string, got " ^ _stringify json)

let char json =
  let s = string json in
  if String.length s = 1 then String.get s 0
  else error ("Expected single-character string, got " ^ _stringify json)

let date json = json |> string |> Js.Date.fromString

let nullable decode json =
  if (Obj.magic json : 'a Js.null) == Js.null then Js.null
  else Js.Null.return (decode json)

(* TODO: remove this? *)
let nullAs value json =
  if (Obj.magic json : 'a Js.null) == Js.null then value
  else error ("Expected null, got " ^ _stringify json)

let array decode json =
  if Js.Array.isArray json then (
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    let length = Js.Array.length source in
    let target = _unsafeCreateUninitializedArray length in
    for i = 0 to length - 1 do
      let value =
        try decode (Array.unsafe_get source i)
        with DecodeError err ->
          error
            (error_to_string err
            ^ "\n\tin array at index "
            ^ string_of_int i)
      in
      Array.unsafe_set target i value
    done;
    target)
  else error ("Expected array, got " ^ _stringify json)

let list decode json = json |> array decode |> Array.to_list

let pair decodeA decodeB json =
  if Js.Array.isArray json then
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    let length = Js.Array.length source in
    if length = 2 then
      try
        ( decodeA (Array.unsafe_get source 0),
          decodeB (Array.unsafe_get source 1) )
      with DecodeError err ->
        error (error_to_string err ^ "\n\tin pair/tuple2")
    else
      let length = Js.String.make length in
      error {j|Expected array of length 2, got array of length $length|j}
  else error ("Expected array, got " ^ _stringify json)

let tuple2 = pair

let tuple3 decodeA decodeB decodeC json =
  if Js.Array.isArray json then
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    let length = Js.Array.length source in
    if length = 3 then
      try
        ( decodeA (Array.unsafe_get source 0),
          decodeB (Array.unsafe_get source 1),
          decodeC (Array.unsafe_get source 2) )
      with DecodeError err ->
        error (error_to_string err ^ "\n\tin tuple3")
    else
      let length = Js.String.make length in
      error {j|Expected array of length 3, got array of length $length|j}
  else error ("Expected array, got " ^ _stringify json)

let tuple4 decodeA decodeB decodeC decodeD json =
  if Js.Array.isArray json then
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    let length = Js.Array.length source in
    if length = 4 then
      try
        ( decodeA (Array.unsafe_get source 0),
          decodeB (Array.unsafe_get source 1),
          decodeC (Array.unsafe_get source 2),
          decodeD (Array.unsafe_get source 3) )
      with DecodeError err ->
        error (error_to_string err ^ "\n\tin tuple4")
    else
      let length = Js.String.make length in
      error {j|Expected array of length 4, got array of length $length|j}
  else error ("Expected array, got " ^ _stringify json)

let dict decode json =
  if
    Js.typeof json = "object"
    && (not (Js.Array.isArray json))
    && not ((Obj.magic json : 'a Js.null) == Js.null)
  then (
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t Js.Dict.t) in
    let keys = Js.Dict.keys source in
    let l = Js.Array.length keys in
    let target = Js.Dict.empty () in
    for i = 0 to l - 1 do
      let key = Array.unsafe_get keys i in
      let value =
        try decode (Js.Dict.unsafeGet source key)
        with DecodeError err ->
          error (error_to_string err ^ "\n\tin dict")
      in
      Js.Dict.set target key value
    done;
    target)
  else error ("Expected object, got " ^ _stringify json)

let field key decode json =
  if
    Js.typeof json = "object"
    && (not (Js.Array.isArray json))
    && not ((Obj.magic json : 'a Js.null) == Js.null)
  then
    let dict = (Obj.magic (json : Js.Json.t) : Js.Json.t Js.Dict.t) in
    match Js.Dict.get dict key with
    | Some value -> (
        try decode value
        with DecodeError err ->
          error (error_to_string err ^ "\n\tat field '" ^ key ^ "'"))
    | None -> error {j|Expected field '$(key)'|j}
  else error ("Expected object, got " ^ _stringify json)

let rec at key_path decoder =
  match key_path with
  | [ key ] -> field key decoder
  | first :: rest -> field first (at rest decoder)
  | [] ->
      raise
      @@ Invalid_argument
           "Expected key_path to contain at least one element"

let optional decode json =
  try Some (decode json) with DecodeError _ -> None

let oneOf decoders json =
  let rec inner decoders errors =
    match decoders with
    | [] ->
        let formattedErrors =
          "\n- "
          ^ Js.Array.join ~sep:"\n- " (Array.of_list (List.rev_map error_to_string errors))
        in
        error
          ({j|All decoders given to oneOf failed. Here are all the errors: $formattedErrors\nAnd the JSON being decoded: |j}
          ^ _stringify json)
    | decode :: rest -> (
        try decode json with DecodeError e -> inner rest (e :: errors))
  in
  inner decoders []

let either a b = oneOf [ a; b ]

let withDefault default decode json =
  try decode json with DecodeError _ -> default

let map f decode json = f (decode json)
let andThen b a json = b (a json) json
