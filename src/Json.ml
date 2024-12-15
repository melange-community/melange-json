type t = Js.Json.t
type json = t

let to_json t = t
let of_json t = t

type 'a of_json = Js.Json.t -> 'a
type 'a to_json = 'a -> Js.Json.t
type of_json_error = Json_error of string | Unexpected_variant of string

let of_json_error_to_string = function
  | Json_error msg -> msg
  | Unexpected_variant tag -> "unexpected variant: " ^ tag

exception Of_json_error of of_json_error

let of_json_error msg = raise (Of_json_error (Json_error msg))
let to_string t = Js.Json.stringify t

exception Of_string_error of string

external _unsafeCreateUninitializedArray : int -> 'a array = "Array"
[@@mel.new]

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

module Of_json = struct
  let string (json : t) : string =
    if Js.typeof json = "string" then (Obj.magic json : string)
    else of_json_error ("Expected string, got " ^ Js.Json.stringify json)

  let char (json : t) =
    if Js.typeof json = "string" then
      let s = (Obj.magic json : string) in
      if String.length s = 1 then String.get s 0
      else
        of_json_error
          ("Expected single-character string, got "
          ^ Js.Json.stringify json)
    else of_json_error ("Expected string, got " ^ Js.Json.stringify json)

  let bool (json : t) : bool =
    if Js.typeof json = "boolean" then (Obj.magic json : bool)
    else of_json_error ("Expected boolean, got " ^ Js.Json.stringify json)

  let is_int value =
    Js.Float.isFinite value && Js.Math.floor_float value == value

  let int (json : t) : int =
    if Js.typeof json = "number" then
      let v = (Obj.magic json : float) in
      if is_int v then (Obj.magic v : int)
      else
        of_json_error ("Expected integer, got " ^ Js.Json.stringify json)
    else of_json_error ("Expected number, got " ^ Js.Json.stringify json)

  let int64 (json : t) : int64 =
    if Js.typeof json = "string" then
      let v = (Obj.magic json : string) in
      match Int64.of_string_opt v with
      | Some v -> v
      | None ->
          of_json_error
            ("Expected int64 as string, got " ^ Js.Json.stringify json)
    else
      of_json_error
        ("Expected int64 as string, got " ^ Js.Json.stringify json)

  let float (json : t) : float =
    if Js.typeof json = "number" then (Obj.magic json : float)
    else of_json_error ("Expected number, got " ^ Js.Json.stringify json)

  let unit (json : t) : unit =
    if (Obj.magic json : 'a Js.null) == Js.null then ()
    else
      of_json_error
        ("Expected null as unit, got " ^ Js.Json.stringify json)

  let array v_of_json (json : t) =
    if Js.Array.isArray json then (
      let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
      let length = Js.Array.length source in
      let target = _unsafeCreateUninitializedArray length in
      for i = 0 to length - 1 do
        let value =
          try v_of_json (Array.unsafe_get source i)
          with Of_json_error (Json_error err) ->
            of_json_error
              (err ^ "\n\tin array at index " ^ string_of_int i)
        in
        Array.unsafe_set target i value
      done;
      target)
    else of_json_error ("Expected array, got " ^ Js.Json.stringify json)

  let list v_of_json (json : t) : _ list =
    array v_of_json json |> Array.to_list

  let option v_of_json (json : t) : _ option =
    if (Obj.magic json : 'a Js.null) == Js.null then None
    else Some (v_of_json json)

  let js_null v_of_json (json : t) : _ Js.null =
    if (Obj.magic json : 'a Js.null) == Js.null then Js.null
    else Js.Null.return (v_of_json json)

  let js_date json : Js.Date.t = Js.Date.fromString (string json)

  let tuple2 decodeA decodeB json : _ * _ =
    if Js.Array.isArray json then
      let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
      let length = Js.Array.length source in
      if length = 2 then
        try
          ( decodeA (Array.unsafe_get source 0),
            decodeB (Array.unsafe_get source 1) )
        with Of_json_error err ->
          of_json_error
            (of_json_error_to_string err ^ "\n\tin pair/tuple2")
      else
        let length = Js.String.make length in
        of_json_error
          {j|Expected array of length 2 as tuple, got array of length $length|j}
    else
      of_json_error
        ("Expected array as tuple, got " ^ Js.Json.stringify json)

  let tuple3 decodeA decodeB decodeC json : _ * _ * _ =
    if Js.Array.isArray json then
      let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
      let length = Js.Array.length source in
      if length = 3 then
        try
          ( decodeA (Array.unsafe_get source 0),
            decodeB (Array.unsafe_get source 1),
            decodeC (Array.unsafe_get source 2) )
        with Of_json_error err ->
          of_json_error (of_json_error_to_string err ^ "\n\tin tuple3")
      else
        let length = Js.String.make length in
        of_json_error
          {j|Expected array of length 3 as tuple, got array of length $length|j}
    else
      of_json_error
        ("Expected array as tuple, got " ^ Js.Json.stringify json)

  let tuple4 decodeA decodeB decodeC decodeD json : _ * _ * _ * _ =
    if Js.Array.isArray json then
      let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
      let length = Js.Array.length source in
      if length = 4 then
        try
          ( decodeA (Array.unsafe_get source 0),
            decodeB (Array.unsafe_get source 1),
            decodeC (Array.unsafe_get source 2),
            decodeD (Array.unsafe_get source 3) )
        with Of_json_error err ->
          of_json_error (of_json_error_to_string err ^ "\n\tin tuple4")
      else
        let length = Js.String.make length in
        of_json_error
          {j|Expected array of length 4 as tuple, got array of length $length|j}
    else
      of_json_error
        ("Expected array as tuple, got " ^ Js.Json.stringify json)

  let js_dict decode json : _ Js.Dict.t =
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
          with Of_json_error err ->
            of_json_error
              (of_json_error_to_string err
              ^ "\n\tin object at key '"
              ^ key
              ^ "'")
        in
        Js.Dict.set target key value
      done;
      target)
    else
      of_json_error
        ("Expected object as dict, got " ^ Js.Json.stringify json)

  let result ok_of_json err_of_json (json : t) : (_, _) result =
    if Js.Array.isArray json then
      let array = (Obj.magic json : Js.Json.t array) in
      let len = Js.Array.length array in
      if Stdlib.( > ) len 0 then
        let tag = Js.Array.unsafe_get array 0 in
        if Stdlib.( = ) (Js.typeof tag) "string" then
          let tag = (Obj.magic tag : string) in
          if Stdlib.( = ) tag "Ok" then (
            if Stdlib.( <> ) len 2 then
              of_json_error
                ("Expected array of length 2 as result 'Ok', got "
                ^ Js.Json.stringify json);
            Ok (ok_of_json (Js.Array.unsafe_get array 1)))
          else if Stdlib.( = ) tag "Error" then (
            if Stdlib.( <> ) len 2 then
              of_json_error
                ("Expected array of length 2 as result 'Error', got "
                ^ Js.Json.stringify json);
            Error (err_of_json (Js.Array.unsafe_get array 1)))
          else of_json_error "invalid JSON"
        else
          of_json_error
            ("Expected non-empty array with element being a string, got "
            ^ Js.Json.stringify json)
      else
        of_json_error
          ("Expected non-empty array, got " ^ Js.Json.stringify json)
    else
      of_json_error
        ("Expected a non-empty array, got " ^ Js.Json.stringify json)

  let at' key decode json =
    if
      Js.typeof json = "object"
      && (not (Js.Array.isArray json))
      && not ((Obj.magic json : 'a Js.null) == Js.null)
    then
      let dict = (Obj.magic (json : Js.Json.t) : Js.Json.t Js.Dict.t) in
      match Js.Dict.get dict key with
      | Some value -> (
          try decode value
          with Of_json_error err ->
            of_json_error
              (of_json_error_to_string err ^ "\n\tat field '" ^ key ^ "'")
          )
      | None -> of_json_error {j|Expected field '$(key)'|j}
    else of_json_error ("Expected object, got " ^ Js.Json.stringify json)

  let rec at key_path decoder =
    match key_path with
    | [ key ] -> at' key decoder
    | first :: rest -> at' first (at rest decoder)
    | [] ->
        raise
        @@ Invalid_argument
             "Expected key_path to contain at least one element"

  let one_of decoders json =
    let rec inner decoders errors =
      match decoders with
      | [] ->
          let formattedErrors =
            "\n- "
            ^ Js.Array.join ~sep:"\n- " (Array.of_list (List.rev errors))
          in
          of_json_error
            ({j|All decoders given to oneOf failed. Here are all the errors: $formattedErrors\nAnd the JSON being decoded: |j}
            ^ Js.Json.stringify json)
      | decode :: rest -> (
          try decode json
          with Of_json_error e -> inner rest (e :: errors))
    in
    inner decoders []

  let either a b = one_of [ a; b ]

  let try_or_none decode json =
    try Some (decode json) with Of_json_error _ -> None

  let try_of_default default decode json =
    try decode json with Of_json_error _ -> default

  let map f decode json = f (decode json)
end

module To_json = struct
  external string : string -> t = "%identity"
  external bool : bool -> t = "%identity"
  external int : int -> t = "%identity"

  let int64 : int64 -> t = fun v -> Obj.magic (Int64.to_string v)

  external float : float -> t = "%identity"

  let unit () : t = Obj.magic Js.null

  let array v_to_json vs : t =
    let vs : Js.Json.t array = Js.Array.map ~f:v_to_json vs in
    Obj.magic vs

  let list v_to_json vs : t =
    let vs = Array.of_list vs in
    array v_to_json vs

  let option v_to_json v : t =
    match v with None -> Obj.magic Js.null | Some v -> v_to_json v

  let result a_to_json b_to_json v : t =
    match v with
    | Ok x -> Obj.magic [| string "Ok"; a_to_json x |]
    | Error x -> Obj.magic [| string "Error"; b_to_json x |]

  let char c = string (String.make 1 c)
  let js_date d = string (Js.Date.toJSONUnsafe d)

  let js_null v_to_json v =
    match Js.Null.toOption v with
    | None -> Obj.magic Js.null
    | Some v -> v_to_json v

  external json_dict : Js.Json.t Js.Dict.t -> Js.Json.t = "%identity"

  let js_dict encode d =
    let pairs = Js.Dict.entries d in
    let encodedPairs = Array.map (fun (k, v) -> k, encode v) pairs in
    json_dict (Js.Dict.fromArray encodedPairs)

  external json_array : Js.Json.t array -> Js.Json.t = "%identity"

  let tuple2 encodeA encodeB (a, b) =
    json_array [| encodeA a; encodeB b |]

  let tuple3 encodeA encodeB encodeC (a, b, c) =
    json_array [| encodeA a; encodeB b; encodeC c |]

  let tuple4 encodeA encodeB encodeC encodeD (a, b, c, d) =
    json_array [| encodeA a; encodeB b; encodeC c; encodeD d |]

  external string_array : string array -> Js.Json.t = "%identity"
  external float_array : float array -> Js.Json.t = "%identity"
  external int_array : int array -> Js.Json.t = "%identity"
  external bool_array : bool array -> Js.Json.t = "%identity"
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

module Decode = struct
  type 'a decoder = 'a of_json

  let id json = json
  let bool = Of_json.bool
  let float = Of_json.float
  let int = Of_json.int
  let string = Of_json.string
  let char = Of_json.char
  let date json = Of_json.js_date json
  let nullable = Of_json.js_null
  let array = Of_json.array
  let list = Of_json.list
  let pair = Of_json.tuple2
  let tuple2 = Of_json.tuple2
  let tuple3 = Of_json.tuple3
  let tuple4 = Of_json.tuple4
  let dict = Of_json.js_dict
  let field = Of_json.at'
  let at = Of_json.at
  let optional = Of_json.try_or_none
  let withDefault = Of_json.try_of_default
  let oneOf = Of_json.one_of
  let either = Of_json.either
  let map = Of_json.map
  let andThen b a json = b (a json) json

  let nullAs value json =
    if (Obj.magic json : 'a Js.null) == Js.null then value
    else of_json_error "Expected null"
end

module Encode = struct
  type 'a encoder = 'a to_json

  external null : t = "null"

  let string = To_json.string
  let float = To_json.float
  let int = To_json.int
  let bool = To_json.bool
  let char = To_json.char
  let date = To_json.js_date
  let list = To_json.list
  let array = To_json.array
  let nullable = To_json.option
  let withDefault d encode = function None -> d | Some v -> encode v
  let jsonDict = To_json.json_dict
  let dict = To_json.js_dict
  let object_ props = To_json.json_dict (Js.Dict.fromList props)
  let jsonArray = To_json.json_array
  let pair = To_json.tuple2
  let tuple2 = To_json.tuple2
  let tuple3 = To_json.tuple3
  let tuple4 = To_json.tuple4
  let stringArray = To_json.string_array
  let numberArray = To_json.float_array
  let boolArray = To_json.bool_array
end

exception ParseError = Of_string_error

let parse s = try Some (Js.Json.parseExn s) with _ -> None

let parseOrRaise s =
  try Js.Json.parseExn s
  with Js.Exn.Error e ->
    let message =
      match Js.Exn.message e with Some m -> m | None -> "Unknown error"
    in
    raise @@ ParseError message

external stringify : Js.Json.t -> string = "JSON.stringify"
