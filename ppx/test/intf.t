
  $ echo "type ('a, 'b) either [@@deriving json]" | ../browser/ppx_deriving_json_js_test.exe -intf -
  type ('a, 'b) either[@@deriving json]
  include
    sig
      [@@@ocaml.warning "-32"]
      val either_of_json :
        (Js.Json.t -> 'a) -> (Js.Json.t -> 'b) -> Js.Json.t -> ('a, 'b) either
      val either_to_json :
        ('a -> Js.Json.t) -> ('b -> Js.Json.t) -> ('a, 'b) either -> Js.Json.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ echo "type ('a, 'b) either [@@deriving json]" | ../native/ppx_deriving_json_native_test.exe -intf -
  type ('a, 'b) either[@@deriving json]
  include
    sig
      [@@@ocaml.warning "-32"]
      val either_of_json :
        (Yojson.Basic.t -> 'a) ->
          (Yojson.Basic.t -> 'b) -> Yojson.Basic.t -> ('a, 'b) either
      val either_to_json :
        ('a -> Yojson.Basic.t) ->
          ('b -> Yojson.Basic.t) -> ('a, 'b) either -> Yojson.Basic.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
