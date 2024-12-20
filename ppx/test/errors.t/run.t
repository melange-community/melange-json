  $ dune build ./prettify.exe
$ ocamlopt -dsource _build/default/prettify.pp.ml
  $ dune exec ./prettify.exe -- ok.json
  {
    "a": [ "Foo" ],
    "foo": [ [ "A" ], [ "Foo" ], [ "B", 123 ], [ "C", 234, "hello" ] ],
    "b": "where are you ?",
    "c": [ 123, 234, 345 ],
    "d": [ 123, [ 1.2, 2.3, 2.4 ], "i am here" ]
  }
  $ dune exec ./prettify.exe -- tag_as_string.json
  Fatal error: exception  exception Ppx_deriving_json_runtime.Of_json_error(Json_error {|expected ["A"] or ["Foo"] or ["B", _] or ["C", _, _] or ["D", { _ }] but got "A"|})
  [2]
  $ dune exec ./prettify.exe -- wrong_core_type.json
  Fatal error: exception  exception Ppx_deriving_json_runtime.Of_json_error(Json_error {|expected int but got string: "i am a string"|})
  [2]
  $ dune exec ./prettify.exe -- deep_culprit.json
  Fatal error: exception  exception Ppx_deriving_json_runtime.Of_json_error(Json_error {|expected string but got object: {"a": _, "foo": _, "b": _, "c": _, "d": _, }|})
  [2]

  $ dune exec ./prettify.exe -- wide_culprit.json
  Fatal error: exception  exception Ppx_deriving_json_runtime.Of_json_error(Json_error {|expected string but got array: [123, 234, 345, 123, 234, 345, 123, 234, ...]|})
  [2]
