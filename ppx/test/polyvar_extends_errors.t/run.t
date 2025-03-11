  $ dune build ./prettify.exe
Uncomment to debug
$ ocamlopt -dsource _build/default/prettify.pp.ml
  $ dune exec ./prettify.exe -- ok_parent.json
  { "extended": [ "P" ], "not_extended": [ "C", 123, "hello" ] }
  $ dune exec ./prettify.exe -- ok_child.json
  { "extended": [ "C", 123, "hello" ], "not_extended": [ "C", 123, "hello" ] }
  $ dune exec ./prettify.exe -- wrong_parent_tag.json
  Fatal error: exception Melange_json__Errors.Internal_unexpected_variant("unexpected variant")
  [2]
  $ dune exec ./prettify.exe -- wrong_child_tag.json
  Fatal error: exception Melange_json__Errors.Internal_unexpected_variant("unexpected variant")
  [2]
