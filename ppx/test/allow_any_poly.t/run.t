  $ dune build ./prettify.exe
Uncomment to debug
$ ocamlopt -dsource _build/default/prettify.pp.ml
  $ dune exec ./prettify.exe -- '{ "a": ["Foo"] }'
  got Foo
  { "a": "Foo" }
  $ dune exec ./prettify.exe -- '{ "a": [ "Bar" ] }'
  got Other
  { "a": [ "Bar" ] }
  $ dune exec ./prettify.exe -- '{ "a": [ "Foo", 1, 2, 3 ] }'
  got Other
  { "a": [ "Foo", 1, 2, 3 ] }




