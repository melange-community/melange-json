
  $ echo '(lang dune 3.11)
  > (implicit_transitive_deps false)
  > ' >> dune-project
  $ echo '
  > (executable 
  >   (name main)
  >   (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  >   (preprocess (pps melange-json-native.ppx)))' > dune

  $ echo '
  > open Example
  > let () = Cases.run ()
  >   ~json_to_string:Yojson.Basic.to_string
  >   ~json_of_string:Yojson.Basic.from_string
  > ' >> main.ml

  $ dune build ./main.exe
  File "example.ml", line 1, characters 0-33:
  1 | type user = int [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module Yojson
  [1]

  $ dune exec ./main.exe
  File "example.ml", line 1, characters 0-33:
  1 | type user = int [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module Yojson
  [1]
