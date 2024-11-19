
  $ echo '(lang dune 3.11)
  > (implicit_transitive_deps false)
  > ' >> dune-project
  $ echo '
  > (executable 
  >   (name main)
  >   (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  >   (preprocess (pps melange-json-native.ppx)))' > dune

  $ echo '
  > let () = Example.test ()
  > let () = print_endline "*** json_string deriver tests ***"
  > let () = Example_json_string.test ()
  > ' >> main.ml

  $ dune build ./main.exe
  File "example.ml", line 24, characters 27-32:
  24 | type array_list = { a: int array; b: int list} [@@deriving json]
                                  ^^^^^
  Error: Unbound value array_of_json
  Hint: Did you mean param_of_json?
  [1]

  $ dune exec ./main.exe
  File "example.ml", line 24, characters 27-32:
  24 | type array_list = { a: int array; b: int list} [@@deriving json]
                                  ^^^^^
  Error: Unbound value array_of_json
  Hint: Did you mean param_of_json?
  [1]
