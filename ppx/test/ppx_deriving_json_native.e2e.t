
  $ echo '(lang dune 3.11)
  > (implicit_transitive_deps false)
  > ' >> dune-project
  $ echo '
  > (executable
  >   (name main)
  >   (flags :standard -w -37-69 -open Melange_json.Primitives)
  >   (preprocess (pps melange-json-native.ppx)))' > dune

  $ echo '
  > let () = Example.test ()
  > let () = print_endline "*** json_string deriver tests ***"
  > let () = Example_json_string.test ()
  > ' >> main.ml

  $ dune build ./main.exe
  File "example.ml", line 80, characters 37-77:
  80 |   C ({|["A",{"a":5,"extra":true}]|}, disallow_extra_fields_wrong_attr_of_json, disallow_extra_fields_wrong_attr_to_json, A {a=5});
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value disallow_extra_fields_wrong_attr_of_json
  [1]

  $ dune exec ./main.exe
  File "example.ml", line 80, characters 37-77:
  80 |   C ({|["A",{"a":5,"extra":true}]|}, disallow_extra_fields_wrong_attr_of_json, disallow_extra_fields_wrong_attr_to_json, A {a=5});
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value disallow_extra_fields_wrong_attr_of_json
  [1]
