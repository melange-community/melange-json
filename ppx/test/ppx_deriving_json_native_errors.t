

  $ echo 'type t = { a: int option; [@drop_default] } [@@deriving json]' | ../native/ppx_deriving_json_native_test.exe -impl -
  File "-", line 1, characters 11-41:
  1 | type t = { a: int option; [@drop_default] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: found [@drop_default] attribute without [@option]
  [1]

