

  $ echo 'type t = { a: int option; [@drop_default] } [@@deriving json]' | ../native/ppx_deriving_json_native_test.exe -impl -
  Fatal error: exception Ppx_deriving_json_native__Ppx_deriving_tools.Error(_, "found [@drop_default] attribute without [@option]")
  [2]

