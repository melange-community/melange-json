

  $ echo 'type t = { a: int option; [@drop_default] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-41:
  1 | type t = { a: int option; [@drop_default] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default] requires either [@option] or [@default]
  [1]

  $ echo 'type t = { a: int option; [@option] [@default Some 0] [@drop_default] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-69:
  1 | type t = { a: int option; [@option] [@default Some 0] [@drop_default] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default] cannot be used with both [@option] and [@default]
  [1]

  $ echo 'type t = { a: int; [@drop_default (=)] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-38:
  1 | type t = { a: int; [@drop_default (=)] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default expr] requires [@default]
  [1]

  $ echo 'type t = { a: int option; [@option] [@drop_default (=)] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-55:
  1 | type t = { a: int option; [@option] [@drop_default (=)] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default expr] cannot be used with [@option]
  [1]

  $ echo 'type t = { a: int option; [@option] [@drop_default] [@drop_default_if_json_equal] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-81:
  1 | type t = { a: int option; [@option] [@drop_default] [@drop_default_if_json_equal] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default] and [@drop_default_if_json_equal] are mutually exclusive
  [1]

  $ echo 'type t = { a: int option; [@option] [@default Some 0] [@drop_default_if_json_equal] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-83:
  1 | type t = { a: int option; [@option] [@default Some 0] [@drop_default_if_json_equal] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default_if_json_equal] cannot be used with both [@option] and [@default]. Use [@json.default] only.
  [1]

  $ echo 'type t = { a: int option; [@option] [@drop_default_if_json_equal] } [@@deriving json]' | ../browser/ppx_deriving_json_js_test.exe -impl -
  File "-", line 1, characters 11-65:
  1 | type t = { a: int option; [@option] [@drop_default_if_json_equal] } [@@deriving json]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@drop_default_if_json_equal] cannot be used with [@option]. Use [@drop_default] instead.
  [1]
