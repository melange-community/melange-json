
  $ echo '(lang dune 3.11)
  > (using melange 0.1)
  > (implicit_transitive_deps false)
  > ' >> dune-project

  $ echo '
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules example main)
  >  (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  >  (preprocess (pps melange-json.ppx)))
  > (melange.emit
  >  (alias js)
  >  (target output)
  >  (modules)
  >  (libraries lib)
  >  (module_systems commonjs))' > dune

  $ echo '
  > open Example
  > let () = Cases.run ()
  >   ~json_to_string:Js.Json.stringify
  >   ~json_of_string:Js.Json.parseExn
  > ' >> main.ml

  $ dune build @js
  File "example.ml", line 6, characters 0-60:
  6 | type record = { name : string; age : int } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 6, characters 0-60:
  6 | type record = { name : string; age : int } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 7, characters 0-132:
  7 | type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 7, characters 0-132:
  7 | type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 8, characters 0-70:
  8 | type record_opt = { k : int option; [@json.option] } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 9, characters 26-27:
  9 | type sum = A | B of int | C of { name : string } [@@deriving json]
                                ^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 17, characters 0-80:
  17 | type allow_extra_fields = {a: int} [@@deriving json] [@@json.allow_extra_fields]
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 18, characters 27-28:
  18 | type allow_extra_fields2 = A of {a: int} [@json.allow_extra_fields] [@@deriving json]
                                  ^
  Alert unprocessed: `[@mel.*]' attributes found in external declaration. Did you forget to preprocess with `melange.ppx'?
  
  File "example.ml", line 6, characters 0-60:
  6 | type record = { name : string; age : int } [@@deriving json]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: ## is not a valid value identifier.
  [1]

  $ node ./_build/default/output/main.js
  node:internal/modules/cjs/loader:1137
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/output/main.js'
      at Module._resolveFilename (node:internal/modules/cjs/loader:1134:15)
      at Module._load (node:internal/modules/cjs/loader:975:27)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:128:12)
      at node:internal/main/run_main_module:28:49 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v18.19.0
  [1]
