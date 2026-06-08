
  $ echo '(lang dune 3.11)
  > (using melange 0.1)' > dune-project

  $ echo '
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules example example_json_string main)
  >  (libraries melange-json)
  >  (flags :standard -w -20-37-69 -open Melange_json.Primitives)
  >  (preprocess (pps melange.ppx melange-json.ppx)))
  > (melange.emit
  >  (alias js)
  >  (target output)
  >  (modules)
  >  (libraries lib)
  >  (module_systems commonjs))' > dune

  $ echo '
  > let () = print_endline "*** json deriver tests ***"
  > let () = Example.test ()
  > let () = print_endline "*** json_string deriver tests ***"
  > let () = Example_json_string.test ()
  > ' >> main.ml

  $ dune build @js
  File "example.ml", line 80, characters 37-77:
  80 |   C ({|["A",{"a":5,"extra":true}]|}, disallow_extra_fields_wrong_attr_of_json, disallow_extra_fields_wrong_attr_to_json, A {a=5});
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value disallow_extra_fields_wrong_attr_of_json
  [1]

  $ node ./_build/default/output/main.js
  node:internal/modules/cjs/loader:1368
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/output/main.js'
      at Function._resolveFilename (node:internal/modules/cjs/loader:1365:15)
      at defaultResolveImpl (node:internal/modules/cjs/loader:1021:19)
      at resolveForCJSWithHooks (node:internal/modules/cjs/loader:1026:22)
      at Function._load (node:internal/modules/cjs/loader:1175:37)
      at TracingChannel.traceSync (node:diagnostics_channel:322:14)
      at wrapModuleLoad (node:internal/modules/cjs/loader:235:24)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:171:5)
      at node:internal/main/run_main_module:36:49 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v22.18.0
  [1]
