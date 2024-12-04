#!/bin/bash

cat > main.ml
cat main.ml > main_js.ml

echo '(lang dune 3.11)
(implicit_transitive_deps false)
(using melange 0.1)
' > dune-project

echo '
(library
 (name lib)
 (modes melange)
 (modules main_js)
 (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
 (preprocess (pps melange.ppx melange-json.ppx)))
(melange.emit
 (alias js)
 (target output)
 (modules)
 (libraries lib)
 (module_systems commonjs))
(executable 
  (name main)
  (modules main)
  (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  (preprocess (pps melange-json-native.ppx)))
' > dune

echo '=== ppx output:native ==='
../native/ppx_deriving_json_native_test.exe main.ml
echo '=== ppx output:browser ==='
../browser/ppx_deriving_json_js_test.exe main_js.ml
echo '=== stdout:native ==='
dune exec ./main.exe || exit 1
echo '=== stdout:js ==='
dune build @js || exit 1
node ./_build/default/output/main_js.js
