update-test: ppx_deriving_json_native.t ppx_deriving_json_js.t

ppx_deriving_json_js.t: example.ml
	bash ./gen_test.sh ../browser/ppx_deriving_json_js_test.exe > $@

ppx_deriving_json_native.t: example.ml
	bash ./gen_test.sh ../native/ppx_deriving_json_native_test.exe > $@
