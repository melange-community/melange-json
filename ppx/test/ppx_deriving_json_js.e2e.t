
  $ echo '(lang dune 3.11)
  > (using melange 0.1)' > dune-project

  $ echo '
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules example example_json_string main)
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

  $ node ./_build/default/output/main.js
  *** json deriver tests ***
  JSON    DATA: 1
  JSON REPRINT: 1
  JSON    DATA: "9223372036854775807"
  JSON REPRINT: "9223372036854775807"
  JSON    DATA: 1.1
  JSON REPRINT: 1.1
  JSON    DATA: 1.0
  JSON REPRINT: 1
  JSON    DATA: 42
  JSON REPRINT: 42
  JSON    DATA: "OK"
  JSON REPRINT: "OK"
  JSON    DATA: "some"
  JSON REPRINT: "some"
  JSON    DATA: ["Ok", 1]
  JSON REPRINT: ["Ok",1]
  JSON    DATA: ["Error", "oops"]
  JSON REPRINT: ["Error","oops"]
  JSON    DATA: [42, "works"]
  JSON REPRINT: [42,"works"]
  JSON    DATA: {"name":"N","age":1}
  JSON REPRINT: {"name":"N","age":1}
  JSON    DATA: ["A"]
  JSON REPRINT: "A"
  JSON    DATA: ["B", 42]
  JSON REPRINT: ["B",42]
  JSON    DATA: ["C", {"name": "cname"}]
  JSON REPRINT: ["C",{"name":"cname"}]
  JSON    DATA: ["A"]
  JSON REPRINT: "A"
  JSON    DATA: ["S2", 42, "hello"]
  JSON REPRINT: ["S2",42,"hello"]
  JSON    DATA: ["B", 42]
  JSON REPRINT: ["B",42]
  JSON    DATA: ["C"]
  JSON REPRINT: "C"
  JSON    DATA: ["P2", 42, "hello"]
  JSON REPRINT: ["P2",42,"hello"]
  JSON    DATA: ["Fix",["Fix",["Fix",["A"]]]]
  JSON REPRINT: ["Fix",["Fix",["Fix","A"]]]
  JSON    DATA: ["Fix",["Fix",["Fix",["A"]]]]
  JSON REPRINT: ["Fix",["Fix",["Fix","A"]]]
  JSON    DATA: ["A"]
  JSON REPRINT: "A"
  JSON    DATA: ["b_aliased"]
  JSON REPRINT: "b_aliased"
  JSON    DATA: ["b"]
  JSON REPRINT: "b"
  JSON    DATA: ["A_aliased"]
  JSON REPRINT: "A_aliased"
  JSON    DATA: {"my_name":"N","my_age":1}
  JSON REPRINT: {"my_name":"N","my_age":1}
  JSON    DATA: {"my_name":"N"}
  JSON REPRINT: {"my_name":"N","my_age":100}
  JSON    DATA: {}
  JSON REPRINT: {"k":null}
  JSON    DATA: {"k":42}
  JSON REPRINT: {"k":42}
  JSON    DATA: ["A",1]
  JSON REPRINT: ["A",1]
  JSON    DATA: ["B","ok"]
  JSON REPRINT: ["B","ok"]
  JSON    DATA: {"a":1,"b":2}
  JSON REPRINT: {"a":1}
  JSON    DATA: ["A",{"a":1,"b":2}]
  JSON REPRINT: ["A",{"a":1}]
  JSON    DATA: {"a":1}
  JSON REPRINT: {"a":1}
  JSON    DATA: {"a":1,"b_opt":2}
  JSON REPRINT: {"a":1,"b_opt":2}
  JSON    DATA: {"a":[1],"b":[2]}
  JSON REPRINT: {"a":[1],"b":[2]}
  JSON    DATA: ["Circle", 5.0]
  JSON REPRINT: ["Circle",5]
  JSON    DATA: ["Rectangle", 10.0, 20.0]
  JSON REPRINT: ["Rectangle",10,20]
  JSON    DATA: ["Point", {"x": 1.0, "y": 2.0}]
  JSON REPRINT: ["Point",{"x":1,"y":2}]
  JSON    DATA: ["Empty"]
  JSON REPRINT: "Empty"
  JSON    DATA: "Empty"
  JSON REPRINT: "Empty"
  
  Testing error cases:
  ERROR CASE DATA: 42
  Got expected error: expected a JSON object but got 42
  ERROR CASE DATA: [1]
  Got expected error: expected a JSON array of length 2 but got [1]
  ERROR CASE DATA: [1,2,3]
  Got expected error: expected a JSON array of length 2 but got [1, 2, 3]
  ERROR CASE DATA: [1,2]
  Got expected error: expected a JSON array of length 3 but got [1, 2]
  ERROR CASE DATA: [1,2,3,4]
  Got expected error: expected a JSON array of length 3 but got [1, 2, 3, 4]
  ERROR CASE DATA: 42
  Got expected error: expected a non empty JSON array but got 42
  ERROR CASE DATA: "Yellow"
  Got expected error: expected a non empty JSON array but got "Yellow"
  ERROR CASE DATA: ["Circle"]
  Got expected error: expected a JSON array of length 2 but got ["Circle"]
  ERROR CASE DATA: ["Rectangle", 10.0]
  Got expected error: expected a JSON array of length 3 but got ["Rectangle", 10]
  ERROR CASE DATA: ["Point", 1.0, 2.0]
  Got expected error: expected a JSON array of length 2 but got ["Point", 1, 2]
  *** json_string deriver tests ***
  ** To_json_string **
  A 42 -> ["A",42]
  B false -> ["B",false]
  ** Of_json_string **
  ["A", 42] = A 42 -> true
  ["B", false] = B false -> true
  ** Json_string **
  A 42 -> ["A",42]
  B false -> ["B",false]
  ["A", 42] = A 42 -> true
  ["B", false] = B false -> true
