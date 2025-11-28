type user = int [@@deriving json]
type userid = int64 [@@deriving json]
type floaty = float [@@deriving json]
type 'a param = 'a [@@deriving json]
type opt = string option [@@deriving json]
type res = (int, string) result [@@deriving json]
type tuple = int * string [@@deriving json]
type record = { name : string; age : int } [@@deriving json]
type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
type record_opt = { k : int option; [@json.option] } [@@deriving json]
type sum = A | B of int | C of { name : string } [@@deriving json]
type sum2 = S2 of int * string [@@deriving json]
type other = [ `C ] [@@deriving json]
type poly = [ `A | `B of int | other ] [@@deriving json]
type poly2 = [ `P2 of int * string ] [@@deriving json]
type 'a c = [ `C of 'a ] [@@deriving json]
type recur = A | Fix of recur [@@deriving json]
type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
type evar = A | B [@json.name "b_aliased"] [@@deriving json]
type epoly = [ `a [@json.name "A_aliased"] | `b ] [@@deriving json]
type ('a, 'b) p2 = A of 'a | B of 'b [@@deriving json]
type allow_extra_fields = {a: int} [@@deriving json] [@@json.allow_extra_fields]
type allow_extra_fields2 = A of {a: int} [@json.allow_extra_fields] [@@deriving json]
type drop_default_option = { a: int; b_opt: int option; [@option] [@json.drop_default] } [@@deriving json]
type array_list = { a: int array; b: int list} [@@deriving json]
type json = Melange_json.t
type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
type color = Red | Green | Blue [@@deriving json]
type shape = | Circle of float  (* radius *) | Rectangle of float * float  (* width * height *) | Point of { x: float; y: float } | Empty [@@deriving json]
type legacy_variant = LegacyA | LegacyB of int [@@deriving json] [@@json.legacy_variant]

let of_json_cases = [
  C ({|1|}, user_of_json, user_to_json, 1);
  C ({|"9223372036854775807"|}, userid_of_json, userid_to_json, 9223372036854775807L);
  C ({|1.1|}, floaty_of_json, floaty_to_json, 1.1);
  C ({|1.0|}, floaty_of_json, floaty_to_json, 1.0);
  C ({|42|}, floaty_of_json, floaty_to_json, 42.0);
  C ({|"OK"|}, (param_of_json string_of_json), (param_to_json string_to_json), "OK");
  C ({|"some"|}, opt_of_json, opt_to_json, (Some "some"));
  C ({|["Ok", 1]|}, res_of_json, res_to_json, Ok 1);
  C ({|["Error", "oops"]|}, res_of_json, res_to_json, Error "oops");
  C ({|[42, "works"]|}, tuple_of_json, tuple_to_json, (42, "works"));
  C ({|{"name":"N","age":1}|}, record_of_json, record_to_json, {name="N"; age=1});
  C ({|["A"]|}, sum_of_json, sum_to_json, (A : sum));
  C ({|["B", 42]|}, sum_of_json, sum_to_json, (B 42 : sum));
  C ({|["C", {"name": "cname"}]|}, sum_of_json, sum_to_json, (C {name="cname"} : sum));
  C ({|["A"]|}, sum_of_json, sum_to_json, (A : sum));
  C ({|["S2", 42, "hello"]|}, sum2_of_json, sum2_to_json, (S2 (42, "hello")));
  C ({|["B", 42]|}, poly_of_json, poly_to_json, (`B 42 : poly));
  C ({|["C"]|}, poly_of_json, poly_to_json, (`C : poly));
  C ({|["P2", 42, "hello"]|}, poly2_of_json, poly2_to_json, (`P2 (42, "hello") : poly2));
  C ({|["Fix",["Fix",["Fix",["A"]]]]|}, recur_of_json, recur_to_json, (Fix (Fix (Fix A))));
  C ({|["Fix",["Fix",["Fix",["A"]]]]|}, polyrecur_of_json, polyrecur_to_json, (`Fix (`Fix (`Fix `A))));
  C ({|["A"]|}, evar_of_json, evar_to_json, (A : evar));
  C ({|["b_aliased"]|}, evar_of_json, evar_to_json, (B : evar)); (* variant B repr as "b_aliased" in JSON *)
  C ({|["b"]|}, epoly_of_json, epoly_to_json, (`b : epoly));
  C ({|["A_aliased"]|}, epoly_of_json, epoly_to_json, (`a : epoly)); (* polyvariant `a aliased to "A_aliased"*)
  C ({|{"my_name":"N","my_age":1}|}, record_aliased_of_json, record_aliased_to_json, {name="N"; age=1});
  C ({|{"my_name":"N"}|}, record_aliased_of_json, record_aliased_to_json, {name="N"; age=100});
  C ({|{}|}, record_opt_of_json, record_opt_to_json, {k=None});
  C ({|{"k":42}|}, record_opt_of_json, record_opt_to_json, {k=Some 42});
  C ({|["A",1]|}, p2_of_json int_of_json string_of_json, p2_to_json int_to_json string_to_json, A 1);
  C ({|["B","ok"]|}, p2_of_json int_of_json string_of_json, p2_to_json int_to_json string_to_json, B "ok");
  C ({|{"a":1,"b":2}|}, allow_extra_fields_of_json, allow_extra_fields_to_json, {a=1});
  C ({|["A",{"a":1,"b":2}]|}, allow_extra_fields2_of_json, allow_extra_fields2_to_json, A {a=1});
  C ({|{"a":1}|}, drop_default_option_of_json, drop_default_option_to_json, {a=1; b_opt=None});
  C ({|{"a":1,"b_opt":2}|}, drop_default_option_of_json, drop_default_option_to_json, {a=1; b_opt=Some 2});
  C ({|{"a":[1],"b":[2]}|}, array_list_of_json, array_list_to_json, {a=[|1|]; b=[2]});
  C ({|["Circle", 5.0]|}, shape_of_json, shape_to_json, Circle 5.0);
  C ({|["Rectangle", 10.0, 20.0]|}, shape_of_json, shape_to_json, Rectangle (10.0, 20.0));
  C ({|["Point", {"x": 1.0, "y": 2.0}]|}, shape_of_json, shape_to_json, Point {x=1.0; y=2.0});
  C ({|["Empty"]|}, shape_of_json, shape_to_json, Empty);
  C ({|"Empty"|}, shape_of_json, shape_to_json, Empty);
  (* legacy_variant: with [@@json.legacy_variant], payloadless variants serialize as lists *)
  C ({|["LegacyA"]|}, legacy_variant_of_json, legacy_variant_to_json, LegacyA);
  C ({|"LegacyA"|}, legacy_variant_of_json, legacy_variant_to_json, LegacyA);  (* can still parse string format *)
  C ({|["LegacyB", 42]|}, legacy_variant_of_json, legacy_variant_to_json, LegacyB 42);
]
let run' (C (data, of_json, to_json, v)) =
  print_endline (Printf.sprintf "JSON    DATA: %s" data);
  let json = Melange_json.of_string data in
  let v' = of_json json in
  assert (v' = v);
  let json' = to_json v' in
  let data' = Melange_json.to_string json' in
  print_endline (Printf.sprintf "JSON REPRINT: %s" data')

(* Error cases for object validation *)
type must_be_object = { field: int } [@@deriving json]
type must_be_array_2 = (int * int) [@@deriving json]
type must_be_array_3 = (int * int * int) [@@deriving json]

let error_cases = [
  (* Should fail with "expected a JSON object" *)
  C ({|42|}, must_be_object_of_json, must_be_object_to_json, {field=1});

  (* Should fail with "expected a JSON array of length 2" *)
  C ({|[1]|}, must_be_array_2_of_json, must_be_array_2_to_json, (1, 2));
  C ({|[1,2,3]|}, must_be_array_2_of_json, must_be_array_2_to_json, (1, 2));

  (* Should fail with "expected a JSON array of length 3" *)
  C ({|[1,2]|}, must_be_array_3_of_json, must_be_array_3_to_json, (1, 2, 3));
  C ({|[1,2,3,4]|}, must_be_array_3_of_json, must_be_array_3_to_json, (1, 2, 3));

  (* Should fail with "expected a JSON string representing a variant" *)
  C ({|42|}, color_of_json, color_to_json, Red);
  C ({|"Yellow"|}, color_of_json, color_to_json, Red);

  (* Should fail with shape validation *)
  C ({|["Circle"]|}, shape_of_json, shape_to_json, Circle 1.0);
  C ({|["Rectangle", 10.0]|}, shape_of_json, shape_to_json, Rectangle (10.0, 20.0));
  C ({|["Point", 1.0, 2.0]|}, shape_of_json, shape_to_json, Point {x=1.0; y=2.0});
]

let run_error_case' (C (data, of_json, _to_json, _v)) =
  print_endline (Printf.sprintf "ERROR CASE DATA: %s" data);
  let json = Melange_json.of_string data in
  try
    let _v' = of_json json in
    print_endline "Error: should have failed"
  with Melange_json.Of_json_error (Json_error msg) ->
    print_endline (Printf.sprintf "Got expected error: %s" msg)

let test () =
  List.iter run' of_json_cases;
  print_endline "\nTesting error cases:";
  List.iter run_error_case' error_cases
