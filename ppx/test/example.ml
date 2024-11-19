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
type other = [ `C ] [@@deriving json] type poly = [ `A | `B of int | other ] [@@deriving json]
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

type json = Ppx_deriving_json_runtime.t
type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
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
  C ({|["P2", 42, "hello"]|}, poly2_of_json, poly2_to_json, (`P2 (42, "hello") : poly2));
  C ({|["Fix",["Fix",["Fix",["A"]]]]|}, recur_of_json, recur_to_json, (Fix (Fix (Fix A))));
  C ({|["Fix",["Fix",["Fix",["A"]]]]|}, polyrecur_of_json, polyrecur_to_json, (`Fix (`Fix (`Fix `A))));
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
]
let run' (C (data, of_json, to_json, v)) =
  print_endline (Printf.sprintf "JSON    DATA: %s" data);
  let json = Ppx_deriving_json_runtime.of_string data in
  let v' = of_json json in
  assert (v' = v);
  let json' = to_json v' in
  let data' = Ppx_deriving_json_runtime.to_string json' in
  print_endline (Printf.sprintf "JSON REPRINT: %s" data')
let test () =
  List.iter run' of_json_cases
