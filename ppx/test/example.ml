type user = int [@@deriving json]
type 'a param = 'a [@@deriving json]
type opt = string option [@@deriving json]
type res = (int, string) result [@@deriving json]
type tuple = int * string [@@deriving json]
type record = { name : string; age : int } [@@deriving json]
type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
type record_opt = { k : int option; [@json.option] } [@@deriving json]
type sum = A | B of int | C of { name : string } [@@deriving json]
type other = [ `C ] [@@deriving json] type poly = [ `A | `B of int | other ] [@@deriving json]
type 'a c = [ `C of 'a ] [@@deriving json]
type recur = A | Fix of recur [@@deriving json]
type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
type evar = A | B [@json.as "b_aliased"] [@@deriving json]
type epoly = [ `a [@json.as "A_aliased"] | `b ] [@@deriving json]
type ('a, 'b) p2 = A of 'a | B of 'b [@@deriving json]
type allow_extra_fields = {a: int} [@@deriving json] [@@json.allow_extra_fields]
type allow_extra_fields2 = A of {a: int} [@json.allow_extra_fields] [@@deriving json]
type drop_default_option = { a: int; b_opt: int option; [@option] [@json.drop_default] } [@@deriving json]

module Cases = struct 
  type json = Ppx_deriving_json_runtime.t
  type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
  let of_json_cases = [
    C ({|1|}, user_of_json, user_to_json, 1);
    C ({|"OK"|}, (param_of_json string_of_json), (param_to_json string_to_json), "OK");
    C ({|"some"|}, opt_of_json, opt_to_json, (Some "some"));
    C ({|["Ok", 1]|}, res_of_json, res_to_json, Ok 1);
    C ({|["Error", "oops"]|}, res_of_json, res_to_json, Error "oops");
    C ({|[42, "works"]|}, tuple_of_json, tuple_to_json, (42, "works"));
    C ({|{"name":"N","age":1}|}, record_of_json, record_to_json, {name="N"; age=1});
    C ({|["A"]|}, sum_of_json, sum_to_json, (A : sum));
    C ({|["B", 42]|}, sum_of_json, sum_to_json, (B 42 : sum));
    C ({|["C", {"name": "cname"}]|}, sum_of_json, sum_to_json, (C {name="cname"} : sum));
    C ({|["A"]|}, poly_of_json, poly_to_json, (`A : poly));
    C ({|["B", 42]|}, poly_of_json, poly_to_json, (`B 42 : poly));
    C ({|["Fix",["Fix",["Fix",["A"]]]]|}, recur_of_json, recur_to_json, (Fix (Fix (Fix A))));
    C ({|["Fix",["Fix",["Fix",["A"]]]]|}, polyrecur_of_json, polyrecur_to_json, (`Fix (`Fix (`Fix `A))));
    C ({|"A"|}, evar_of_json, evar_to_json, (A : evar));
    C ({|"b_aliased"|}, evar_of_json, evar_to_json, (B : evar)); (* variant B repr as "b_aliased" in JSON *)
    C ({|"b"|}, epoly_of_json, epoly_to_json, (`b : epoly));
    C ({|"A_aliased"|}, epoly_of_json, epoly_to_json, (`a : epoly)); (* polyvariant `a aliased to "A_aliased"*)
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
  ]
  let run' ~json_of_string ~json_to_string (C (data, of_json, to_json, v)) =
    print_endline (Printf.sprintf "JSON    DATA: %s" data);
    let json = json_of_string data in
    let v' = of_json json in
    assert (v' = v);
    let json' = to_json v' in
    let data' = json_to_string json' in
    print_endline (Printf.sprintf "JSON REPRINT: %s" data')
  let run ~json_of_string ~json_to_string () =
    List.iter (run' ~json_of_string ~json_to_string) of_json_cases
end

