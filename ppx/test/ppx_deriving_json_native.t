  $ alias run='../native/ppx_deriving_json_native_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type userid = int64 [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type floaty = float [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type 'a param = 'a [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type opt = string option [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type res = (int, string) result [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type tuple = int * string [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type record = { name : string; age : int } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type record_opt = { k : int option; [@json.option] } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type sum = A | B of int | C of { name : string } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type sum2 = S2 of int * string [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type other = [ `C ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type poly = [ `A | `B of int | other ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type poly2 = [ `P2 of int * string ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type 'a c = [ `C of 'a ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type recur = A | Fix of recur [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type evar = A | B [@json.name "b_aliased"] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type epoly = [ `a [@json.name "A_aliased"] | `b ] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type ('a, 'b) p2 = A of 'a | B of 'b [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type allow_extra_fields = {a: int} [@@deriving json] [@@json.allow_extra_fields]
  > EOF

  $ cat <<"EOF" | run
  > type allow_extra_fields2 = A of {a: int} [@json.allow_extra_fields] [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type drop_default_option = { a: int; b_opt: int option; [@option] [@json.drop_default] } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type array_list = { a: int array; b: int list} [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type json = Melange_json.t
  > EOF

  $ cat <<"EOF" | run
  > type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
  > EOF

  $ cat <<"EOF" | run
  > type color = Red | Green | Blue [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type shape =
  > EOF

  $ cat <<"EOF" | run
  > type must_be_object = { field: int } [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type must_be_array_2 = (int * int) [@@deriving json]
  > EOF

  $ cat <<"EOF" | run
  > type must_be_array_3 = (int * int * int) [@@deriving json]
  > EOF

