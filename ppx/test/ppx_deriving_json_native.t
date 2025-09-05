  $ alias run='../native/ppx_deriving_json_native_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF
  type user = int [@@deriving json]
  
  include struct
    let _ = fun (_ : user) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_of_json = (fun x -> int_of_json x : Yojson.Basic.t -> user)
    let _ = user_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_to_json = (fun x -> int_to_json x : user -> Yojson.Basic.t)
    let _ = user_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type userid = int64 [@@deriving json]
  > EOF
  type userid = int64 [@@deriving json]
  
  include struct
    let _ = fun (_ : userid) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec userid_of_json =
      (fun x -> int64_of_json x : Yojson.Basic.t -> userid)
  
    let _ = userid_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec userid_to_json =
      (fun x -> int64_to_json x : userid -> Yojson.Basic.t)
  
    let _ = userid_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type floaty = float [@@deriving json]
  > EOF
  type floaty = float [@@deriving json]
  
  include struct
    let _ = fun (_ : floaty) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec floaty_of_json =
      (fun x -> float_of_json x : Yojson.Basic.t -> floaty)
  
    let _ = floaty_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec floaty_to_json =
      (fun x -> float_to_json x : floaty -> Yojson.Basic.t)
  
    let _ = floaty_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a param = 'a [@@deriving json]
  > EOF
  type 'a param = 'a [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a param) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_of_json a_of_json =
      (fun x -> a_of_json x : Yojson.Basic.t -> 'a param)
  
    let _ = param_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_to_json a_to_json =
      (fun x -> a_to_json x : 'a param -> Yojson.Basic.t)
  
    let _ = param_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type opt = string option [@@deriving json]
  > EOF
  type opt = string option [@@deriving json]
  
  include struct
    let _ = fun (_ : opt) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_of_json =
      (fun x -> (option_of_json string_of_json) x : Yojson.Basic.t -> opt)
  
    let _ = opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_to_json =
      (fun x -> (option_to_json string_to_json) x : opt -> Yojson.Basic.t)
  
    let _ = opt_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type res = (int, string) result [@@deriving json]
  > EOF
  type res = (int, string) result [@@deriving json]
  
  include struct
    let _ = fun (_ : res) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec res_of_json =
      (fun x -> (result_of_json int_of_json string_of_json) x
        : Yojson.Basic.t -> res)
  
    let _ = res_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec res_to_json =
      (fun x -> (result_to_json int_to_json string_to_json) x
        : res -> Yojson.Basic.t)
  
    let _ = res_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type tuple = int * string [@@deriving json]
  > EOF
  type tuple = int * string [@@deriving json]
  
  include struct
    let _ = fun (_ : tuple) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_of_json =
      (fun x ->
         match x with
         | `List [ x_0; x_1 ] -> int_of_json x_0, string_of_json x_1
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected a JSON array of length 2"
        : Yojson.Basic.t -> tuple)
  
    let _ = tuple_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_to_json =
      (fun x ->
         match x with
         | x_0, x_1 -> `List [ int_to_json x_0; string_to_json x_1 ]
        : tuple -> Yojson.Basic.t)
  
    let _ = tuple_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type record = { name : string; age : int } [@@deriving json]
  > EOF
  type record = { name : string; age : int } [@@deriving json]
  
  include struct
    let _ = fun (_ : record) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_name = ref Stdlib.Option.None in
             let x_age = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "name" ->
                       x_name := Stdlib.Option.Some (string_of_json v)
                   | "age" -> x_age := Stdlib.Option.Some (int_of_json v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               name =
                 (match Stdlib.( ! ) x_name with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"name\"");
               age =
                 (match Stdlib.( ! ) x_age with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"age\"");
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> record)
  
    let _ = record_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ = ("age", int_to_json x_age) :: bnds__001_ in
                let bnds__001_ =
                  ("name", string_to_json x_name) :: bnds__001_
                in
                bnds__001_)
        : record -> Yojson.Basic.t)
  
    let _ = record_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type record_aliased = { name : string; [@json.key "my_name"] age : int; [@json.key "my_age"] [@json.default 100] } [@@deriving json]
  > EOF
  type record_aliased = {
    name : string; [@json.key "my_name"]
    age : int; [@json.key "my_age"] [@json.default 100]
  }
  [@@deriving json]
  
  include struct
    let _ = fun (_ : record_aliased) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_aliased_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_name = ref Stdlib.Option.None in
             let x_age = ref (Stdlib.Option.Some 100) in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "my_name" ->
                       x_name := Stdlib.Option.Some (string_of_json v)
                   | "my_age" -> x_age := Stdlib.Option.Some (int_of_json v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               name =
                 (match Stdlib.( ! ) x_name with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"my_name\"");
               age =
                 (match Stdlib.( ! ) x_age with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None -> 100);
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> record_aliased)
  
    let _ = record_aliased_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_aliased_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ =
                  ("my_age", int_to_json x_age) :: bnds__001_
                in
                let bnds__001_ =
                  ("my_name", string_to_json x_name) :: bnds__001_
                in
                bnds__001_)
        : record_aliased -> Yojson.Basic.t)
  
    let _ = record_aliased_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type record_opt = { k : int option; [@json.option] } [@@deriving json]
  > EOF
  type record_opt = { k : int option [@json.option] } [@@deriving json]
  
  include struct
    let _ = fun (_ : record_opt) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_opt_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_k = ref (Stdlib.Option.Some Stdlib.Option.None) in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "k" ->
                       x_k :=
                         Stdlib.Option.Some ((option_of_json int_of_json) v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               k =
                 (match Stdlib.( ! ) x_k with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None -> Stdlib.Option.None);
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> record_opt)
  
    let _ = record_opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_opt_to_json =
      (fun x ->
         match x with
         | { k = x_k } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ =
                  ("k", (option_to_json int_to_json) x_k) :: bnds__001_
                in
                bnds__001_)
        : record_opt -> Yojson.Basic.t)
  
    let _ = record_opt_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type sum = A | B of int | C of { name : string } [@@deriving json]
  > EOF
  type sum = A | B of int | C of { name : string } [@@deriving json]
  
  include struct
    let _ = fun (_ : sum) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> A
         | `List [ `String "B"; x_0 ] -> B (int_of_json x_0)
         | `List [ `String "C"; `Assoc fs ] ->
             let x_name = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "name" ->
                       x_name := Stdlib.Option.Some (string_of_json v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             C
               {
                 name =
                   (match Stdlib.( ! ) x_name with
                   | Stdlib.Option.Some v -> v
                   | Stdlib.Option.None ->
                       Melange_json.of_json_error ~json:x
                         "expected field \"name\"");
               }
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"A\"] or [\"B\", _] or [\"C\", { _ }]"
        : Yojson.Basic.t -> sum)
  
    let _ = sum_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_to_json =
      (fun x ->
         match x with
         | A -> `String "A"
         | B x_0 -> `List [ `String "B"; int_to_json x_0 ]
         | C { name = x_name } ->
             `List
               [
                 `String "C";
                 `Assoc
                   (let bnds__001_ = [] in
                    let bnds__001_ =
                      ("name", string_to_json x_name) :: bnds__001_
                    in
                    bnds__001_);
               ]
        : sum -> Yojson.Basic.t)
  
    let _ = sum_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type sum2 = S2 of int * string [@@deriving json]
  > EOF
  type sum2 = S2 of int * string [@@deriving json]
  
  include struct
    let _ = fun (_ : sum2) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum2_of_json =
      (fun x ->
         match x with
         | `List [ `String "S2"; x_0; x_1 ] ->
             S2 (int_of_json x_0, string_of_json x_1)
         | _ -> Melange_json.of_json_error ~json:x "expected [\"S2\", _, _]"
        : Yojson.Basic.t -> sum2)
  
    let _ = sum2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum2_to_json =
      (fun x ->
         match x with
         | S2 (x_0, x_1) ->
             `List [ `String "S2"; int_to_json x_0; string_to_json x_1 ]
        : sum2 -> Yojson.Basic.t)
  
    let _ = sum2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type other = [ `C ] [@@deriving json]
  > EOF
  type other = [ `C ] [@@deriving json]
  
  include struct
    let _ = fun (_ : other) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_of_json =
      (fun x ->
         match x with
         | `List (`String "C" :: []) | `String "C" -> `C
         | x ->
             Melange_json.of_json_unexpected_variant ~json:x
               "expected [\"C\"]"
        : Yojson.Basic.t -> other)
  
    let _ = other_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_to_json =
      (fun x -> match x with `C -> `String "C" : other -> Yojson.Basic.t)
  
    let _ = other_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type poly = [ `A | `B of int | other ] [@@deriving json]
  > EOF
  type poly = [ `A | `B of int | other ] [@@deriving json]
  
  include struct
    let _ = fun (_ : poly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> `A
         | `List [ `String "B"; x_0 ] -> `B (int_of_json x_0)
         | x -> (
             match other_of_json x with
             | x -> (x :> [ `A | `B of int | other ])
             | exception
                 Melange_json.Of_json_error
                   (Melange_json.Unexpected_variant _) ->
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"A\"] or [\"B\", _]")
        : Yojson.Basic.t -> poly)
  
    let _ = poly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_to_json =
      (fun x ->
         match x with
         | `A -> `String "A"
         | `B x_0 -> `List [ `String "B"; int_to_json x_0 ]
         | #other as x -> other_to_json x
        : poly -> Yojson.Basic.t)
  
    let _ = poly_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type poly2 = [ `P2 of int * string ] [@@deriving json]
  > EOF
  type poly2 = [ `P2 of int * string ] [@@deriving json]
  
  include struct
    let _ = fun (_ : poly2) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly2_of_json =
      (fun x ->
         match x with
         | `List [ `String "P2"; x_0; x_1 ] ->
             `P2 (int_of_json x_0, string_of_json x_1)
         | x ->
             Melange_json.of_json_unexpected_variant ~json:x
               "expected [\"P2\", _, _]"
        : Yojson.Basic.t -> poly2)
  
    let _ = poly2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly2_to_json =
      (fun x ->
         match x with
         | `P2 (x_0, x_1) ->
             `List [ `String "P2"; int_to_json x_0; string_to_json x_1 ]
        : poly2 -> Yojson.Basic.t)
  
    let _ = poly2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a c = [ `C of 'a ] [@@deriving json]
  > EOF
  type 'a c = [ `C of 'a ] [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a c) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_of_json a_of_json =
      (fun x ->
         match x with
         | `List [ `String "C"; x_0 ] -> `C (a_of_json x_0)
         | x ->
             Melange_json.of_json_unexpected_variant ~json:x
               "expected [\"C\", _]"
        : Yojson.Basic.t -> 'a c)
  
    let _ = c_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_to_json a_to_json =
      (fun x ->
         match x with `C x_0 -> `List [ `String "C"; a_to_json x_0 ]
        : 'a c -> Yojson.Basic.t)
  
    let _ = c_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type recur = A | Fix of recur [@@deriving json]
  > EOF
  type recur = A | Fix of recur [@@deriving json]
  
  include struct
    let _ = fun (_ : recur) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> A
         | `List [ `String "Fix"; x_0 ] -> Fix (recur_of_json x_0)
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"A\"] or [\"Fix\", _]"
        : Yojson.Basic.t -> recur)
  
    let _ = recur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_to_json =
      (fun x ->
         match x with
         | A -> `String "A"
         | Fix x_0 -> `List [ `String "Fix"; recur_to_json x_0 ]
        : recur -> Yojson.Basic.t)
  
    let _ = recur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
  > EOF
  type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
  
  include struct
    let _ = fun (_ : polyrecur) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> `A
         | `List [ `String "Fix"; x_0 ] -> `Fix (polyrecur_of_json x_0)
         | x ->
             Melange_json.of_json_unexpected_variant ~json:x
               "expected [\"A\"] or [\"Fix\", _]"
        : Yojson.Basic.t -> polyrecur)
  
    let _ = polyrecur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_to_json =
      (fun x ->
         match x with
         | `A -> `String "A"
         | `Fix x_0 -> `List [ `String "Fix"; polyrecur_to_json x_0 ]
        : polyrecur -> Yojson.Basic.t)
  
    let _ = polyrecur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type evar = A | B [@json.name "b_aliased"] [@@deriving json]
  > EOF
  type evar = A | B [@json.name "b_aliased"] [@@deriving json]
  
  include struct
    let _ = fun (_ : evar) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec evar_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> A
         | `List (`String "b_aliased" :: []) | `String "b_aliased" -> B
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"A\"] or [\"B\"]"
        : Yojson.Basic.t -> evar)
  
    let _ = evar_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec evar_to_json =
      (fun x -> match x with A -> `String "A" | B -> `String "b_aliased"
        : evar -> Yojson.Basic.t)
  
    let _ = evar_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type epoly = [ `a [@json.name "A_aliased"] | `b ] [@@deriving json]
  > EOF
  type epoly = [ `a [@json.name "A_aliased"] | `b ] [@@deriving json]
  
  include struct
    let _ = fun (_ : epoly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec epoly_of_json =
      (fun x ->
         match x with
         | `List (`String "A_aliased" :: []) | `String "A_aliased" -> `a
         | `List (`String "b" :: []) | `String "b" -> `b
         | x ->
             Melange_json.of_json_unexpected_variant ~json:x
               "expected [\"a\"] or [\"b\"]"
        : Yojson.Basic.t -> epoly)
  
    let _ = epoly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec epoly_to_json =
      (fun x -> match x with `a -> `String "A_aliased" | `b -> `String "b"
        : epoly -> Yojson.Basic.t)
  
    let _ = epoly_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type ('a, 'b) p2 = A of 'a | B of 'b [@@deriving json]
  > EOF
  type ('a, 'b) p2 = A of 'a | B of 'b [@@deriving json]
  
  include struct
    let _ = fun (_ : ('a, 'b) p2) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec p2_of_json a_of_json b_of_json =
      (fun x ->
         match x with
         | `List [ `String "A"; x_0 ] -> A (a_of_json x_0)
         | `List [ `String "B"; x_0 ] -> B (b_of_json x_0)
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"A\", _] or [\"B\", _]"
        : Yojson.Basic.t -> ('a, 'b) p2)
  
    let _ = p2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec p2_to_json a_to_json b_to_json =
      (fun x ->
         match x with
         | A x_0 -> `List [ `String "A"; a_to_json x_0 ]
         | B x_0 -> `List [ `String "B"; b_to_json x_0 ]
        : ('a, 'b) p2 -> Yojson.Basic.t)
  
    let _ = p2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type allow_extra_fields = {a: int} [@@deriving json] [@@json.allow_extra_fields]
  > EOF
  type allow_extra_fields = { a : int }
  [@@deriving json] [@@json.allow_extra_fields]
  
  include struct
    let _ = fun (_ : allow_extra_fields) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_a = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "a" -> x_a := Stdlib.Option.Some (int_of_json v)
                   | name -> ());
                   iter fs
             in
             iter fs;
             {
               a =
                 (match Stdlib.( ! ) x_a with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"a\"");
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> allow_extra_fields)
  
    let _ = allow_extra_fields_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields_to_json =
      (fun x ->
         match x with
         | { a = x_a } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ = ("a", int_to_json x_a) :: bnds__001_ in
                bnds__001_)
        : allow_extra_fields -> Yojson.Basic.t)
  
    let _ = allow_extra_fields_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type allow_extra_fields2 = A of {a: int} [@json.allow_extra_fields] [@@deriving json]
  > EOF
  type allow_extra_fields2 = A of { a : int } [@json.allow_extra_fields]
  [@@deriving json]
  
  include struct
    let _ = fun (_ : allow_extra_fields2) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields2_of_json =
      (fun x ->
         match x with
         | `List [ `String "A"; `Assoc fs ] ->
             let x_a = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "a" -> x_a := Stdlib.Option.Some (int_of_json v)
                   | name -> ());
                   iter fs
             in
             iter fs;
             A
               {
                 a =
                   (match Stdlib.( ! ) x_a with
                   | Stdlib.Option.Some v -> v
                   | Stdlib.Option.None ->
                       Melange_json.of_json_error ~json:x
                         "expected field \"a\"");
               }
         | _ -> Melange_json.of_json_error ~json:x "expected [\"A\", { _ }]"
        : Yojson.Basic.t -> allow_extra_fields2)
  
    let _ = allow_extra_fields2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields2_to_json =
      (fun x ->
         match x with
         | A { a = x_a } ->
             `List
               [
                 `String "A";
                 `Assoc
                   (let bnds__001_ = [] in
                    let bnds__001_ = ("a", int_to_json x_a) :: bnds__001_ in
                    bnds__001_);
               ]
        : allow_extra_fields2 -> Yojson.Basic.t)
  
    let _ = allow_extra_fields2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type drop_default_option = { a: int; b_opt: int option; [@option] [@json.drop_default] } [@@deriving json]
  > EOF
  type drop_default_option = {
    a : int;
    b_opt : int option; [@option] [@json.drop_default]
  }
  [@@deriving json]
  
  include struct
    let _ = fun (_ : drop_default_option) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec drop_default_option_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_a = ref Stdlib.Option.None in
             let x_b_opt = ref (Stdlib.Option.Some Stdlib.Option.None) in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "a" -> x_a := Stdlib.Option.Some (int_of_json v)
                   | "b_opt" ->
                       x_b_opt :=
                         Stdlib.Option.Some ((option_of_json int_of_json) v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               a =
                 (match Stdlib.( ! ) x_a with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"a\"");
               b_opt =
                 (match Stdlib.( ! ) x_b_opt with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None -> Stdlib.Option.None);
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> drop_default_option)
  
    let _ = drop_default_option_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec drop_default_option_to_json =
      (fun x ->
         match x with
         | { a = x_a; b_opt = x_b_opt } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ =
                  match x_b_opt with
                  | Stdlib.Option.None -> bnds__001_
                  | Stdlib.Option.Some _ ->
                      ("b_opt", (option_to_json int_to_json) x_b_opt)
                      :: bnds__001_
                in
                let bnds__001_ = ("a", int_to_json x_a) :: bnds__001_ in
                bnds__001_)
        : drop_default_option -> Yojson.Basic.t)
  
    let _ = drop_default_option_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type array_list = { a: int array; b: int list} [@@deriving json]
  > EOF
  type array_list = { a : int array; b : int list } [@@deriving json]
  
  include struct
    let _ = fun (_ : array_list) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec array_list_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_a = ref Stdlib.Option.None in
             let x_b = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "a" ->
                       x_a :=
                         Stdlib.Option.Some ((array_of_json int_of_json) v)
                   | "b" ->
                       x_b :=
                         Stdlib.Option.Some ((list_of_json int_of_json) v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               a =
                 (match Stdlib.( ! ) x_a with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"a\"");
               b =
                 (match Stdlib.( ! ) x_b with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"b\"");
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> array_list)
  
    let _ = array_list_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec array_list_to_json =
      (fun x ->
         match x with
         | { a = x_a; b = x_b } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ =
                  ("b", (list_to_json int_to_json) x_b) :: bnds__001_
                in
                let bnds__001_ =
                  ("a", (array_to_json int_to_json) x_a) :: bnds__001_
                in
                bnds__001_)
        : array_list -> Yojson.Basic.t)
  
    let _ = array_list_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type json = Melange_json.t
  > EOF
  type json = Melange_json.t

  $ cat <<"EOF" | run
  > type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
  > EOF
  type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json

  $ cat <<"EOF" | run
  > type color = Red | Green | Blue [@@deriving json]
  > EOF
  type color = Red | Green | Blue [@@deriving json]
  
  include struct
    let _ = fun (_ : color) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec color_of_json =
      (fun x ->
         match x with
         | `List (`String "Red" :: []) | `String "Red" -> Red
         | `List (`String "Green" :: []) | `String "Green" -> Green
         | `List (`String "Blue" :: []) | `String "Blue" -> Blue
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"Red\"] or [\"Green\"] or [\"Blue\"]"
        : Yojson.Basic.t -> color)
  
    let _ = color_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec color_to_json =
      (fun x ->
         match x with
         | Red -> `String "Red"
         | Green -> `String "Green"
         | Blue -> `String "Blue"
        : color -> Yojson.Basic.t)
  
    let _ = color_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type shape = | Circle of float  (* radius *) | Rectangle of float * float  (* width * height *) | Point of { x: float; y: float } | Empty [@@deriving json]
  > EOF
  type shape =
    | Circle of float
    | Rectangle of float * float
    | Point of { x : float; y : float }
    | Empty
  [@@deriving json]
  
  include struct
    let _ = fun (_ : shape) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec shape_of_json =
      (fun x ->
         match x with
         | `List [ `String "Circle"; x_0 ] -> Circle (float_of_json x_0)
         | `List [ `String "Rectangle"; x_0; x_1 ] ->
             Rectangle (float_of_json x_0, float_of_json x_1)
         | `List [ `String "Point"; `Assoc fs ] ->
             let x_x = ref Stdlib.Option.None in
             let x_y = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "x" -> x_x := Stdlib.Option.Some (float_of_json v)
                   | "y" -> x_y := Stdlib.Option.Some (float_of_json v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             Point
               {
                 x =
                   (match Stdlib.( ! ) x_x with
                   | Stdlib.Option.Some v -> v
                   | Stdlib.Option.None ->
                       Melange_json.of_json_error ~json:x
                         "expected field \"x\"");
                 y =
                   (match Stdlib.( ! ) x_y with
                   | Stdlib.Option.Some v -> v
                   | Stdlib.Option.None ->
                       Melange_json.of_json_error ~json:x
                         "expected field \"y\"");
               }
         | `List (`String "Empty" :: []) | `String "Empty" -> Empty
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"Circle\", _] or [\"Rectangle\", _, _] or \
                [\"Point\", { _ }] or [\"Empty\"]"
        : Yojson.Basic.t -> shape)
  
    let _ = shape_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec shape_to_json =
      (fun x ->
         match x with
         | Circle x_0 -> `List [ `String "Circle"; float_to_json x_0 ]
         | Rectangle (x_0, x_1) ->
             `List
               [ `String "Rectangle"; float_to_json x_0; float_to_json x_1 ]
         | Point { x = x_x; y = x_y } ->
             `List
               [
                 `String "Point";
                 `Assoc
                   (let bnds__001_ = [] in
                    let bnds__001_ =
                      ("y", float_to_json x_y) :: bnds__001_
                    in
                    let bnds__001_ =
                      ("x", float_to_json x_x) :: bnds__001_
                    in
                    bnds__001_);
               ]
         | Empty -> `String "Empty"
        : shape -> Yojson.Basic.t)
  
    let _ = shape_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type must_be_object = { field: int } [@@deriving json]
  > EOF
  type must_be_object = { field : int } [@@deriving json]
  
  include struct
    let _ = fun (_ : must_be_object) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_object_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_field = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "field" ->
                       x_field := Stdlib.Option.Some (int_of_json v)
                   | name ->
                       Melange_json.of_json_error ~json:x
                         (Stdlib.Printf.sprintf
                            {|did not expect field "%s"|} name));
                   iter fs
             in
             iter fs;
             {
               field =
                 (match Stdlib.( ! ) x_field with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x
                       "expected field \"field\"");
             }
         | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
        : Yojson.Basic.t -> must_be_object)
  
    let _ = must_be_object_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_object_to_json =
      (fun x ->
         match x with
         | { field = x_field } ->
             `Assoc
               (let bnds__001_ = [] in
                let bnds__001_ =
                  ("field", int_to_json x_field) :: bnds__001_
                in
                bnds__001_)
        : must_be_object -> Yojson.Basic.t)
  
    let _ = must_be_object_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type must_be_array_2 = (int * int) [@@deriving json]
  > EOF
  type must_be_array_2 = int * int [@@deriving json]
  
  include struct
    let _ = fun (_ : must_be_array_2) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_2_of_json =
      (fun x ->
         match x with
         | `List [ x_0; x_1 ] -> int_of_json x_0, int_of_json x_1
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected a JSON array of length 2"
        : Yojson.Basic.t -> must_be_array_2)
  
    let _ = must_be_array_2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_2_to_json =
      (fun x ->
         match x with
         | x_0, x_1 -> `List [ int_to_json x_0; int_to_json x_1 ]
        : must_be_array_2 -> Yojson.Basic.t)
  
    let _ = must_be_array_2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type must_be_array_3 = (int * int * int) [@@deriving json]
  > EOF
  type must_be_array_3 = int * int * int [@@deriving json]
  
  include struct
    let _ = fun (_ : must_be_array_3) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_3_of_json =
      (fun x ->
         match x with
         | `List [ x_0; x_1; x_2 ] ->
             int_of_json x_0, int_of_json x_1, int_of_json x_2
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected a JSON array of length 3"
        : Yojson.Basic.t -> must_be_array_3)
  
    let _ = must_be_array_3_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_3_to_json =
      (fun x ->
         match x with
         | x_0, x_1, x_2 ->
             `List [ int_to_json x_0; int_to_json x_1; int_to_json x_2 ]
        : must_be_array_3 -> Yojson.Basic.t)
  
    let _ = must_be_array_3_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type legacy_variant = A | B of int [@@deriving json] [@@json.no_args_variant_cases_as_arrays]
  > EOF
  type legacy_variant = A | B of int
  [@@deriving json] [@@json.no_args_variant_cases_as_arrays]
  
  include struct
    let _ = fun (_ : legacy_variant) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec legacy_variant_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) | `String "A" -> A
         | `List [ `String "B"; x_0 ] -> B (int_of_json x_0)
         | _ ->
             Melange_json.of_json_error ~json:x
               "expected [\"A\"] or [\"B\", _]"
        : Yojson.Basic.t -> legacy_variant)
  
    let _ = legacy_variant_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec legacy_variant_to_json =
      (fun x ->
         match x with
         | A -> `List [ `String "A" ]
         | B x_0 -> `List [ `String "B"; int_to_json x_0 ]
        : legacy_variant -> Yojson.Basic.t)
  
    let _ = legacy_variant_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

