  $ alias run='../browser/ppx_deriving_json_js_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF
  type user = int [@@deriving json]
  
  include struct
    let _ = fun (_ : user) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_of_json = (fun x -> int_of_json x : Js.Json.t -> user)
    let _ = user_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_to_json = (fun x -> int_to_json x : user -> Js.Json.t)
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
      (fun x -> int64_of_json x : Js.Json.t -> userid)
  
    let _ = userid_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec userid_to_json =
      (fun x -> int64_to_json x : userid -> Js.Json.t)
  
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
      (fun x -> float_of_json x : Js.Json.t -> floaty)
  
    let _ = floaty_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec floaty_to_json =
      (fun x -> float_to_json x : floaty -> Js.Json.t)
  
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
      (fun x -> a_of_json x : Js.Json.t -> 'a param)
  
    let _ = param_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_to_json a_to_json =
      (fun x -> a_to_json x : 'a param -> Js.Json.t)
  
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
      (fun x -> (option_of_json string_of_json) x : Js.Json.t -> opt)
  
    let _ = opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_to_json =
      (fun x -> (option_to_json string_to_json) x : opt -> Js.Json.t)
  
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
        : Js.Json.t -> res)
  
    let _ = res_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec res_to_json =
      (fun x -> (result_to_json int_to_json string_to_json) x
        : res -> Js.Json.t)
  
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
         if
           Stdlib.( && ) (Js.Array.isArray x)
             (Stdlib.( = )
                (Js.Array.length (Obj.magic x : Js.Json.t array))
                2)
         then
           let es = (Obj.magic x : Js.Json.t array) in
           ( int_of_json (Js.Array.unsafe_get es 0),
             string_of_json (Js.Array.unsafe_get es 1) )
         else
           Melange_json.of_json_error ~json:x
             "expected a JSON array of length 2"
        : Js.Json.t -> tuple)
  
    let _ = tuple_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_to_json =
      (fun x ->
         match x with
         | x_0, x_1 ->
             (Obj.magic [| int_to_json x_0; string_to_json x_1 |]
               : Js.Json.t)
        : tuple -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs =
           (Obj.magic x
             : < name : Js.Json.t Js.undefined
               ; age : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           name =
             (match Js.Undefined.toOption fs##name with
             | Stdlib.Option.Some v -> string_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"name\" to be present");
           age =
             (match Js.Undefined.toOption fs##age with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"age\" to be present");
         }
        : Js.Json.t -> record)
  
    let _ = record_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             (Obj.magic
                [%mel.obj
                  { name = string_to_json x_name; age = int_to_json x_age }]
               : Js.Json.t)
        : record -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs =
           (Obj.magic x
             : < my_name : Js.Json.t Js.undefined
               ; my_age : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           name =
             (match Js.Undefined.toOption fs##my_name with
             | Stdlib.Option.Some v -> string_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"my_name\" to be present");
           age =
             (match Js.Undefined.toOption fs##my_age with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None -> 100);
         }
        : Js.Json.t -> record_aliased)
  
    let _ = record_aliased_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_aliased_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             (Obj.magic
                [%mel.obj
                  {
                    my_name = string_to_json x_name;
                    my_age = int_to_json x_age;
                  }]
               : Js.Json.t)
        : record_aliased -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs = (Obj.magic x : < k : Js.Json.t Js.undefined > Js.t) in
         {
           k =
             (match Js.Undefined.toOption fs##k with
             | Stdlib.Option.Some v -> (option_of_json int_of_json) v
             | Stdlib.Option.None -> Stdlib.Option.None);
         }
        : Js.Json.t -> record_opt)
  
    let _ = record_opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_opt_to_json =
      (fun x ->
         match x with
         | { k = x_k } ->
             (Obj.magic [%mel.obj { k = (option_to_json int_to_json) x_k }]
               : Js.Json.t)
        : record_opt -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else A
               else if Stdlib.( = ) tag "B" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else B (int_of_json (Js.Array.unsafe_get array 1))
               else if Stdlib.( = ) tag "C" then (
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else
                   let fs = Js.Array.unsafe_get array 1 in
                   if
                     Stdlib.not
                       (Stdlib.( && )
                          (Stdlib.( = ) (Js.typeof fs) "object")
                          (Stdlib.( && )
                             (Stdlib.not (Js.Array.isArray fs))
                             (Stdlib.not
                                (Stdlib.( == )
                                   (Obj.magic fs : 'a Js.null)
                                   Js.null))))
                   then
                     Melange_json.of_json_error ~json:fs
                       "expected a JSON object";
                   let fs =
                     (Obj.magic fs : < name : Js.Json.t Js.undefined > Js.t)
                   in
                   C
                     {
                       name =
                         (match Js.Undefined.toOption fs##name with
                         | Stdlib.Option.Some v -> string_of_json v
                         | Stdlib.Option.None ->
                             Melange_json.of_json_error ~json:x
                               "expected field \"name\" to be present");
                     })
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"C\", { _ }] or [\"B\", _] or [\"A\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then A
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> sum)
  
    let _ = sum_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic (Obj.magic "A" : Js.Json.t) : Js.Json.t)
         | B x_0 ->
             (Obj.magic [| (Obj.magic "B" : Js.Json.t); int_to_json x_0 |]
               : Js.Json.t)
         | C { name = x_name } ->
             (Obj.magic
                [|
                  (Obj.magic "C" : Js.Json.t);
                  (Obj.magic [%mel.obj { name = string_to_json x_name }]
                    : Js.Json.t);
                |]
               : Js.Json.t)
        : sum -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "S2" then
                 if Stdlib.( <> ) len 3 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 3"
                 else
                   S2
                     ( int_of_json (Js.Array.unsafe_get array 1),
                       string_of_json (Js.Array.unsafe_get array 2) )
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"S2\", _, _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> sum2)
  
    let _ = sum2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum2_to_json =
      (fun x ->
         match x with
         | S2 (x_0, x_1) ->
             (Obj.magic
                [|
                  (Obj.magic "S2" : Js.Json.t);
                  int_to_json x_0;
                  string_to_json x_1;
                |]
               : Js.Json.t)
        : sum2 -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "C" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else `C
               else
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"C\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "C" then `C
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> other)
  
    let _ = other_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_to_json =
      (fun x ->
         match x with
         | `C -> (Obj.magic (Obj.magic "C" : Js.Json.t) : Js.Json.t)
        : other -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else `A
               else if Stdlib.( = ) tag "B" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else `B (int_of_json (Js.Array.unsafe_get array 1))
               else
                 match other_of_json x with
                 | e -> (e :> [ `A | `B of int | other ])
                 | exception
                     Melange_json.Of_json_error
                       (Melange_json.Unexpected_variant _) ->
                     Melange_json.of_json_unexpected_variant ~json:x
                       "expected [\"A\"] or [\"B\", _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then `A
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> poly)
  
    let _ = poly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_to_json =
      (fun x ->
         match x with
         | `A -> (Obj.magic (Obj.magic "A" : Js.Json.t) : Js.Json.t)
         | `B x_0 ->
             (Obj.magic [| (Obj.magic "B" : Js.Json.t); int_to_json x_0 |]
               : Js.Json.t)
         | #other as x -> other_to_json x
        : poly -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "P2" then
                 if Stdlib.( <> ) len 3 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 3"
                 else
                   `P2
                     ( int_of_json (Js.Array.unsafe_get array 1),
                       string_of_json (Js.Array.unsafe_get array 2) )
               else
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"P2\", _, _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> poly2)
  
    let _ = poly2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly2_to_json =
      (fun x ->
         match x with
         | `P2 (x_0, x_1) ->
             (Obj.magic
                [|
                  (Obj.magic "P2" : Js.Json.t);
                  int_to_json x_0;
                  string_to_json x_1;
                |]
               : Js.Json.t)
        : poly2 -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "C" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else `C (a_of_json (Js.Array.unsafe_get array 1))
               else
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"C\", _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> 'a c)
  
    let _ = c_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_to_json a_to_json =
      (fun x ->
         match x with
         | `C x_0 ->
             (Obj.magic [| (Obj.magic "C" : Js.Json.t); a_to_json x_0 |]
               : Js.Json.t)
        : 'a c -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else A
               else if Stdlib.( = ) tag "Fix" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else Fix (recur_of_json (Js.Array.unsafe_get array 1))
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"Fix\", _] or [\"A\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then A
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> recur)
  
    let _ = recur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic (Obj.magic "A" : Js.Json.t) : Js.Json.t)
         | Fix x_0 ->
             (Obj.magic
                [| (Obj.magic "Fix" : Js.Json.t); recur_to_json x_0 |]
               : Js.Json.t)
        : recur -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else `A
               else if Stdlib.( = ) tag "Fix" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else `Fix (polyrecur_of_json (Js.Array.unsafe_get array 1))
               else
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"A\"] or [\"Fix\", _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then `A
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> polyrecur)
  
    let _ = polyrecur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_to_json =
      (fun x ->
         match x with
         | `A -> (Obj.magic (Obj.magic "A" : Js.Json.t) : Js.Json.t)
         | `Fix x_0 ->
             (Obj.magic
                [| (Obj.magic "Fix" : Js.Json.t); polyrecur_to_json x_0 |]
               : Js.Json.t)
        : polyrecur -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else A
               else if Stdlib.( = ) tag "b_aliased" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else B
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"B\"] or [\"A\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then A
           else if Stdlib.( = ) tag "b_aliased" then B
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> evar)
  
    let _ = evar_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec evar_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic (Obj.magic "A" : Js.Json.t) : Js.Json.t)
         | B -> (Obj.magic (Obj.magic "b_aliased" : Js.Json.t) : Js.Json.t)
        : evar -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A_aliased" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else `a
               else if Stdlib.( = ) tag "b" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else `b
               else
                 Melange_json.of_json_unexpected_variant ~json:x
                   "expected [\"a\"] or [\"b\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A_aliased" then `a
           else if Stdlib.( = ) tag "b" then `b
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> epoly)
  
    let _ = epoly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec epoly_to_json =
      (fun x ->
         match x with
         | `a -> (Obj.magic (Obj.magic "A_aliased" : Js.Json.t) : Js.Json.t)
         | `b -> (Obj.magic (Obj.magic "b" : Js.Json.t) : Js.Json.t)
        : epoly -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else A (a_of_json (Js.Array.unsafe_get array 1))
               else if Stdlib.( = ) tag "B" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else B (b_of_json (Js.Array.unsafe_get array 1))
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"B\", _] or [\"A\", _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> ('a, 'b) p2)
  
    let _ = p2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec p2_to_json a_to_json b_to_json =
      (fun x ->
         match x with
         | A x_0 ->
             (Obj.magic [| (Obj.magic "A" : Js.Json.t); a_to_json x_0 |]
               : Js.Json.t)
         | B x_0 ->
             (Obj.magic [| (Obj.magic "B" : Js.Json.t); b_to_json x_0 |]
               : Js.Json.t)
        : ('a, 'b) p2 -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs = (Obj.magic x : < a : Js.Json.t Js.undefined > Js.t) in
         {
           a =
             (match Js.Undefined.toOption fs##a with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"a\" to be present");
         }
        : Js.Json.t -> allow_extra_fields)
  
    let _ = allow_extra_fields_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields_to_json =
      (fun x ->
         match x with
         | { a = x_a } ->
             (Obj.magic [%mel.obj { a = int_to_json x_a }] : Js.Json.t)
        : allow_extra_fields -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then (
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else
                   let fs = Js.Array.unsafe_get array 1 in
                   if
                     Stdlib.not
                       (Stdlib.( && )
                          (Stdlib.( = ) (Js.typeof fs) "object")
                          (Stdlib.( && )
                             (Stdlib.not (Js.Array.isArray fs))
                             (Stdlib.not
                                (Stdlib.( == )
                                   (Obj.magic fs : 'a Js.null)
                                   Js.null))))
                   then
                     Melange_json.of_json_error ~json:fs
                       "expected a JSON object";
                   let fs =
                     (Obj.magic fs : < a : Js.Json.t Js.undefined > Js.t)
                   in
                   A
                     {
                       a =
                         (match Js.Undefined.toOption fs##a with
                         | Stdlib.Option.Some v -> int_of_json v
                         | Stdlib.Option.None ->
                             Melange_json.of_json_error ~json:x
                               "expected field \"a\" to be present");
                     })
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"A\", { _ }]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> allow_extra_fields2)
  
    let _ = allow_extra_fields2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields2_to_json =
      (fun x ->
         match x with
         | A { a = x_a } ->
             (Obj.magic
                [|
                  (Obj.magic "A" : Js.Json.t);
                  (Obj.magic [%mel.obj { a = int_to_json x_a }] : Js.Json.t);
                |]
               : Js.Json.t)
        : allow_extra_fields2 -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs =
           (Obj.magic x
             : < a : Js.Json.t Js.undefined
               ; b_opt : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           a =
             (match Js.Undefined.toOption fs##a with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"a\" to be present");
           b_opt =
             (match Js.Undefined.toOption fs##b_opt with
             | Stdlib.Option.Some v -> (option_of_json int_of_json) v
             | Stdlib.Option.None -> Stdlib.Option.None);
         }
        : Js.Json.t -> drop_default_option)
  
    let _ = drop_default_option_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec drop_default_option_to_json =
      (fun x ->
         match x with
         | { a = x_a; b_opt = x_b_opt } ->
             (Obj.magic
                [%mel.obj
                  {
                    a = int_to_json x_a;
                    b_opt =
                      (match x_b_opt with
                      | Stdlib.Option.None -> Js.Undefined.empty
                      | Stdlib.Option.Some _ ->
                          Js.Undefined.return
                            ((option_to_json int_to_json) x_b_opt));
                  }]
               : Js.Json.t)
        : drop_default_option -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs =
           (Obj.magic x
             : < a : Js.Json.t Js.undefined ; b : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           a =
             (match Js.Undefined.toOption fs##a with
             | Stdlib.Option.Some v -> (array_of_json int_of_json) v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"a\" to be present");
           b =
             (match Js.Undefined.toOption fs##b with
             | Stdlib.Option.Some v -> (list_of_json int_of_json) v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"b\" to be present");
         }
        : Js.Json.t -> array_list)
  
    let _ = array_list_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec array_list_to_json =
      (fun x ->
         match x with
         | { a = x_a; b = x_b } ->
             (Obj.magic
                [%mel.obj
                  {
                    a = (array_to_json int_to_json) x_a;
                    b = (list_to_json int_to_json) x_b;
                  }]
               : Js.Json.t)
        : array_list -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "Red" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else Red
               else if Stdlib.( = ) tag "Green" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else Green
               else if Stdlib.( = ) tag "Blue" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else Blue
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"Blue\"] or [\"Green\"] or [\"Red\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "Red" then Red
           else if Stdlib.( = ) tag "Green" then Green
           else if Stdlib.( = ) tag "Blue" then Blue
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> color)
  
    let _ = color_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec color_to_json =
      (fun x ->
         match x with
         | Red -> (Obj.magic (Obj.magic "Red" : Js.Json.t) : Js.Json.t)
         | Green -> (Obj.magic (Obj.magic "Green" : Js.Json.t) : Js.Json.t)
         | Blue -> (Obj.magic (Obj.magic "Blue" : Js.Json.t) : Js.Json.t)
        : color -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "Circle" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else Circle (float_of_json (Js.Array.unsafe_get array 1))
               else if Stdlib.( = ) tag "Rectangle" then
                 if Stdlib.( <> ) len 3 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 3"
                 else
                   Rectangle
                     ( float_of_json (Js.Array.unsafe_get array 1),
                       float_of_json (Js.Array.unsafe_get array 2) )
               else if Stdlib.( = ) tag "Point" then (
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else
                   let fs = Js.Array.unsafe_get array 1 in
                   if
                     Stdlib.not
                       (Stdlib.( && )
                          (Stdlib.( = ) (Js.typeof fs) "object")
                          (Stdlib.( && )
                             (Stdlib.not (Js.Array.isArray fs))
                             (Stdlib.not
                                (Stdlib.( == )
                                   (Obj.magic fs : 'a Js.null)
                                   Js.null))))
                   then
                     Melange_json.of_json_error ~json:fs
                       "expected a JSON object";
                   let fs =
                     (Obj.magic fs
                       : < x : Js.Json.t Js.undefined
                         ; y : Js.Json.t Js.undefined >
                         Js.t)
                   in
                   Point
                     {
                       x =
                         (match Js.Undefined.toOption fs##x with
                         | Stdlib.Option.Some v -> float_of_json v
                         | Stdlib.Option.None ->
                             Melange_json.of_json_error ~json:x
                               "expected field \"x\" to be present");
                       y =
                         (match Js.Undefined.toOption fs##y with
                         | Stdlib.Option.Some v -> float_of_json v
                         | Stdlib.Option.None ->
                             Melange_json.of_json_error ~json:x
                               "expected field \"y\" to be present");
                     })
               else if Stdlib.( = ) tag "Empty" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else Empty
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"Empty\"] or [\"Point\", { _ }] or \
                    [\"Rectangle\", _, _] or [\"Circle\", _]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "Empty" then Empty
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> shape)
  
    let _ = shape_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec shape_to_json =
      (fun x ->
         match x with
         | Circle x_0 ->
             (Obj.magic
                [| (Obj.magic "Circle" : Js.Json.t); float_to_json x_0 |]
               : Js.Json.t)
         | Rectangle (x_0, x_1) ->
             (Obj.magic
                [|
                  (Obj.magic "Rectangle" : Js.Json.t);
                  float_to_json x_0;
                  float_to_json x_1;
                |]
               : Js.Json.t)
         | Point { x = x_x; y = x_y } ->
             (Obj.magic
                [|
                  (Obj.magic "Point" : Js.Json.t);
                  (Obj.magic
                     [%mel.obj
                       { x = float_to_json x_x; y = float_to_json x_y }]
                    : Js.Json.t);
                |]
               : Js.Json.t)
         | Empty -> (Obj.magic (Obj.magic "Empty" : Js.Json.t) : Js.Json.t)
        : shape -> Js.Json.t)
  
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
         if
           Stdlib.not
             (Stdlib.( && )
                (Stdlib.( = ) (Js.typeof x) "object")
                (Stdlib.( && )
                   (Stdlib.not (Js.Array.isArray x))
                   (Stdlib.not
                      (Stdlib.( == ) (Obj.magic x : 'a Js.null) Js.null))))
         then Melange_json.of_json_error ~json:x "expected a JSON object";
         let fs = (Obj.magic x : < field : Js.Json.t Js.undefined > Js.t) in
         {
           field =
             (match Js.Undefined.toOption fs##field with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"field\" to be present");
         }
        : Js.Json.t -> must_be_object)
  
    let _ = must_be_object_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_object_to_json =
      (fun x ->
         match x with
         | { field = x_field } ->
             (Obj.magic [%mel.obj { field = int_to_json x_field }]
               : Js.Json.t)
        : must_be_object -> Js.Json.t)
  
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
         if
           Stdlib.( && ) (Js.Array.isArray x)
             (Stdlib.( = )
                (Js.Array.length (Obj.magic x : Js.Json.t array))
                2)
         then
           let es = (Obj.magic x : Js.Json.t array) in
           ( int_of_json (Js.Array.unsafe_get es 0),
             int_of_json (Js.Array.unsafe_get es 1) )
         else
           Melange_json.of_json_error ~json:x
             "expected a JSON array of length 2"
        : Js.Json.t -> must_be_array_2)
  
    let _ = must_be_array_2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_2_to_json =
      (fun x ->
         match x with
         | x_0, x_1 ->
             (Obj.magic [| int_to_json x_0; int_to_json x_1 |] : Js.Json.t)
        : must_be_array_2 -> Js.Json.t)
  
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
         if
           Stdlib.( && ) (Js.Array.isArray x)
             (Stdlib.( = )
                (Js.Array.length (Obj.magic x : Js.Json.t array))
                3)
         then
           let es = (Obj.magic x : Js.Json.t array) in
           ( int_of_json (Js.Array.unsafe_get es 0),
             int_of_json (Js.Array.unsafe_get es 1),
             int_of_json (Js.Array.unsafe_get es 2) )
         else
           Melange_json.of_json_error ~json:x
             "expected a JSON array of length 3"
        : Js.Json.t -> must_be_array_3)
  
    let _ = must_be_array_3_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec must_be_array_3_to_json =
      (fun x ->
         match x with
         | x_0, x_1, x_2 ->
             (Obj.magic
                [| int_to_json x_0; int_to_json x_1; int_to_json x_2 |]
               : Js.Json.t)
        : must_be_array_3 -> Js.Json.t)
  
    let _ = must_be_array_3_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type legacy_variant = A | B of int [@@deriving json] [@@json.legacy_variant]
  > EOF
  type legacy_variant = A | B of int
  [@@deriving json] [@@json.legacy_variant]
  
  include struct
    let _ = fun (_ : legacy_variant) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec legacy_variant_of_json =
      (fun x ->
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if Stdlib.( > ) len 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Stdlib.( = ) (Js.typeof tag) "string" then
               let tag = (Obj.magic tag : string) in
               if Stdlib.( = ) tag "A" then
                 if Stdlib.( <> ) len 1 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 1"
                 else A
               else if Stdlib.( = ) tag "B" then
                 if Stdlib.( <> ) len 2 then
                   Melange_json.of_json_error ~json:x
                     "expected a JSON array of length 2"
                 else B (int_of_json (Js.Array.unsafe_get array 1))
               else
                 Melange_json.of_json_error ~json:x
                   "expected [\"B\", _] or [\"A\"]"
             else
               Melange_json.of_json_error ~json:x
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else if Stdlib.( = ) (Js.typeof x) "string" then
           let tag = (Obj.magic x : string) in
           if Stdlib.( = ) tag "A" then A
           else
             Melange_json.of_json_error ~json:x
               "expected a non empty JSON array"
         else
           Melange_json.of_json_error ~json:x
             "expected a non empty JSON array"
        : Js.Json.t -> legacy_variant)
  
    let _ = legacy_variant_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec legacy_variant_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic [| (Obj.magic "A" : Js.Json.t) |] : Js.Json.t)
         | B x_0 ->
             (Obj.magic [| (Obj.magic "B" : Js.Json.t); int_to_json x_0 |]
               : Js.Json.t)
        : legacy_variant -> Js.Json.t)
  
    let _ = legacy_variant_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

