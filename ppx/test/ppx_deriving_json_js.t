  $ alias run='../browser/ppx_deriving_json_js_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF
  type user = int [@@deriving json]
  
  include struct
    let _ = fun (_ : user) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_of_json =
      (fun x -> (int_of_json x [@ocaml.warning "-ignored-extra-argument"])
        : Js.Json.t -> user)
  
    let _ = user_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_to_json =
      (fun x -> (int_to_json x [@ocaml.warning "-ignored-extra-argument"])
        : user -> Js.Json.t)
  
    let _ = user_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a param = 'a [@@deriving json]
  > EOF
  type 'a param = 'a [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a param) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_of_json a_of_json : Js.Json.t -> 'a param =
     fun x -> (a_of_json x [@ocaml.warning "-ignored-extra-argument"])
  
    let _ = param_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_to_json a_to_json : 'a param -> Js.Json.t =
     fun x -> (a_to_json x [@ocaml.warning "-ignored-extra-argument"])
  
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
      (fun x ->
         ((option_of_json string_of_json
           [@ocaml.warning "-ignored-extra-argument"])
            x [@ocaml.warning "-ignored-extra-argument"])
        : Js.Json.t -> opt)
  
    let _ = opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_to_json =
      (fun x ->
         ((option_to_json string_to_json
           [@ocaml.warning "-ignored-extra-argument"])
            x [@ocaml.warning "-ignored-extra-argument"])
        : opt -> Js.Json.t)
  
    let _ = opt_to_json
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
           Stdlib.( && )
             (Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"])
             (Stdlib.( = )
                (Js.Array.length
                   ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
                     : Js.Json.t array)
                 [@ocaml.warning "-ignored-extra-argument"])
                2 [@ocaml.warning "-ignored-extra-argument"])
           [@ocaml.warning "-ignored-extra-argument"]
         then
           let es =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           ( (int_of_json
                (Js.Array.unsafe_get es 0
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"]),
             (string_of_json
                (Js.Array.unsafe_get es 1
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"]) )
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a JSON array of length 2"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> tuple)
  
    let _ = tuple_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_to_json =
      (fun x ->
         match x with
         | x_0, x_1 ->
             ((Obj.magic
                 [|
                   (int_to_json x_0
                    [@ocaml.warning "-ignored-extra-argument"]);
                   (string_to_json x_1
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
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
                (Stdlib.( = )
                   (Js.typeof x [@ocaml.warning "-ignored-extra-argument"])
                   "object" [@ocaml.warning "-ignored-extra-argument"])
                (Stdlib.( && )
                   (Stdlib.not
                      (Js.Array.isArray x
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                   (Stdlib.not
                      (Stdlib.( == )
                         ((Obj.magic x
                           [@ocaml.warning "-ignored-extra-argument"])
                           : 'a Js.null)
                         Js.null [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"])
           [@ocaml.warning "-ignored-extra-argument"]
         then
           Ppx_deriving_json_runtime.of_json_error "expected a JSON object"
           [@ocaml.warning "-ignored-extra-argument"];
         let fs =
           ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
             : < name : Js.Json.t Js.undefined
               ; age : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           name =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#name
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 string_of_json v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"name\""
                 [@ocaml.warning "-ignored-extra-argument"]);
           age =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#age
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 int_of_json v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"age\""
                 [@ocaml.warning "-ignored-extra-argument"]);
         }
        : Js.Json.t -> record)
  
    let _ = record_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             ((Obj.magic
                 (let module J = struct
                    external unsafe_expr :
                      name:'a0 -> age:'a1 -> < name : 'a0 ; age : 'a1 > Js.t
                      = ""
                        "\132\149\166\190\000\000\000\019\000\000\000\t\000\000\000\023\000\000\000\022\145\160\160A\144$name\160\160A\144#age@"
                    [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                  end in
                 (J.unsafe_expr
                    ~name:
                      (string_to_json x_name
                       [@ocaml.warning "-ignored-extra-argument"])
                    ~age:
                      (int_to_json x_age
                       [@ocaml.warning "-ignored-extra-argument"])
                  [@ocaml.warning "-ignored-extra-argument"]))
               [@ocaml.warning "-ignored-extra-argument"])
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
                (Stdlib.( = )
                   (Js.typeof x [@ocaml.warning "-ignored-extra-argument"])
                   "object" [@ocaml.warning "-ignored-extra-argument"])
                (Stdlib.( && )
                   (Stdlib.not
                      (Js.Array.isArray x
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                   (Stdlib.not
                      (Stdlib.( == )
                         ((Obj.magic x
                           [@ocaml.warning "-ignored-extra-argument"])
                           : 'a Js.null)
                         Js.null [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"])
           [@ocaml.warning "-ignored-extra-argument"]
         then
           Ppx_deriving_json_runtime.of_json_error "expected a JSON object"
           [@ocaml.warning "-ignored-extra-argument"];
         let fs =
           ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
             : < my_name : Js.Json.t Js.undefined
               ; my_age : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           name =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#my_name
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 string_of_json v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"my_name\""
                 [@ocaml.warning "-ignored-extra-argument"]);
           age =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#my_age
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 int_of_json v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None -> 100);
         }
        : Js.Json.t -> record_aliased)
  
    let _ = record_aliased_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_aliased_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             ((Obj.magic
                 (let module J = struct
                    external unsafe_expr :
                      my_name:'a0 ->
                      my_age:'a1 ->
                      < my_name : 'a0 ; my_age : 'a1 > Js.t
                      = ""
                        "\132\149\166\190\000\000\000\025\000\000\000\t\000\000\000\024\000\000\000\022\145\160\160A\144'my_name\160\160A\144&my_age@"
                    [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                  end in
                 (J.unsafe_expr
                    ~my_name:
                      (string_to_json x_name
                       [@ocaml.warning "-ignored-extra-argument"])
                    ~my_age:
                      (int_to_json x_age
                       [@ocaml.warning "-ignored-extra-argument"])
                  [@ocaml.warning "-ignored-extra-argument"]))
               [@ocaml.warning "-ignored-extra-argument"])
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
                (Stdlib.( = )
                   (Js.typeof x [@ocaml.warning "-ignored-extra-argument"])
                   "object" [@ocaml.warning "-ignored-extra-argument"])
                (Stdlib.( && )
                   (Stdlib.not
                      (Js.Array.isArray x
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                   (Stdlib.not
                      (Stdlib.( == )
                         ((Obj.magic x
                           [@ocaml.warning "-ignored-extra-argument"])
                           : 'a Js.null)
                         Js.null [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"])
           [@ocaml.warning "-ignored-extra-argument"]
         then
           Ppx_deriving_json_runtime.of_json_error "expected a JSON object"
           [@ocaml.warning "-ignored-extra-argument"];
         let fs =
           ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
             : < k : Js.Json.t Js.undefined > Js.t)
         in
         {
           k =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#k
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 (option_of_json int_of_json
                  [@ocaml.warning "-ignored-extra-argument"])
                   v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None -> Stdlib.Option.None);
         }
        : Js.Json.t -> record_opt)
  
    let _ = record_opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_opt_to_json =
      (fun x ->
         match x with
         | { k = x_k } ->
             ((Obj.magic
                 (let module J = struct
                    external unsafe_expr : k:'a0 -> < k : 'a0 > Js.t
                      = ""
                        "\132\149\166\190\000\000\000\b\000\000\000\005\000\000\000\012\000\000\000\012\145\160\160A\144!k@"
                    [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                  end in
                 (J.unsafe_expr
                    ~k:
                      ((option_to_json int_to_json
                        [@ocaml.warning "-ignored-extra-argument"])
                         x_k [@ocaml.warning "-ignored-extra-argument"])
                  [@ocaml.warning "-ignored-extra-argument"]))
               [@ocaml.warning "-ignored-extra-argument"])
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
         if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"]
         then
           let array =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           let len =
             (Js.Array.length array
              [@ocaml.warning "-ignored-extra-argument"])
           in
           if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
           then
             let tag =
               (Js.Array.unsafe_get array 0
                [@ocaml.warning "-ignored-extra-argument"])
             in
             if
               Stdlib.( = )
                 (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
                 "string" [@ocaml.warning "-ignored-extra-argument"]
             then
               let tag =
                 ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                   : string)
               in
               if
                 Stdlib.( = ) tag "A"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 1
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1"
                   [@ocaml.warning "-ignored-extra-argument"];
                 A)
               else if
                 Stdlib.( = ) tag "B"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 B
                   (int_of_json
                      (Js.Array.unsafe_get array 1
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"]))
               else if
                 Stdlib.( = ) tag "C"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 let fs =
                   (Js.Array.unsafe_get array 1
                    [@ocaml.warning "-ignored-extra-argument"])
                 in
                 if
                   Stdlib.not
                     (Stdlib.( && )
                        (Stdlib.( = )
                           (Js.typeof fs
                            [@ocaml.warning "-ignored-extra-argument"])
                           "object"
                         [@ocaml.warning "-ignored-extra-argument"])
                        (Stdlib.( && )
                           (Stdlib.not
                              (Js.Array.isArray fs
                               [@ocaml.warning "-ignored-extra-argument"])
                            [@ocaml.warning "-ignored-extra-argument"])
                           (Stdlib.not
                              (Stdlib.( == )
                                 ((Obj.magic fs
                                   [@ocaml.warning
                                     "-ignored-extra-argument"])
                                   : 'a Js.null)
                                 Js.null
                               [@ocaml.warning "-ignored-extra-argument"])
                            [@ocaml.warning "-ignored-extra-argument"])
                         [@ocaml.warning "-ignored-extra-argument"])
                      [@ocaml.warning "-ignored-extra-argument"])
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON object"
                   [@ocaml.warning "-ignored-extra-argument"];
                 let fs =
                   ((Obj.magic fs [@ocaml.warning "-ignored-extra-argument"])
                     : < name : Js.Json.t Js.undefined > Js.t)
                 in
                 C
                   {
                     name =
                       (match
                          Js.Undefined.toOption
                            (Js.OO.unsafe_downgrade fs)#name
                          [@ocaml.warning "-ignored-extra-argument"]
                        with
                       | Stdlib.Option.Some v ->
                           string_of_json v
                           [@ocaml.warning "-ignored-extra-argument"]
                       | Stdlib.Option.None ->
                           Ppx_deriving_json_runtime.of_json_error
                             "missing field \"name\""
                           [@ocaml.warning "-ignored-extra-argument"]);
                   })
               else
                 Ppx_deriving_json_runtime.of_json_error "invalid JSON"
                 [@ocaml.warning "-ignored-extra-argument"]
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string" [@ocaml.warning "-ignored-extra-argument"]
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
             [@ocaml.warning "-ignored-extra-argument"]
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> sum)
  
    let _ = sum_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_to_json =
      (fun x ->
         match x with
         | A ->
             ((Obj.magic
                 [|
                   (string_to_json "A"
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | B x_0 ->
             ((Obj.magic
                 [|
                   (string_to_json "B"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   (int_to_json x_0
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | C { name = x_name } ->
             ((Obj.magic
                 [|
                   (string_to_json "C"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   ((Obj.magic
                       (let module J = struct
                          external unsafe_expr :
                            name:'a0 -> < name : 'a0 > Js.t
                            = ""
                              "\132\149\166\190\000\000\000\011\000\000\000\005\000\000\000\r\000\000\000\012\145\160\160A\144$name@"
                          [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                        end in
                       (J.unsafe_expr
                          ~name:
                            (string_to_json x_name
                             [@ocaml.warning "-ignored-extra-argument"])
                        [@ocaml.warning "-ignored-extra-argument"]))
                     [@ocaml.warning "-ignored-extra-argument"])
                     : Js.Json.t);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
        : sum -> Js.Json.t)
  
    let _ = sum_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type other = [ `C ] [@@deriving json] type poly = [ `A | `B of int | other ] [@@deriving json]
  > EOF
  type other = [ `C ] [@@deriving json]
  
  include struct
    let _ = fun (_ : other) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_of_json_poly =
      (fun x ->
         let tag =
           (Ppx_deriving_json_runtime.Primitives.string_of_json x
            [@ocaml.warning "-ignored-extra-argument"])
         in
         if Stdlib.( = ) tag "C" [@ocaml.warning "-ignored-extra-argument"]
         then Some `C
         else None
        : Js.Json.t -> other option)
  
    and other_of_json =
      (fun x ->
         match
           other_of_json_poly x [@ocaml.warning "-ignored-extra-argument"]
         with
         | Some x -> x
         | None ->
             Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> other)
  
    let _ = other_of_json_poly
    and _ = other_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_to_json =
      (fun x ->
         match x with
         | `C ->
             ((Obj.magic
                 (string_to_json "C"
                  [@ocaml.warning "-ignored-extra-argument"])
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
        : other -> Js.Json.t)
  
    let _ = other_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
  type poly = [ `A | `B of int | other ] [@@deriving json]
  
  include struct
    let _ = fun (_ : poly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_of_json_poly =
      (fun x ->
         if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"]
         then
           let array =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           let len =
             (Js.Array.length array
              [@ocaml.warning "-ignored-extra-argument"])
           in
           if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
           then
             let tag =
               (Js.Array.unsafe_get array 0
                [@ocaml.warning "-ignored-extra-argument"])
             in
             if
               Stdlib.( = )
                 (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
                 "string" [@ocaml.warning "-ignored-extra-argument"]
             then
               let tag =
                 ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                   : string)
               in
               if
                 Stdlib.( = ) tag "A"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 1
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1"
                   [@ocaml.warning "-ignored-extra-argument"];
                 Some `A)
               else if
                 Stdlib.( = ) tag "B"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 Some
                   (`B
                     (int_of_json
                        (Js.Array.unsafe_get array 1
                         [@ocaml.warning "-ignored-extra-argument"])
                      [@ocaml.warning "-ignored-extra-argument"])))
               else
                 match
                   other_of_json_poly x
                   [@ocaml.warning "-ignored-extra-argument"]
                 with
                 | Some x -> (Some x :> [ `A | `B of int | other ] option)
                 | None -> None
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string" [@ocaml.warning "-ignored-extra-argument"]
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
             [@ocaml.warning "-ignored-extra-argument"]
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> poly option)
  
    and poly_of_json =
      (fun x ->
         match
           poly_of_json_poly x [@ocaml.warning "-ignored-extra-argument"]
         with
         | Some x -> x
         | None ->
             Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> poly)
  
    let _ = poly_of_json_poly
    and _ = poly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_to_json =
      (fun x ->
         match x with
         | `A ->
             ((Obj.magic
                 [|
                   (string_to_json "A"
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | `B x_0 ->
             ((Obj.magic
                 [|
                   (string_to_json "B"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   (int_to_json x_0
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | #other as x ->
             other_to_json x [@ocaml.warning "-ignored-extra-argument"]
        : poly -> Js.Json.t)
  
    let _ = poly_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a c = [ `C of 'a ] [@@deriving json]
  > EOF
  type 'a c = [ `C of 'a ] [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a c) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_of_json_poly a_of_json : Js.Json.t -> 'a c option =
     fun x ->
      if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"] then
        let array =
          ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
            : Js.Json.t array)
        in
        let len =
          (Js.Array.length array [@ocaml.warning "-ignored-extra-argument"])
        in
        if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
        then
          let tag =
            (Js.Array.unsafe_get array 0
             [@ocaml.warning "-ignored-extra-argument"])
          in
          if
            Stdlib.( = )
              (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
              "string" [@ocaml.warning "-ignored-extra-argument"]
          then
            let tag =
              ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                : string)
            in
            if
              Stdlib.( = ) tag "C"
              [@ocaml.warning "-ignored-extra-argument"]
            then (
              if
                Stdlib.( <> ) len 2
                [@ocaml.warning "-ignored-extra-argument"]
              then
                Ppx_deriving_json_runtime.of_json_error
                  "expected a JSON array of length 2"
                [@ocaml.warning "-ignored-extra-argument"];
              Some
                (`C
                  (a_of_json
                     (Js.Array.unsafe_get array 1
                      [@ocaml.warning "-ignored-extra-argument"])
                   [@ocaml.warning "-ignored-extra-argument"])))
            else None
          else
            Ppx_deriving_json_runtime.of_json_error
              "expected a non empty JSON array with element being a string"
            [@ocaml.warning "-ignored-extra-argument"]
        else
          Ppx_deriving_json_runtime.of_json_error
            "expected a non empty JSON array"
          [@ocaml.warning "-ignored-extra-argument"]
      else
        Ppx_deriving_json_runtime.of_json_error
          "expected a non empty JSON array"
        [@ocaml.warning "-ignored-extra-argument"]
  
    and c_of_json a_of_json : Js.Json.t -> 'a c =
     fun x ->
      match
        (c_of_json_poly a_of_json [@ocaml.warning "-ignored-extra-argument"])
          x [@ocaml.warning "-ignored-extra-argument"]
      with
      | Some x -> x
      | None ->
          Ppx_deriving_json_runtime.of_json_error "invalid JSON"
          [@ocaml.warning "-ignored-extra-argument"]
  
    let _ = c_of_json_poly
    and _ = c_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_to_json a_to_json : 'a c -> Js.Json.t =
     fun x ->
      match x with
      | `C x_0 ->
          ((Obj.magic
              [|
                (string_to_json "C"
                 [@ocaml.warning "-ignored-extra-argument"]);
                (a_to_json x_0 [@ocaml.warning "-ignored-extra-argument"]);
              |] [@ocaml.warning "-ignored-extra-argument"])
            : Js.Json.t)
  
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
         if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"]
         then
           let array =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           let len =
             (Js.Array.length array
              [@ocaml.warning "-ignored-extra-argument"])
           in
           if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
           then
             let tag =
               (Js.Array.unsafe_get array 0
                [@ocaml.warning "-ignored-extra-argument"])
             in
             if
               Stdlib.( = )
                 (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
                 "string" [@ocaml.warning "-ignored-extra-argument"]
             then
               let tag =
                 ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                   : string)
               in
               if
                 Stdlib.( = ) tag "A"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 1
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1"
                   [@ocaml.warning "-ignored-extra-argument"];
                 A)
               else if
                 Stdlib.( = ) tag "Fix"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 Fix
                   (recur_of_json
                      (Js.Array.unsafe_get array 1
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"]))
               else
                 Ppx_deriving_json_runtime.of_json_error "invalid JSON"
                 [@ocaml.warning "-ignored-extra-argument"]
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string" [@ocaml.warning "-ignored-extra-argument"]
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
             [@ocaml.warning "-ignored-extra-argument"]
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> recur)
  
    let _ = recur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_to_json =
      (fun x ->
         match x with
         | A ->
             ((Obj.magic
                 [|
                   (string_to_json "A"
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | Fix x_0 ->
             ((Obj.magic
                 [|
                   (string_to_json "Fix"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   (recur_to_json x_0
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
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
  
    let rec polyrecur_of_json_poly =
      (fun x ->
         if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"]
         then
           let array =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           let len =
             (Js.Array.length array
              [@ocaml.warning "-ignored-extra-argument"])
           in
           if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
           then
             let tag =
               (Js.Array.unsafe_get array 0
                [@ocaml.warning "-ignored-extra-argument"])
             in
             if
               Stdlib.( = )
                 (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
                 "string" [@ocaml.warning "-ignored-extra-argument"]
             then
               let tag =
                 ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                   : string)
               in
               if
                 Stdlib.( = ) tag "A"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 1
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1"
                   [@ocaml.warning "-ignored-extra-argument"];
                 Some `A)
               else if
                 Stdlib.( = ) tag "Fix"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 Some
                   (`Fix
                     (polyrecur_of_json
                        (Js.Array.unsafe_get array 1
                         [@ocaml.warning "-ignored-extra-argument"])
                      [@ocaml.warning "-ignored-extra-argument"])))
               else None
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string" [@ocaml.warning "-ignored-extra-argument"]
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
             [@ocaml.warning "-ignored-extra-argument"]
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> polyrecur option)
  
    and polyrecur_of_json =
      (fun x ->
         match
           polyrecur_of_json_poly x
           [@ocaml.warning "-ignored-extra-argument"]
         with
         | Some x -> x
         | None ->
             Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> polyrecur)
  
    let _ = polyrecur_of_json_poly
    and _ = polyrecur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_to_json =
      (fun x ->
         match x with
         | `A ->
             ((Obj.magic
                 [|
                   (string_to_json "A"
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | `Fix x_0 ->
             ((Obj.magic
                 [|
                   (string_to_json "Fix"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   (polyrecur_to_json x_0
                    [@ocaml.warning "-ignored-extra-argument"]);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
        : polyrecur -> Js.Json.t)
  
    let _ = polyrecur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type evar = A | B [@json.as "b_aliased"] [@@deriving json]
  > EOF
  type evar = A | B [@json.as "b_aliased"] [@@deriving json]
  
  include struct
    let _ = fun (_ : evar) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec evar_of_json =
      (fun x ->
         let tag =
           (Ppx_deriving_json_runtime.Primitives.string_of_json x
            [@ocaml.warning "-ignored-extra-argument"])
         in
         if Stdlib.( = ) tag "A" [@ocaml.warning "-ignored-extra-argument"]
         then A
         else if
           Stdlib.( = ) tag "b_aliased"
           [@ocaml.warning "-ignored-extra-argument"]
         then B
         else
           Ppx_deriving_json_runtime.of_json_error "invalid JSON"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> evar)
  
    let _ = evar_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec evar_to_json =
      (fun x ->
         match x with
         | A ->
             ((Obj.magic
                 (string_to_json "A"
                  [@ocaml.warning "-ignored-extra-argument"])
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | B ->
             ((Obj.magic
                 (string_to_json "b_aliased"
                  [@ocaml.warning "-ignored-extra-argument"])
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
        : evar -> Js.Json.t)
  
    let _ = evar_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type epoly = [ `a [@json.as "A_aliased"] | `b ] [@@deriving json]
  > EOF
  type epoly = [ `a [@json.as "A_aliased"] | `b ] [@@deriving json]
  
  include struct
    let _ = fun (_ : epoly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec epoly_of_json_poly =
      (fun x ->
         let tag =
           (Ppx_deriving_json_runtime.Primitives.string_of_json x
            [@ocaml.warning "-ignored-extra-argument"])
         in
         if
           Stdlib.( = ) tag "A_aliased"
           [@ocaml.warning "-ignored-extra-argument"]
         then Some `a
         else if
           Stdlib.( = ) tag "b" [@ocaml.warning "-ignored-extra-argument"]
         then Some `b
         else None
        : Js.Json.t -> epoly option)
  
    and epoly_of_json =
      (fun x ->
         match
           epoly_of_json_poly x [@ocaml.warning "-ignored-extra-argument"]
         with
         | Some x -> x
         | None ->
             Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> epoly)
  
    let _ = epoly_of_json_poly
    and _ = epoly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec epoly_to_json =
      (fun x ->
         match x with
         | `a ->
             ((Obj.magic
                 (string_to_json "A_aliased"
                  [@ocaml.warning "-ignored-extra-argument"])
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
         | `b ->
             ((Obj.magic
                 (string_to_json "b"
                  [@ocaml.warning "-ignored-extra-argument"])
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
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
  
    let rec p2_of_json a_of_json b_of_json : Js.Json.t -> ('a, 'b) p2 =
     fun x ->
      if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"] then
        let array =
          ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
            : Js.Json.t array)
        in
        let len =
          (Js.Array.length array [@ocaml.warning "-ignored-extra-argument"])
        in
        if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
        then
          let tag =
            (Js.Array.unsafe_get array 0
             [@ocaml.warning "-ignored-extra-argument"])
          in
          if
            Stdlib.( = )
              (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
              "string" [@ocaml.warning "-ignored-extra-argument"]
          then
            let tag =
              ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                : string)
            in
            if
              Stdlib.( = ) tag "A"
              [@ocaml.warning "-ignored-extra-argument"]
            then (
              if
                Stdlib.( <> ) len 2
                [@ocaml.warning "-ignored-extra-argument"]
              then
                Ppx_deriving_json_runtime.of_json_error
                  "expected a JSON array of length 2"
                [@ocaml.warning "-ignored-extra-argument"];
              A
                (a_of_json
                   (Js.Array.unsafe_get array 1
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"]))
            else if
              Stdlib.( = ) tag "B"
              [@ocaml.warning "-ignored-extra-argument"]
            then (
              if
                Stdlib.( <> ) len 2
                [@ocaml.warning "-ignored-extra-argument"]
              then
                Ppx_deriving_json_runtime.of_json_error
                  "expected a JSON array of length 2"
                [@ocaml.warning "-ignored-extra-argument"];
              B
                (b_of_json
                   (Js.Array.unsafe_get array 1
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"]))
            else
              Ppx_deriving_json_runtime.of_json_error "invalid JSON"
              [@ocaml.warning "-ignored-extra-argument"]
          else
            Ppx_deriving_json_runtime.of_json_error
              "expected a non empty JSON array with element being a string"
            [@ocaml.warning "-ignored-extra-argument"]
        else
          Ppx_deriving_json_runtime.of_json_error
            "expected a non empty JSON array"
          [@ocaml.warning "-ignored-extra-argument"]
      else
        Ppx_deriving_json_runtime.of_json_error
          "expected a non empty JSON array"
        [@ocaml.warning "-ignored-extra-argument"]
  
    let _ = p2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec p2_to_json a_to_json b_to_json : ('a, 'b) p2 -> Js.Json.t =
     fun x ->
      match x with
      | A x_0 ->
          ((Obj.magic
              [|
                (string_to_json "A"
                 [@ocaml.warning "-ignored-extra-argument"]);
                (a_to_json x_0 [@ocaml.warning "-ignored-extra-argument"]);
              |] [@ocaml.warning "-ignored-extra-argument"])
            : Js.Json.t)
      | B x_0 ->
          ((Obj.magic
              [|
                (string_to_json "B"
                 [@ocaml.warning "-ignored-extra-argument"]);
                (b_to_json x_0 [@ocaml.warning "-ignored-extra-argument"]);
              |] [@ocaml.warning "-ignored-extra-argument"])
            : Js.Json.t)
  
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
                (Stdlib.( = )
                   (Js.typeof x [@ocaml.warning "-ignored-extra-argument"])
                   "object" [@ocaml.warning "-ignored-extra-argument"])
                (Stdlib.( && )
                   (Stdlib.not
                      (Js.Array.isArray x
                       [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                   (Stdlib.not
                      (Stdlib.( == )
                         ((Obj.magic x
                           [@ocaml.warning "-ignored-extra-argument"])
                           : 'a Js.null)
                         Js.null [@ocaml.warning "-ignored-extra-argument"])
                    [@ocaml.warning "-ignored-extra-argument"])
                 [@ocaml.warning "-ignored-extra-argument"])
              [@ocaml.warning "-ignored-extra-argument"])
           [@ocaml.warning "-ignored-extra-argument"]
         then
           Ppx_deriving_json_runtime.of_json_error "expected a JSON object"
           [@ocaml.warning "-ignored-extra-argument"];
         let fs =
           ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
             : < a : Js.Json.t Js.undefined > Js.t)
         in
         {
           a =
             (match
                Js.Undefined.toOption (Js.OO.unsafe_downgrade fs)#a
                [@ocaml.warning "-ignored-extra-argument"]
              with
             | Stdlib.Option.Some v ->
                 int_of_json v [@ocaml.warning "-ignored-extra-argument"]
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"a\""
                 [@ocaml.warning "-ignored-extra-argument"]);
         }
        : Js.Json.t -> allow_extra_fields)
  
    let _ = allow_extra_fields_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields_to_json =
      (fun x ->
         match x with
         | { a = x_a } ->
             ((Obj.magic
                 (let module J = struct
                    external unsafe_expr : a:'a0 -> < a : 'a0 > Js.t
                      = ""
                        "\132\149\166\190\000\000\000\b\000\000\000\005\000\000\000\012\000\000\000\012\145\160\160A\144!a@"
                    [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                  end in
                 (J.unsafe_expr
                    ~a:
                      (int_to_json x_a
                       [@ocaml.warning "-ignored-extra-argument"])
                  [@ocaml.warning "-ignored-extra-argument"]))
               [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
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
         if Js.Array.isArray x [@ocaml.warning "-ignored-extra-argument"]
         then
           let array =
             ((Obj.magic x [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t array)
           in
           let len =
             (Js.Array.length array
              [@ocaml.warning "-ignored-extra-argument"])
           in
           if Stdlib.( > ) len 0 [@ocaml.warning "-ignored-extra-argument"]
           then
             let tag =
               (Js.Array.unsafe_get array 0
                [@ocaml.warning "-ignored-extra-argument"])
             in
             if
               Stdlib.( = )
                 (Js.typeof tag [@ocaml.warning "-ignored-extra-argument"])
                 "string" [@ocaml.warning "-ignored-extra-argument"]
             then
               let tag =
                 ((Obj.magic tag [@ocaml.warning "-ignored-extra-argument"])
                   : string)
               in
               if
                 Stdlib.( = ) tag "A"
                 [@ocaml.warning "-ignored-extra-argument"]
               then (
                 if
                   Stdlib.( <> ) len 2
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2"
                   [@ocaml.warning "-ignored-extra-argument"];
                 let fs =
                   (Js.Array.unsafe_get array 1
                    [@ocaml.warning "-ignored-extra-argument"])
                 in
                 if
                   Stdlib.not
                     (Stdlib.( && )
                        (Stdlib.( = )
                           (Js.typeof fs
                            [@ocaml.warning "-ignored-extra-argument"])
                           "object"
                         [@ocaml.warning "-ignored-extra-argument"])
                        (Stdlib.( && )
                           (Stdlib.not
                              (Js.Array.isArray fs
                               [@ocaml.warning "-ignored-extra-argument"])
                            [@ocaml.warning "-ignored-extra-argument"])
                           (Stdlib.not
                              (Stdlib.( == )
                                 ((Obj.magic fs
                                   [@ocaml.warning
                                     "-ignored-extra-argument"])
                                   : 'a Js.null)
                                 Js.null
                               [@ocaml.warning "-ignored-extra-argument"])
                            [@ocaml.warning "-ignored-extra-argument"])
                         [@ocaml.warning "-ignored-extra-argument"])
                      [@ocaml.warning "-ignored-extra-argument"])
                   [@ocaml.warning "-ignored-extra-argument"]
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON object"
                   [@ocaml.warning "-ignored-extra-argument"];
                 let fs =
                   ((Obj.magic fs [@ocaml.warning "-ignored-extra-argument"])
                     : < a : Js.Json.t Js.undefined > Js.t)
                 in
                 A
                   {
                     a =
                       (match
                          Js.Undefined.toOption
                            (Js.OO.unsafe_downgrade fs)#a
                          [@ocaml.warning "-ignored-extra-argument"]
                        with
                       | Stdlib.Option.Some v ->
                           int_of_json v
                           [@ocaml.warning "-ignored-extra-argument"]
                       | Stdlib.Option.None ->
                           Ppx_deriving_json_runtime.of_json_error
                             "missing field \"a\""
                           [@ocaml.warning "-ignored-extra-argument"]);
                   })
               else
                 Ppx_deriving_json_runtime.of_json_error "invalid JSON"
                 [@ocaml.warning "-ignored-extra-argument"]
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string" [@ocaml.warning "-ignored-extra-argument"]
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
             [@ocaml.warning "-ignored-extra-argument"]
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
           [@ocaml.warning "-ignored-extra-argument"]
        : Js.Json.t -> allow_extra_fields2)
  
    let _ = allow_extra_fields2_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec allow_extra_fields2_to_json =
      (fun x ->
         match x with
         | A { a = x_a } ->
             ((Obj.magic
                 [|
                   (string_to_json "A"
                    [@ocaml.warning "-ignored-extra-argument"]);
                   ((Obj.magic
                       (let module J = struct
                          external unsafe_expr : a:'a0 -> < a : 'a0 > Js.t
                            = ""
                              "\132\149\166\190\000\000\000\b\000\000\000\005\000\000\000\012\000\000\000\012\145\160\160A\144!a@"
                          [@@ocaml.warning "-unboxable-type-in-prim-decl"]
                        end in
                       (J.unsafe_expr
                          ~a:
                            (int_to_json x_a
                             [@ocaml.warning "-ignored-extra-argument"])
                        [@ocaml.warning "-ignored-extra-argument"]))
                     [@ocaml.warning "-ignored-extra-argument"])
                     : Js.Json.t);
                 |] [@ocaml.warning "-ignored-extra-argument"])
               : Js.Json.t)
        : allow_extra_fields2 -> Js.Json.t)
  
    let _ = allow_extra_fields2_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

