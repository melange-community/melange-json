  $ dune build ./prettify.exe
Uncomment to debug
  $ ocamlopt -dsource _build/default/prettify.pp.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = []
    }]
  type variant =
    | Other of Yojson.Basic.t [@allow_any ]
    | Foo [@@deriving json]
  include
    struct
      let _ = fun (_ : variant) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec variant_of_json =
        (fun x ->
           match x with
           | `List ((`String "Foo")::[]) -> Foo
           | _ -> Other x
           | _ ->
               Ppx_deriving_json_runtime.of_json_error ~json:x
                 "expected [\"Other\", _] or [\"Foo\"]" : Yojson.Basic.t ->
                                                            variant)
      let _ = variant_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec variant_to_json =
        (fun x ->
           match x with | Other x_0 -> x_0 | Foo -> `List [`String "Foo"] : 
        variant -> Yojson.Basic.t)
      let _ = variant_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type j = {
    a: variant }[@@deriving json]
  include
    struct
      let _ = fun (_ : j) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec j_of_json =
        (fun x ->
           match x with
           | `Assoc fs ->
               let x_a = ref Stdlib.Option.None in
               let rec iter =
                 function
                 | [] -> ()
                 | (n', v)::fs ->
                     ((match n' with
                       | "a" -> x_a := (Stdlib.Option.Some (variant_of_json v))
                       | name ->
                           Ppx_deriving_json_runtime.of_json_error ~json:x
                             (Stdlib.Printf.sprintf
                                {|did not expect field "%s"|} name));
                      iter fs) in
               (iter fs;
                {
                  a =
                    ((match Stdlib.(!) x_a with
                      | Stdlib.Option.Some v -> v
                      | Stdlib.Option.None ->
                          Ppx_deriving_json_runtime.of_json_error ~json:x
                            "expected field \"a\""))
                })
           | _ ->
               Ppx_deriving_json_runtime.of_json_error ~json:x
                 "expected a JSON object" : Yojson.Basic.t -> j)
      let _ = j_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec j_to_json =
        (fun x ->
           match x with
           | { a = x_a } ->
               `Assoc
                 (let bnds__001_ = [] in
                  let bnds__001_ = ("a", (variant_to_json x_a)) :: bnds__001_ in
                  bnds__001_) : j -> Yojson.Basic.t)
      let _ = j_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    ((((((Sys.argv.(1)) |> Yojson.Basic.from_string) |> j_of_json) |>
         (fun j ->
            (match j with
             | { a = Other _ } -> print_endline "got Other"
             | { a = Foo } -> print_endline "got Foo");
            j))
        |> j_to_json)
       |> Yojson.Basic.pretty_to_string)
      |> print_endline
  File "prettify.ml", line 4, characters 11-25:
  4 | | Other of Yojson.Basic.t [@allow_any]
                 ^^^^^^^^^^^^^^
  Error: Unbound module Yojson
  [2]
  $ dune exec ./prettify.exe -- '{ "a": ["Foo"] }'
  got Foo
  { "a": [ "Foo" ] }
  $ dune exec ./prettify.exe -- '{ "a": [ "Bar" ] }'
  got Other
  { "a": [ "Bar" ] }
  $ dune exec ./prettify.exe -- '{ "a": [ "Foo", 1, 2, 3 ] }'
  got Other
  { "a": [ "Foo", 1, 2, 3 ] }




