We can alias poly varaints:
  $ echo '
  > type t = [`A | `B] [@@deriving json]
  > type u = t [@@deriving json]
  > let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  > let () = assert (u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|}) = `B)
  > ' | ./run.sh
  === ppx output:native ===
  type t = [ `A  | `B ][@@deriving json]
  include
    struct
      let _ = fun (_ : t) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec of_json =
        (fun x ->
           match x with
           | `List ((`String "A")::[]) -> `A
           | `List ((`String "B")::[]) -> `B
           | x ->
               raise
                 (Ppx_deriving_json_runtime.Of_json_error
                    (Ppx_deriving_json_runtime.Unexpected_variant
                       "unexpected variant")) : Yojson.Basic.t -> t)
      let _ = of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec to_json =
        (fun x ->
           match x with | `A -> `List [`String "A"] | `B -> `List [`String "B"] : 
        t -> Yojson.Basic.t)
      let _ = to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type u = t[@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json = (fun x -> of_json x : Yojson.Basic.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json = (fun x -> to_json x : u -> Yojson.Basic.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  === ppx output:browser ===
  type t = [ `A  | `B ][@@deriving json]
  include
    struct
      let _ = fun (_ : t) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec of_json =
        (fun x ->
           if Js.Array.isArray x
           then
             let array = (Obj.magic x : Js.Json.t array) in
             let len = Js.Array.length array in
             (if Stdlib.(>) len 0
              then
                let tag = Js.Array.unsafe_get array 0 in
                (if Stdlib.(=) (Js.typeof tag) "string"
                 then
                   let tag = (Obj.magic tag : string) in
                   (if Stdlib.(=) tag "A"
                    then
                      (if Stdlib.(<>) len 1
                       then
                         Ppx_deriving_json_runtime.of_json_error
                           "expected a JSON array of length 1";
                       `A)
                    else
                      if Stdlib.(=) tag "B"
                      then
                        (if Stdlib.(<>) len 1
                         then
                           Ppx_deriving_json_runtime.of_json_error
                             "expected a JSON array of length 1";
                         `B)
                      else
                        raise
                          (Ppx_deriving_json_runtime.Of_json_error
                             (Ppx_deriving_json_runtime.Unexpected_variant
                                "unexpected variant")))
                 else
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a non empty JSON array with element being a string")
              else
                Ppx_deriving_json_runtime.of_json_error
                  "expected a non empty JSON array")
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array" : Js.Json.t -> t)
      let _ = of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec to_json =
        (fun x ->
           match x with
           | `A -> (Obj.magic [|(Obj.magic "A" : Js.Json.t)|] : Js.Json.t)
           | `B -> (Obj.magic [|(Obj.magic "B" : Js.Json.t)|] : Js.Json.t) : 
        t -> Js.Json.t)
      let _ = to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type u = t[@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json = (fun x -> of_json x : Js.Json.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json = (fun x -> to_json x : u -> Js.Json.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  === stdout:native ===
  ["A"]
  === stdout:js ===
  ["A"]

We can extend aliased polyvariants:
  $ echo '
  > type t = [`A | `B] [@@deriving json]
  > type u = [t | `C] [@@deriving json]
  > let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  > let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  > let () = assert (u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|}) = `B)
  > let () = assert (u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|}) = `C)
  > ' | ./run.sh
  === ppx output:native ===
  type t = [ `A  | `B ][@@deriving json]
  include
    struct
      let _ = fun (_ : t) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec of_json =
        (fun x ->
           match x with
           | `List ((`String "A")::[]) -> `A
           | `List ((`String "B")::[]) -> `B
           | x ->
               raise
                 (Ppx_deriving_json_runtime.Of_json_error
                    (Ppx_deriving_json_runtime.Unexpected_variant
                       "unexpected variant")) : Yojson.Basic.t -> t)
      let _ = of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec to_json =
        (fun x ->
           match x with | `A -> `List [`String "A"] | `B -> `List [`String "B"] : 
        t -> Yojson.Basic.t)
      let _ = to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type u = [ | t | `C ][@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json =
        (fun x ->
           match x with
           | `List ((`String "C")::[]) -> `C
           | x ->
               (match of_json x with
                | x -> (x :> [ | t | `C ])
                | exception Ppx_deriving_json_runtime.Of_json_error
                    (Ppx_deriving_json_runtime.Unexpected_variant _) ->
                    raise
                      (Ppx_deriving_json_runtime.Of_json_error
                         (Ppx_deriving_json_runtime.Unexpected_variant
                            "unexpected variant"))) : Yojson.Basic.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json =
        (fun x ->
           match x with | #t as x -> to_json x | `C -> `List [`String "C"] : 
        u -> Yojson.Basic.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|})) = `C)
  === ppx output:browser ===
  type t = [ `A  | `B ][@@deriving json]
  include
    struct
      let _ = fun (_ : t) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec of_json =
        (fun x ->
           if Js.Array.isArray x
           then
             let array = (Obj.magic x : Js.Json.t array) in
             let len = Js.Array.length array in
             (if Stdlib.(>) len 0
              then
                let tag = Js.Array.unsafe_get array 0 in
                (if Stdlib.(=) (Js.typeof tag) "string"
                 then
                   let tag = (Obj.magic tag : string) in
                   (if Stdlib.(=) tag "A"
                    then
                      (if Stdlib.(<>) len 1
                       then
                         Ppx_deriving_json_runtime.of_json_error
                           "expected a JSON array of length 1";
                       `A)
                    else
                      if Stdlib.(=) tag "B"
                      then
                        (if Stdlib.(<>) len 1
                         then
                           Ppx_deriving_json_runtime.of_json_error
                             "expected a JSON array of length 1";
                         `B)
                      else
                        raise
                          (Ppx_deriving_json_runtime.Of_json_error
                             (Ppx_deriving_json_runtime.Unexpected_variant
                                "unexpected variant")))
                 else
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a non empty JSON array with element being a string")
              else
                Ppx_deriving_json_runtime.of_json_error
                  "expected a non empty JSON array")
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array" : Js.Json.t -> t)
      let _ = of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec to_json =
        (fun x ->
           match x with
           | `A -> (Obj.magic [|(Obj.magic "A" : Js.Json.t)|] : Js.Json.t)
           | `B -> (Obj.magic [|(Obj.magic "B" : Js.Json.t)|] : Js.Json.t) : 
        t -> Js.Json.t)
      let _ = to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type u = [ | t | `C ][@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json =
        (fun x ->
           if Js.Array.isArray x
           then
             let array = (Obj.magic x : Js.Json.t array) in
             let len = Js.Array.length array in
             (if Stdlib.(>) len 0
              then
                let tag = Js.Array.unsafe_get array 0 in
                (if Stdlib.(=) (Js.typeof tag) "string"
                 then
                   let tag = (Obj.magic tag : string) in
                   match of_json x with
                   | e -> (e :> [ | t | `C ])
                   | exception Ppx_deriving_json_runtime.Of_json_error
                       (Ppx_deriving_json_runtime.Unexpected_variant _) ->
                       (if Stdlib.(=) tag "C"
                        then
                          (if Stdlib.(<>) len 1
                           then
                             Ppx_deriving_json_runtime.of_json_error
                               "expected a JSON array of length 1";
                           `C)
                        else
                          raise
                            (Ppx_deriving_json_runtime.Of_json_error
                               (Ppx_deriving_json_runtime.Unexpected_variant
                                  "unexpected variant")))
                 else
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a non empty JSON array with element being a string")
              else
                Ppx_deriving_json_runtime.of_json_error
                  "expected a non empty JSON array")
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array" : Js.Json.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json =
        (fun x ->
           match x with
           | #t as x -> to_json x
           | `C -> (Obj.magic [|(Obj.magic "C" : Js.Json.t)|] : Js.Json.t) : 
        u -> Js.Json.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|})) = `C)
  === stdout:native ===
  ["A"]
  ["C"]
  === stdout:js ===
  File ".lib.objs/melange/_unknown_", line 1, characters 0-0:
  melc: internal error, uncaught exception:
        Not_found
        
  [1]

We can extend poly variants which are placed behind signatures:
  $ echo '
  > module P : sig
  >   type t = [`A | `B] [@@deriving json]
  > end = struct
  >   type t = [`A | `B] [@@deriving json]
  > end
  > type u = [P.t | `C] [@@deriving json]
  > let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  > let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  > let () = assert (u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|}) = `B)
  > let () = assert (u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|}) = `C)
  > ' | ./run.sh
  === ppx output:native ===
  module P :
    sig
      type t = [ `A  | `B ][@@deriving json]
      include
        sig
          [@@@ocaml.warning "-32"]
          val of_json : Yojson.Basic.t -> t
          val to_json : t -> Yojson.Basic.t
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end =
    struct
      type t = [ `A  | `B ][@@deriving json]
      include
        struct
          let _ = fun (_ : t) -> ()
          [@@@ocaml.warning "-39-11-27"]
          let rec of_json =
            (fun x ->
               match x with
               | `List ((`String "A")::[]) -> `A
               | `List ((`String "B")::[]) -> `B
               | x ->
                   raise
                     (Ppx_deriving_json_runtime.Of_json_error
                        (Ppx_deriving_json_runtime.Unexpected_variant
                           "unexpected variant")) : Yojson.Basic.t -> t)
          let _ = of_json
          [@@@ocaml.warning "-39-11-27"]
          let rec to_json =
            (fun x ->
               match x with
               | `A -> `List [`String "A"]
               | `B -> `List [`String "B"] : t -> Yojson.Basic.t)
          let _ = to_json
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end 
  type u = [ | P.t | `C ][@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json =
        (fun x ->
           match x with
           | `List ((`String "C")::[]) -> `C
           | x ->
               (match P.of_json x with
                | x -> (x :> [ | P.t | `C ])
                | exception Ppx_deriving_json_runtime.Of_json_error
                    (Ppx_deriving_json_runtime.Unexpected_variant _) ->
                    raise
                      (Ppx_deriving_json_runtime.Of_json_error
                         (Ppx_deriving_json_runtime.Unexpected_variant
                            "unexpected variant"))) : Yojson.Basic.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json =
        (fun x ->
           match x with | #P.t as x -> P.to_json x | `C -> `List [`String "C"] : 
        u -> Yojson.Basic.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|})) = `C)
  === ppx output:browser ===
  module P :
    sig
      type t = [ `A  | `B ][@@deriving json]
      include
        sig
          [@@@ocaml.warning "-32"]
          val of_json : Js.Json.t -> t
          val to_json : t -> Js.Json.t
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end =
    struct
      type t = [ `A  | `B ][@@deriving json]
      include
        struct
          let _ = fun (_ : t) -> ()
          [@@@ocaml.warning "-39-11-27"]
          let rec of_json =
            (fun x ->
               if Js.Array.isArray x
               then
                 let array = (Obj.magic x : Js.Json.t array) in
                 let len = Js.Array.length array in
                 (if Stdlib.(>) len 0
                  then
                    let tag = Js.Array.unsafe_get array 0 in
                    (if Stdlib.(=) (Js.typeof tag) "string"
                     then
                       let tag = (Obj.magic tag : string) in
                       (if Stdlib.(=) tag "A"
                        then
                          (if Stdlib.(<>) len 1
                           then
                             Ppx_deriving_json_runtime.of_json_error
                               "expected a JSON array of length 1";
                           `A)
                        else
                          if Stdlib.(=) tag "B"
                          then
                            (if Stdlib.(<>) len 1
                             then
                               Ppx_deriving_json_runtime.of_json_error
                                 "expected a JSON array of length 1";
                             `B)
                          else
                            raise
                              (Ppx_deriving_json_runtime.Of_json_error
                                 (Ppx_deriving_json_runtime.Unexpected_variant
                                    "unexpected variant")))
                     else
                       Ppx_deriving_json_runtime.of_json_error
                         "expected a non empty JSON array with element being a string")
                  else
                    Ppx_deriving_json_runtime.of_json_error
                      "expected a non empty JSON array")
               else
                 Ppx_deriving_json_runtime.of_json_error
                   "expected a non empty JSON array" : Js.Json.t -> t)
          let _ = of_json
          [@@@ocaml.warning "-39-11-27"]
          let rec to_json =
            (fun x ->
               match x with
               | `A -> (Obj.magic [|(Obj.magic "A" : Js.Json.t)|] : Js.Json.t)
               | `B -> (Obj.magic [|(Obj.magic "B" : Js.Json.t)|] : Js.Json.t) : 
            t -> Js.Json.t)
          let _ = to_json
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end 
  type u = [ | P.t | `C ][@@deriving json]
  include
    struct
      let _ = fun (_ : u) -> ()
      [@@@ocaml.warning "-39-11-27"]
      let rec u_of_json =
        (fun x ->
           if Js.Array.isArray x
           then
             let array = (Obj.magic x : Js.Json.t array) in
             let len = Js.Array.length array in
             (if Stdlib.(>) len 0
              then
                let tag = Js.Array.unsafe_get array 0 in
                (if Stdlib.(=) (Js.typeof tag) "string"
                 then
                   let tag = (Obj.magic tag : string) in
                   match P.of_json x with
                   | e -> (e :> [ | P.t | `C ])
                   | exception Ppx_deriving_json_runtime.Of_json_error
                       (Ppx_deriving_json_runtime.Unexpected_variant _) ->
                       (if Stdlib.(=) tag "C"
                        then
                          (if Stdlib.(<>) len 1
                           then
                             Ppx_deriving_json_runtime.of_json_error
                               "expected a JSON array of length 1";
                           `C)
                        else
                          raise
                            (Ppx_deriving_json_runtime.Of_json_error
                               (Ppx_deriving_json_runtime.Unexpected_variant
                                  "unexpected variant")))
                 else
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a non empty JSON array with element being a string")
              else
                Ppx_deriving_json_runtime.of_json_error
                  "expected a non empty JSON array")
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array" : Js.Json.t -> u)
      let _ = u_of_json
      [@@@ocaml.warning "-39-11-27"]
      let rec u_to_json =
        (fun x ->
           match x with
           | #P.t as x -> P.to_json x
           | `C -> (Obj.magic [|(Obj.magic "C" : Js.Json.t)|] : Js.Json.t) : 
        u -> Js.Json.t)
      let _ = u_to_json
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `A))
  let () = print_endline (Ppx_deriving_json_runtime.to_string (u_to_json `C))
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["B"]|})) = `B)
  let () =
    assert ((u_of_json (Ppx_deriving_json_runtime.of_string {|["C"]|})) = `C)
  === stdout:native ===
  ["A"]
  ["C"]
  === stdout:js ===
  File ".lib.objs/melange/_unknown_", line 1, characters 0-0:
  melc: internal error, uncaught exception:
        Not_found
        
  [1]
