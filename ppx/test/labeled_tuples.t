%to_json for labeled tuples:
  $ echo '
  > let _ = [%to_json: a:string * b:int]
  > ' | ./run.sh
  === ppx output:native ===
  let _ =
    fun x ->
      match x with
      | (~a:x_a, ~b:x_b) ->
          `Assoc
            (let bnds__001_ = [] in
             let bnds__001_ = ("b", (int_to_json x_b)) :: bnds__001_ in
             let bnds__001_ = ("a", (string_to_json x_a)) :: bnds__001_ in
             bnds__001_)
  === ppx output:browser ===
  let _ =
    fun x ->
      match x with
      | (~a:x_a, ~b:x_b) ->
          (Obj.magic
             ([%mel.obj { a = (string_to_json x_a); b = (int_to_json x_b) }]) : 
          Js.Json.t)
  === stdout:native ===
  === stdout:js ===

%of_json for labeled tuples:
  $ echo '
  > let _ = [%of_json: a:string * b:int]
  > ' | ./run.sh
  === ppx output:native ===
  let _ =
    fun x ->
      match x with
      | `Assoc fs ->
          let x_a = ref Stdlib.Option.None in
          let x_b = ref Stdlib.Option.None in
          let rec iter =
            function
            | [] -> ()
            | (n', v)::fs ->
                ((match n' with
                  | "a" -> x_a := (Stdlib.Option.Some (string_of_json v))
                  | "b" -> x_b := (Stdlib.Option.Some (int_of_json v))
                  | _name -> ());
                 iter fs) in
          (iter fs;
           (~a:((match Stdlib.(!) x_a with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Melange_json.of_json_error ~json:x "expected field \"a\"")),
             ~b:((match Stdlib.(!) x_b with
                  | Stdlib.Option.Some v -> v
                  | Stdlib.Option.None ->
                      Melange_json.of_json_error ~json:x "expected field \"b\""))))
      | _ -> Melange_json.of_json_error ~json:x "expected a JSON object"
  === ppx output:browser ===
  let _ =
    fun x ->
      if
        Stdlib.not
          (Stdlib.(&&) (Stdlib.(=) (Js.typeof x) "object")
             (Stdlib.(&&) (Stdlib.not (Js.Array.isArray x))
                (Stdlib.not (Stdlib.(==) (Obj.magic x : 'a Js.null) Js.null))))
      then Melange_json.of_json_error ~json:x "expected a JSON object";
      (let fs =
         (Obj.magic x : <
                          a: Js.Json.t Js.undefined  ;b: Js.Json.t Js.undefined
                                                          > 
                          Js.t) in
       (~a:(match Js.Undefined.toOption (fs ## a) with
            | Stdlib.Option.Some v -> string_of_json v
            | Stdlib.Option.None ->
                Melange_json.of_json_error ~json:x
                  "expected field \"a\" to be present"),
         ~b:(match Js.Undefined.toOption (fs ## b) with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Melange_json.of_json_error ~json:x
                   "expected field \"b\" to be present")))
  === stdout:native ===
  === stdout:js ===
