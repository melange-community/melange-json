exception Of_string_error of string

include Ppx_deriving_json_exception
module Classify = Ppx_deriving_json_classify

let with_buffer f =
  let buffer = Buffer.create 1 in
  f (Buffer.add_string buffer);
  Buffer.contents buffer

let show_json_type json =
  json |> Classify.classify |> function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"

let show_json_error ?depth json =
  with_buffer (fun emit ->
      let rec loop ?depth json =
        let json = Classify.classify json in
        let depth = Option.map (fun i -> i - 1) depth in
        match depth with
        | Some 0 -> emit "_"
        | _ -> (
            match json with
            | `Assoc assoc ->
                emit "{";
                List.iter
                  (fun (k, v) ->
                    emit {|"|};
                    emit k;
                    emit {|": |};
                    loop ?depth v;
                    emit {|, |})
                  assoc;
                emit "}"
            | `Bool bool -> emit (if bool then "true" else "false")
            | `Float float -> emit (string_of_float float)
            | `Int int -> emit (string_of_int int)
            | `List li ->
                emit "[";
                List.iter
                  (fun elt ->
                    loop ?depth elt;
                    emit ", ")
                  li;
                emit "]"
            | `Null -> emit "null"
            | `String str ->
                emit {|"|};
                emit (String.escaped str);
                emit {|"|})
      in

      (loop ?depth:(Option.map (fun i -> i + 1) depth)) json)

let of_json_msg_error msg = raise (Of_json_error (Json_error msg))

let of_json_error ?(depth = 2) ~json msg =
  of_json_msg_error
    (with_buffer (fun emit ->
         emit msg;
         emit " but got ";
         emit (show_json_error ~depth json)))

let of_json_error_type_mismatch json expected =
  of_json_msg_error
    (with_buffer (fun emit ->
         emit "expected ";
         emit expected;
         emit " but got ";
         emit (show_json_type json);
         emit ": ";
         emit (show_json_error ~depth:1 json)))
