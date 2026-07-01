(* Regression test for
   https://github.com/melange-community/melange-json/issues/73

   The native backend is built on [Yojson.Safe], so integer literals that
   overflow the native [int] range parse successfully (as [`Intlit])
   instead of raising at parse time, and round-trip losslessly through
   [to_string]. *)

let show s =
  match Melange_json.of_string s with
  | json ->
      Printf.printf "parsed  : %s\nreprint : %s\n" s
        (Melange_json.to_string json)
  | exception Melange_json.Of_string_error msg ->
      Printf.printf "parse error for %s: %s\n" s msg

let () =
  (* A big integer that does not fit a native [int]. *)
  show {|123456789012345678901234567890|};
  (* ... and nested inside a structure. *)
  show {|{"big":999999999999999999999,"small":42}|};
  (* Regular ints still parse as [`Int]. *)
  show {|42|}

let big = Melange_json.of_string {|123456789012345678901234567890|}

let () =
  (* [classify] normalizes the out-of-range integer to [`Float], matching
     how a JS runtime represents it. *)
  match Melange_json.classify big with
  | `Float f -> Printf.printf "classify: float %g\n" f
  | `Int i -> Printf.printf "classify: int %d\n" i
  | _ -> print_endline "classify: other"

let () =
  (* Decoding it as a [float] succeeds. *)
  Printf.printf "as float: %g\n" (Melange_json.Of_json.float big)

let () =
  (* Decoding it as an [int] fails with a precise error rather than
     crashing at parse time. *)
  match Melange_json.Of_json.int big with
  | _ -> print_endline "unexpected: decoded as int"
  | exception Melange_json.Of_json_error e ->
      print_endline (Melange_json.of_json_error_to_string e)
