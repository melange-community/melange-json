[@@@alert "-deprecated"]

open Jest
open Expect
open Melange_json

let () =
  describe "parse" (fun () ->
      test "success" (fun () ->
          expect @@ parse "null" |> toEqual (Some Encode.null));

      test "error" (fun () -> expect @@ parse "{" |> toEqual None));

  describe "parseOrRaise" (fun () ->
      test "success" (fun () ->
          expect @@ parseOrRaise "null" |> toEqual Encode.null);

      test "error" (fun () ->
          try
            let (_ : Js.Json.t) = parseOrRaise "{" in
            fail "should throw"
          with
          | ParseError "Unexpected end of JSON input"
          | ParseError
              (* Node.js v20 *)
              "Expected property name or '}' in JSON at position 1"
          | ParseError
              (* Node.js v21 *)
              "Expected property name or '}' in JSON at position 1 (line \
               1 column 2)"
          ->
            pass));

  test "stringify" (fun () ->
      expect @@ stringify Encode.null |> toEqual "null");

  describe "classify" (fun () ->
      test "null" (fun () ->
          expect @@ classify Encode.null |> toEqual `Null);

      test "bool" (fun () ->
          expect @@ classify (Encode.bool true) |> toEqual (`Bool true));

      test "int" (fun () ->
          expect @@ classify (Encode.int 42) |> toEqual (`Int 42));

      test "float" (fun () ->
          expect @@ classify (Encode.float 1.5) |> toEqual (`Float 1.5));

      test "string" (fun () ->
          expect @@ classify (Encode.string "hi")
          |> toEqual (`String "hi"));

      test "list - recursively classifies elements" (fun () ->
          let json =
            Encode.jsonArray
              [| Encode.int 1; Encode.string "a"; Encode.null |]
          in
          expect @@ classify json
          |> toEqual (`List [ `Int 1; `String "a"; `Null ]));

      test "assoc - recursively classifies values" (fun () ->
          let json =
            Encode.object_ [ "n", Encode.null; "x", Encode.int 7 ]
          in
          expect @@ classify json
          |> toEqual (`Assoc [ "n", `Null; "x", `Int 7 ]));

      test "nested - fully traverses tree" (fun () ->
          let json =
            Encode.object_
              [
                ( "items",
                  Encode.jsonArray
                    [| Encode.bool false; Encode.float 3.14 |] );
              ]
          in
          expect @@ classify json
          |> toEqual
               (`Assoc [ "items", `List [ `Bool false; `Float 3.14 ] ])));

  describe "declassify" (fun () ->
      test "null" (fun () ->
          expect @@ declassify `Null |> toEqual Encode.null);

      test "bool" (fun () ->
          expect @@ declassify (`Bool true) |> toEqual (Encode.bool true));

      test "int" (fun () ->
          expect @@ declassify (`Int 42) |> toEqual (Encode.int 42));

      test "float" (fun () ->
          expect @@ declassify (`Float 1.5) |> toEqual (Encode.float 1.5));

      test "string" (fun () ->
          expect @@ declassify (`String "hi")
          |> toEqual (Encode.string "hi"));

      test "list - recursively declassifies elements" (fun () ->
          expect
          @@ declassify (`List [ `Int 1; `String "a"; `Null ])
          |> toEqual
               (Encode.jsonArray
                  [| Encode.int 1; Encode.string "a"; Encode.null |]));

      test "assoc - recursively declassifies values" (fun () ->
          expect
          @@ declassify (`Assoc [ ("n", `Null); ("x", `Int 7) ])
          |> toEqual (Encode.object_ [ ("n", Encode.null); ("x", Encode.int 7) ]));

      test "nested - fully traverses tree" (fun () ->
          expect
          @@ declassify
               (`Assoc [ ("items", `List [ `Bool false; `Float 3.14 ]) ])
          |> toEqual
               (Encode.object_
                  [
                    ( "items",
                      Encode.jsonArray
                        [| Encode.bool false; Encode.float 3.14 |] );
                  ]));

      test "roundtrip classify/declassify" (fun () ->
          let json =
            Encode.object_
              [
                ( "items",
                  Encode.jsonArray
                    [| Encode.bool false; Encode.float 3.14 |] );
              ]
          in
          expect @@ declassify (classify json) |> toEqual json))
