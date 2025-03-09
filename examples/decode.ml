(* Decoding a fixed JSON data structure using Melange_json.Of_json *)
let mapJsonObjectString f decoder (encoder : int -> Js.Json.t) str =
  let json = Melange_json.of_string str in
  Melange_json.Of_json.(js_dict decoder json)
  |> Js.Dict.map ~f:(fun [@u] v -> f v)
  |> Melange_json.To_json.js_dict encoder
  |> Melange_json.to_string

let sum = Array.fold_left ( + ) 0

(* prints `{ "foo": 6, "bar": 24 }` *)
let () =
  Js.log
  @@ mapJsonObjectString sum
       Melange_json.Of_json.(array int)
       Melange_json.To_json.int
       {|
      {
        "foo": [1, 2, 3],
        "bar": [9, 8, 7]
      }
    |}

(* Error handling *)
let () =
  let json = {|{ "y": 42 } |} |> Melange_json.of_string in
  match Melange_json.Of_json.(field "x" int json) with
  | x -> Js.log x
  | exception Melange_json.Of_json_error err -> Js.log ("Error:" ^ err)
