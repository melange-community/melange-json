(* Parsing a JSON string using Melange_json.parseOrRaise *)

let arrayOfInts str =
  let json = Melange_json.of_string str in
  Melange_json.Of_json.(array int json)

(* prints `[3, 2, 1]` *)
let _ = Js.log (arrayOfInts "[1, 2, 3]" |> Js.Array.reverseInPlace)
