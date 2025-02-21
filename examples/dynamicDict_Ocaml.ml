(*
    Handling an object with dynamic keys for sub-objects.
    example:
    {
        static: "hello",
        dynamics: {
            "undetermined1": 2
            "undetermined2": 6
        }
    }

    Where the "undetermined" keys, are unknown at compile time.
    Could be dynamic JS keys generated by user or generally at run time.
*)

type obj = { static : string; dynamics : int Js.Dict.t }

module Of_json = struct
  let obj json =
    Melange_json.Of_json.
      {
        static = json |> field "static" string;
        dynamics = json |> field "dynamics" (js_dict int);
      }
end

module To_json = struct
  let obj c =
    Melange_json.To_json.(
      json_dict
        (Js.Dict.fromList
           [
             "static", c.static |> string;
             "dynamics", c.dynamics |> js_dict int;
           ]))
end

let data =
  {| {
  "static": "hi",
  "dynamics": { "hello": 5, "random": 8 }
} |}

let decodedData = data |> Melange_json.of_string |> Of_json.obj

(*
  Will log [ 'hi', { hello: 5, random: 8 } ]
*)
let _ = decodedData |> Js.log

(*
  Will log { static: 'hi', dynamics: { hello: 5, random: 8 } }
*)
let encodedDataBack = decodedData |> To_json.obj |> Js.log
