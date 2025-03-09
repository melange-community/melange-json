type variant =
[ `Other of Yojson.Basic.t [@allow_any]
| `Foo
]
[@@deriving json]

type j = {
  a : variant;
}
[@@deriving json]

let () =
  Sys.argv.(1)
  |> Yojson.Basic.from_string
  |> j_of_json
  |> (fun j ->
        (match j with {a=`Other _} -> print_endline "got Other" | {a=`Foo} -> print_endline "got Foo");
        j
     )
  |> j_to_json
  |> Yojson.Basic.pretty_to_string
  |> print_endline
