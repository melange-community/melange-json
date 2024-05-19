# ppx_deriving_json

A [ppx deriver plugin][] for converting OCaml values to and from JSON. Works
both in native (using `yojson` JSON representation) and [melange][] (using
`Js.Json.t` JSON representation.

## Installation

While the package is not yet available in the official opam repository, you
either need to pin the package or add a custom opam repository:

```sh
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
```

Then you can install the package:

```sh
opam install -y ppx_deriving_json
```

Finally, add the `dune` configuration to your project:

```dune
(executable
 ...
 (preprocess (pps ppx_deriving_json.native)))

(library
 (modes melange)
 (preprocess (pps ppx_deriving_json.browser)))
```

Note that you need to use the `ppx_deriving_json.native` preprocessor for
native and the `ppx_deriving_json.browser` preprocessor for melange.

## Usage

To generate JSON converters for a type, add the `[@@deriving json]` attribute to a type declaration:

```ocaml
type t = {
  a: int;
  b: string;
} [@@deriving json]
```

This will generate the following pair of functions for native:

```ocaml
val of_json : Yojson.Basic.json -> t
val to_json : t -> Yojson.Basic.json
```

and the following pair of functions for melange:

```ocaml
val of_json : Js.Json.t -> t
val to_json : t -> Js.Json.t
```

### Generating JSON converters from type expressions

You can also generate JSON converters for a type expression using the `to_json` and `of_json` extension points:

```ocaml
let json = [%to_json: int * string] (42, "foo")
```

### Enumeration-like variants

Note that variants where all constructors have no arguments are treated as
enumeration-like variants:

```ocaml
type t = A | B [@@deriving json]
```

Such variants are represented as strings in JSON:

```ocaml
let json = to_json A
(* json = `String "A" *)
```

### `[@json.default E]`: default values for records

You can specify default values for record fields using the `[@json.default E]` attribute:

```ocaml
type t = {
  a: int;
  b: string [@json.default "-"];
} [@@deriving of_json]

let t = of_json (`Assoc ["a", `Int 42])
(* t = { a = 42; b = "-"; } *)
```

### `[@json.key "S"]`: customizing keys for record fields

You can specify custom keys for record fields using the `[@json.key E]` attribute:

```ocaml
type t = {
  a: int [@json.key "A"];
  b: string [@json.key "B"];
} [@@deriving of_json]

let t = of_json (`Assoc ["A", `Int 42; "B", `String "foo"])
(* t = { a = 42; b = "foo"; } *)
```

### `[@json.as "S"]`: customizing the representation of a variant case

You can specify custom representation for a variant case using the `[@json.as E]` attribute:

```ocaml
type t = A | B [@json.as "bbb"] [@@deriving json]

let json = to_json B
(* json = `String "bbb" *)
```

[ppx deriver plugin]: https://ocaml.org/docs/metaprogramming#attributes-and-derivers
[melange]: https://melange.re
