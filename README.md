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

Finally add the `dune` configuration to your project:

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

This will generate the following pair of functions:

```ocaml
val of_json : Yojson.Basic.json -> t
val to_json : t -> Yojson.Basic.json
```

### Generating JSON converters from a type expressions

You can also generate JSON converters for a type expression using the `json_of` and `json_of` extension points:

```ocaml
let json = [%json_of: int * string] (42, "foo")
```

[ppx deriver plugin]: https://ocaml.org/docs/metaprogramming#attributes-and-derivers
[melange]: https://melange.re
