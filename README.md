# ppx_deriving_toml

A [ppx deriver plugin][] for converting OCaml values to and from TOML.

## Usage

Add the following `dune` configuration to your project:

```dune
(executable
 ...
 (preprocess (pps ppx_deriving_toml)))
```

To generate JSON converters for a type, add the `[@@deriving toml]` attribute to a type declaration:

```ocaml
type t = {
  a: int;
  b: string;
} [@@deriving toml]
```

This will generate the following pair of functions for native:

```ocaml
val of_toml : Otoml.t -> t
val to_toml : t -> Otoml.t
```

[ppx deriver plugin]: https://ocaml.org/docs/metaprogramming#attributes-and-derivers
