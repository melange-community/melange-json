# melange-json

Compositional JSON encode/decode library and PPX for
[Melange](https://melange.re/).

Based on [@glennsl/bs-json](https://github.com/glennsl/bs-json).

The Decode module in particular provides a basic set of decoder functions to be composed into more complex decoders. A
decoder is a function that takes a `Js.Json.t` and either returns a value of the desired type if successful or raises a
`DecodeError` exception if not. Other functions accept a decoder and produce another decoder. Like `array`, which when
given a decoder for type `t` will return a decoder that tries to produce a value of type `t array`. So to decode an
`int array` you combine `Json.Decode.int` with `Json.Decode.array` into `Json.Decode.(array int)`. An array of arrays of
ints? `Json.Decode.(array (array int))`. Dict containing arrays of ints? `Json.Decode.(dict (array int))`.

## Example

```reason
type line = {
  start: point,
  end_: point,
  thickness: option(int)
}
and point = {
  x: int,
  y: int
};

module Decode = {
  let point = json =>
    Json.Decode.{
      x: json |> field("x", int),
      y: json |> field("y", int)
    };

  let line = json =>
    Json.Decode.{
      start:     json |> field("start", point),
      end_:      json |> field("end", point),
      thickness: json |> optional(field("thickness", int))
    };
};

let data = {| {
  "start": { "x": 1, "y": -4 },
  "end":   { "x": 5, "y": 8 }
} |};

let line = data |> Json.parseOrRaise
                |> Decode.line;
```

NOTE: `Json.Decode.{ ... }` creates an ordinary record, but also opens the `Json.Decode` module locally, within the
scope delimited by the curly braces, so we don't have to qualify the functions we use from it, like `field`, `int` and
`optional` here. You can also use `Json.Decode.( ... )` to open the module locally within the parentheses, if you're not
creating a record.

See [examples](./examples/) for more.

## Installation

Install [opam](https://opam.ocaml.org/) package manager.

Then:

```sh
opam install melange-json
```

# Setup

Add `melange-json` to the `libraries` field in your `dune` file:

```lisp
; ...
  (libraries melange-json)
; ...
```

## Documentation

### API

For the moment, please see the interface files:

* [Json](./src/Json.mli)
* [Json.Encode](./src/Json_encode.mli)
* [Json.Decode](./src/Json_decode.mli)

### Writing custom decoders and encoders

If you look at the type signature of `Json.Decode.array`, for example, you'll see it takes an `'a decoder` and returns an
`'a array decoder`. `'a decoder` is just an alias for `Js.Json.t -> 'a`, so if we expand the type signature of `array`
we'll get `(Js.Json.t -> 'a) -> Js.Json.t -> 'a array`. We can now see that it is a function that takes a decoder and
returns a function, itself a decoder. Applying the `int` decoder to `array` will give us an `int array decoder`, a
function `Js.Json.t -> int array`.

If you've written a function that takes just `Js.Json.t` and returns user-defined types of your own, you've already been
writing composable decoders! Let's look at `Decode.point` from the example above:

```reason
let point = json => {
  open! Json.Decode;
  {
    x: json |> field("x", int),
    y: json |> field("y", int)
  };
};
```

This is a function `Js.Json.t -> point`, or a `point decoder`. So if we'd like to decode an array of points, we can just
pass it to `Json.Decode.array` to get a `point array decoder` in return.

#### Builders

To write a decoder _builder_ like `Json.Decode.array` we need to take another decoder as an argument, and thanks to
currying we just need to apply it where we'd otherwise use a fixed decoder. Say we want to be able to decode both
`int point`s and `float point`s. First we'd have to parameterize the type:

```reason
type point('a) = {
  x: 'a,
  y: 'a
}
```

Then we can change our `point` function from above to take and use a decoder argument:

```reason
let point = (decodeNumber, json) => {
  open! Json.Decode;
  {
    x: json |> field("x", decodeNumber),
    y: json |> field("y", decodeNumber)
  };
};
```

And if we wish we can now create aliases for each variant:

```reason
let intPoint = point(Json.Decode.int);
let floatPoint = point(Json.Decode.float);
```

#### Encoders

Encoders work exactly the same way, just in reverse. `'a encoder` is just an alias for `'a -> Js.Json.t`, and this also
transfers to composition: `'a encoder -> 'a array encoder` expands to `('a -> Js.Json.t) -> 'a array -> Js.Json.t`.

## PPX

A [ppx deriver
plugin](https://ocaml.org/docs/metaprogramming#attributes-and-derivers) is
provided to automatically convert OCaml values to and from JSON.

### Installation

The PPX is included in the `melange-json` package. To use it, just add the
`dune` configuration to your project:

```dune
(library
 (modes melange)
 (preprocess (pps melange-json.ppx)))
```

### Usage

To generate JSON converters for a type, add the `[@@deriving json]` attribute to
a type declaration:

```ocaml
type t = {
  a: int;
  b: string;
} [@@deriving json]
```

This will generate the following pair of functions:

```ocaml
val of_json : Js.Json.t -> t
val to_json : t -> Js.Json.t
```

#### Generating JSON converters from type expressions

You can also generate JSON converters for a type expression using the `to_json`
and `of_json` extension points:

```ocaml
let json = [%to_json: int * string] (42, "foo")
```

#### Enumeration-like variants

Note that variants where all constructors have no arguments are treated as
enumeration-like variants:

```ocaml
type t = A | B [@@deriving json]
```

Such variants are represented as strings in JSON:

```ocaml
let json = to_json A
(* "A" *)
```

#### `[@json.default E]`: default values for records

You can specify default values for record fields using the `[@json.default E]`
attribute:

```ocaml
type t = {
  a: int;
  b: string [@json.default "-"];
} [@@deriving of_json]

let t = of_json (Json.parseOrRaise {|{"a": 42}|})
(* t = { a = 42; b = "-"; } *)
```

#### `[@json.option]`: a shortcut for `[@json.default None]`

When a field has type `_ option` then you can use the `[@json.option]` attribute
to specify that the default value is `None`:

```ocaml
type t = {
  a: int;
  b: string option [@json.option];
} [@@deriving of_json]

let t = of_json (Json.parseOrRaise {|{"a": 42}|})
(* t = { a = 42; b = None; } *)
```

#### `[@json.key "S"]`: customizing keys for record fields

You can specify custom keys for record fields using the `[@json.key E]`
attribute:

```ocaml
type t = {
  a: int [@json.key "A"];
  b: string [@json.key "B"];
} [@@deriving of_json]

let t = of_json (Json.parseOrRaise {|{"A": 42, "B": "foo"}|})
(* t = { a = 42; b = "foo"; } *)
```

#### `[@json.as "S"]`: customizing the representation of a variant case

You can specify custom representation for a variant case using the `[@json.as
E]` attribute:

```ocaml
type t = A | B [@json.as "bbb"] [@@deriving json]

let json = to_json B
(* "bbb" *)
```

## License

This work is dual-licensed under LGPL 3.0 and MPL 2.0.
You can choose between one of them if you use this work.

Please see LICENSE.LGPL-3.0 and LICENSE.MPL-2.0 for the full text of each license.

`SPDX-License-Identifier: LGPL-3.0 OR MPL-2.0`
