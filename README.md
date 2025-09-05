# melange-json

Compositional JSON encode/decode library and PPX for
[Melange](https://melange.re/).

Based on [@glennsl/bs-json](https://github.com/glennsl/bs-json).

The Decode module in particular provides a basic set of decoder functions to be
composed into more complex decoders. A decoder is a function that takes a
`Js.Json.t` and either returns a value of the desired type if successful or
raises an `Of_json_error` exception if not. Other functions accept a decoder and
produce another decoder. Like `array`, which when given a decoder for type `t`
will return a decoder that tries to produce a value of type `t array`. So to
decode an `int array` you combine `Melange_json.Of_json.int` with `Melange_json.Of_json.array`
into `Melange_json.Of_json.(array int)`. An array of arrays of ints? `Melange_json.Of_json.(array
(array int))`. Dict containing arrays of ints? `Melange_json.Of_json.(dict (array int))`.

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
    Melange_json.Of_json.{
      x: json |> field("x", int),
      y: json |> field("y", int)
    };

  let line = json =>
    Melange_json.Of_json.{
      start:     json |> field("start", point),
      end_:      json |> field("end", point),
      thickness: json |> try_or_none(field("thickness", int))
    };
};

let data = {| {
  "start": { "x": 1, "y": -4 },
  "end":   { "x": 5, "y": 8 }
} |};

let line = data |> Melange_json.of_string
                |> Decode.line;
```

NOTE: `Melange_json.Of_json.{ ... }` creates an ordinary record, but also opens the
`Melange_json.Of_json` module locally, within the scope delimited by the curly braces, so
we don't have to qualify the functions we use from it, like `field`, `int` and
`try_or_none` here. You can also use `Melange_json.Of_json.( ... )` to open the module
locally within the parentheses, if you're not creating a record.

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

* [Melange_json](./src/melange_json.mli)

### Writing custom decoders and encoders

If you look at the type signature of `Melange_json.Decode.array`, for example, you'll
see it takes an `'a decoder` and returns an `'a array decoder`. `'a decoder` is
just an alias for `Js.Json.t -> 'a`, so if we expand the type signature of
`array` we'll get `(Js.Json.t -> 'a) -> Js.Json.t -> 'a array`. We can now see
that it is a function that takes a decoder and returns a function, itself a
decoder. Applying the `int` decoder to `array` will give us an `int array
decoder`, a function `Js.Json.t -> int array`.

If you've written a function that takes just `Js.Json.t` and returns
user-defined types of your own, you've already been writing composable decoders!
Let's look at `Decode.point` from the example above:

```reason
let point = json => {
  open! Melange_json.Decode;
  {
    x: json |> field("x", int),
    y: json |> field("y", int)
  };
};
```

This is a function `Js.Json.t -> point`, or a `point decoder`. So if we'd like
to decode an array of points, we can just pass it to `Melange_json.Of_json.array` to get
a `point array decoder` in return.

#### Builders

To write a decoder _builder_ like `Melange_json.Of_json.array` we need to take another
decoder as an argument, and thanks to currying we just need to apply it where
we'd otherwise use a fixed decoder. Say we want to be able to decode both `int
point`s and `float point`s. First we'd have to parameterize the type:

```reason
type point('a) = {
  x: 'a,
  y: 'a
}
```

Then we can change our `point` function from above to take and use a decoder
argument:

```reason
let point = (decodeNumber, json) => {
  open! Melange_json.Decode;
  {
    x: json |> field("x", decodeNumber),
    y: json |> field("y", decodeNumber)
  };
};
```

And if we wish we can now create aliases for each variant:

```reason
let intPoint = point(Melange_json.Of_json.int);
let floatPoint = point(Melange_json.Of_json.float);
```

#### Encoders

Encoders work exactly the same way, just in reverse. `'a encoder` is just an
alias for `'a -> Js.Json.t`, and this also transfers to composition: `'a encoder
-> 'a array encoder` expands to `('a -> Js.Json.t) -> 'a array -> Js.Json.t`.

## PPX for Melange

A [ppx deriver
plugin](https://ocaml.org/docs/metaprogramming#attributes-and-derivers) is
provided to automatically convert Melange values to and from JSON.

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
the type declaration, ensuring the converters for primitives like `int` and
`string` are in scope if necessary:

```ocaml
open Melange_json.Primitives

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

#### `[@json.default E]`: default values for records

You can specify default values for record fields using the `[@json.default E]`
attribute:

```ocaml
type t = {
  a: int;
  b: string [@json.default "-"];
} [@@deriving of_json]

let t = of_json (Melange_json.of_string {|{"a": 42}|})
(* t = { a = 42; b = "-"; } *)
```

#### `[@json.allow_extra_fields]` on records

Sometimes, the JSON objects might contain keys that are not part of the OCaml
type definition. The `[@json.allow_extra_fields]` attribute allows you to
gracefully ignore such additional fields instead of raising an error during
deserialization.

This attribute can be used on records, even when they are embedded in other
types.

> **Note:** For the Melange PPX, ignoring extra fields is the default behavior -
> you don't need to explicitly add the `[@json.allow_extra_fields]` attribute.
> The attribute is primarily useful for the native PPX where strict field
> checking is the default.

**Example 1: Ignoring extra fields in records**

```ocaml
type allow_extra_fields = {
  a: int;
} [@@deriving json] [@@json.allow_extra_fields]

let t = allow_extra_fields_of_json (Json.parseOrRaise {|{"a": 42, "extra": "ignore me"}|})
(* t = { a = 42 } *)
```

The additional key `"extra"` in the JSON input is ignored, and the record is
successfully deserialized.

**Example 2: Ignoring extra fields in inline records**

```ocaml
type allow_extra_fields2 = 
  | A of { a: int } [@json.allow_extra_fields] 
  [@@deriving json]

let t = allow_extra_fields2_of_json (Json.parseOrRaise {|{"tag": "A", "a": 42, "extra": "ignore me"}|})
(* t = A { a = 42 } *)
```

In this case, the `[@json.allow_extra_fields]` attribute is applied directly to
the inline record in the variant constructor. This allows the variant to ignore
extra fields in the JSON payload while properly deserializing the fields that
match the type definition.

#### `[@json.option]`: a shortcut for `[@json.default None]`

When a field has type `_ option` then you can use the `[@json.option]` attribute
to specify that the default value is `None`:

```ocaml
type t = {
  a: int;
  b: string option [@json.option];
} [@@deriving of_json]

let t = of_json (Melange_json.of_string {|{"a": 42}|})
(* t = { a = 42; b = None; } *)
```

#### `[@json.drop_default]`: drop default values from JSON

When a field has `[@option]` attribute one can use `[@json.drop_default]`
attribute to make the generated `to_json` function to drop the field if it's
value is `None`:

```ocaml
type t = {
  a: int;
  b: string option [@json.option] [@json.drop_default];
} [@@deriving to_json]

let t = to_json { a = 1; b = None; }
(* {"a": 1} *)
```

#### `[@json.key "S"]`: customizing keys for record fields

You can specify custom keys for record fields using the `[@json.key E]`
attribute:

```ocaml
type t = {
  a: int [@json.key "A"];
  b: string [@json.key "B"];
} [@@deriving of_json]

let t = of_json (Melange_json.of_string {|{"A": 42, "B": "foo"}|})
(* t = { a = 42; b = "foo"; } *)
```

#### Variant serialization behavior

The PPX serializes variants differently based on whether they have payloads:

- **Variants without payloads** are serialized as **strings**
- **Variants with payloads** are serialized as **lists** with the first element being the constructor name

```ocaml
type string_option = 
| None
| Some of string
[@@deriving json]

(* None serializes to: "None" *)
(* Some "hello" serializes to: ["Some", "hello"] *)
```

For backward compatibility, the old list format (e.g., `["None"]`) can still be parsed when deserializing.

#### `[@json.name "S"]`: customizing the representation of a variant case

You can specify custom representation for a variant case using the `[@json.name
E]` attribute:

```ocaml
type t = A | B [@json.name "bbb"] [@@deriving json]

let json = to_json B
(* "bbb" *)
```

#### `[@@deriving json_string]`: a shortcut for JSON string conversion

For convenience, one can use `[@@deriving json_string]` to generate converters
directly to and from JSON strings:

```ocaml
type t = A [@@deriving json, json_string]

let "\"A\"" = to_json_string A
let A = of_json_string "\"A\""
```

Similarly, there's `[@@deriving to_json_string]` and `[@@deriving
of_json_string]` to generate the converters separately.

## PPX for OCaml native

A similar PPX is exposed in the `melange-json-native` package, which works with
the `yojson` JSON representation instead of `Js.Json.t`.

### Installation

The PPX is included in `melange-json-native` package, so that package will have
to be installed first:

```sh
opam install melange-json-native
```

To use it, add the `dune` configuration to your project:

```dune
(executable
 ...
 (preprocess (pps melange-json-native.ppx)))
```

### Usage

From the usage perspective, the PPX is similar to the Melange one:

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

Refer to the [PPX for Melange](#ppx-for-melange) section for more details on
usage patterns.

## License

This work is dual-licensed under LGPL 3.0 and MPL 2.0. You can choose between
one of them if you use this work.

Please see LICENSE.LGPL-3.0 and LICENSE.MPL-2.0 for the full text of each
license.

`SPDX-License-Identifier: LGPL-3.0 OR MPL-2.0`
