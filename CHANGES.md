## Unpublished

- PPX: Add runtime for `result`
  ([#13](https://github.com/melange-community/melange-json/pull/13))
- PPX: Add `yojson` as runtime dep for the native version
  ([#15](https://github.com/melange-community/melange-json/pull/15))
- PPX: Rename `[@json.as]` to `[@json.name]`
- PPX: Drop special encoding for enumeration-like variants (variants with each
  constructor having no arguments).
  ([#26](https://github.com/melange-community/melange-json/pull/26))
- PPX: change JSON representation of polyvariants, make it compatible with 
  ppx_deriving_yojson and ppx_yojson_conv
  ([#27](https://github.com/melange-community/melange-json/pull/27))
- PPX: Consistent use of exceptions in runtime.
  ([#28](https://github.com/melange-community/melange-json/pull/28))

## 1.3.0 (2024-08-28)

- PPX: Qualify usages of infix operators with `Stdlib`
  ([#11](https://github.com/melange-community/melange-json/pull/11))
- Add `melange-json-native` package
  ([#12](https://github.com/melange-community/melange-json/pull/12))
- Add `[@drop_default]` attribute to drop `None` values from JSON
  representation
  ([#17](https://github.com/melange-community/melange-json/pull/17))

## 1.2.0 (2024-08-16)

- Port PPX from @andreypopp/ppx_deriving_json
  ([#10](https://github.com/melange-community/melange-json/pull/10))

## 1.1.0 (2024-02-03)

- Require Melange v3
  ([#6](https://github.com/melange-community/melange-json/pull/6))

## 1.0.0 (2023-10-09)

- Initial release

## Old versions changes (from bs-json)

### 5.0.4
* Rewrote `Encode.list` to be stack-safe and much faster.

### 5.0.2
* Added `Json.Decode.id`

### 5.0.1
* Dual licensed as LGPL-3.0 and MPL-2.0. MPL is mostly equivalent to LGPL but relaxes its restriction on linking, which works better with the JavaScript packaging and distribution model.

### 5.0.0
* Removed deprecated `arrayOf` encoder
* Renamed `dict` encoder to `jsonDict`
* Added new `dict` encoder that takes an additional encoder argument used to encode the contained values, and so it's consistent with the respective `dict` decoder.

### 4.0.0
* Bumped `bs-platform` peer dependency to 5.0.4 to stop the compiler's complaining.

### 3.0.0
* Replace usage of `Js.Date.toJSON` with `Js.Date.toJSONUsafe`, which is exactly the same, just to avoid deprecation warnings for end users (Thanks Bob!)
* Requires `bs-platform` >= 4.0.2

### 2.0.0
* Removed `Json.Decode.boolean`, `Json.Encode.boolean`, `Json.Encode.booleanArray`
* Requires `bs-platform` >= 3.0.0

### 1.3.1
* Reverted commits that broke backwards compatibility despite only affecting the implementation

### 1.3.0
* Deprecated `Json.Decode.boolean`, `Json.Encode.boolean`, `Json.Encode.booleanArray`
* Added `Json.Encode.boolArray`

### 1.2.0
* Added `Json.Encode.char` and `Json.Decode.char`

### 1.1.0
* Added "stack traces" to higher-order decoders, making it easier to find the location of an error.

### 1.0.1
* Moved repository from `reasonml-community/bs-json` to `glennsl/bs-json`
* Renamed NPM package from `bs-json` to `@glennsl/bs-json`

### 1.0.0
* Replaced `Json.Encoder.array` with `Json.Encode.arrayOf` renamed to `array`. Deprecated `arrayOf` alias.
* Added `Json.parse`, `Json.parseOrRaise`, `Json.stringify`
* Added `date` encoder and decoder
* Added `tuple2`/`tuple3`/`tuple4` encoders and decoders
* Fixed bug where js integers > 32-bit were rejected as integers by Json.Decode.int (#15)

### 0.2.4
* Added `Json.Encode.bool`
* Added `Json.Encode.pair`
* Added `Json.Encode.withDefault`
* Added `Json.Encode.nullable`
* Added `Json.Encode.arrayOf`
* Added `Json.Encode.jsonArray` as replacement for `Json.Encode.array`
* Deprecated `Json.Encode.array`

### 0.2.3
* Fixed embarrassing bug where an API was used that isn't available on IE (honestly more embarrassed on behalf of IE though)

### 0.2.2
* Added `Json.Decode.pair`

### 0.2.1
* Added `Json.Encode.list`

### 0.2.0
* Breaking: Renamed `Json.Encode.object_` to `Json.Encode.dict`
* Added `Json.Encode.object_` taking a list of properties instead of a Json.Dict.t as before
