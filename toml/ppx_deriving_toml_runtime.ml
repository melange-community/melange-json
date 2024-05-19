open Otoml

exception Of_toml_error of string

let of_toml_error msg = raise (Of_toml_error msg)

let string_of_toml = function
  | TomlString s -> s
  | _ -> of_toml_error "Expected a string"

let int_of_toml = function
  | TomlInteger i -> i
  | _ -> of_toml_error "Expected an int"

let float_of_toml = function
  | TomlFloat f -> f
  | _ -> of_toml_error "Expected a float"

let bool_of_toml = function
  | TomlBoolean b -> b
  | _ -> of_toml_error "Expected a bool"

let string_to_toml s = TomlString s
let int_to_toml i = TomlInteger i
let float_to_toml f = TomlFloat f
let bool_to_toml b = TomlBoolean b

let list_of_toml of_toml = function
  | TomlArray xs -> List.map of_toml xs
  | TomlTableArray xs -> List.map of_toml xs
  | _ -> of_toml_error "Expected a list"

let list_to_toml to_toml xs = TomlArray (List.map to_toml xs)
