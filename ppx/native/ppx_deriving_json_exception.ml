type error = Json_error of string | Unexpected_variant of string

exception Of_json_error of error