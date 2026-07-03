val schema_version : string

type t = Ppx_deriving_jsonschema_runtime_classify.t

val classify : t -> t
val declassify : t -> t

val json_schema : ?id:string -> ?title:string -> ?description:string -> ?definitions:(string * t) list -> t -> t

module Primitives : module type of Ppx_deriving_jsonschema_runtime_primitives
