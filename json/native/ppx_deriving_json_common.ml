open Ppxlib
open Ppx_deriving_tools.Conv

let get_of_variant_case ?mark_as_seen ~variant ~polyvariant = function
  | Vcs_ctx_variant ctx -> Attribute.get ?mark_as_seen variant ctx
  | Vcs_ctx_polyvariant ctx -> Attribute.get ?mark_as_seen polyvariant ctx

let get_of_variant ?mark_as_seen ~variant ~polyvariant = function
  | Vrt_ctx_variant ctx -> Attribute.get ?mark_as_seen variant ctx
  | Vrt_ctx_polyvariant ctx -> Attribute.get ?mark_as_seen polyvariant ctx

let attr_json_as ctx =
  Attribute.declare "json.as" ctx
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let vcs_attr_json_as =
  let variant = attr_json_as Attribute.Context.constructor_declaration in
  let polyvariant = attr_json_as Attribute.Context.rtag in
  get_of_variant_case ~variant ~polyvariant

let ld_attr_json_key =
  Attribute.get
    (Attribute.declare "json.key" Attribute.Context.label_declaration
       Ast_pattern.(single_expr_payload (estring __'))
       (fun x -> x))

let ld_attr_json_default =
  Attribute.get
    (Attribute.declare "json.default" Attribute.Context.label_declaration
       Ast_pattern.(single_expr_payload __)
       (fun x -> x))
