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

let ld_attr_json_option =
  Attribute.get
    (Attribute.declare "json.option" Attribute.Context.label_declaration
       Ast_pattern.(pstr nil)
       ())

let attr_json_allow_extra_fields ctx =
  Attribute.declare "json.allow_extra_fields" ctx
    Ast_pattern.(pstr nil)
    ()

let td_attr_json_allow_extra_fields =
  Attribute.get
    (attr_json_allow_extra_fields Attribute.Context.type_declaration)

let cd_attr_json_allow_extra_fields =
  Attribute.get
    (attr_json_allow_extra_fields
       Attribute.Context.constructor_declaration)

let ld_attr_json_default =
  Attribute.get
    (Attribute.declare "json.default" Attribute.Context.label_declaration
       Ast_pattern.(single_expr_payload __)
       (fun x -> x))

let ld_attr_default ld =
  match ld_attr_json_default ld with
  | Some e -> Some e
  | None -> (
      match ld_attr_json_option ld with
      | Some () ->
          let loc = ld.pld_loc in
          Some [%expr Stdlib.Option.None]
      | None -> None)
