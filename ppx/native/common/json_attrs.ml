open Ppxlib

let string_attr name ctx =
  Attribute.declare name ctx
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let expr_attr name ctx =
  Attribute.declare name ctx
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let unit_attr name ctx =
  Attribute.declare name ctx Ast_pattern.(pstr nil) ()

(* [@json.name "..."] on a variant constructor / polymorphic-variant tag. *)
let attr_json_name_cd =
  string_attr "json.name" Attribute.Context.constructor_declaration

let attr_json_name_rtag = string_attr "json.name" Attribute.Context.rtag

(* [@@json.compact_variants] on a type declaration. *)
let compact_variants =
  unit_attr "json.compact_variants" Attribute.Context.type_declaration

let td_attr_json_compact_variants = Attribute.get compact_variants

let is_compact_variants td =
  Option.is_some (td_attr_json_compact_variants td)

(* [@json.allow_any] flag on a variant constructor / polymorphic-variant tag. *)
let allow_any_cd =
  Attribute.declare_flag "json.allow_any"
    Attribute.Context.constructor_declaration

let allow_any_rtag =
  Attribute.declare_flag "json.allow_any" Attribute.Context.rtag

(* [@json.catch_all] marks a variant constructor as the catch-all for any
   unrecognised string tag. The decoder routes both bare unknown strings and
   unknown array variants ["future_tag", ...] to this constructor (in the
   array case any payload is captured into the [payload] field for lossless
   round-trip). The encoder writes the value back in the same wire shape.
   Pairs naturally with [@@json.compact_variants] so the known cases are
   also bare strings. *)
let catch_all_cd =
  Attribute.declare_flag "json.catch_all"
    Attribute.Context.constructor_declaration

let catch_all_rtag =
  Attribute.declare_flag "json.catch_all" Attribute.Context.rtag

(* [@json.key "..."] on a record field. *)
let json_key = string_attr "json.key" Attribute.Context.label_declaration
let ld_attr_json_key = Attribute.get json_key

(* [@json.option] on a record field. *)
let json_option =
  unit_attr "json.option" Attribute.Context.label_declaration

let ld_attr_json_option = Attribute.get json_option

(* [@@json.allow_extra_fields] / [@json.allow_extra_fields]. *)
let allow_extra_fields_td =
  unit_attr "json.allow_extra_fields" Attribute.Context.type_declaration

let allow_extra_fields_cd =
  unit_attr "json.allow_extra_fields"
    Attribute.Context.constructor_declaration

let td_attr_json_allow_extra_fields = Attribute.get allow_extra_fields_td
let cd_attr_json_allow_extra_fields = Attribute.get allow_extra_fields_cd

(* [@@json.disallow_extra_fields] / [@json.disallow_extra_fields]. *)
let disallow_extra_fields_td =
  unit_attr "json.disallow_extra_fields"
    Attribute.Context.type_declaration

let disallow_extra_fields_cd =
  unit_attr "json.disallow_extra_fields"
    Attribute.Context.constructor_declaration

let td_attr_json_disallow_extra_fields =
  Attribute.get disallow_extra_fields_td

let cd_attr_json_disallow_extra_fields =
  Attribute.get disallow_extra_fields_cd

(* [@json.default expr] on a record field. *)
let json_default =
  expr_attr "json.default" Attribute.Context.label_declaration

let ld_attr_json_default = Attribute.get json_default

(* [@json.drop_default] (flag) or [@json.drop_default f] (comparison fn). *)
let json_drop_default =
  Attribute.declare "json.drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(
      pstr (map0 ~f:None nil) (* flag form *)
      ||| single_expr_payload (map1 ~f:Option.some __)
      (* comparison function *))
    (fun x -> x)

let ld_attr_json_drop_default = Attribute.get json_drop_default

(* [@json.drop_default_if_json_equal] flag on a record field. *)
let json_drop_default_if_json_equal =
  Attribute.declare_flag "json.drop_default_if_json_equal"
    Attribute.Context.label_declaration

let ld_attr_json_drop_default_if_json_equal =
  Attribute.get json_drop_default_if_json_equal

(* All declared attributes, passed to [Deriving.Generator.V2.make ~attributes]
   so ppxlib knows the derivers legitimately consume them. *)
let attributes =
  [
    Attribute.T attr_json_name_cd;
    Attribute.T attr_json_name_rtag;
    Attribute.T compact_variants;
    Attribute.T allow_any_cd;
    Attribute.T allow_any_rtag;
    Attribute.T catch_all_cd;
    Attribute.T catch_all_rtag;
    Attribute.T json_key;
    Attribute.T json_option;
    Attribute.T allow_extra_fields_td;
    Attribute.T allow_extra_fields_cd;
    Attribute.T disallow_extra_fields_td;
    Attribute.T disallow_extra_fields_cd;
    Attribute.T json_default;
    Attribute.T json_drop_default;
    Attribute.T json_drop_default_if_json_equal;
  ]
