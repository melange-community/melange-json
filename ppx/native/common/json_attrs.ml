open StdLabels
open Ppxlib
open Ast_builder.Default

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

(* Read an attribute off a variant case regardless of whether it is a regular
   variant constructor or a polymorphic-variant tag, dispatching on the source
   node to the appropriately-typed attribute declaration. *)
let get_of_variant_case ?mark_as_seen ~variant ~polyvariant = function
  | `Variant_ctx ctx -> Attribute.get ?mark_as_seen variant ctx
  | `Polyvariant_ctx ctx -> Attribute.get ?mark_as_seen polyvariant ctx

(* [@json.name "..."] on a variant constructor / polymorphic-variant tag. *)
let attr_json_name_cd =
  string_attr "json.name" Attribute.Context.constructor_declaration

let attr_json_name_rtag = string_attr "json.name" Attribute.Context.rtag

let vcs_attr_json_name =
  get_of_variant_case ~variant:attr_json_name_cd
    ~polyvariant:attr_json_name_rtag

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

let vcs_attr_json_allow_any ?mark_as_seen ctx =
  match
    get_of_variant_case ~variant:allow_any_cd ~polyvariant:allow_any_rtag
      ?mark_as_seen ctx
  with
  | None -> false
  | Some () -> true

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

let vcs_attr_json_catch_all ?mark_as_seen ctx =
  match
    get_of_variant_case ~variant:catch_all_cd ~polyvariant:catch_all_rtag
      ?mark_as_seen ctx
  with
  | None -> false
  | Some () -> true

(* [@json.key "..."] on a record field. *)
let json_key = string_attr "json.key" Attribute.Context.label_declaration
let ld_attr_json_key = Attribute.get json_key

(* [@json.option] on a record field. *)
let json_option =
  unit_attr "json.option" Attribute.Context.label_declaration

let ld_attr_json_option = Attribute.get json_option

(* [@@json.allow_extra_fields] / [@json.allow_extra_fields] and their
   [@@json.disallow_extra_fields] / [@json.disallow_extra_fields] counterparts.
   [td_allow_extra_fields] / [cd_allow_extra_fields] resolve the pair into a
   single policy (extra fields are permitted unless explicitly disallowed). *)
let allow_extra_fields_td =
  unit_attr "json.allow_extra_fields" Attribute.Context.type_declaration

let allow_extra_fields_cd =
  unit_attr "json.allow_extra_fields"
    Attribute.Context.constructor_declaration

let td_attr_json_allow_extra_fields = Attribute.get allow_extra_fields_td
let cd_attr_json_allow_extra_fields = Attribute.get allow_extra_fields_cd

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

let allow_extra_fields ~loc allow disallow =
  match allow, disallow with
  | Some (), Some () ->
      Location.raise_errorf ~loc
        "[@json.allow_extra_fields] and [@json.disallow_extra_fields] \
         are mutually exclusive"
  | _, Some () -> false
  | _ -> true

let td_allow_extra_fields td =
  allow_extra_fields ~loc:td.ptype_loc
    (td_attr_json_allow_extra_fields td)
    (td_attr_json_disallow_extra_fields td)

let cd_allow_extra_fields cd =
  allow_extra_fields ~loc:cd.pcd_loc
    (cd_attr_json_allow_extra_fields cd)
    (cd_attr_json_disallow_extra_fields cd)

(* [@json.default expr] on a record field. [ld_attr_default] also treats a
   [@json.option] field as defaulting to [None]. *)
let json_default =
  expr_attr "json.default" Attribute.Context.label_declaration

let ld_attr_json_default = Attribute.get json_default

let ld_attr_default ld =
  match ld_attr_json_default ld with
  | Some e -> Some e
  | None -> (
      match ld_attr_json_option ld with
      | Some () ->
          let loc = ld.pld_loc in
          Some [%expr Stdlib.Option.None]
      | None -> None)

(* [@json.drop_default] (flag) or [@json.drop_default f] (comparison fn), and
   [@json.drop_default_if_json_equal]. [ld_drop_default] resolves the whole
   family (together with [@option]/[@default]) into the drop policy to apply. *)
let json_drop_default =
  Attribute.declare "json.drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(
      pstr (map0 ~f:None nil) (* flag form *)
      ||| single_expr_payload (map1 ~f:Option.some __)
      (* comparison function *))
    (fun x -> x)

let ld_attr_json_drop_default = Attribute.get json_drop_default

let json_drop_default_if_json_equal =
  Attribute.declare_flag "json.drop_default_if_json_equal"
    Attribute.Context.label_declaration

let ld_attr_json_drop_default_if_json_equal =
  Attribute.get json_drop_default_if_json_equal

let equal_affix = Expansion_helpers.Prefix "equal"

let rec equal_of_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_constr (lid, args) ->
      let fn =
        pexp_ident ~loc
          { loc; txt = Expansion_helpers.mangle_lid equal_affix lid.txt }
      in
      List.fold_left (List.rev args) ~init:fn ~f:(fun acc arg ->
          pexp_apply ~loc acc [ Nolabel, equal_of_core_type ~loc arg ])
  | Ptyp_var name -> evar ~loc (Expansion_helpers.mangle equal_affix name)
  | _ ->
      Location.raise_errorf ~loc
        "[@drop_default]: cannot derive equal for this type, provide a \
         comparison function via [@drop_default f]"

let ld_drop_default ld =
  let loc = ld.pld_loc in
  let drop_default = ld_attr_json_drop_default ld in
  let drop_json_equal = ld_attr_json_drop_default_if_json_equal ld in
  match drop_default, drop_json_equal with
  | Some _, Some () ->
      Location.raise_errorf ~loc
        "[@drop_default] and [@drop_default_if_json_equal] are mutually \
         exclusive"
  | None, None -> `No
  | None, Some () -> begin
      match ld_attr_json_option ld, ld_attr_json_default ld with
      | None, Some def -> `Drop_default_if_json_equal def
      | Some (), None ->
          Location.raise_errorf ~loc
            "[@drop_default_if_json_equal] cannot be used with \
             [@option]. Use [@drop_default] instead."
      | Some (), Some _ ->
          Location.raise_errorf ~loc
            "[@drop_default_if_json_equal] cannot be used with both \
             [@option] and [@default]. Use [@json.default] only."
      | None, None ->
          Location.raise_errorf ~loc
            "[@drop_default_if_json_equal] requires [@json.default]"
    end
  | Some None, None -> begin
      (* flag form: [@json.drop_default] *)
      match ld_attr_json_option ld, ld_attr_json_default ld with
      | Some (), None -> `Drop_option
      | None, Some def ->
          let cmp = equal_of_core_type ~loc ld.pld_type in
          `Drop_default (cmp, def)
      | Some (), Some _ ->
          Location.raise_errorf ~loc
            "[@drop_default] cannot be used with both [@option] and \
             [@default]"
      | None, None ->
          Location.raise_errorf ~loc
            "[@drop_default] requires either [@option] or [@default]"
    end
  | Some (Some cmp), None -> begin
      (* expression form: [@json.drop_default expr] *)
      match ld_attr_json_option ld, ld_attr_json_default ld with
      | None, Some def -> `Drop_default (cmp, def)
      | Some (), _ ->
          Location.raise_errorf ~loc
            "[@drop_default expr] cannot be used with [@option]"
      | None, None ->
          Location.raise_errorf ~loc
            "[@drop_default expr] requires [@default]"
    end

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
