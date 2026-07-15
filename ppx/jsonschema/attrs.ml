open Ppxlib

type config = {
  polymorphic_variant_tuple : bool;
      (** Preserve the implicit tuple in a polymorphic variant. This
          option breaks compatibility with yojson derivers. *)
  ocaml_doc : bool;
      (** Use [ocaml.doc] attributes (i.e. [(** ... *)] comments) as a
          fallback for [[@jsonschema.description]] when the explicit
          annotation is absent. *)
}

let string_attr name ctx =
  Attribute.declare name ctx
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let expr_attr name ctx =
  Attribute.declare name ctx
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let key = string_attr "jsonschema.key" Attribute.Context.label_declaration
let ref = string_attr "jsonschema.ref" Attribute.Context.label_declaration

let variant_name =
  string_attr "jsonschema.name" Attribute.Context.constructor_declaration

let polymorphic_variant_name =
  string_attr "jsonschema.name" Attribute.Context.rtag

let td_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let cd_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let td_disallow_extra_fields =
  Attribute.declare "jsonschema.disallow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let cd_disallow_extra_fields =
  Attribute.declare "jsonschema.disallow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let option =
  Attribute.declare_flag "jsonschema.option"
    Attribute.Context.label_declaration

let ld_description =
  string_attr "jsonschema.description" Attribute.Context.label_declaration

let td_description =
  string_attr "jsonschema.description" Attribute.Context.type_declaration

let cd_description =
  string_attr "jsonschema.description"
    Attribute.Context.constructor_declaration

let ct_description =
  string_attr "jsonschema.description" Attribute.Context.core_type

let rtag_description =
  string_attr "jsonschema.description" Attribute.Context.rtag

let td_format =
  string_attr "jsonschema.format" Attribute.Context.type_declaration

let ld_format =
  string_attr "jsonschema.format" Attribute.Context.label_declaration

let ct_format =
  string_attr "jsonschema.format" Attribute.Context.core_type

let td_maximum =
  expr_attr "jsonschema.maximum" Attribute.Context.type_declaration

let ld_maximum =
  expr_attr "jsonschema.maximum" Attribute.Context.label_declaration

let ct_maximum =
  expr_attr "jsonschema.maximum" Attribute.Context.core_type

let td_minimum =
  expr_attr "jsonschema.minimum" Attribute.Context.type_declaration

let ld_minimum =
  expr_attr "jsonschema.minimum" Attribute.Context.label_declaration

let ct_minimum =
  expr_attr "jsonschema.minimum" Attribute.Context.core_type

let ct_attrs = expr_attr "jsonschema.attrs" Attribute.Context.core_type

let td_attrs =
  expr_attr "jsonschema.attrs" Attribute.Context.type_declaration

let ld_attrs =
  expr_attr "jsonschema.attrs" Attribute.Context.label_declaration

let ld_default =
  expr_attr "jsonschema.default" Attribute.Context.label_declaration

(* We intentionally do not use [Attribute.get] for [ocaml.doc]/[doc]. These are
   compiler-reserved attributes, and [ppxlib] rejects registering them via
   [Attribute.declare]. We therefore inspect the raw attribute list directly
   with an [Ast_pattern] that matches both the name and the standard string
   payload shape in one go. *)
let doc_attr_pattern =
  Ast_pattern.(
    attribute
      ~name:(string "ocaml.doc" ||| string "doc")
      ~payload:(single_expr_payload (estring __')))

(* A node can carry several [ocaml.doc]/[doc] attributes — e.g. a user writing
   one doc comment before a record field and another after. We collect every
   match and join them with a blank line so each comment reads as its own
   paragraph. The returned location is that of the first matching attribute. *)
let find_doc_attr attrs =
  let matches =
    List.filter_map
      (fun attr ->
        Ast_pattern.parse_res doc_attr_pattern attr.attr_loc attr Fun.id
        |> Result.to_option
        |> Option.map (fun ({ txt; loc } : string Location.loc) ->
            { txt = String.trim txt; loc }))
      attrs
  in
  match matches with
  | [] -> None
  | [ single ] -> Some single
  | first :: _ as all ->
      Some
        {
          txt = String.concat "\n\n" (List.map (fun x -> x.txt) all);
          loc = first.loc;
        }

let td_compact_variants =
  Attribute.declare_flag "jsonschema.compact_variants"
    Attribute.Context.type_declaration

let attributes =
  [
    Attribute.T key;
    Attribute.T ref;
    Attribute.T variant_name;
    Attribute.T polymorphic_variant_name;
    Attribute.T td_allow_extra_fields;
    Attribute.T cd_allow_extra_fields;
    Attribute.T td_disallow_extra_fields;
    Attribute.T cd_disallow_extra_fields;
    Attribute.T option;
    Attribute.T ld_description;
    Attribute.T td_description;
    Attribute.T cd_description;
    Attribute.T ct_description;
    Attribute.T rtag_description;
    Attribute.T td_format;
    Attribute.T ld_format;
    Attribute.T ct_format;
    Attribute.T td_maximum;
    Attribute.T ld_maximum;
    Attribute.T ct_maximum;
    Attribute.T td_minimum;
    Attribute.T ld_minimum;
    Attribute.T ct_minimum;
    Attribute.T ct_attrs;
    Attribute.T td_attrs;
    Attribute.T ld_attrs;
    Attribute.T ld_default;
    Attribute.T td_compact_variants;
  ]

let fallback_description ~ocaml_doc explicit_desc attrs node =
  match Attribute.get explicit_desc node with
  | Some _ as x -> x
  | None -> if ocaml_doc then find_doc_attr attrs else None

let ld_description ~ocaml_doc (ld : label_declaration) =
  fallback_description ~ocaml_doc ld_description ld.pld_attributes ld

let td_description ~ocaml_doc (td : type_declaration) =
  fallback_description ~ocaml_doc td_description td.ptype_attributes td

let cd_description ~ocaml_doc (cd : constructor_declaration) =
  fallback_description ~ocaml_doc cd_description cd.pcd_attributes cd

let ct_description ~ocaml_doc (ct : core_type) =
  fallback_description ~ocaml_doc ct_description ct.ptyp_attributes ct

let rtag_description ~ocaml_doc (rf : row_field) =
  fallback_description ~ocaml_doc rtag_description rf.prf_attributes rf

let args () =
  Deriving.Args.(
    empty +> flag "polymorphic_variant_tuple" +> flag "ocaml_doc")
