open StdLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_tools.Conv

let get_of_variant_case ?mark_as_seen ~variant ~polyvariant = function
  | Vcs_ctx_variant ctx -> Attribute.get ?mark_as_seen variant ctx
  | Vcs_ctx_polyvariant ctx -> Attribute.get ?mark_as_seen polyvariant ctx

let get_of_variant ?mark_as_seen ~variant ~polyvariant = function
  | Vrt_ctx_variant ctx -> Attribute.get ?mark_as_seen variant ctx
  | Vrt_ctx_polyvariant ctx -> Attribute.get ?mark_as_seen polyvariant ctx

let attr_json_name ctx =
  Attribute.declare "json.name" ctx
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let vcs_attr_json_name =
  let variant =
    attr_json_name Attribute.Context.constructor_declaration
  in
  let polyvariant = attr_json_name Attribute.Context.rtag in
  get_of_variant_case ~variant ~polyvariant

let attr_json_allow_any ctx = Attribute.declare_flag "json.allow_any" ctx

let vcs_attr_json_allow_any =
  let variant =
    attr_json_allow_any Attribute.Context.constructor_declaration
  in
  let polyvariant = attr_json_allow_any Attribute.Context.rtag in
  fun ?mark_as_seen ctx ->
    match get_of_variant_case ~variant ~polyvariant ?mark_as_seen ctx with
    | None -> false
    | Some () -> true

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

let ld_attr_json_drop_default =
  Attribute.get
    (Attribute.declare "json.drop_default"
       Attribute.Context.label_declaration
       Ast_pattern.(
         pstr (map0 ~f:None nil) (* flag form *)
         ||| single_expr_payload (map1 ~f:Option.some __)
         (* comparison function *))
       (fun x -> x))

let ld_attr_json_drop_default_if_json_equal =
  Attribute.get
    (Attribute.declare_flag "json.drop_default_if_json_equal"
       Attribute.Context.label_declaration)

let ld_attr_default ld =
  match ld_attr_json_default ld with
  | Some e -> Some e
  | None -> (
      match ld_attr_json_option ld with
      | Some () ->
          let loc = ld.pld_loc in
          Some [%expr Stdlib.Option.None]
      | None -> None)

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

let expand_via ~what ~through make ~ctxt (rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let expand_one (td : type_declaration) =
    let loc = td.ptype_loc in
    let pat =
      let { txt; loc } = td.ptype_name in
      let txt = Expansion_helpers.mangle what txt in
      ppat_var ~loc { Location.txt; loc }
    in
    let name_of_td_param idx (ty, _) =
      match ty.ptyp_desc with
      | Ptyp_any -> Printf.sprintf "_%d" idx
      | Ptyp_var name -> name
      | _ ->
          Location.raise_errorf ~loc:ty.ptyp_loc
            "unsupported type parameter"
    in
    let names = List.mapi td.ptype_params ~f:name_of_td_param in
    let expr =
      let of_json =
        let { txt; loc = _ } = td.ptype_name in
        let txt = Expansion_helpers.mangle through txt in
        let of_json = pexp_ident ~loc { loc; txt = lident txt } in
        pexp_apply ~loc of_json
          (List.map names ~f:(fun name -> Nolabel, evar ~loc name))
      in
      let body = make ~loc of_json in
      List.fold_left (List.rev names) ~init:body ~f:(fun e name ->
          [%expr fun [%p pvar ~loc name] -> [%e e]])
    in
    value_binding ~loc ~pat ~expr
  in
  pstr_value_list ~loc rec_flag (List.map tds ~f:expand_one)

module Of_json_string = struct
  let expand =
    expand_via ~what:(Expansion_helpers.Suffix "of_json_string")
      ~through:(Expansion_helpers.Suffix "of_json") (fun ~loc of_json ->
        [%expr fun _json -> [%e of_json] (Melange_json.of_string _json)])

  let register ~of_json () =
    Deriving.add "of_json_string"
      ~str_type_decl:
        (Deriving.Generator.V2.make ~deps:[ of_json ] Deriving.Args.empty
           expand)
end

module To_json_string = struct
  let expand =
    expand_via ~what:(Expansion_helpers.Suffix "to_json_string")
      ~through:(Expansion_helpers.Suffix "to_json") (fun ~loc to_json ->
        [%expr fun _data -> Melange_json.to_string ([%e to_json] _data)])

  let register ~to_json () =
    Deriving.add "to_json_string"
      ~str_type_decl:
        (Deriving.Generator.V2.make ~deps:[ to_json ] Deriving.Args.empty
           expand)
end

module Json_string = struct
  let expand ~ctxt tds =
    Of_json_string.expand ~ctxt tds @ To_json_string.expand ~ctxt tds

  let register ~json () =
    Deriving.add "json_string"
      ~str_type_decl:
        (Deriving.Generator.V2.make ~deps:[ json ] Deriving.Args.empty
           expand)
end
