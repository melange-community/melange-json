open StdLabels
open Ppxlib
open Ast_builder.Default

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
       Ast_pattern.(pstr nil)
       ())

let ld_attr_default ld =
  match ld_attr_json_default ld with
  | Some e -> Some e
  | None -> (
      match ld_attr_json_option ld with
      | Some () ->
          let loc = ld.pld_loc in
          Some [%expr Stdlib.Option.None]
      | None -> None)

let ld_drop_default ld =
  let loc = ld.pld_loc in
  match ld_attr_json_drop_default ld, ld_attr_json_option ld with
  | Some (), None ->
      Location.raise_errorf ~loc
        "found [@drop_default] attribute without [@option]"
  | Some (), Some () -> `Drop_option
  | None, _ -> `No

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
        [%expr
          fun _json ->
            [%e of_json] (Ppx_deriving_json_runtime.of_string _json)])

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
        [%expr
          fun _data ->
            Ppx_deriving_json_runtime.to_string ([%e to_json] _data)])

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
