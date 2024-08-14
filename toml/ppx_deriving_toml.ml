open Ppxlib
open Ast_builder.Default
open Ppx_deriving_tools
open Ppx_deriving_tools.Conv

module Attr = struct
  let get_of_variant_case ?mark_as_seen ~variant ~polyvariant = function
    | Vcs_ctx_variant ctx -> Attribute.get ?mark_as_seen variant ctx
    | Vcs_ctx_polyvariant ctx ->
        Attribute.get ?mark_as_seen polyvariant ctx

  let attr_as ctx =
    Attribute.declare "toml.as" ctx
      Ast_pattern.(single_expr_payload (estring __'))
      (fun x -> x)

  let vcs_as =
    let variant = attr_as Attribute.Context.constructor_declaration in
    let polyvariant = attr_as Attribute.Context.rtag in
    get_of_variant_case ~variant ~polyvariant

  let ld_key =
    Attribute.get
      (Attribute.declare "toml.key" Attribute.Context.label_declaration
         Ast_pattern.(single_expr_payload (estring __'))
         (fun x -> x))

  let ld_default =
    Attribute.get
      (Attribute.declare "toml.default"
         Attribute.Context.label_declaration
         Ast_pattern.(single_expr_payload __)
         (fun x -> x))

  let ld_option =
    Attribute.has_flag
      (Attribute.declare_flag "toml.option"
         Attribute.Context.label_declaration)
end

module Of_toml = struct
  let with_refs ~loc ~name fs inner =
    let gen_name n = sprintf "x_%s" n in
    let gen_expr (n : label loc) =
      pexp_ident ~loc:n.loc { loc = n.loc; txt = lident (gen_name n.txt) }
    in
    List.foldi fs ~init:(inner gen_expr) ~f:(fun next idx ld ->
        let n = name idx ld in
        let patt =
          ppat_var ~loc:n.loc { loc = n.loc; txt = gen_name n.txt }
        in
        [%expr
          let [%p patt] = ref None in
          [%e next]])

  let build_tuple ~loc derive ts x =
    let p, es = gen_pat_list ~loc "x" (List.length ts) in
    let n = List.length ts in
    [%expr
      match [%e x] with
      | Otoml.TomlArray [%p p] | Otoml.TomlTableArray [%p p] ->
          [%e
            let args =
              List.fold_left
                (List.rev (List.combine es ts))
                ~init:[]
                ~f:(fun prev (x, t) ->
                  let this = derive t x in
                  this :: prev)
            in
            pexp_tuple ~loc args]
      | x ->
          Ppx_deriving_toml_runtime.of_toml_error
            [%e
              estring ~loc
                (sprintf "expected a TOML array of length %i" n)]]

  let build_record ~ignore_type_field ~loc derive fs x make =
    with_refs ~loc ~name:(fun _idx ld -> ld.pld_name) fs @@ fun ename ->
    let handle_field k v =
      let fail_case =
        [%pat? name]
        --> [%expr
              Ppx_deriving_toml_runtime.of_toml_error
                (Stdlib.Printf.sprintf "unknown field: %s" name)]
      in
      let cases =
        List.fold_left (List.rev fs) ~init:[ fail_case ]
          ~f:(fun next ld ->
            let is_option = Attr.ld_option ld in
            let key =
              Option.get_or ~default:ld.pld_name (Attr.ld_key ld)
            in
            let patt = pstring ~loc:key.loc key.txt in
            let expr =
              match is_option with
              | false ->
                  [%expr
                    [%e ename ld.pld_name] :=
                      Stdlib.Option.Some [%e derive ld.pld_type v]]
              | true -> (
                  match ld.pld_type with
                  | [%type: [%t? t] option] ->
                      [%expr
                        [%e ename ld.pld_name] :=
                          Stdlib.Option.Some
                            (Stdlib.Option.Some [%e derive t v])]
                  | _ -> pexp_error ~loc "expected an option type")
            in
            (patt --> expr) :: next)
      in
      let cases =
        match ignore_type_field with
        | false -> cases
        | true -> ([%pat? "type"] --> [%expr ()]) :: cases
      in
      pexp_match ~loc k cases
    in
    let build =
      let fields =
        List.map fs ~f:(fun ld ->
            let key =
              Option.get_or ~default:ld.pld_name (Attr.ld_key ld)
            in
            let default = Attr.ld_default ld in
            let is_option = Attr.ld_option ld in
            ( map_loc lident ld.pld_name,
              [%expr
                match Stdlib.( ! ) [%e ename ld.pld_name] with
                | Stdlib.Option.Some v -> v
                | Stdlib.Option.None ->
                    [%e
                      match default, is_option with
                      | Some default, _ -> default
                      | None, true -> [%expr None]
                      | None, false ->
                          [%expr
                            Ppx_deriving_toml_runtime.of_toml_error
                              [%e
                                estring ~loc:key.loc
                                  (sprintf "missing field %S" key.txt)]]]]
            ))
      in
      pexp_record ~loc fields None
    in
    [%expr
      List.iter
        (fun (name, v) -> [%e handle_field [%expr name] [%expr v]])
        [%e x];
      [%e make build]]

  let derive_of_tuple derive t x =
    let loc = t.tpl_loc in
    build_tuple ~loc derive t.tpl_types x

  let derive_of_record derive t x =
    let loc = t.rcd_loc in
    pexp_match ~loc x
      [
        [%pat? Otoml.TomlTable x | Otoml.TomlInlineTable x]
        --> build_record ~ignore_type_field:false ~loc derive t.rcd_fields
              [%expr x] Fun.id;
        [%pat? _]
        --> [%expr
              Ppx_deriving_toml_runtime.of_toml_error
                [%e estring ~loc (sprintf "expected a TOML table")]];
      ]

  let derive_of_variant_case derive make vcs next =
    match vcs with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as ctx) in
        [%expr
          match tag with
          | [%p pstring ~loc:n.loc n.txt] -> [%e make None]
          | _ -> [%e next]]
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as t.tpl_ctx) in
        let arity = List.length t.tpl_types in
        if arity = 0 then
          [%expr
            match tag with
            | [%p pstring ~loc:n.loc n.txt] -> [%e make None]
            | _ -> [%e next]]
        else
          [%expr
            match tag with
            | [%p pstring ~loc:n.loc n.txt] ->
                let x =
                  match List.assoc "args" x with
                  | x -> x
                  | exception Not_found ->
                      Ppx_deriving_toml_runtime.of_toml_error
                        "missing \"args\" field in TOML table"
                in
                [%e
                  make
                    (Some (build_tuple ~loc derive t.tpl_types [%expr x]))]
            | _ -> [%e next]]
    | Vcs_record (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as t.rcd_ctx) in
        [%expr
          match tag with
          | [%p pstring ~loc:n.loc n.txt] ->
              [%e
                build_record ~ignore_type_field:true ~loc derive
                  t.rcd_fields [%expr x] (fun e -> make (Some e))]
          | _ -> [%e next]]

  let derive_of_variant _derive t body x =
    let loc = t.vrt_loc in
    [%expr
      let tag, x =
        match [%e x] with
        | Otoml.TomlTable x | Otoml.TomlInlineTable x -> (
            match List.assoc "type" x with
            | Otoml.TomlString tag -> tag, x
            | _ | (exception Not_found) ->
                Ppx_deriving_toml_runtime.of_toml_error
                  "expected a TOML table with a 'type' field")
        | _ ->
            Ppx_deriving_toml_runtime.of_toml_error
              "expected a TOML table"
      in
      [%e body]]

  let deriving : Ppx_deriving_tools.deriving =
    deriving_of () ~name:"of_toml"
      ~of_t:(fun ~loc -> [%type: Otoml.t])
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_toml_runtime.of_toml_error "invalid TOML"])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant
      ~derive_of_variant_case
end

module To_toml = struct
  let derive_of_tuple derive t es =
    let loc = t.tpl_loc in
    let es =
      List.combine t.tpl_types es
      |> List.map ~f:(fun (t, e) -> derive t e)
    in
    [%expr Otoml.TomlArray [%e elist ~loc es]]

  let derive_of_record derive t es =
    let loc = t.rcd_loc in
    let es =
      List.combine t.rcd_fields es
      |> List.rev
      |> List.fold_left ~init:[%expr []] ~f:(fun prev (ld, x) ->
             let n =
               Option.get_or ~default:ld.pld_name (Attr.ld_key ld)
             in
             let n = estring ~loc:n.loc n.txt in
             match Attr.ld_option ld with
             | true -> (
                 match ld.pld_type with
                 | [%type: [%t? t] option] ->
                     [%expr
                       match [%e x] with
                       | Some x ->
                           ([%e n], [%e derive t [%expr x]]) :: [%e prev]
                       | None -> [%e prev]]
                 | _ ->
                     [%expr
                       [%e pexp_error ~loc "expected an option type"]
                       :: [%e prev]])
             | false ->
                 [%expr ([%e n], [%e derive ld.pld_type x]) :: [%e prev]])
    in
    [%expr Otoml.TomlTable [%e es]]

  let derive_of_variant_case derive vcs es =
    match vcs with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as ctx) in
        [%expr
          Ppx_deriving_toml_runtime.string_to_toml
            [%e estring ~loc:n.loc n.txt]]
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as t.tpl_ctx) in
        [%expr
          let args = [%e derive_of_tuple derive t es] in
          Otoml.TomlInlineTable
            [
              "type", Otoml.TomlString [%e estring ~loc:n.loc n.txt];
              "args", args;
            ]]
    | Vcs_record (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (Attr.vcs_as t.rcd_ctx) in
        [%expr
          match [%e derive_of_record derive t es] with
          | Otoml.TomlTable t ->
              Otoml.TomlTable
                (("type", Otoml.TomlString [%e estring ~loc:n.loc n.txt])
                :: t)
          | Otoml.TomlInlineTable t ->
              Otoml.TomlInlineTable
                (("type", Otoml.TomlString [%e estring ~loc:n.loc n.txt])
                :: t)
          | _ -> assert false]

  let deriving : Ppx_deriving_tools.deriving =
    deriving_to () ~name:"to_toml"
      ~t_to:(fun ~loc -> [%type: Otoml.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
end

let () =
  let _ = Ppx_deriving_tools.register Of_toml.deriving in
  let _ = Ppx_deriving_tools.register To_toml.deriving in
  let _ =
    Ppx_deriving_tools.(
      register_combined "toml" [ To_toml.deriving; Of_toml.deriving ])
  in
  ()
