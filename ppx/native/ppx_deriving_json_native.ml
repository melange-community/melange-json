open Printf
open StdLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_tools
open Ppx_deriving_tools.Conv
open Ppx_deriving_json_common

module Of_json = struct
  let with_refs ~loc prefix fs inner =
    let gen_name n = sprintf "%s_%s" prefix n in
    let gen_expr (n : label loc) =
      pexp_ident ~loc:n.loc { loc = n.loc; txt = lident (gen_name n.txt) }
    in
    List.fold_left (List.rev fs) ~init:(inner gen_expr) ~f:(fun next ld ->
        let n = ld.pld_name in
        let patt =
          ppat_var ~loc:n.loc { loc = n.loc; txt = gen_name n.txt }
        in
        [%expr
          let [%p patt] =
            ref
              [%e
                match ld_attr_default ld with
                | Some default -> [%expr Stdlib.Option.Some [%e default]]
                | None -> [%expr Stdlib.Option.None]]
          in
          [%e next]])

  let build_tuple ~loc derive es ts =
    let args =
      List.fold_left
        (List.rev (List.combine es ts))
        ~init:[]
        ~f:(fun prev (x, t) ->
          let this = derive t x in
          this :: prev)
    in
    pexp_tuple ~loc args

  let build_record ~allow_extra_fields ~loc derive fs x make =
    with_refs ~loc "x" fs @@ fun ename ->
    let handle_field k v =
      let fail_case =
        [%pat? name]
        -->
        if allow_extra_fields then [%expr ()]
        else
          [%expr
            Ppx_deriving_json_runtime.of_json_error
              (Stdlib.Printf.sprintf "unknown field: %s" name)]
      in
      let cases =
        List.fold_left (List.rev fs) ~init:[ fail_case ]
          ~f:(fun next ld ->
            let key =
              Option.value ~default:ld.pld_name (ld_attr_json_key ld)
            in
            pstring ~loc:key.loc key.txt
            --> [%expr
                  [%e ename ld.pld_name] :=
                    Stdlib.Option.Some [%e derive ld.pld_type v]]
            :: next)
      in
      pexp_match ~loc k cases
    in
    let build =
      let fields =
        List.map fs ~f:(fun ld ->
            let key =
              Option.value ~default:ld.pld_name (ld_attr_json_key ld)
            in
            let default = ld_attr_default ld in
            ( map_loc lident ld.pld_name,
              [%expr
                match Stdlib.( ! ) [%e ename ld.pld_name] with
                | Stdlib.Option.Some v -> v
                | Stdlib.Option.None ->
                    [%e
                      match default with
                      | Some default -> default
                      | None ->
                          [%expr
                            Ppx_deriving_json_runtime.of_json_error
                              [%e
                                estring ~loc:key.loc
                                  (sprintf "missing field %S" key.txt)]]]]
            ))
      in
      pexp_record ~loc fields None
    in
    [%expr
      let rec iter = function
        | [] -> ()
        | (n', v) :: fs ->
            [%e handle_field [%expr n'] [%expr v]];
            iter fs
      in
      iter [%e x];
      [%e make build]]

  let derive_of_tuple derive t x =
    let loc = t.tpl_loc in
    let n = List.length t.tpl_types in
    let xpatt, xexprs = gen_pat_list ~loc "x" n in
    let xpatt = [%pat? `List [%p xpatt]] in
    pexp_match ~loc x
      [
        xpatt --> build_tuple ~loc derive xexprs t.tpl_types;
        [%pat? _]
        --> [%expr
              Ppx_deriving_json_runtime.of_json_error
                [%e
                  estring ~loc
                    (sprintf "expected a JSON array of length %i" n)]];
      ]

  let derive_of_record derive t x =
    let loc = t.rcd_loc in
    let allow_extra_fields =
      Option.is_some (td_attr_json_allow_extra_fields t.rcd_ctx)
    in
    pexp_match ~loc x
      [
        [%pat? `Assoc fs]
        --> build_record ~allow_extra_fields ~loc derive t.rcd_fields
              [%expr fs] Fun.id;
        [%pat? _]
        --> [%expr
              Ppx_deriving_json_runtime.of_json_error
                [%e estring ~loc (sprintf "expected a JSON object")]];
      ]

  let derive_of_variant_case derive make vcs =
    match vcs with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as ctx) in
        [%pat? `String [%p pstring ~loc:n.loc n.txt]] --> make None
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as t.tpl_ctx) in
        let arity = List.length t.tpl_types in
        if arity = 0 then
          [%pat? `List [ `String [%p pstring ~loc:n.loc n.txt] ]]
          --> make None
        else
          let xpatt, xexprs = gen_pat_list ~loc "x" arity in
          [%pat?
            `List (`String [%p pstring ~loc:n.loc n.txt] :: [%p xpatt])]
          --> make (Some (build_tuple ~loc derive xexprs t.tpl_types))
    | Vcs_record (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as t.rcd_ctx) in
        let allow_extra_fields =
          match t.rcd_ctx with
          | Vcs_ctx_variant cd ->
              Option.is_some (cd_attr_json_allow_extra_fields cd)
          | Vcs_ctx_polyvariant _ -> false
        in
        [%pat? `List [ `String [%p pstring ~loc:n.loc n.txt]; `Assoc fs ]]
        --> build_record ~allow_extra_fields ~loc derive t.rcd_fields
              [%expr fs] (fun e -> make (Some e))

  let deriving : Ppx_deriving_tools.deriving =
    deriving_of_match () ~name:"of_json"
      ~of_t:(fun ~loc -> [%type: Yojson.Basic.t])
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_json_runtime.of_json_error "invalid JSON"])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
end

module To_json = struct
  let gen_exp_pat ~loc prefix =
    let n = gen_symbol ~prefix () in
    evar ~loc n, pvar ~loc n

  let derive_of_tuple derive t es =
    let loc = t.tpl_loc in
    let es = List.map2 t.tpl_types es ~f:derive in
    [%expr `List [%e elist ~loc es]]

  let derive_of_record derive t es =
    let loc = t.rcd_loc in
    let ebnds, pbnds = gen_exp_pat ~loc "bnds" in
    let e =
      List.combine t.rcd_fields es
      |> List.fold_left ~init:ebnds ~f:(fun acc (ld, x) ->
             let key =
               Option.value ~default:ld.pld_name (ld_attr_json_key ld)
             in
             let k = estring ~loc:key.loc key.txt in
             let v = derive ld.pld_type x in
             let ebnds =
               match ld_drop_default ld with
               | `No -> [%expr ([%e k], [%e v]) :: [%e ebnds]]
               | `Drop_option ->
                   [%expr
                     match [%e x] with
                     | Stdlib.Option.None -> [%e ebnds]
                     | Stdlib.Option.Some _ ->
                         ([%e k], [%e v]) :: [%e ebnds]]
             in
             [%expr
               let [%p pbnds] = [%e ebnds] in
               [%e acc]])
    in
    [%expr
      `Assoc
        (let [%p pbnds] = [] in
         [%e e])]

  let derive_of_variant_case derive vcs es =
    match vcs with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as ctx) in
        [%expr `String [%e estring ~loc:n.loc n.txt]]
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as t.tpl_ctx) in
        [%expr
          `List
            (`String [%e estring ~loc:n.loc n.txt]
            :: [%e elist ~loc (List.map2 t.tpl_types es ~f:derive)])]
    | Vcs_record (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_as t.rcd_ctx) in
        [%expr
          `List
            (`String [%e estring ~loc:n.loc n.txt]
            :: [ [%e derive_of_record derive t es] ])]

  let deriving : Ppx_deriving_tools.deriving =
    deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Yojson.Basic.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
end

let () =
  let _ = Ppx_deriving_tools.register Of_json.deriving in
  let _ = Ppx_deriving_tools.register To_json.deriving in
  let _ =
    Ppx_deriving_tools.(
      register_combined "json" [ To_json.deriving; Of_json.deriving ])
  in
  ()
