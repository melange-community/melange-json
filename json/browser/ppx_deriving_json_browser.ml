open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_tools
open Ppx_deriving_tools.Conv
open Ppx_deriving_json_common

module Of_json = struct
  let build_tuple ~loc derive si (ts : core_type list) e =
    pexp_tuple ~loc
      (List.mapi ts ~f:(fun i t ->
           derive t
             [%expr Js.Array.unsafe_get [%e e] [%e eint ~loc (si + i)]]))

  let build_js_type ~loc (fs : label_declaration list) =
    let f ld =
      let n = ld.pld_name in
      let n = Option.get_or ~default:n (ld_attr_json_key ld) in
      let pof_desc = Otag (n, [%type: Js.Json.t Js.undefined]) in
      { pof_loc = loc; pof_attributes = []; pof_desc }
    in
    let row = ptyp_object ~loc (List.map fs ~f) Closed in
    [%type: [%t row] Js.t]

  let build_record ~loc derive (fs : label_declaration list) x make =
    let handle_field fs ld =
      ( map_loc lident ld.pld_name,
        let n = ld.pld_name in
        let n = Option.get_or ~default:n (ld_attr_json_key ld) in
        [%expr
          match
            Js.Undefined.toOption
              [%e fs] ## [%e pexp_ident ~loc:n.loc (map_loc lident n)]
          with
          | Stdlib.Option.Some v -> [%e derive ld.pld_type [%expr v]]
          | Stdlib.Option.None ->
              [%e
                match ld_attr_json_default ld with
                | Some default -> default
                | None ->
                    [%expr
                      Ppx_deriving_json_runtime.of_json_error
                        [%e
                          estring ~loc (sprintf "missing field %S" n.txt)]]]]
      )
    in
    [%expr
      let fs = (Obj.magic [%e x] : [%t build_js_type ~loc fs]) in
      [%e
        make
          (pexp_record ~loc
             (List.map fs ~f:(handle_field [%expr fs]))
             None)]]

  let eis_json_object ~loc x =
    [%expr
      Js.typeof [%e x] = "object"
      && (not (Js.Array.isArray [%e x]))
      && not ((Obj.magic [%e x] : 'a Js.null) == Js.null)]

  let ensure_json_object ~loc x =
    [%expr
      if not [%e eis_json_object ~loc x] then
        Ppx_deriving_json_runtime.of_json_error
          [%e estring ~loc (sprintf "expected a JSON object")]]

  let ensure_json_array_len ~loc n len =
    [%expr
      if [%e len] <> [%e eint ~loc n] then
        Ppx_deriving_json_runtime.of_json_error
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

  let derive_of_tuple derive t x =
    let loc = t.tpl_loc in
    let n = List.length t.tpl_types in
    [%expr
      if
        Js.Array.isArray [%e x]
        && Js.Array.length (Obj.magic [%e x] : Js.Json.t array)
           = [%e eint ~loc n]
      then
        let es = (Obj.magic [%e x] : Js.Json.t array) in
        [%e build_tuple ~loc derive 0 t.tpl_types [%expr es]]
      else
        Ppx_deriving_json_runtime.of_json_error
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

  let derive_of_record derive t x =
    let loc = t.rcd_loc in
    [%expr
      [%e ensure_json_object ~loc x];
      [%e build_record ~loc derive t.rcd_fields x Fun.id]]

  let derive_of_variant _derive t body x =
    let loc = t.vrt_loc in
    let is_enum =
      List.for_all t.vrt_cases ~f:(function
        | Vcs_enum _ -> true
        | _ -> false)
    in
    match is_enum with
    | true ->
        [%expr
          let tag =
            Ppx_deriving_json_runtime.Primitives.string_of_json [%e x]
          in
          [%e body]]
    | false ->
        [%expr
          if Js.Array.isArray [%e x] then
            let array = (Obj.magic [%e x] : Js.Json.t array) in
            let len = Js.Array.length array in
            if len > 0 then
              let tag = Js.Array.unsafe_get array 0 in
              if Js.typeof tag = "string" then
                let tag = (Obj.magic tag : string) in
                [%e body]
              else
                Ppx_deriving_json_runtime.of_json_error
                  "expected a non empty JSON array with element being a \
                   string"
            else
              Ppx_deriving_json_runtime.of_json_error
                "expected a non empty JSON array"
          else
            Ppx_deriving_json_runtime.of_json_error
              "expected a non empty JSON array"]

  let derive_of_variant_case derive make c next =
    match c with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as ctx) in
        [%expr
          if tag = [%e estring ~loc:n.loc n.txt] then [%e make None]
          else [%e next]]
    | Vcs_record (n, r) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as r.rcd_ctx) in
        [%expr
          if tag = [%e estring ~loc:n.loc n.txt] then (
            [%e ensure_json_array_len ~loc 2 [%expr len]];
            let fs = Js.Array.unsafe_get array 1 in
            [%e ensure_json_object ~loc [%expr fs]];
            [%e
              build_record ~loc derive r.rcd_fields [%expr fs] (fun e ->
                  make (Some e))])
          else [%e next]]
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as t.tpl_ctx) in
        let arity = List.length t.tpl_types in
        [%expr
          if tag = [%e estring ~loc:n.loc n.txt] then (
            [%e ensure_json_array_len ~loc (arity + 1) [%expr len]];
            [%e
              if arity = 0 then make None
              else
                make
                  (Some
                     (build_tuple ~loc derive 1 t.tpl_types [%expr array]))])
          else [%e next]]

  let deriving : Ppx_deriving_tools.deriving =
    deriving_of () ~name:"of_json"
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_json_runtime.of_json_error "invalid JSON"])
      ~of_t:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant
      ~derive_of_variant_case
end

module To_json = struct
  let as_json ~loc x = [%expr (Obj.magic [%e x] : Js.Json.t)]

  let derive_of_tuple derive t es =
    let loc = t.tpl_loc in
    as_json ~loc (pexp_array ~loc (List.map2 t.tpl_types es ~f:derive))

  let derive_of_record derive t es =
    let loc = t.rcd_loc in
    let fs =
      List.map2 t.rcd_fields es ~f:(fun ld x ->
          let n = ld.pld_name in
          let n = Option.get_or ~default:n (ld_attr_json_key ld) in
          let this = derive ld.pld_type x in
          map_loc lident n, this)
    in
    let record = pexp_record ~loc fs None in
    as_json ~loc [%expr [%mel.obj [%e record]]]

  let derive_of_variant_case derive c es =
    match c with
    | Vcs_enum (n, ctx) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as ctx) in
        let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
        as_json ~loc tag
    | Vcs_record (n, r) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as r.rcd_ctx) in
        let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
        let es = [ derive_of_record derive r es ] in
        as_json ~loc (pexp_array ~loc (tag :: es))
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.get_or ~default:n (vcs_attr_json_as t.tpl_ctx) in
        let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
        let es = List.map2 t.tpl_types es ~f:derive in
        as_json ~loc (pexp_array ~loc (tag :: es))

  let deriving : Ppx_deriving_tools.deriving =
    deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
end

let () =
  let _ = Ppx_deriving_tools.register Of_json.deriving in
  let _ = Ppx_deriving_tools.register To_json.deriving in
  let _ =
    Ppx_deriving_tools.register_combined "json"
      [ To_json.deriving; Of_json.deriving ]
  in
  ()
