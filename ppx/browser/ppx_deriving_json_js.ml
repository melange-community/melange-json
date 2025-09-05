open Printf
open StdLabels
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
      let n = Option.value ~default:n (ld_attr_json_key ld) in
      let pof_desc = Otag (n, [%type: Js.Json.t Js.undefined]) in
      { pof_loc = loc; pof_attributes = []; pof_desc }
    in
    let row = ptyp_object ~loc (List.map fs ~f) Closed in
    [%type: [%t row] Js.t]

  let build_record ~loc derive (fs : label_declaration list) x make =
    let handle_field fs ld =
      ( map_loc lident ld.pld_name,
        let n = ld.pld_name in
        let n = Option.value ~default:n (ld_attr_json_key ld) in
        [%expr
          match
            Js.Undefined.toOption
              [%e fs]##[%e pexp_ident ~loc:n.loc (map_loc lident n)]
          with
          | Stdlib.Option.Some v -> [%e derive ld.pld_type [%expr v]]
          | Stdlib.Option.None ->
              [%e
                match ld_attr_default ld with
                | Some default -> default
                | None ->
                    [%expr
                      Melange_json.of_json_error ~json:x
                        [%e
                          estring ~loc
                            (sprintf "expected field %S to be present"
                               n.txt)]]]] )
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
      Stdlib.( && )
        (Stdlib.( = ) (Js.typeof [%e x]) "object")
        (Stdlib.( && )
           (Stdlib.not (Js.Array.isArray [%e x]))
           (Stdlib.not
              (Stdlib.( == ) (Obj.magic [%e x] : 'a Js.null) Js.null)))]

  let ensure_json_object ~loc x =
    [%expr
      if Stdlib.not [%e eis_json_object ~loc x] then
        Melange_json.of_json_error ~json:[%e x]
          [%e estring ~loc (sprintf "expected a JSON object")]]

  let ensure_json_array_len ~loc ~allow_any_constr ~else_ n len x =
    [%expr
      if Stdlib.( <> ) [%e len] [%e eint ~loc n] then
        [%e
          match allow_any_constr with
          | Some allow_any_constr -> allow_any_constr x
          | None ->
              [%expr
                Melange_json.of_json_error ~json:[%e x]
                  [%e
                    estring ~loc
                      (sprintf "expected a JSON array of length %i" n)]]]
      else [%e else_]]

  let derive_of_tuple derive t x =
    let loc = t.tpl_loc in
    let n = List.length t.tpl_types in
    [%expr
      if
        Stdlib.( && )
          (Js.Array.isArray [%e x])
          (Stdlib.( = )
             (Js.Array.length (Obj.magic [%e x] : Js.Json.t array))
             [%e eint ~loc n])
      then
        let es = (Obj.magic [%e x] : Js.Json.t array) in
        [%e build_tuple ~loc derive 0 t.tpl_types [%expr es]]
      else
        Melange_json.of_json_error ~json:[%e x]
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

  let derive_of_record derive t x =
    let loc = t.rcd_loc in
    [%expr
      [%e ensure_json_object ~loc x];
      [%e build_record ~loc derive t.rcd_fields x Fun.id]]

  let derive_of_variant _derive t ~allow_any_constr body x =
    let loc = t.vrt_loc in
    (* Check if we have zero-arity cases that need string tag decoding *)
    let has_string_tag_cases =
      List.exists t.vrt_cases ~f:vcs_should_serialize_as_string
    in
    (* Check if we have any cases that need tag comparison (non-allow_any cases) *)
    let has_tag_comparison_cases =
      List.exists t.vrt_cases ~f:(function
        | Vcs_record (_, r) -> not (vcs_attr_json_allow_any r.rcd_ctx)
        | Vcs_tuple (_, t) -> not (vcs_attr_json_allow_any t.tpl_ctx))
    in
    let needs_tag = has_string_tag_cases || has_tag_comparison_cases in

    let decode_string_tag =
      let pairs =
        List.filter_map t.vrt_cases ~f:(function
          | Vcs_record _ -> None
          | Vcs_tuple (n, tcase) ->
              if List.is_empty tcase.tpl_types then
                let n' =
                  Option.value ~default:n
                    (vcs_attr_json_name tcase.tpl_ctx)
                in
                let tag_name = estring ~loc:n'.loc n'.txt in
                let ctor =
                  match tcase.tpl_ctx with
                  | Vcs_ctx_variant _ ->
                      pexp_construct (map_loc lident n) ~loc:n.loc None
                  | Vcs_ctx_polyvariant _ ->
                      pexp_variant ~loc:n.loc n.txt None
                in
                Some (tag_name, ctor)
              else None)
      in
      List.fold_right pairs
        ~init:
          [%expr
            [%e
              match allow_any_constr with
              | Some allow_any_constr -> allow_any_constr x
              | None ->
                  [%expr
                    Melange_json.of_json_error ~json:[%e x]
                      "expected a non empty JSON array"]]]
        ~f:(fun (tag_name, ctor) acc ->
          [%expr
            if Stdlib.( = ) tag [%e tag_name] then [%e ctor] else [%e acc]])
    in
    [%expr
      if Js.Array.isArray [%e x] then
        let array = (Obj.magic [%e x] : Js.Json.t array) in
        let len = Js.Array.length array in
        if Stdlib.( > ) len 0 then
          [%e
            if needs_tag then
              [%expr
                let tag = Js.Array.unsafe_get array 0 in
                if Stdlib.( = ) (Js.typeof tag) "string" then
                  let tag = (Obj.magic tag : string) in
                  [%e body]
                else
                  [%e
                    match allow_any_constr with
                    | Some allow_any_constr -> allow_any_constr x
                    | None ->
                        [%expr
                          Melange_json.of_json_error ~json:[%e x]
                            "expected a non empty JSON array with \
                             element being a string"]]]
            else body]
        else
          [%e
            match allow_any_constr with
            | Some allow_any_constr -> allow_any_constr x
            | None ->
                [%expr
                  Melange_json.of_json_error ~json:[%e x]
                    "expected a non empty JSON array"]]
      else if Stdlib.( = ) (Js.typeof [%e x]) "string" then
        [%e
          if has_string_tag_cases then
            [%expr
              let tag = (Obj.magic [%e x] : string) in
              [%e decode_string_tag]]
          else decode_string_tag]
      else
        [%e
          match allow_any_constr with
          | Some allow_any_constr -> allow_any_constr x
          | None ->
              [%expr
                Melange_json.of_json_error ~json:[%e x]
                  "expected a non empty JSON array"]]]

  let derive_of_variant_case derive make c ~allow_any_constr next =
    match c with
    | Vcs_record (n, r) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_name r.rcd_ctx) in
        [%expr
          if Stdlib.( = ) tag [%e estring ~loc:n.loc n.txt] then
            [%e
              ensure_json_array_len ~loc ~allow_any_constr 2 [%expr len]
                [%expr x]
                ~else_:
                  [%expr
                    let fs = Js.Array.unsafe_get array 1 in
                    [%e ensure_json_object ~loc [%expr fs]];
                    [%e
                      build_record ~loc derive r.rcd_fields [%expr fs]
                        (fun e -> make (Some e))]]]
          else [%e next]]
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_name t.tpl_ctx) in
        let arity = List.length t.tpl_types in
        [%expr
          if Stdlib.( = ) tag [%e estring ~loc:n.loc n.txt] then
            [%e
              ensure_json_array_len ~loc ~allow_any_constr (arity + 1)
                [%expr len] [%expr x]
                ~else_:
                  (if arity = 0 then make None
                   else
                     make
                       (Some
                          (build_tuple ~loc derive 1 t.tpl_types
                             [%expr array])))]
          else [%e next]]

  let is_allow_any_constr vcs =
    Ppx_deriving_json_common.vcs_attr_json_allow_any vcs

  let deriving : Ppx_deriving_tools.deriving =
    deriving_of () ~name:"of_json"
      ~of_t:(fun ~loc -> [%type: Js.Json.t])
      ~is_allow_any_constr ~derive_of_tuple ~derive_of_record
      ~derive_of_variant ~derive_of_variant_case
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
          let k =
            let k = ld.pld_name in
            Option.value ~default:k (ld_attr_json_key ld)
          in
          let v =
            let v = derive ld.pld_type x in
            match ld_drop_default ld with
            | `No -> v
            | `Drop_option ->
                [%expr
                  match [%e x] with
                  | Stdlib.Option.None -> Js.Undefined.empty
                  | Stdlib.Option.Some _ -> Js.Undefined.return [%e v]]
          in
          map_loc lident k, v)
    in
    let record = pexp_record ~loc fs None in
    as_json ~loc [%expr [%mel.obj [%e record]]]

  let derive_of_variant_case derive td_opt c es =
    let legacy =
      match td_opt with
      | Some td -> Option.is_some (td_attr_no_args_variant_cases_as_arrays_variant td)
      | None -> false
    in
    match c with
    | Vcs_record (n, r) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_name r.rcd_ctx) in
        let tag =
          [%expr (Obj.magic [%e estring ~loc:n.loc n.txt] : Js.Json.t)]
        in
        let es = [ derive_of_record derive r es ] in
        as_json ~loc (pexp_array ~loc (tag :: es))
    | Vcs_tuple (_n, t) when vcs_attr_json_allow_any t.tpl_ctx -> (
        match es with
        | [ x ] -> x
        | es ->
            failwith
              (sprintf "expected a tuple of length 1, got %i"
                 (List.length es)))
    | Vcs_tuple (n, t) ->
        let loc = n.loc in
        let n = Option.value ~default:n (vcs_attr_json_name t.tpl_ctx) in
        let tag =
          [%expr (Obj.magic [%e estring ~loc:n.loc n.txt] : Js.Json.t)]
        in
        if List.is_empty t.tpl_types && not legacy then
          as_json ~loc
            [%expr (Obj.magic [%e estring ~loc:n.loc n.txt] : Js.Json.t)]
        else
          let es = List.map2 t.tpl_types es ~f:derive in
          as_json ~loc (pexp_array ~loc (tag :: es))

  let deriving : Ppx_deriving_tools.deriving =
    deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
end

let () =
  let of_json = Ppx_deriving_tools.register Of_json.deriving in
  let to_json = Ppx_deriving_tools.register To_json.deriving in
  let json =
    Ppx_deriving_tools.register_combined "json"
      [ To_json.deriving; Of_json.deriving ]
  in
  let (_ : Deriving.t) = Of_json_string.register ~of_json () in
  let (_ : Deriving.t) = To_json_string.register ~to_json () in
  let (_ : Deriving.t) = Json_string.register ~json () in
  ()
