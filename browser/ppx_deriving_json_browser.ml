open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_tools.Deriving_helper
open Utils

module Of_json = struct
  let build_tuple ~loc derive si ts e =
    pexp_tuple ~loc
      (List.mapi ts ~f:(fun i t ->
           derive ~loc t
             [%expr Js.Array.unsafe_get [%e e] [%e eint ~loc (si + i)]]))

  let build_js_type ~loc fs =
    let f (n, attrs, _) =
      let n_key =
        get_json_key_string_payload attrs |> Option.get_or ~default:n
      in
      let pof_desc = Otag (n_key, [%type: Js.Json.t Js.undefined]) in
      { pof_loc = loc; pof_attributes = []; pof_desc }
    in
    let row = ptyp_object ~loc (List.map fs ~f) Closed in
    [%type: [%t row] Js.t]

  let build_record ~loc derive fs x make =
    let handle_field fs (n, attrs, t) =
      ( map_loc lident n,
        let n_key =
          get_json_key_string_payload attrs |> Option.get_or ~default:n
        in
        let n_default = get_json_default_expr_payload attrs in
        [%expr
          match
            Js.Undefined.toOption
              [%e fs] ## [%e pexp_ident ~loc:n_key.loc (map_loc lident n_key)]
          with
          | Stdlib.Option.Some v -> [%e derive ~loc t [%expr v]]
          | Stdlib.Option.None ->
              [%e
                match n_default with
                | Some default -> default
                | None ->
                    [%expr
                      Ppx_deriving_json_runtime.of_json_error
                        [%e
                          estring ~loc
                            (sprintf "missing field %S" n_key.txt)]]]] )
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

  let derive_of_tuple ~loc derive ts x =
    let n = List.length ts in
    [%expr
      if
        Js.Array.isArray [%e x]
        && Js.Array.length (Obj.magic [%e x] : Js.Json.t array)
           = [%e eint ~loc n]
      then
        let es = (Obj.magic [%e x] : Js.Json.t array) in
        [%e build_tuple ~loc derive 0 ts [%expr es]]
      else
        Ppx_deriving_json_runtime.of_json_error
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

  let derive_of_record ~loc derive fs x =
    [%expr
      [%e ensure_json_object ~loc x];
      [%e build_record ~loc derive fs x Fun.id]]

  let derive_of_variant ~loc _derive cases x =
    [%expr
      if Js.Array.isArray [%e x] then
        let array = (Obj.magic [%e x] : Js.Json.t array) in
        let len = Js.Array.length array in
        if len > 0 then
          let tag = Js.Array.unsafe_get array 0 in
          if Js.typeof tag = "string" then
            let tag = (Obj.magic tag : string) in
            [%e cases]
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

  let derive_of_enum_variant ~loc _derive cases x =
    [%expr
      let enum =
        Ppx_deriving_json_runtime.Primitives.string_of_json [%e x]
      in
      [%e cases]]

  let derive_of_variant_case ~loc ~attrs derive make (n : label loc) ts
      next =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    let arity = List.length ts in
    [%expr
      if tag = [%e estring ~loc:n_as.loc n_as.txt] then (
        [%e ensure_json_array_len ~loc (arity + 1) [%expr len]];
        [%e
          if arity = 0 then make None
          else make (Some (build_tuple ~loc derive 1 ts [%expr array]))])
      else [%e next]]

  let derive_of_enum_variant_case ~loc ~attrs _derive make (n : label loc)
      _ts next =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    [%expr
      if enum = [%e estring ~loc:n_as.loc n_as.txt] then [%e make None]
      else [%e next]]

  let derive_of_variant_case_record ~loc ~attrs derive make
      (n : label loc) fs next =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    [%expr
      if tag = [%e estring ~loc:n_as.loc n_as.txt] then (
        [%e ensure_json_array_len ~loc 2 [%expr len]];
        let fs = Js.Array.unsafe_get array 1 in
        [%e ensure_json_object ~loc [%expr fs]];
        [%e
          build_record ~loc derive fs [%expr fs] (fun e -> make (Some e))])
      else [%e next]]

  let deriving =
    Ppx_deriving_tools.deriving_of () ~name:"of_json"
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_json_runtime.of_json_error "invalid JSON"])
      ~of_t:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant
      ~derive_of_variant_case ~derive_of_variant_case_record
      ~derive_of_enum_variant_case ~derive_of_enum_variant
end

module To_json = struct
  let as_json ~loc x = [%expr (Obj.magic [%e x] : Js.Json.t)]

  let derive_of_tuple ~loc derive ts es =
    as_json ~loc (pexp_array ~loc (List.map2 ts es ~f:(derive ~loc)))

  let derive_of_record ~loc derive fs es =
    let fs =
      List.map2 fs es ~f:(fun (n, attrs, t) x ->
          let n_key =
            get_json_key_string_payload attrs |> Option.get_or ~default:n
          in
          let this = derive ~loc t x in
          map_loc lident n_key, this)
    in
    let record = pexp_record ~loc fs None in
    as_json ~loc [%expr [%mel.obj [%e record]]]

  let derive_of_variant_case ~loc ~attrs derive n ts es =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    let tag =
      [%expr string_to_json [%e estring ~loc:n_as.loc n_as.txt]]
    in
    let es = List.map2 ts es ~f:(derive ~loc) in
    as_json ~loc (pexp_array ~loc (tag :: es))

  let derive_of_enum_variant_case ~loc ~attrs _derive n _ts _es =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    let tag =
      [%expr string_to_json [%e estring ~loc:n_as.loc n_as.txt]]
    in
    as_json ~loc tag

  let derive_of_variant_case_record ~loc ~attrs derive n fs es =
    let n_as =
      get_json_as_string_payload attrs |> Option.get_or ~default:n
    in
    let tag =
      [%expr string_to_json [%e estring ~loc:n_as.loc n_as.txt]]
    in
    let es = [ derive_of_record ~loc derive fs es ] in
    as_json ~loc (pexp_array ~loc (tag :: es))

  let deriving =
    Ppx_deriving_tools.deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
      ~derive_of_variant_case_record ~derive_of_enum_variant_case
end

let () =
  let _ = Ppx_deriving_tools.register Of_json.deriving in
  let _ = Ppx_deriving_tools.register To_json.deriving in
  let _ =
    Ppx_deriving_tools.(
      register (combined ~name:"json" Of_json.deriving To_json.deriving))
  in
  ()
