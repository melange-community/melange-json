open ContainersLabels
open Ppxlib

let get_attribute_by_name attributes name =
  let filtered =
    attributes
    |> List.filter ~f:(fun { attr_name = { txt; _ }; _ } ->
           String.equal txt name)
  in
  match filtered with [ attr ] -> Some attr | _ -> None

let get_expr_from_payload { attr_payload = payload; _ } =
  match payload with
  | PStr ({ pstr_desc; _ } :: []) -> (
      match pstr_desc with Pstr_eval (expr, _) -> Some expr | _ -> None)
  | _ -> None

let get_const_string_from_expr expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) -> { txt; loc }
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "expected a string constant"

let get_json_as_string_payload attrs =
  get_attribute_by_name attrs "json.as"
  |> Option.flat_map get_expr_from_payload
  |> Option.map get_const_string_from_expr

let get_json_key_string_payload attrs =
  get_attribute_by_name attrs "json.key"
  |> Option.flat_map get_expr_from_payload
  |> Option.map get_const_string_from_expr

let get_json_default_expr_payload attrs =
  get_attribute_by_name attrs "json.default"
  |> Option.flat_map get_expr_from_payload
