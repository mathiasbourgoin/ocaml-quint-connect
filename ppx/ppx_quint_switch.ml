(*
  PPX rewriter for [%switch] and [%switch_bind] extensions.

  ── [%switch] ────────────────────────────────────────────────────────────────
  Input:
    [%switch (step_expr, [
      ("ActionName", fun s -> body);
      ...
    ])]

  Output (returns ('a, string) result):
    let __switch_step_N = step_expr in
    match __switch_step_N.Ocaml_quint_connect.Itf.Step.action_name with
    | Some "ActionName" -> Ok ((fun s -> body) __switch_step_N)
    | ...
    | None -> Error "switch: step has no action"
    | Some __switch_unknown -> Error ("switch: unmatched action: " ^ __switch_unknown)

  ── [%switch_bind] ────────────────────────────────────────────────────────────
  Input:
    [%switch_bind (step_expr, [
      ("ActionName", ["p1"; "p2?"; ...], fun p1 p2_opt ... -> body);
      ...
    ])]

  Params ending in '?' are optional (bound via Switch.param_opt → 'a option).
  Required params are bound via Switch.param → (Value.t, string) result;
  errors short-circuit and are returned as Error.

  Output (returns ('a, string) result):
    let __switch_step_N = step_expr in
    match __switch_step_N.Ocaml_quint_connect.Itf.Step.action_name with
    | Some "ActionName" ->
        (match Switch.param __step "p1" with
         | Error e -> Error e
         | Ok __v0 ->
           let __v1 = Switch.param_opt __step "p2" in
           Ok ((fun p1 p2_opt -> body) __v0 __v1))
    | ...
    | None -> Error "switch: step has no action"
    | Some __switch_unknown -> Error ("switch: unmatched action: " ^ __switch_unknown)
*)

open Ppxlib
open Ast_builder.Default

(* ── Shared helpers ───────────────────────────────────────────────────────── *)

let default_arms ~loc step_var =
  let unknown_var = gen_symbol ~prefix:"__switch_u" () in
  [ case
      ~lhs:(ppat_construct ~loc { txt = Lident "None"; loc } None)
      ~guard:None
      ~rhs:[%expr Error "switch: step has no action"]
  ; case
      ~lhs:(ppat_construct ~loc
        { txt = Lident "Some"; loc }
        (Some (ppat_var ~loc { txt = unknown_var; loc })))
      ~guard:None
      ~rhs:[%expr Error ("switch: unmatched action: " ^ [%e evar ~loc unknown_var])]
  ]
  |> fun arms ->
  (* prepend the step variable so it's not unused *)
  ignore step_var; arms

let action_name_expr ~loc step_var =
  [%expr ([%e evar ~loc step_var]).Ocaml_quint_connect.Itf.Step.action_name]

(* ── [%switch] ───────────────────────────────────────────────────────────── *)

let extract_arms ~loc expr =
  let rec go e =
    match e.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
    | Pexp_construct
        ( { txt = Lident "::"; _ }
        , Some { pexp_desc = Pexp_tuple [hd; tl]; _ } ) ->
      let arm =
        match hd.pexp_desc with
        | Pexp_tuple
            [ { pexp_desc = Pexp_constant (Pconst_string (action, _, _)); _ }
            ; fn_expr ] ->
          (action, fn_expr)
        | _ ->
          Location.raise_errorf ~loc
            "[%%switch]: each arm must be a pair (\"ActionName\", fun s -> ...)"
      in
      arm :: go tl
    | _ ->
      Location.raise_errorf ~loc
        "[%%switch]: arms must be a list literal [ (\"A\", fn); ... ]"
  in
  go expr

let expand_switch ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match payload.pexp_desc with
  | Pexp_tuple [step_expr; arms_expr] ->
    let arms = extract_arms ~loc arms_expr in
    let step_var = gen_symbol ~prefix:"__switch_step" () in
    let match_arms =
      List.map (fun (action, fn_expr) ->
        case
          ~lhs:(ppat_construct ~loc
            { txt = Lident "Some"; loc }
            (Some (ppat_constant ~loc (Pconst_string (action, loc, None)))))
          ~guard:None
          ~rhs:[%expr Ok ([%e fn_expr] [%e evar ~loc step_var])]
      ) arms
    in
    let match_expr =
      pexp_match ~loc
        (action_name_expr ~loc step_var)
        (match_arms @ default_arms ~loc step_var)
    in
    pexp_let ~loc Nonrecursive
      [ value_binding ~loc
          ~pat:(ppat_var ~loc { txt = step_var; loc })
          ~expr:step_expr ]
      match_expr
  | _ ->
    Location.raise_errorf ~loc
      "[%%switch]: expected a pair (step_expr, arms_list)"

(* ── [%switch_bind] ──────────────────────────────────────────────────────── *)

let extract_string_list ~loc expr =
  let rec go e =
    match e.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
    | Pexp_construct
        ( { txt = Lident "::"; _ }
        , Some { pexp_desc = Pexp_tuple [hd; tl]; _ } ) ->
      let s =
        match hd.pexp_desc with
        | Pexp_constant (Pconst_string (s, _, _)) -> s
        | _ ->
          Location.raise_errorf ~loc
            "[%%switch_bind]: param names must be string literals (e.g. \"p1\" or \"p2?\")"
      in
      s :: go tl
    | _ ->
      Location.raise_errorf ~loc
        "[%%switch_bind]: param list must be a list literal [\"p1\"; \"p2?\"; ...]"
  in
  go expr

let extract_bind_arms ~loc expr =
  let rec go e =
    match e.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
    | Pexp_construct
        ( { txt = Lident "::"; _ }
        , Some { pexp_desc = Pexp_tuple [hd; tl]; _ } ) ->
      let arm =
        match hd.pexp_desc with
        | Pexp_tuple [name_expr; params_expr; fn_expr] ->
          let action =
            match name_expr.pexp_desc with
            | Pexp_constant (Pconst_string (s, _, _)) -> s
            | _ ->
              Location.raise_errorf ~loc
                "[%%switch_bind]: action name must be a string literal"
          in
          let params = extract_string_list ~loc params_expr in
          (action, params, fn_expr)
        | _ ->
          Location.raise_errorf ~loc
            "[%%switch_bind]: each arm must be a triple \
             (\"ActionName\", [\"p1\"; \"p2?\"; ...], fun p1 p2_opt -> ...)"
      in
      arm :: go tl
    | _ ->
      Location.raise_errorf ~loc
        "[%%switch_bind]: arms must be a list literal"
  in
  go expr

(* Build the body for one [%switch_bind] arm.
   Generates nested match/let bindings for all params, then calls fn_expr
   with all bound values, wrapped in Ok. *)
let build_bind_body ~loc step_var fn_expr params =
  (* Assign a fresh internal variable name to each param position *)
  let param_vars =
    List.mapi (fun i _ -> Printf.sprintf "__switch_vb%d" i) params
  in
  (* Build: fn_expr v0 v1 v2 ... *)
  let fn_call =
    List.fold_left
      (fun acc var -> [%expr [%e acc] [%e evar ~loc var]])
      fn_expr
      param_vars
  in
  (* Innermost expression: Ok (fn_call) *)
  let inner = [%expr Ok [%e fn_call]] in
  (* Fold right: wrap each param from last to first *)
  List.fold_right2
    (fun param_name var acc ->
      let is_opt =
        String.length param_name > 0
        && param_name.[String.length param_name - 1] = '?'
      in
      let clean =
        if is_opt then String.sub param_name 0 (String.length param_name - 1)
        else param_name
      in
      if is_opt then
        (* let var = Switch.param_opt step "clean" in acc *)
        pexp_let ~loc Nonrecursive
          [ value_binding ~loc
              ~pat:(ppat_var ~loc { txt = var; loc })
              ~expr:[%expr Ocaml_quint_connect.Itf.Switch.param_opt
                [%e evar ~loc step_var]
                [%e estring ~loc clean]] ]
          acc
      else
        (* match Switch.param step "clean" with Error e -> Error e | Ok var -> acc *)
        let err_var = gen_symbol ~prefix:"__switch_eb" () in
        [%expr
          match Ocaml_quint_connect.Itf.Switch.param
            [%e evar ~loc step_var]
            [%e estring ~loc clean]
          with
          | Error [%p ppat_var ~loc { txt = err_var; loc }] ->
            Error [%e evar ~loc err_var]
          | Ok [%p ppat_var ~loc { txt = var; loc }] ->
            [%e acc]])
    params param_vars inner

let expand_switch_bind ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match payload.pexp_desc with
  | Pexp_tuple [step_expr; arms_expr] ->
    let arms = extract_bind_arms ~loc arms_expr in
    let step_var = gen_symbol ~prefix:"__switch_step" () in
    let match_arms =
      List.map (fun (action, params, fn_expr) ->
        case
          ~lhs:(ppat_construct ~loc
            { txt = Lident "Some"; loc }
            (Some (ppat_constant ~loc (Pconst_string (action, loc, None)))))
          ~guard:None
          ~rhs:(build_bind_body ~loc step_var fn_expr params)
      ) arms
    in
    let match_expr =
      pexp_match ~loc
        (action_name_expr ~loc step_var)
        (match_arms @ default_arms ~loc step_var)
    in
    pexp_let ~loc Nonrecursive
      [ value_binding ~loc
          ~pat:(ppat_var ~loc { txt = step_var; loc })
          ~expr:step_expr ]
      match_expr
  | _ ->
    Location.raise_errorf ~loc
      "[%%switch_bind]: expected a pair (step_expr, arms_list)"

(* ── Registration ─────────────────────────────────────────────────────────── *)

let switch_ext =
  Extension.V3.declare "switch"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_switch

let switch_bind_ext =
  Extension.V3.declare "switch_bind"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_switch_bind

let () =
  Driver.register_transformation "quint_switch"
    ~extensions:[ switch_ext; switch_bind_ext ]
