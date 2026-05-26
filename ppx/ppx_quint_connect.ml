(*
  PPX rewriter for [let%quint_test].

  Input (at structure level):
    let%quint_test my_test = {
      spec   = spec_expr;
      test   = test_expr;
      driver = driver_expr;
    }

  Output:
    let my_test =
      Alcotest.test_case "my_test" `Quick (fun () ->
        match Ocaml_quint_connect.Quint_test.run_test
          spec_expr test_expr driver_expr
        with
        | Ok ()   -> ()
        | Error __msg -> Alcotest.fail __msg
      )
*)

open Ppxlib
open Ast_builder.Default

(* Extract a named field expression from a record field list. *)
let extract_field ~loc fields name =
  match
    List.find_opt (fun ({ Location.txt; _ }, _) ->
      match txt with Longident.Lident n -> n = name | _ -> false
    ) fields
  with
  | Some (_, expr) -> expr
  | None ->
    Location.raise_errorf ~loc
      "[let%%quint_test]: missing required field '%s' in record" name

let expand_quint_test ~ctxt pat expr : structure_item =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  (* Extract the binding name from the pattern *)
  let name = match pat.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | _ ->
      Location.raise_errorf ~loc:pat.ppat_loc
        "[let%%quint_test]: only simple variable patterns are supported \
         (e.g. let%%quint_test my_test = ...)"
  in
  (* Extract record fields: spec, test, driver *)
  let spec_expr, test_expr, driver_expr = match expr.pexp_desc with
    | Pexp_record (fields, None) ->
      ( extract_field ~loc fields "spec"
      , extract_field ~loc fields "test"
      , extract_field ~loc fields "driver" )
    | _ ->
      Location.raise_errorf ~loc
        "[let%%quint_test]: expected a record \
         { spec = ...; test = ...; driver = ... }"
  in
  (* Build the test body *)
  let body =
    [%expr
      match
        Ocaml_quint_connect.Quint_test.run_test
          [%e spec_expr]
          [%e test_expr]
          [%e driver_expr]
      with
      | Ok () -> ()
      | Error __msg -> Alcotest.fail __msg]
  in
  (* Wrap in Alcotest.test_case *)
  let test_case_expr =
    [%expr
      Alcotest.test_case
        [%e estring ~loc name]
        `Quick
        (fun () -> [%e body])]
  in
  pstr_value ~loc Nonrecursive
    [ value_binding ~loc
        ~pat:(ppat_var ~loc { txt = name; loc })
        ~expr:test_case_expr ]

(* ── quint_run ──────────────────────────────────────────────────────────────
   Input:
     let%quint_run my_sim = { spec = spec_expr; driver = driver_expr }
     let%quint_run my_sim = { spec = spec_expr; driver = driver_expr; traces = N }

   Output (without traces):
     let my_sim = Alcotest.test_case "my_sim" `Quick (fun () ->
       match Ocaml_quint_connect.Quint_test.run_simulation spec_expr driver_expr with
       | Ok ()   -> ()
       | Error __msg -> Alcotest.fail __msg)

   Output (with traces = N):
     let my_sim = Alcotest.test_case "my_sim" `Quick (fun () ->
       match Ocaml_quint_connect.Quint_test.run_simulation ~traces:N spec_expr driver_expr with
       | Ok ()   -> ()
       | Error __msg -> Alcotest.fail __msg)
*)

let extract_field_opt fields name =
  List.find_opt (fun ({ Location.txt; _ }, _) ->
    match txt with Longident.Lident n -> n = name | _ -> false
  ) fields
  |> Option.map snd

let expand_quint_run ~ctxt pat expr : structure_item =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let name = match pat.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | _ ->
      Location.raise_errorf ~loc:pat.ppat_loc
        "[let%%quint_run]: only simple variable patterns are supported"
  in
  let spec_expr, driver_expr, traces_expr_opt = match expr.pexp_desc with
    | Pexp_record (fields, None) ->
      let spec = extract_field ~loc fields "spec" in
      let driver = extract_field ~loc fields "driver" in
      let traces = extract_field_opt fields "traces" in
      (spec, driver, traces)
    | _ ->
      Location.raise_errorf ~loc
        "[let%%quint_run]: expected a record { spec = ...; driver = ...; traces = ... (opt) }"
  in
  let run_call = match traces_expr_opt with
    | None ->
      [%expr
        Ocaml_quint_connect.Quint_test.run_simulation
          [%e spec_expr]
          [%e driver_expr]]
    | Some traces_expr ->
      [%expr
        Ocaml_quint_connect.Quint_test.run_simulation
          ~traces:[%e traces_expr]
          [%e spec_expr]
          [%e driver_expr]]
  in
  let body =
    [%expr
      match [%e run_call] with
      | Ok () -> ()
      | Error __msg -> Alcotest.fail __msg]
  in
  let test_case_expr =
    [%expr
      Alcotest.test_case
        [%e estring ~loc name]
        `Quick
        (fun () -> [%e body])]
  in
  pstr_value ~loc Nonrecursive
    [ value_binding ~loc
        ~pat:(ppat_var ~loc { txt = name; loc })
        ~expr:test_case_expr ]

(* ── Registration ──────────────────────────────────────────────────────────── *)

let quint_test_ext =
  Extension.V3.declare "quint_test"
    Extension.Context.structure_item
    Ast_pattern.(
      pstr
        (pstr_value nonrecursive
           (value_binding ~pat:__ ~expr:__ ^:: nil)
         ^:: nil))
    expand_quint_test

let quint_run_ext =
  Extension.V3.declare "quint_run"
    Extension.Context.structure_item
    Ast_pattern.(
      pstr
        (pstr_value nonrecursive
           (value_binding ~pat:__ ~expr:__ ^:: nil)
         ^:: nil))
    expand_quint_run

let () =
  Driver.register_transformation "quint_connect"
    ~extensions:[ quint_test_ext; quint_run_ext ]
