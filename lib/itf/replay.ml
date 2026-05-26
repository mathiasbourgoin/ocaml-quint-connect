let rec value_to_yojson : Itf.Value.t -> Yojson.Basic.t = function
  | Itf.Value.Bool b    -> `Bool b
  | Itf.Value.Int n     -> `Int n
  | Itf.Value.Float f   -> `Float f
  | Itf.Value.Str s     -> `String s
  | Itf.Value.Null      -> `Null
  | Itf.Value.BigInt s  -> `Assoc [("#bigint", `String s)]
  | Itf.Value.Set lst   -> `Assoc [("#set",  `List (List.map value_to_yojson lst))]
  | Itf.Value.List lst  -> `List (List.map value_to_yojson lst)
  | Itf.Value.Tuple lst -> `Assoc [("#tup",  `List (List.map value_to_yojson lst))]
  | Itf.Value.Record fields ->
    `Assoc (List.map (fun (k, v) -> (k, value_to_yojson v)) fields)
  | Itf.Value.Map pairs ->
    `Assoc [("#map", `List (List.map (fun (k, v) ->
      `List [value_to_yojson k; value_to_yojson v]) pairs))]

let bindings_to_yojson bindings =
  `Assoc (List.map (fun (k, v) -> (k, value_to_yojson v)) bindings)

let is_verbose () =
  match Sys.getenv_opt "QUINT_VERBOSE" with
  | Some "1" -> true
  | _        -> false

let print_step_verbose (step : Itf.Step.t) =
  (match step.action_name with
   | Some name -> Printf.eprintf "[replay] action: %s\n%!" name
   | None      -> Printf.eprintf "[replay] action: (none)\n%!");
  List.iter (fun (k, v) ->
    Printf.eprintf "[replay]   nondet %s = %s\n%!" k
      (Yojson.Basic.to_string (value_to_yojson v))
  ) step.nondet_picks

module Make (D : Itf.DRIVER) (S : Itf.STATE with type driver = D.t) = struct
  let run (trace : Itf.Trace.t) : (unit, int * string) result =
    let driver  = D.create () in
    let verbose = is_verbose () in
    let rec loop i = function
      | [] -> Ok ()
      | step :: rest ->
        if verbose then print_step_verbose step;
        (match D.step driver step with
         | Error e -> Error (i, "driver.step failed: " ^ e)
         | Ok () ->
           let impl_state    = S.of_driver driver in
           let expected_json = bindings_to_yojson step.Itf.Step.bindings in
           (match S.of_yojson expected_json with
            | Error e -> Error (i, "of_yojson failed: " ^ e)
            | Ok expected_state ->
              if S.equal impl_state expected_state then
                loop (i + 1) rest
              else
                let diff = Printf.sprintf
                  "expected: %s\ngot:      %s"
                  (Yojson.Basic.pretty_to_string expected_json)
                  (Yojson.Basic.pretty_to_string (S.to_yojson impl_state))
                in
                Error (i, diff)))
    in
    loop 0 trace
end
