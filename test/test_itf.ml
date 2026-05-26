open Ocaml_quint_connect

(* --- sample ITF JSON strings --- *)

let valid_itf_json = {|{
  "#meta": { "format": "ITF", "formatVersion": "0.1" },
  "vars": ["x", "y"],
  "states": [
    {
      "#meta": { "index": 0, "actionTaken": "Init", "nondetPicks": {} },
      "x": { "#bigint": "0" },
      "y": false
    },
    {
      "#meta": { "index": 1, "actionTaken": "Step", "nondetPicks": { "choice": { "#bigint": "1" } } },
      "x": { "#bigint": "1" },
      "y": true
    }
  ]
}|}

let missing_meta_json = {|{
  "vars": ["x"],
  "states": [{ "x": 0 }]
}|}

let malformed_json = {|{ not valid json |}

let nondet_json = {|{
  "#meta": { "format": "ITF" },
  "vars": ["counter"],
  "states": [
    {
      "#meta": {
        "index": 0,
        "actionTaken": "Pick",
        "nondetPicks": {
          "n":   { "#bigint": "42" },
          "flag": true
        }
      },
      "counter": { "#bigint": "42" }
    }
  ]
}|}

let malformed_map_json = {|{
  "#meta": { "format": "ITF" },
  "vars": ["m"],
  "states": [
    {
      "#meta": { "index": 0 },
      "m": { "#map": [ [1, 2, 3] ] }
    }
  ]
}|}

let null_json = {|{
  "#meta": { "format": "ITF" },
  "vars": ["v"],
  "states": [
    {
      "#meta": { "index": 0 },
      "v": null
    }
  ]
}|}

(* --- helpers --- *)

let get_ok = function
  | Ok v    -> v
  | Error e -> Alcotest.failf "Expected Ok, got Error: %s" e

let get_error = function
  | Error e -> e
  | Ok _    -> Alcotest.fail "Expected Error, got Ok"

(* --- tests --- *)

let test_valid_parse () =
  let traces = get_ok (Itf.parse_string valid_itf_json) in
  Alcotest.(check int) "one trace" 1 (List.length traces);
  let trace = List.hd traces in
  Alcotest.(check int) "two steps" 2 (List.length trace);
  let step0 = List.nth trace 0 in
  Alcotest.(check (option string)) "action name Init"
    (Some "Init") step0.Itf.Step.action_name;
  (* check x binding exists *)
  let x_val = List.assoc "x" step0.Itf.Step.bindings in
  Alcotest.(check bool) "x is BigInt"
    true (match x_val with Itf.Value.BigInt "0" -> true | _ -> false)

let test_action_names () =
  let traces = get_ok (Itf.parse_string valid_itf_json) in
  let trace = List.hd traces in
  let step1 = List.nth trace 1 in
  Alcotest.(check (option string)) "action name Step"
    (Some "Step") step1.Itf.Step.action_name

let test_missing_meta () =
  let err = get_error (Itf.parse_string missing_meta_json) in
  (* error message must actually mention #meta — not vacuously true *)
  Alcotest.(check bool) "error mentions #meta"
    true (let low = String.lowercase_ascii err in
          String.length low > 0 &&
          (try let _ = Str.search_forward (Str.regexp_string "#meta") low 0 in true
           with Not_found -> false))

let test_malformed_json () =
  let err = get_error (Itf.parse_string malformed_json) in
  Alcotest.(check bool) "non-empty error" true (String.length err > 0)

let test_nondet_picks () =
  let traces = get_ok (Itf.parse_string nondet_json) in
  let trace = List.hd traces in
  let step = List.hd trace in
  Alcotest.(check int) "two nondet picks" 2 (List.length step.Itf.Step.nondet_picks);
  let n_val = List.assoc "n" step.Itf.Step.nondet_picks in
  Alcotest.(check bool) "n pick is BigInt 42"
    true (match n_val with Itf.Value.BigInt "42" -> true | _ -> false);
  let flag_val = List.assoc "flag" step.Itf.Step.nondet_picks in
  Alcotest.(check bool) "flag pick is Bool true"
    true (match flag_val with Itf.Value.Bool true -> true | _ -> false)

let test_bindings_completeness () =
  let traces = get_ok (Itf.parse_string valid_itf_json) in
  let trace = List.hd traces in
  let step0 = List.hd trace in
  (* both x and y must be present *)
  Alcotest.(check bool) "x in bindings" true (List.mem_assoc "x" step0.Itf.Step.bindings);
  Alcotest.(check bool) "y in bindings" true (List.mem_assoc "y" step0.Itf.Step.bindings)

(* AC2: malformed #map entry must return Error, not raise *)
let test_malformed_map_entry () =
  let result = Itf.parse_string malformed_map_json in
  Alcotest.(check bool) "malformed map entry returns Error (not raises)"
    true (match result with Error _ -> true | Ok _ -> false)

(* Minor fix: JSON null must map to Value.Null, not Value.Str "null" *)
let test_null_value () =
  let traces = get_ok (Itf.parse_string null_json) in
  let trace = List.hd traces in
  let step = List.hd trace in
  let v_val = List.assoc "v" step.Itf.Step.bindings in
  Alcotest.(check bool) "null maps to Value.Null (not Str \"null\")"
    true (match v_val with Itf.Value.Null -> true | _ -> false)

(* --- suite --- *)

let () =
  Alcotest.run "ITF trace parser" [
    "valid parse", [
      Alcotest.test_case "parses two steps"            `Quick test_valid_parse;
      Alcotest.test_case "action names per step"       `Quick test_action_names;
      Alcotest.test_case "all bindings present"        `Quick test_bindings_completeness;
    ];
    "error handling", [
      Alcotest.test_case "missing #meta → Error"       `Quick test_missing_meta;
      Alcotest.test_case "malformed JSON → Error"      `Quick test_malformed_json;
      Alcotest.test_case "malformed #map → Error"      `Quick test_malformed_map_entry;
    ];
    "nondet picks", [
      Alcotest.test_case "nondet_picks preserved"      `Quick test_nondet_picks;
    ];
    "value types", [
      Alcotest.test_case "null → Value.Null"           `Quick test_null_value;
    ];
  ]
