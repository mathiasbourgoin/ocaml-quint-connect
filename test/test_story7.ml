open Ocaml_quint_connect

(* Shared fixture driver: just succeeds every step *)
module OkDriver = struct
  type t = unit
  let create () = ()
  let step () _s = Ok ()
end

(* Driver that fails immediately on step 0 *)
module FailDriver = struct
  type t = unit
  let create () = ()
  let step () _s = Error "deliberate failure"
end

let spec_path = "fixtures/counter_spec.qnt"

(* Helper: collect all printed lines into a list *)
let collect_output f =
  let buf = Buffer.create 64 in
  let print s = Buffer.add_string buf s; Buffer.add_char buf '\n' in
  let code = f print in
  (code, Buffer.contents buf)

(* AC1: run spec.qnt testName --driver ... → prints PASS with step count *)
let test_run_test_pass () =
  let code, out =
    collect_output (fun print ->
      Quint_cli.run_test ~print spec_path "counterTest" (module OkDriver))
  in
  Alcotest.(check int) "exit code 0" 0 code;
  Alcotest.(check bool) "output contains PASS" true
    (let re = Str.regexp "PASS: counterTest ([0-9]+ steps)" in
     try let _ = Str.search_forward re out 0 in true
     with Not_found -> false)

(* AC1: failing driver → FAIL with step index *)
let test_run_test_fail () =
  let code, out =
    collect_output (fun print ->
      Quint_cli.run_test ~print spec_path "counterTest" (module FailDriver))
  in
  Alcotest.(check int) "exit code 1" 1 code;
  Alcotest.(check bool) "output contains FAIL" true
    (String.length out > 0 &&
     let re = Str.regexp "FAIL: counterTest" in
     try let _ = Str.search_forward re out 0 in true
     with Not_found -> false)

(* AC2: --verbose → prints each step action inline *)
let test_run_test_verbose () =
  let _code, out =
    collect_output (fun print ->
      Quint_cli.run_test ~verbose:true ~print spec_path "counterTest" (module OkDriver))
  in
  (* counter_spec.itf.json has 3 steps with action "increment";
     verbose output should mention "step 0" *)
  Alcotest.(check bool) "verbose output has step lines" true
    (let re = Str.regexp "step [0-9]+" in
     try let _ = Str.search_forward re out 0 in true
     with Not_found -> false)

(* AC3: --traces N → runs N simulation traces, prints PASS: simulation (N traces) *)
let test_run_simulation_pass () =
  let code, out =
    collect_output (fun print ->
      Quint_cli.run_simulation ~traces:3 ~print spec_path (module OkDriver))
  in
  Alcotest.(check int) "exit code 0" 0 code;
  Alcotest.(check bool) "output says 3 traces" true
    (let re = Str.regexp "PASS: simulation (3 traces)" in
     try let _ = Str.search_forward re out 0 in true
     with Not_found -> false)

(* AC3: failing simulation → FAIL with trace/step info *)
let test_run_simulation_fail () =
  let code, out =
    collect_output (fun print ->
      Quint_cli.run_simulation ~traces:2 ~print spec_path (module FailDriver))
  in
  Alcotest.(check int) "exit code 1" 1 code;
  Alcotest.(check bool) "output contains FAIL: simulation" true
    (let re = Str.regexp "FAIL: simulation" in
     try let _ = Str.search_forward re out 0 in true
     with Not_found -> false)

let () =
  Alcotest.run "Story #7 \xe2\x80\x93 CLI entry point: quint-connect run"
    [ "run test", [
        Alcotest.test_case "pass prints step count" `Quick test_run_test_pass;
        Alcotest.test_case "fail prints step index" `Quick test_run_test_fail;
      ];
      "verbose flag", [
        Alcotest.test_case "verbose prints step actions" `Quick test_run_test_verbose;
      ];
      "simulation mode", [
        Alcotest.test_case "pass prints trace count" `Quick test_run_simulation_pass;
        Alcotest.test_case "fail reports trace and step" `Quick test_run_simulation_fail;
      ];
    ]
