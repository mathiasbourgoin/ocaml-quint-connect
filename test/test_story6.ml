open Ocaml_quint_connect

module CounterDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s = d.counter <- d.counter + 1; Ok ()
end

let spec_path = "fixtures/counter_spec.qnt"

(* AC1: let%quint_run generates an Alcotest test_case that runs multiple simulation traces *)
let%quint_run counter_simulation = {
  spec = spec_path;
  driver = (module CounterDriver)
}

(* AC2: ~traces:10 — exactly 10 simulation traces are generated and replayed *)
let test_traces_count () =
  let call_count = ref 0 in
  let module CountingDriver = struct
    type t = unit
    let create () = incr call_count; ()
    let step () _s = Ok ()
  end in
  (match Quint_test.run_simulation ~traces:10 spec_path (module CountingDriver) with
   | Error msg -> Alcotest.fail ("expected Ok but got Error: " ^ msg)
   | Ok () -> ());
  Alcotest.(check int) "exactly 10 traces" 10 !call_count

(* AC3: any trace fails — trace index and step are reported *)
let test_failing_trace_reported () =
  (* Driver that always fails on its 2nd step *)
  let module FailingDriver = struct
    type t = { mutable steps : int }
    let create () = { steps = 0 }
    let step d _s =
      d.steps <- d.steps + 1;
      if d.steps >= 2 then Error "deliberate failure"
      else Ok ()
  end in
  match Quint_test.run_simulation ~traces:3 spec_path (module FailingDriver) with
  | Ok () -> Alcotest.fail "expected Error but got Ok"
  | Error msg ->
    Alcotest.(check bool) "error contains 'trace'" true
      (let re = Str.regexp "trace [0-9]+" in
       try let _ = Str.search_forward re msg 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "error contains 'step'" true
      (let re = Str.regexp "step [0-9]+" in
       try let _ = Str.search_forward re msg 0 in true
       with Not_found -> false)

let () =
  Alcotest.run "Story #6 \xe2\x80\x93 ppx_quint_connect: quint_run attribute"
    [ "quint_run", [
        counter_simulation;
        Alcotest.test_case "traces count" `Quick test_traces_count;
        Alcotest.test_case "failing trace reported" `Quick test_failing_trace_reported;
      ]
    ]
