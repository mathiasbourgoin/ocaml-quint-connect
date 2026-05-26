open Ocaml_quint_connect

module CounterDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s = d.counter <- d.counter + 1; Ok ()
end

let spec_path = "fixtures/counter_spec.qnt"

(* AC1 + AC3: let%quint_test generates an Alcotest.test_case that passes *)
let%quint_test my_test = {
  spec = spec_path;
  test = "increment";
  driver = (module CounterDriver)
}

(* AC2: spec not found → Error "spec file not found: <path>" *)
let test_spec_not_found () =
  match Quint_test.run_test "/nonexistent/spec.qnt" "increment" (module CounterDriver) with
  | Error msg ->
    let prefix = "spec file not found: " in
    Alcotest.(check bool) "error has right prefix" true
      (String.length msg >= String.length prefix &&
       String.sub msg 0 (String.length prefix) = prefix)
  | Ok () ->
    Alcotest.fail "expected Error but got Ok"

let () =
  Alcotest.run "Demo #5 \xe2\x80\x93 ppx_quint_connect: quint_test attribute"
    [ "quint_test", [
        my_test;
        Alcotest.test_case "spec not found returns error" `Quick test_spec_not_found;
      ]
    ]
