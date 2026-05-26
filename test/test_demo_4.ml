open Ocaml_quint_connect

(* ===== OCaml counter implementation ===== *)

(* AC #11: correct counter driver — increments by 1 on each step *)
module CounterDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s =
    d.counter <- d.counter + 1;
    Ok ()
end

(* AC #12: off-by-two buggy driver *)
module BuggyDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s =
    d.counter <- d.counter + 2;
    Ok ()
end

module CounterState = struct
  type t = int
  type driver = CounterDriver.t
  let of_yojson = function
    | `Assoc [("counter", `Assoc [("#bigint", `String s)])] ->
      (try Ok (int_of_string s) with _ -> Error ("cannot parse bigint: " ^ s))
    | `Assoc [("counter", `Int n)] -> Ok n
    | j -> Error ("unexpected counter state: " ^ Yojson.Basic.to_string j)
  let to_yojson n = `Assoc [("counter", `Assoc [("#bigint", `String (string_of_int n))])]
  let equal = Int.equal
  let of_driver d = d.CounterDriver.counter
end

module BuggyState = struct
  type t = int
  type driver = BuggyDriver.t
  let of_yojson = CounterState.of_yojson
  let to_yojson = CounterState.to_yojson
  let equal = Int.equal
  let of_driver d = d.BuggyDriver.counter
end

module R  = Replay.Make(CounterDriver)(CounterState)
module RB = Replay.Make(BuggyDriver)(BuggyState)

let fixture_path = "fixtures/counter_trace.itf.json"

(* AC #11 *)
let test_integration_ok () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    (match R.run trace with
     | Ok () -> ()
     | Error (i, diff) ->
       Alcotest.fail (Printf.sprintf "replay failed at step %d: %s" i diff))

(* AC #12 — returns Error *)
let test_integration_buggy_returns_error () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    (match RB.run trace with
     | Ok () -> Alcotest.fail "expected Error from buggy driver but got Ok"
     | Error (_, _) -> ())

(* AC #12 — diff content *)
let test_integration_buggy_diff_content () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    (match RB.run trace with
     | Ok () -> Alcotest.fail "expected Error from buggy driver but got Ok"
     | Error (_, diff) ->
       Alcotest.(check bool) "diff mentions expected" true
         (let re = Str.regexp "expected" in
          try ignore (Str.search_forward re diff 0); true
          with Not_found -> false);
       Alcotest.(check bool) "diff is non-empty" true (String.length diff > 0))

(* AC #13 *)
let test_integration_timing () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    let t0 = Unix.gettimeofday () in
    ignore (R.run trace);
    let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    Alcotest.(check bool) "completes in < 500ms" true (elapsed_ms < 500.0)

let () =
  Alcotest.run "Story #4 — Integration test: replay Quint trace"
    [ "integration", [
        Alcotest.test_case "ok replay against correct counter"   `Quick test_integration_ok;
        Alcotest.test_case "buggy driver returns Error"          `Quick test_integration_buggy_returns_error;
        Alcotest.test_case "buggy diff shows expected vs actual" `Quick test_integration_buggy_diff_content;
        Alcotest.test_case "timing < 500ms"                      `Quick test_integration_timing;
      ]
    ]