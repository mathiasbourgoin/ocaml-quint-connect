open Ocaml_quint_connect

(* ===== OCaml counter implementation ===== *)

module CounterDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s =
    d.counter <- d.counter + 1;
    Ok ()
end

(* Off-by-two buggy driver *)
module BuggyDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s =
    d.counter <- d.counter + 2;
    Ok ()
end

(* State module for the correct counter *)
module CounterState = struct
  type t = int
  type driver = CounterDriver.t
  let of_yojson = function
    | `Assoc [("counter", `Assoc [("#bigint", `String s)])] ->
      (try Ok (int_of_string s)
       with _ -> Error ("cannot parse bigint: " ^ s))
    | `Assoc [("counter", `Int n)] -> Ok n
    | j -> Error ("unexpected counter state: " ^ Yojson.Basic.to_string j)
  let to_yojson n =
    `Assoc [("counter", `Assoc [("#bigint", `String (string_of_int n))])]
  let equal = Int.equal
  let of_driver d = d.CounterDriver.counter
end

(* State module for the buggy counter *)
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

(* AC1: successfully replays counter trace against correct implementation *)
let test_integration_ok () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    (match R.run trace with
     | Ok () -> ()
     | Error (i, diff) ->
       Alcotest.fail (Printf.sprintf "replay failed at step %d: %s" i diff))

(* AC2: off-by-two bug returns Error with a diff showing expected vs actual *)
let test_integration_buggy () =
  match Itf.parse_file fixture_path with
  | Error e -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok [] -> Alcotest.fail "no traces in fixture"
  | Ok (trace :: _) ->
    (match RB.run trace with
     | Ok () ->
       Alcotest.fail "expected Error from buggy driver but got Ok"
     | Error (_, diff) ->
       Alcotest.(check bool) "diff is non-empty" true
         (String.length diff > 0);
       Alcotest.(check bool) "diff mentions 'expected'" true
         (let re = Str.regexp "expected" in
          try ignore (Str.search_forward re diff 0); true
          with Not_found -> false))

(* AC3: integration test completes in < 500ms *)
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
  Alcotest.run "Story #4 \xe2\x80\x93 Integration test: replay Quint trace"
    [ "integration", [
        Alcotest.test_case "ok replay against correct counter"    `Quick test_integration_ok;
        Alcotest.test_case "buggy driver returns error with diff" `Quick test_integration_buggy;
        Alcotest.test_case "timing < 500ms"                      `Quick test_integration_timing;
      ]
    ]
