open Ocaml_quint_connect

(* ===== Test fixtures ===== *)

(* Minimal driver: mutable counter incremented on each step *)
module CounterDriver = struct
  type t = { mutable count : int }
  let create () = { count = 0 }
  let step d _s =
    d.count <- d.count + 1;
    Ok ()
end

(* Driver that always fails on step *)
module FailingDriver = struct
  type t = unit
  let create () = ()
  let step () _s = Error "simulated driver failure"
end

(* State = the current counter value; expected ITF form: {"count": n} *)
module CounterState = struct
  type t = int
  type driver = CounterDriver.t
  let of_yojson = function
    | `Assoc [("count", `Int n)] -> Ok n
    | _ -> Error "expected {count: int}"
  let to_yojson n = `Assoc [("count", `Int n)]
  let equal = Int.equal
  let of_driver d = d.CounterDriver.count
end

(* State whose of_yojson always fails *)
module BadYojsonState = struct
  type t = unit
  type driver = CounterDriver.t
  let of_yojson _ = Error "cannot parse this state"
  let to_yojson () = `Null
  let equal () () = true
  let of_driver _d = ()
end

(* State for the failing driver (accepts any JSON, ignores it) *)
module FailingDriverState = struct
  type t = unit
  type driver = FailingDriver.t
  let of_yojson _ = Ok ()
  let to_yojson () = `Null
  let equal () () = true
  let of_driver _ = ()
end

let make_step action_name count =
  Itf.Step.{
    action_name  = Some action_name;
    bindings     = [("count", Itf.Value.Int count)];
    nondet_picks = [];
  }

module R  = Replay.Make(CounterDriver)(CounterState)
module RF = Replay.Make(FailingDriver)(FailingDriverState)
module RB = Replay.Make(CounterDriver)(BadYojsonState)

(* Redirect stderr to a temp file, run [f], restore stderr, return (result, captured_string) *)
let capture_stderr f =
  let tmpfile = Filename.temp_file "replay_test" ".stderr" in
  let fd = Unix.openfile tmpfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
  let old_stderr = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let result =
    Fun.protect f ~finally:(fun () ->
      Unix.dup2 old_stderr Unix.stderr;
      Unix.close old_stderr)
  in
  let ic = open_in tmpfile in
  let content = In_channel.input_all ic in
  close_in ic;
  Sys.remove tmpfile;
  (result, content)

(* ===== AC1 + AC3: all steps match → Ok () ===== *)

let test_replay_ok () =
  (* After step 0: count = 1; after step 1: count = 2 *)
  let trace = [make_step "Inc" 1; make_step "Inc" 2] in
  match R.run trace with
  | Ok () -> ()
  | Error (i, diff) ->
    Alcotest.fail (Printf.sprintf "unexpected error at step %d: %s" i diff)

let test_replay_empty () =
  match R.run [] with
  | Ok () -> ()
  | Error (i, d) ->
    Alcotest.fail (Printf.sprintf "unexpected error at step %d: %s" i d)

(* ===== AC2: diverging state → Error (step_index, diff_string) ===== *)

let test_replay_diverge_first_step () =
  (* Driver increments to 1, but trace expects 99 *)
  let trace = [make_step "Inc" 99] in
  match R.run trace with
  | Error (0, diff) ->
    Alcotest.(check bool) "diff is non-empty" true (String.length diff > 0)
  | Error (i, _) ->
    Alcotest.fail (Printf.sprintf "wrong step index: got %d, expected 0" i)
  | Ok () ->
    Alcotest.fail "expected Error for diverging state"

let test_replay_diverge_second_step () =
  (* First step matches (count=1), second diverges (count=99 vs actual 2) *)
  let trace = [make_step "Inc" 1; make_step "Inc" 99] in
  match R.run trace with
  | Error (1, _) -> ()
  | Error (i, _) ->
    Alcotest.fail (Printf.sprintf "wrong step index: got %d, expected 1" i)
  | Ok () ->
    Alcotest.fail "expected Error at step 1"

let test_replay_error_contains_expected () =
  let trace = [make_step "Inc" 99] in
  match R.run trace with
  | Error (_, diff) ->
    Alcotest.(check bool) "diff mentions 'expected'" true
      (let re = Str.regexp "expected" in
       try ignore (Str.search_forward re diff 0); true
       with Not_found -> false)
  | Ok () ->
    Alcotest.fail "expected Error"

(* ===== Error path: D.step returns Error ===== *)

let test_replay_driver_step_error () =
  let trace = [
    Itf.Step.{
      action_name  = Some "Any";
      bindings     = [];
      nondet_picks = [];
    }
  ] in
  match RF.run trace with
  | Error (0, msg) ->
    Alcotest.(check bool) "error message mentions 'driver'" true
      (let re = Str.regexp "driver" in
       try ignore (Str.search_forward re msg 0); true
       with Not_found -> false)
  | Error (i, _) ->
    Alcotest.fail (Printf.sprintf "wrong step index: got %d, expected 0" i)
  | Ok () ->
    Alcotest.fail "expected Error from failing driver"

(* ===== Error path: S.of_yojson returns Error ===== *)

let test_replay_of_yojson_error () =
  (* BadYojsonState.of_yojson always fails *)
  let trace = [make_step "Inc" 1] in
  match RB.run trace with
  | Error (0, msg) ->
    Alcotest.(check bool) "error message mentions 'of_yojson'" true
      (let re = Str.regexp "of_yojson" in
       try ignore (Str.search_forward re msg 0); true
       with Not_found -> false)
  | Error (i, _) ->
    Alcotest.fail (Printf.sprintf "wrong step index: got %d, expected 0" i)
  | Ok () ->
    Alcotest.fail "expected Error from failing of_yojson"

(* ===== AC4: QUINT_VERBOSE=1 — action and nondet picks printed to stderr ===== *)

let test_replay_verbose () =
  Unix.putenv "QUINT_VERBOSE" "1";
  let trace = [
    Itf.Step.{
      action_name  = Some "Init";
      bindings     = [("count", Itf.Value.Int 1)];
      nondet_picks = [("x", Itf.Value.Int 42)];
    }
  ] in
  let (result, captured) = capture_stderr (fun () -> R.run trace) in
  Unix.putenv "QUINT_VERBOSE" "";
  (match result with
   | Ok () -> ()
   | Error (i, d) ->
     Alcotest.fail (Printf.sprintf "verbose mode changed result at step %d: %s" i d));
  Alcotest.(check bool) "action name printed to stderr" true
    (let re = Str.regexp "Init" in
     try ignore (Str.search_forward re captured 0); true
     with Not_found -> false);
  Alcotest.(check bool) "nondet pick printed to stderr" true
    (let re = Str.regexp "nondet" in
     try ignore (Str.search_forward re captured 0); true
     with Not_found -> false)

(* ===== Test runner ===== *)

let () =
  Alcotest.run "Story #3 \xe2\x80\x93 Trace replay engine"
    [ "replay", [
        Alcotest.test_case "ok on matching trace"      `Quick test_replay_ok;
        Alcotest.test_case "ok on empty trace"         `Quick test_replay_empty;
        Alcotest.test_case "error at step 0"           `Quick test_replay_diverge_first_step;
        Alcotest.test_case "error at step 1"           `Quick test_replay_diverge_second_step;
        Alcotest.test_case "diff mentions expected"    `Quick test_replay_error_contains_expected;
        Alcotest.test_case "driver.step error"         `Quick test_replay_driver_step_error;
        Alcotest.test_case "of_yojson error"           `Quick test_replay_of_yojson_error;
        Alcotest.test_case "verbose mode"              `Quick test_replay_verbose;
      ]
    ]
