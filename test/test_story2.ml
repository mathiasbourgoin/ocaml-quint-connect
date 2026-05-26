open Ocaml_quint_connect

(* ===== AC1: DRIVER module type =====
   Must provide: type t, create : unit -> t, step : t -> Step.t -> (unit, string) result *)

let test_driver_create () =
  let module D : Itf.DRIVER = struct
    type t = { counter : int }
    let create () = { counter = 0 }
    let step _ _s = Ok ()
  end in
  let _d = D.create () in
  ()

let test_driver_step_ok () =
  let module D : Itf.DRIVER = struct
    type t = { counter : int }
    let create () = { counter = 0 }
    let step _ _s = Ok ()
  end in
  let d = D.create () in
  let s = Itf.Step.{ action_name = Some "Init"; bindings = []; nondet_picks = [] } in
  match D.step d s with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("expected Ok, got Error: " ^ e)

let test_driver_step_error () =
  let module D : Itf.DRIVER = struct
    type t = unit
    let create () = ()
    let step () s =
      match s.Itf.Step.action_name with
      | Some "bad" -> Error "bad action"
      | _ -> Ok ()
  end in
  let d = D.create () in
  let s = Itf.Step.{ action_name = Some "bad"; bindings = []; nondet_picks = [] } in
  match D.step d s with
  | Error "bad action" -> ()
  | Ok () -> Alcotest.fail "expected Error"
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

(* ===== AC2: STATE module type =====
   Must provide: type t, of_yojson, equal, of_driver *)

module ConcreteState = struct
  type t = int
  type driver = string
  let of_yojson = function
    | `Int n -> Ok n
    | _ -> Error "expected int"
  let to_yojson n = `Int n
  let equal (a : int) (b : int) = a = b
  let of_driver (s : string) = String.length s
end

let _ : (module Itf.STATE) = (module ConcreteState)

let test_state_of_yojson () =
  Alcotest.(check bool) "of_yojson ok"  true  (Result.is_ok  (ConcreteState.of_yojson (`Int 42)));
  Alcotest.(check bool) "of_yojson err" false (Result.is_ok  (ConcreteState.of_yojson (`String "x")))

let test_state_equal () =
  Alcotest.(check bool) "equal"     true  (ConcreteState.equal 42 42);
  Alcotest.(check bool) "not equal" false (ConcreteState.equal 42 0)

let test_state_of_driver () =
  Alcotest.(check int) "of_driver" 3 (ConcreteState.of_driver "foo")

(* ===== AC3: switch! dispatch macro =====
   Pattern-matches on step.action and binds parameters by name with optional params as option.

   [%switch] returns ('a, string) result:
     - matched arm  → Ok (arm_fn step)
     - None action  → Error "switch: step has no action"
     - no arm match → Error "switch: unmatched action: <name>"

   [%switch_bind] auto-binds named parameters:
     ("Action", ["p1"; "p2?"], fun p1 p2_opt -> ...)
     required params bound via Switch.param (Error propagated),
     optional params bound via Switch.param_opt as 'a option.
*)

(* Helper: unwrap Ok or fail the test *)
let ok_or_fail = function
  | Ok v -> v
  | Error e -> Alcotest.fail ("unexpected Error: " ^ e)

let test_switch_dispatch_correct_arm () =
  let step = Itf.Step.{
    action_name = Some "Init"; bindings = []; nondet_picks = []
  } in
  let result = ok_or_fail [%switch (step, [
    ("Init",  fun _s -> "got_init");
    ("Other", fun _s -> "got_other");
  ])] in
  Alcotest.(check string) "correct arm" "got_init" result

let test_switch_dispatch_second_arm () =
  let step = Itf.Step.{
    action_name = Some "Move"; bindings = []; nondet_picks = []
  } in
  let result = ok_or_fail [%switch (step, [
    ("Init", fun _s -> "got_init");
    ("Move", fun _s -> "got_move");
  ])] in
  Alcotest.(check string) "second arm" "got_move" result

let test_switch_binds_required_param () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 42)];
    nondet_picks = [];
  } in
  let result = ok_or_fail [%switch (step, [
    ("MyAction", fun s ->
      match Itf.Switch.param s "p1" with
      | Ok (Itf.Value.Int n) -> n
      | Ok _ -> -1
      | Error _ -> -2);
  ])] in
  Alcotest.(check int) "required param bound" 42 result

let test_switch_optional_param_present () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 1); ("p2", Itf.Value.Str "hi")];
    nondet_picks = [];
  } in
  let result = ok_or_fail [%switch (step, [
    ("MyAction", fun s ->
      let p2 = Itf.Switch.param_opt s "p2" in
      Option.is_some p2);
  ])] in
  Alcotest.(check bool) "optional param present" true result

let test_switch_optional_param_absent () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 1)];
    nondet_picks = [];
  } in
  let result = ok_or_fail [%switch (step, [
    ("MyAction", fun s ->
      let p2 = Itf.Switch.param_opt s "p2" in
      Option.is_none p2);
  ])] in
  Alcotest.(check bool) "optional param absent is None" true result

(* --- No exceptions escape --- *)

(* Switch.param must return (Value.t, string) result, never raise *)
let test_param_missing_returns_error () =
  let step = Itf.Step.{ action_name = None; bindings = []; nondet_picks = [] } in
  match Itf.Switch.param step "missing" with
  | Ok _  -> Alcotest.fail "expected Error for missing param"
  | Error _ -> ()

(* [%switch] with action_name = None must return Error, not raise *)
let test_switch_none_action_returns_error () =
  let step = Itf.Step.{ action_name = None; bindings = []; nondet_picks = [] } in
  let result : (string, string) result = [%switch (step, [
    ("Init", fun _s -> "got_init");
  ])] in
  match result with
  | Error _ -> ()
  | Ok _    -> Alcotest.fail "expected Error for None action_name"

(* [%switch] with unmatched action must return Error, not raise *)
let test_switch_unmatched_returns_error () =
  let step = Itf.Step.{ action_name = Some "Unknown"; bindings = []; nondet_picks = [] } in
  let result : (string, string) result = [%switch (step, [
    ("Init", fun _s -> "got_init");
  ])] in
  match result with
  | Error _ -> ()
  | Ok _    -> Alcotest.fail "expected Error for unmatched action"

(* --- [%switch_bind]: auto-binds params by name, optional as option --- *)

(* p1 required (Value.t), p2? optional (Value.t option) — p2 absent *)
let test_switch_bind_required () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 42)];
    nondet_picks = [];
  } in
  let result = [%switch_bind (step, [
    ("MyAction", ["p1"; "p2?"], fun p1 p2_opt ->
      match p1 with
      | Itf.Value.Int n -> n + (if Option.is_some p2_opt then 1 else 0)
      | _ -> -1)
  ])] in
  match result with
  | Ok 42 -> ()
  | Ok n  -> Alcotest.fail (Printf.sprintf "expected 42, got %d" n)
  | Error e -> Alcotest.fail e

(* p2? optional — p2 is present in bindings *)
let test_switch_bind_optional_present () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 1); ("p2", Itf.Value.Str "hi")];
    nondet_picks = [];
  } in
  let result = [%switch_bind (step, [
    ("MyAction", ["p1"; "p2?"], fun _p1 p2_opt -> Option.is_some p2_opt)
  ])] in
  match result with
  | Ok true  -> ()
  | Ok false -> Alcotest.fail "expected Some for p2"
  | Error e  -> Alcotest.fail e

(* p2? optional — p2 absent → None, no error *)
let test_switch_bind_optional_absent () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [("p1", Itf.Value.Int 1)];
    nondet_picks = [];
  } in
  let result = [%switch_bind (step, [
    ("MyAction", ["p1"; "p2?"], fun _p1 p2_opt -> Option.is_none p2_opt)
  ])] in
  match result with
  | Ok true  -> ()
  | Ok false -> Alcotest.fail "expected None for absent p2"
  | Error e  -> Alcotest.fail e

(* required param missing → Error propagated, no raise *)
let test_switch_bind_required_missing () =
  let step = Itf.Step.{
    action_name = Some "MyAction";
    bindings    = [];
    nondet_picks = [];
  } in
  let result : (int, string) result = [%switch_bind (step, [
    ("MyAction", ["p1"; "p2?"], fun _p1 _p2_opt -> 42)
  ])] in
  match result with
  | Error _ -> ()
  | Ok _    -> Alcotest.fail "expected Error for missing required param"

(* ===== Test runner ===== *)

let () =
  Alcotest.run "Story #2 – Driver and State module types"
    [ "driver", [
        Alcotest.test_case "create"      `Quick test_driver_create;
        Alcotest.test_case "step ok"     `Quick test_driver_step_ok;
        Alcotest.test_case "step error"  `Quick test_driver_step_error;
      ]
    ; "state", [
        Alcotest.test_case "of_yojson"   `Quick test_state_of_yojson;
        Alcotest.test_case "equal"       `Quick test_state_equal;
        Alcotest.test_case "of_driver"   `Quick test_state_of_driver;
      ]
    ; "switch", [
        Alcotest.test_case "dispatch init arm"      `Quick test_switch_dispatch_correct_arm;
        Alcotest.test_case "dispatch second arm"    `Quick test_switch_dispatch_second_arm;
        Alcotest.test_case "required param bound"   `Quick test_switch_binds_required_param;
        Alcotest.test_case "optional param Some"    `Quick test_switch_optional_param_present;
        Alcotest.test_case "optional param None"    `Quick test_switch_optional_param_absent;
        Alcotest.test_case "param missing -> Error" `Quick test_param_missing_returns_error;
        Alcotest.test_case "None action -> Error"   `Quick test_switch_none_action_returns_error;
        Alcotest.test_case "unmatched -> Error"     `Quick test_switch_unmatched_returns_error;
      ]
    ; "switch_bind", [
        Alcotest.test_case "required param auto-bound"   `Quick test_switch_bind_required;
        Alcotest.test_case "optional present as Some"    `Quick test_switch_bind_optional_present;
        Alcotest.test_case "optional absent as None"     `Quick test_switch_bind_optional_absent;
        Alcotest.test_case "required missing -> Error"   `Quick test_switch_bind_required_missing;
      ]
    ]
