(* Quint 0.32 `run --mbt` traces store the action and the nondeterministic picks as
   `mbt::actionTaken` / `mbt::nondetPicks` state bindings, encode picks as Quint Option
   values, and qualify state variables by module path (kb/spec.md QC-2..QC-4). *)
open Ocaml_quint_connect

let trace () =
  match Itf.parse_file ~unqualify:true "fixtures/mbt032_trace.itf.json" with
  | Ok [ t ] -> t
  | Ok _ -> Alcotest.fail "expected exactly one trace"
  | Error e -> Alcotest.fail ("parse failed: " ^ e)

let actions () =
  let got = List.map (fun s -> s.Itf.Step.action_name) (trace ()) in
  Alcotest.(check (list (option string)))
    "action taken read from mbt::actionTaken"
    [ Some "init"; Some "reset"; Some "reset"; Some "add"; Some "add" ]
    got

let picks_unwrapped () =
  let steps = trace () in
  let add = List.nth steps 3 and reset = List.nth steps 1 in
  (match Itf.Switch.param add "n" with
   | Ok (Itf.Value.BigInt "1") -> ()
   | Ok _ -> Alcotest.fail "Some pick not unwrapped"
   | Error e -> Alcotest.fail e);
  Alcotest.(check bool) "None pick absent" true
    (Itf.Switch.param_opt reset "n" = None)

let no_meta_bindings () =
  List.iter
    (fun s ->
      List.iter
        (fun (k, _) ->
          if String.length k >= 5 && String.sub k 0 5 = "mbt::" then
            Alcotest.fail ("metadata key among bindings: " ^ k))
        s.Itf.Step.bindings)
    (trace ())

let unqualified () =
  let s = List.nth (trace ()) 4 in
  match List.assoc_opt "x" s.Itf.Step.bindings with
  | Some (Itf.Value.BigInt "2") -> ()
  | _ -> Alcotest.fail "qualified variable inst::counter::x not exposed as x"

(* Replay.Make_ext calls the driver's teardown once, on success, divergence, step error
   and exception. [mode] selects how the driver misbehaves. *)
let make_ext_run mode =
  let closed = ref 0 in
  let module D = struct
    type t = { mutable x : int }
    let create () = { x = 0 }
    let step d (s : Itf.Step.t) =
      match (mode, s.action_name) with
      | `Raise, Some "add" -> failwith "boom"
      | `Step_error, Some "add" -> Error "refused"
      | `Diverge, Some "add" -> Ok ()
      | _, Some "reset" -> d.x <- 0; Ok ()
      | _, Some "add" ->
        (match Itf.Switch.param s "n" with
         | Ok (Itf.Value.BigInt n) -> d.x <- d.x + int_of_string n
         | _ -> ());
        Ok ()
      | _ -> Ok ()
    let close _ = incr closed
  end in
  let module S = struct
    type t = int
    type driver = D.t
    let of_yojson = function
      | `Assoc l -> (
        match List.assoc_opt "x" l with
        | Some (`Assoc [ ("#bigint", `String n) ]) -> Ok (int_of_string n)
        | _ -> Error "no x")
      | _ -> Error "not an object"
    let to_yojson n = `Assoc [ ("x", `Assoc [ ("#bigint", `String (string_of_int n)) ]) ]
    let equal = Int.equal
    let of_driver d = d.D.x
  end in
  let module R = Replay.Make_ext (D) (S) in
  let r = try `Result (R.run (trace ())) with Failure m -> `Raised m in
  (r, !closed)

let teardown_called () =
  (match make_ext_run `Ok with
   | `Result (Ok ()), 1 -> ()
   | _ -> Alcotest.fail "success: expected Ok and one close");
  (match make_ext_run `Diverge with
   | `Result (Error (3, _)), 1 -> ()
   | _ -> Alcotest.fail "divergence: expected Error at step 3 and one close");
  (match make_ext_run `Step_error with
   | `Result (Error (3, _)), 1 -> ()
   | _ -> Alcotest.fail "step error: expected Error at step 3 and one close");
  match make_ext_run `Raise with
  | `Raised "boom", 1 -> ()
  | _ -> Alcotest.fail "exception: expected it to propagate and one close"

(* Without ~unqualify, qualified names are kept verbatim (backward compatibility). *)
let qualified_by_default () =
  match Itf.parse_file "fixtures/mbt032_trace.itf.json" with
  | Ok [ t ] ->
    Alcotest.(check bool) "qualified key kept" true
      (List.mem_assoc "inst::counter::x" (List.hd t).Itf.Step.bindings)
  | _ -> Alcotest.fail "parse failed"

(* Legacy #meta picks are not Option-unwrapped, even when they look like Options. *)
let legacy_option_pick () =
  let json =
    {|{"#meta":{},"vars":["x"],"states":[{"#meta":{"index":0,"actionTaken":"a","nondetPicks":{"o":{"tag":"Some","value":1},"p":{"tag":"None","value":{"#tup":[]}}}},"x":0}]}|}
  in
  match Itf.parse_string json with
  | Ok [ [ s ] ] ->
    Alcotest.(check bool) "o kept as record" true
      (match Itf.Switch.param_opt s "o" with Some (Itf.Value.Record _) -> true | _ -> false);
    Alcotest.(check bool) "p kept" true (Itf.Switch.param_opt s "p" <> None)
  | _ -> Alcotest.fail "unexpected shape"

(* A state variable is not shortened onto the name of a nondeterministic pick. *)
let pick_not_shadowed () =
  let json =
    {|{"#meta":{},"vars":["inst::n"],"states":[{"#meta":{"index":0},"mbt::actionTaken":"a","mbt::nondetPicks":{"n":{"tag":"Some","value":1}},"inst::n":7}]}|}
  in
  match Itf.parse_string ~unqualify:true json with
  | Ok [ [ s ] ] ->
    Alcotest.(check bool) "pick wins" true (Itf.Switch.param s "n" = Ok (Itf.Value.Int 1));
    Alcotest.(check bool) "state stays qualified" true (List.mem_assoc "inst::n" s.Itf.Step.bindings)
  | _ -> Alcotest.fail "unexpected shape"

(* #meta wins when well-typed; a mistyped #meta value falls back to the mbt:: binding. *)
let precedence () =
  let json =
    {|{"#meta":{},"vars":[],"states":[{"#meta":{"index":0,"actionTaken":"meta"},"mbt::actionTaken":"bind"},{"#meta":{"index":1,"actionTaken":5},"mbt::actionTaken":"bind"}]}|}
  in
  match Itf.parse_string json with
  | Ok [ [ a; b ] ] ->
    Alcotest.(check (option string)) "meta wins" (Some "meta") a.Itf.Step.action_name;
    Alcotest.(check (option string)) "mistyped meta falls back" (Some "bind") b.Itf.Step.action_name
  | _ -> Alcotest.fail "unexpected shape"

(* Shortening edge cases: last "::", empty suffix never shortened. *)
let unqualify_edges () =
  let json =
    {|{"#meta":{},"vars":[],"states":[{"#meta":{"index":0},"a::b:c":1,"z::":2}]}|}
  in
  match Itf.parse_string ~unqualify:true json with
  | Ok [ [ s ] ] ->
    Alcotest.(check (list string)) "keys" [ "b:c"; "z::" ] (List.map fst s.Itf.Step.bindings)
  | _ -> Alcotest.fail "unexpected shape"

let collision () =
  let json =
    {|{"#meta":{},"vars":["a::x","b::x"],"states":[{"#meta":{"index":0},"a::x":1,"b::x":2}]}|}
  in
  match Itf.parse_string ~unqualify:true json with
  | Ok [ [ s ] ] ->
    Alcotest.(check (list string)) "colliding names stay qualified" [ "a::x"; "b::x" ]
      (List.map fst s.Itf.Step.bindings)
  | Ok _ -> Alcotest.fail "unexpected shape"
  | Error e -> Alcotest.fail e

let legacy_pick () =
  let json =
    {|{"#meta":{},"vars":["x"],"states":[{"#meta":{"index":0,"actionTaken":"add","nondetPicks":{"n":3}},"x":0}]}|}
  in
  match Itf.parse_string json with
  | Ok [ [ s ] ] ->
    Alcotest.(check (option string)) "legacy action" (Some "add") s.Itf.Step.action_name;
    Alcotest.(check bool) "legacy pick kept" true
      (Itf.Switch.param_opt s "n" = Some (Itf.Value.Int 3))
  | Ok _ -> Alcotest.fail "unexpected shape"
  | Error e -> Alcotest.fail e

let () =
  Alcotest.run "mbt032"
    [ ( "quint 0.32 mbt traces",
        [ Alcotest.test_case "action names" `Quick actions;
          Alcotest.test_case "picks unwrapped" `Quick picks_unwrapped;
          Alcotest.test_case "no mbt:: bindings" `Quick no_meta_bindings;
          Alcotest.test_case "unqualified variables" `Quick unqualified;
          Alcotest.test_case "teardown" `Quick teardown_called;
          Alcotest.test_case "name collision" `Quick collision;
          Alcotest.test_case "legacy #meta picks" `Quick legacy_pick;
          Alcotest.test_case "qualified by default" `Quick qualified_by_default;
          Alcotest.test_case "legacy Option-typed pick" `Quick legacy_option_pick;
          Alcotest.test_case "pick not shadowed" `Quick pick_not_shadowed;
          Alcotest.test_case "meta/binding precedence" `Quick precedence;
          Alcotest.test_case "unqualify edge cases" `Quick unqualify_edges ] ) ]
