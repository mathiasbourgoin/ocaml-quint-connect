# ocaml-quint-connect

OCaml bridge for [Quint](https://github.com/informalsystems/quint) formal
specifications. Parse ITF (Informal Trace Format) traces, replay them against
OCaml implementations, and write Alcotest cases that verify your code matches
a Quint model—without requiring Quint to be installed at test time.

---

## Overview

Quint is a specification language for state machines. Once you have a
specification and a generated ITF trace (via `quint run --out-itf`), this
library lets you:

- **Parse** `.itf.json` trace files into typed OCaml values
- **Replay** a trace step-by-step against your real implementation
- **Compare** the implementation's observable state to the Quint model's state
  at each step, producing a diff on divergence
- **Register a driver** loaded at runtime via `Dynlink` so `quint-connect run`
  can drive any implementation without recompiling the CLI

The trace files are pre-generated and committed alongside your tests, so the
test suite runs without any Quint toolchain present.

---

## Installation

```sh
opam install ocaml-quint-connect
```

Or pin from source:

```sh
opam pin add ocaml-quint-connect .
```

---

## Core concepts

### Driver (`Itf.DRIVER`)

A driver wraps your implementation and processes one Quint step at a time:

```ocaml
module type DRIVER = sig
  type t
  val create : unit -> t
  val step   : t -> Itf.Step.t -> (unit, string) result
end
```

`create` initialises a fresh instance; `step` applies one trace step and
returns `Error msg` if the implementation fails.

### State (`Itf.STATE`)

A state module knows how to serialise/deserialise the observable part of your
implementation's state so it can be compared to the Quint model's state:

```ocaml
module type STATE = sig
  type t
  type driver
  val of_yojson : Yojson.Basic.t -> (t, string) result
  val to_yojson : t -> Yojson.Basic.t
  val equal     : t -> t -> bool
  val of_driver : driver -> t
end
```

### Replay (`Replay.Make`)

Functor that ties a driver and state together:

```ocaml
module R = Replay.Make(MyDriver)(MyState)

match R.run trace with
| Ok ()            -> (* all steps match *)
| Error (i, diff)  -> Printf.eprintf "diverged at step %d:\n%s\n" i diff
```

---

## Quickstart

### 1. Define a driver and state

```ocaml
module CounterDriver = struct
  type t = { mutable counter : int }
  let create () = { counter = 0 }
  let step d _s =
    d.counter <- d.counter + 1;
    Ok ()
end

module CounterState = struct
  type t = int
  type driver = CounterDriver.t
  let of_yojson = function
    | `Assoc [("counter", `Assoc [("#bigint", `String s)])] ->
      (try Ok (int_of_string s) with _ -> Error "bad bigint")
    | _ -> Error "unexpected state shape"
  let to_yojson n =
    `Assoc [("counter", `Assoc [("#bigint", `String (string_of_int n))])]
  let equal = Int.equal
  let of_driver d = d.CounterDriver.counter
end
```

### 2. Generate an ITF trace with Quint

```sh
quint run counter.qnt --out-itf test/fixtures/counter_trace.itf.json
```

Commit the `.itf.json` file. It is the source of truth for the test—no live
Quint needed after this point.

### 3. Write an Alcotest case

```ocaml
module R = Replay.Make(CounterDriver)(CounterState)

let test_counter () =
  match Itf.parse_file "fixtures/counter_trace.itf.json" with
  | Error e -> Alcotest.fail ("parse failed: " ^ e)
  | Ok [] -> Alcotest.fail "empty trace"
  | Ok (trace :: _) ->
    match R.run trace with
    | Ok () -> ()
    | Error (i, diff) ->
      Alcotest.fail (Printf.sprintf "step %d: %s" i diff)
```

---

## Trace formats

Both ITF layouts are accepted:

- Quint `run --mbt` (0.32 and later) stores the action and the nondeterministic choices as
  the state bindings `mbt::actionTaken` and `mbt::nondetPicks`, and wraps each choice in a
  Quint `Option`. The parser exposes them as `Step.action_name` and `Step.nondet_picks`
  (a `Some v` choice as `v`; a `None` choice is absent) and removes the `mbt::` keys from
  `Step.bindings`.
- Older traces store `actionTaken` and `nondetPicks` in each state's `#meta`.

State variables qualified by module path, as produced by `quint run --main inst` on an
instance module (`inst::counter::x`), are exposed under their short name (`x`) when no other
variable of the step has the same short name.

### Drivers with resources

A driver that owns resources (a scheduler, domains, files) implements
`Replay.DRIVER_EXT`, which adds `close : t -> unit`; `Replay.Make_ext (D) (S)` calls it once
when the replay ends, whether it matched, diverged or raised.

---

## PPX sugar

Add `ppx_quint_connect` to your `(preprocess (pps ...))` in `dune`:

### `let%quint_test` — replay a named Quint test

```ocaml
let%quint_test my_test = {
  spec   = "specs/counter.qnt";
  test   = "test_counter_increments";
  driver = (module CounterDriver);
}
```

Expands to an `Alcotest.test_case` named `"my_test"` that loads
`specs/counter.itf.json` and replays it.

### `[%switch step]` — dispatch on action name

```ocaml
let step d s =
  [%switch s
    | "create" ->
      let%bind id = Itf.Switch.param s "id" in
      ...
    | "update" ->
      ...
  ]
```

Generates a match on `s.action_name`; unmatched actions return `Error`.
`let%bind` auto-binds a named parameter and propagates errors.

---

## CLI: `quint-connect`

```
quint-connect run SPEC [TEST_NAME] [--driver FILE.cma] [--verbose] [--traces N]
```

| Option | Description |
|--------|-------------|
| `SPEC` | Path to the `.qnt` spec file (the `.itf.json` is derived from it) |
| `TEST_NAME` | Optional test name to include in error messages |
| `--driver FILE.cma` | Dynamically load a driver `.cma` |
| `--verbose` | Print each step action and nondeterministic choices to stderr |
| `--traces N` | Run the trace N times (default: 1) |

Register your driver for dynamic loading:

```ocaml
(* in my_driver.ml, compiled with -linkpkg *)
let () =
  Ocaml_quint_connect.Quint_cli.register_driver (module MyDriver)
```

---

## Action parameter helpers

```ocaml
(* Required — returns Error if missing *)
Itf.Switch.param step "field_name"  (* : (Value.t, string) result *)

(* Optional — returns None if missing *)
Itf.Switch.param_opt step "field_name"  (* : Value.t option *)
```

---

## Verbose mode

Set `QUINT_VERBOSE=1` to print each step's action name and nondeterministic
choices to stderr during replay:

```sh
QUINT_VERBOSE=1 dune runtest
```

---

## License

MIT
