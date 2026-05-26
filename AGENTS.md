# AGENTS.md — ocaml-quint-connect

## Build & Test

```bash
dune build          # compile everything
dune runtest        # run all tests
dune clean          # clean build artifacts
```

After all tests pass, write output to the log path specified in `.epure/context/CONTEXT.md`
(look for the "Build log:" line — it gives the exact filename for this attempt).
**Always run `dune clean` first** so that all tests execute and appear in the log
(cached test runs produce no output, causing the validator to see an empty/incomplete log):
```bash
# Example — use the actual path from CONTEXT.md, e.g.:
(dune clean && dune build 2>&1 && dune runtest 2>&1) > .epure/build_output_N_M.log
```

## Project Structure

```
lib/itf/        -- Core library (ocaml_quint_connect)
  itf.ml        -- Implementation: parse_string/parse_file, DRIVER, STATE, Switch
  itf.mli       -- Public interface
                   Exposes: Value, Step, Trace, DRIVER, STATE, Switch,
                            parse_string, parse_file
  replay.ml     -- Replay.Make(D)(S) functor: runs a parsed trace against a driver
  replay.mli    -- Public interface for Replay
lib/itf/
  quint_test.ml  -- Quint_test.run_test: check spec exists, load .itf.json, replay steps
  quint_test.mli -- Public interface for Quint_test
  quint_cli.ml   -- Quint_cli: run_test/run_simulation (exit-code wrappers), register_driver
  quint_cli.mli  -- Public interface for Quint_cli
ppx/            -- PPX rewriters
  ppx_quint_switch.ml  -- [%switch] and [%switch_bind] expansions to result-returning match
  ppx_quint_connect.ml -- [let%quint_test] and [let%quint_run] expansions to Alcotest.test_case
  dune
test/
  test_itf.ml    -- Alcotest suite for the ITF parser (8 tests)
  test_story2.ml -- Alcotest suite for DRIVER/STATE/switch!/switch_bind (18 tests)
  test_story3.ml -- Alcotest suite for Replay engine (8 tests)
  test_story4.ml -- Alcotest integration test: replay real Quint trace (3 tests)
  test_story5.ml -- Alcotest suite for ppx_quint_connect / quint_test attribute (2 tests)
  test_story6.ml -- Alcotest suite for ppx_quint_connect / quint_run attribute (3 tests)
  test_story7.ml -- Alcotest suite for Quint_cli module (5 tests)
  fixtures/
    counter_spec.qnt         -- Simple Quint counter spec (increment + counterNonNegative)
    counter_trace.itf.json   -- Pre-generated ITF trace fixture (3 increment steps)
    counter_spec.itf.json    -- Same trace, named after the spec (for Quint_test.run_test)
bin/
  main.ml        -- quint-connect CLI binary (arg parsing + Dynlink driver loading)
  dune
```

## Key Architectural Patterns

- **Library name**: `ocaml_quint_connect` (from `lib/itf/dune`)
- **Dependencies**: `yojson` (JSON parsing), `alcotest` + `str` (tests only), `ppxlib` (PPX)
- **Error handling**: all public functions return `(_, string) result` — no exceptions escape
- **ITF values**: `Itf.Value.t` models the full ITF tagged-value set (`#bigint`, `#set`, `#map`, `#tup`, plain records/lists, booleans, ints, floats, strings, `Null`); JSON `null` maps to `Value.Null` (not `Str "null"`)
- **Step**: `Itf.Step.t` holds `action_name`, `bindings` (variable state), and `nondet_picks`
- **Trace**: `Itf.Trace.t = Itf.Step.t list` — one trace per ITF file; `parse_*` returns `Trace.t list` (always length 1 for a single file)
- **DRIVER module type**: `type t`, `create : unit -> t`, `step : t -> Step.t -> (unit, string) result`
- **STATE module type**: `type t`, `type driver`, `of_yojson`, `to_yojson`, `equal`, `of_driver`
- **Replay.Make(D)(S)**: functor producing `run : Trace.t -> (unit, int * string) result`; creates a fresh `D.t` driver, steps through each trace entry, compares `S.of_driver` against `S.of_yojson` of ITF bindings; on mismatch returns `Error (step_index, diff_string)`; when `QUINT_VERBOSE=1` prints action + nondet picks to stderr
- **Switch helpers**: `Itf.Switch.param` returns `(Value.t, string) result` — never raises; `Itf.Switch.param_opt` returns `Value.t option`; both search `bindings` first, then `nondet_picks`
- **[%switch] PPX**: `[%switch (step_expr, [("Action", fun s -> body); ...])]` returns `('a, string) result` — `Ok` on match, `Error` for `None` action or unmatched action; tests use `(preprocess (pps ppx_quint_switch))`
- **[%switch_bind] PPX**: `[%switch_bind (step_expr, [("Action", ["p1"; "p2?"], fun p1 p2_opt -> body)])]` auto-binds named params; required params (no `?`) via `Switch.param` with error propagation; optional (`?` suffix) via `Switch.param_opt` as `'a option`; also returns `('a, string) result`
- **[let%quint_test] PPX**: `let%quint_test my_test = { spec = "path.qnt"; test = "name"; driver = (module M) }` expands to `let my_test = Alcotest.test_case "my_test" \`Quick (fun () -> ...)`. At runtime calls `Quint_test.run_test spec test driver`; uses `(preprocess (per_module ((pps ppx_quint_connect) Test_foo)))` in dune (avoids multi-stanza pps conflicts)
- **Quint_test.run_test**: checks spec exists, loads `<spec_stem>.itf.json`, parses and replays each step via the driver (no state comparison); returns `Error "spec file not found: ..."` when missing
- **Quint_test.run_simulation**: `run_simulation ?traces spec driver` — replays the ITF trace `traces` times (default 5), each with a fresh driver instance; error format: `"simulation trace N step M failed: <msg>"`; used by `[let%quint_run]`
- **[let%quint_run] PPX**: `let%quint_run my_sim = { spec = "path.qnt"; driver = (module M) }` or with `traces = N`; expands to `Alcotest.test_case` calling `Quint_test.run_simulation`; `traces` field is optional; uses same `ppx_quint_connect` library as `[let%quint_test]`
- **Quint_cli module**: `run_test ?verbose ?print spec test_name driver` and `run_simulation ?verbose ?traces ?print spec driver`; both return an exit code (0=pass, 1=fail); `~print` defaults to `print_endline` — inject a custom printer for testing; prints `"PASS: name (N steps)"` / `"FAIL: name at step M: ..."` / `"PASS: simulation (N traces)"`
- **CLI binary** (`bin/main.ml`): `quint-connect run SPEC [TEST_NAME] [--driver FILE.cma] [--verbose] [--traces N]`; loads driver via `Dynlink.loadfile`; driver `.cma` registers itself via `Quint_cli.register_driver`; if no driver registered, defaults to a no-op pass-through driver
- **dune pps multi-stanza caveat**: two `(test ...)` stanzas in the same directory cannot both use `(preprocess (pps ...))` — use `(preprocess (per_module ((pps ...) Module_name)))` to scope each stanza's PPX
- **fixture paths in tests**: use plain relative paths like `"fixtures/foo.json"` (**NOT** `__FILE__`-based or `Sys.executable_name`-based paths); dune copies `(deps ...)` files into the test's CWD so relative paths work correctly

## Workflow Convention

Follow **test-first** (TDD):
1. Write failing tests covering each acceptance criterion
2. Run `dune runtest` — new tests MUST fail
3. Write minimum implementation to pass tests
4. Run full suite — all tests green, no regressions
5. Refactor if needed, keep tests green

## ITF Format Notes

ITF (Informal Trace Format) is produced by `quint run --out-itf`. A trace file is a JSON object with:
- `#meta` (required): top-level metadata
- `vars`: list of variable names
- `states`: array of state objects, each optionally containing:
  - `#meta.actionTaken`: the action/rule name
  - `#meta.nondetPicks`: map of nondeterministic choices
  - All other keys: variable bindings as ITF-encoded values
