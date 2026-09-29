# Changes

## Unreleased

- Read Quint 0.32 `run --mbt` traces: `mbt::actionTaken` and `mbt::nondetPicks` state
  bindings populate `Step.action_name` and `Step.nondet_picks`, with Option-wrapped picks
  unwrapped (`Some v` → `v`, `None` → absent). Legacy `#meta` metadata still takes precedence
  when well-typed.
- **Behaviour change:** `mbt::*` keys are removed from `Step.bindings`.
- `parse_string` / `parse_file` accept `?unqualify` (default `false`): expose module-qualified
  state variables (`inst::mod::x`) under their short name when unambiguous. `Quint_test` and
  the `quint-connect` CLI do not expose this option yet; call `Itf` directly to use it.
  Adding an optional argument breaks only modules that ascribe `Itf` to a signature with
  the old `parse_string`/`parse_file` types.
- `Replay.DRIVER_EXT` and `Replay.Make_ext`: drivers with a `close` called once at the end of
  a replay, whatever its outcome.
- The package is installable with opam (`ocaml-quint-connect`).
