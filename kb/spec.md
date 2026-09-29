---
title: ocaml-quint-connect specification
last-updated: 2026-09-29
status: live-doctrine
owner: human (delegated to tech-lead, 2026-09-29)
schema-version: 2
---

# ocaml-quint-connect specification

The library parses Quint ITF traces into typed OCaml values and replays them step by step
against an OCaml implementation through a driver, comparing observable state after each
step (see README).

- **QC-1** Parse ITF JSON (Apalache ADR-015): `#bigint`, `#set`, `#tup`, `#map`, records.
- **QC-2** Expose, for each step, the action taken and the nondeterministic picks, whether the
  trace stores them in `#meta` (older Quint) or as `mbt::actionTaken` / `mbt::nondetPicks`
  state bindings (Quint `run --mbt`, 0.32); these metadata keys never appear among the state
  bindings.
- **QC-3** Nondeterministic picks of the `mbt::` layout, encoded as Quint `Option` values (`{tag: "Some", value}` /
  `{tag: "None", ...}`) are exposed as the picked value, and absent when `None`.
- **QC-4** With `~unqualify:true`, state variables qualified by module path (`inst::mod::x`)
  are exposed under their unqualified name `x` when no other variable or nondeterministic pick
  of the step has that name; by default keys are kept verbatim.
- **QC-5** Replay runs a driver over a trace; a driver may provide a teardown called when the
  replay ends, whatever the outcome.
- **QC-6** Existing public signatures (`Itf`, `Replay.Make`, `DRIVER`, `STATE`) keep working
  unchanged (additive changes only).
