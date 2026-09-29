---
title: ocaml-quint-connect invariants and quality policy
last-updated: 2026-09-29
status: live-doctrine
owner: human (delegated to tech-lead, 2026-09-29)
schema-version: 2
---

# Invariants and quality policy

- Parsing never raises: errors are returned as `Error msg`.
- Backward compatibility: traces accepted before a change are still accepted, with the same
  result. Recorded exception (2026-09-29, fix/quint-032-mbt-traces): `mbt::actionTaken` and
  `mbt::nondetPicks` no longer appear in `Step.bindings`; they are MBT metadata, now exposed
  as `action_name` and `nondet_picks`.
- Strict TDD: every behaviour change starts with a failing Alcotest case; fixtures are real
  traces produced by `quint run`.
