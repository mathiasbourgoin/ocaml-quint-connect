(** Run a Quint test by loading a pre-generated ITF trace and replaying it
    with the given driver.

    [run_test spec_path test_name driver] checks that [spec_path] exists,
    then loads a trace from the corresponding [.itf.json] file (same path with
    [.qnt] extension replaced by [.itf.json]), and replays the trace steps
    using [driver]. [test_name] identifies the Quint test being run and is
    included in error messages to aid diagnostics.

    Returns [Error "spec file not found: <path>"] if the spec does not exist.
    Returns [Error msg] if any step fails (message includes [test_name]).
    Returns [Ok ()] if all steps succeed. *)
val run_test :
  string ->
  string ->
  (module Itf.DRIVER) ->
  (unit, string) result

(** Run a Quint simulation by loading a pre-generated ITF trace and replaying
    it [traces] times, each time with a fresh driver instance.

    [run_simulation ~traces spec_path driver] checks that [spec_path] exists,
    loads the corresponding [.itf.json] trace, then replays it [traces] times
    using independent driver instances.

    Returns [Error "spec file not found: <path>"] if the spec does not exist.
    Returns [Error "simulation trace N step M failed: <msg>"] on any failure,
    where N is the zero-based trace index and M is the zero-based step index.
    Returns [Ok ()] if all traces succeed. *)
val run_simulation :
  ?traces:int ->
  string ->
  (module Itf.DRIVER) ->
  (unit, string) result
