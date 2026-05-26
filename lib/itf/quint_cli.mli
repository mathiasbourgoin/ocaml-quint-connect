(** Register a driver module for CLI use.
    Called by a dynamically loaded .cma file to expose its driver. *)
val register_driver : (module Itf.DRIVER) -> unit

(** Return the currently registered driver, if any. *)
val registered_driver : unit -> (module Itf.DRIVER) option

(** [run_test ~verbose ~print spec test_name driver]
    Loads the ITF trace for [spec] and replays it with [driver].
    Prints "PASS: test_name (N steps)" on success, "FAIL: ..." on error.
    When [verbose] is true, prints each step's action inline.
    Returns 0 on pass, 1 on fail. *)
val run_test :
  ?verbose:bool ->
  ?print:(string -> unit) ->
  string -> string -> (module Itf.DRIVER) -> int

(** [run_simulation ~verbose ~traces ~print spec driver]
    Runs [traces] simulation replays of the ITF trace for [spec].
    Prints "PASS: simulation (N traces)" on success, "FAIL: ..." on error.
    When [verbose] is true, prints each step's action inline.
    Returns 0 on pass, 1 on fail. *)
val run_simulation :
  ?verbose:bool ->
  ?traces:int ->
  ?print:(string -> unit) ->
  string -> (module Itf.DRIVER) -> int
