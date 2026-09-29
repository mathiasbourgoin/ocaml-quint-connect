module Make (D : Itf.DRIVER) (S : Itf.STATE with type driver = D.t) : sig
  (** [run trace] replays [trace] against a fresh driver instance.
      For each step: calls [D.step], captures state via [S.of_driver],
      and compares with the expected ITF state decoded via [S.of_yojson].
      Returns [Ok ()] if all steps match, or [Error (step_index, diff)]
      on the first diverging step.
      When env var QUINT_VERBOSE=1, prints each step action and
      nondeterministic choices to stderr. *)
  val run : Itf.Trace.t -> (unit, int * string) result
end

(** A driver that owns resources (domains, switches, files) released by [close]. *)
module type DRIVER_EXT = sig
  include Itf.DRIVER

  val close : t -> unit
end

module Make_ext (D : DRIVER_EXT) (S : Itf.STATE with type driver = D.t) : sig
  (** Like [Make.run], and calls [D.close] exactly once when the replay ends,
      whether it matched, diverged, or raised. If the replay raised, that exception is
      re-raised and an exception from [D.close] is ignored; otherwise an exception from
      [D.close] propagates. *)
  val run : Itf.Trace.t -> (unit, int * string) result
end
