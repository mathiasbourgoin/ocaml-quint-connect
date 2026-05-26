module Value : sig
  type t =
    | Bool   of bool
    | Int    of int
    | Float  of float
    | Str    of string
    | Null
    | BigInt of string
    | Set    of t list
    | List   of t list
    | Tuple  of t list
    | Record of (string * t) list
    | Map    of (t * t) list
end

module Step : sig
  type t = {
    action_name  : string option;
    bindings     : (string * Value.t) list;
    nondet_picks : (string * Value.t) list;
  }
end

module Trace : sig
  type t = Step.t list
end

(** Module type for a Quint step driver.
    A driver executes one step at a time, advancing its internal state. *)
module type DRIVER = sig
  type t
  val create : unit -> t
  val step   : t -> Step.t -> (unit, string) result
end

(** Module type for a Quint state representation.
    A state can be decoded from JSON, compared for equality, and reconstructed
    from a driver's internal state. *)
module type STATE = sig
  type t
  type driver
  val of_yojson : Yojson.Basic.t -> (t, string) result
  val to_yojson : t -> Yojson.Basic.t
  val equal     : t -> t -> bool
  val of_driver : driver -> t
end

(** Helpers for use inside [%switch] arms to retrieve action parameters
    by name from a step's bindings. *)
module Switch : sig
  (** [param step name] returns [Ok v] if [name] is bound in [step],
      [Error msg] otherwise. Never raises. *)
  val param     : Step.t -> string -> (Value.t, string) result

  (** [param_opt step name] returns [Some v] if [name] is bound in [step],
      [None] otherwise. *)
  val param_opt : Step.t -> string -> Value.t option
end

val parse_string : string -> (Trace.t list, string) result
val parse_file   : string -> (Trace.t list, string) result
