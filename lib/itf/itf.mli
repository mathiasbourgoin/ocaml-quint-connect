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

(** [parse_string json] parses an ITF trace. Both layouts are read: MBT metadata in each
    state's [#meta] (older Quint) or as [mbt::actionTaken] / [mbt::nondetPicks] state
    bindings (Quint 0.32 [run --mbt]), whose picks are Option-unwrapped; [mbt::] keys never
    appear in [Step.bindings]. With [~unqualify:true], a state variable qualified by module
    path ([inst::mod::x]) is exposed as [x] when no other variable or pick of the step has
    that name; the default keeps keys verbatim. Never raises. *)
val parse_string : ?unqualify:bool -> string -> (Trace.t list, string) result

(** Same as [parse_string], reading the file at the given path. *)
val parse_file   : ?unqualify:bool -> string -> (Trace.t list, string) result
