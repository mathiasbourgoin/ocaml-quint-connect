module Value = struct
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

module Step = struct
  type t = {
    action_name  : string option;
    bindings     : (string * Value.t) list;
    nondet_picks : (string * Value.t) list;
  }
end

module Trace = struct
  type t = Step.t list
end

module type DRIVER = sig
  type t
  val create : unit -> t
  val step   : t -> Step.t -> (unit, string) result
end

module type STATE = sig
  type t
  type driver
  val of_yojson : Yojson.Basic.t -> (t, string) result
  val to_yojson : t -> Yojson.Basic.t
  val equal     : t -> t -> bool
  val of_driver : driver -> t
end

module Switch = struct
  let param step name =
    match List.assoc_opt name step.Step.bindings with
    | Some v -> Ok v
    | None ->
      (match List.assoc_opt name step.Step.nondet_picks with
       | Some v -> Ok v
       | None -> Error ("Switch.param: parameter not found: " ^ name))

  let param_opt step name =
    match List.assoc_opt name step.Step.bindings with
    | Some _ as r -> r
    | None -> List.assoc_opt name step.Step.nondet_picks
end

(* ---- ITF value parser ---- *)

let rec parse_value (j : Yojson.Basic.t) : (Value.t, string) result =
  match j with
  | `Bool b   -> Ok (Value.Bool b)
  | `Int n    -> Ok (Value.Int n)
  | `Float f  -> Ok (Value.Float f)
  | `String s -> Ok (Value.Str s)
  | `Null     -> Ok Value.Null
  | `List lst ->
    let rec go acc = function
      | []       -> Ok (Value.List (List.rev acc))
      | x :: xs  ->
        (match parse_value x with
         | Error e -> Error e
         | Ok v    -> go (v :: acc) xs)
    in
    go [] lst
  | `Assoc fields ->
    (* ITF tagged values *)
    (match fields with
     | [("#bigint", `String s)] -> Ok (Value.BigInt s)
     | [("#set",    `List lst)] ->
       let rec go acc = function
         | []      -> Ok (Value.Set (List.rev acc))
         | x :: xs ->
           (match parse_value x with
            | Error e -> Error e
            | Ok v    -> go (v :: acc) xs)
       in
       go [] lst
     | [("#tup",    `List lst)] ->
       let rec go acc = function
         | []      -> Ok (Value.Tuple (List.rev acc))
         | x :: xs ->
           (match parse_value x with
            | Error e -> Error e
            | Ok v    -> go (v :: acc) xs)
       in
       go [] lst
     | [("#map",    `List pairs)] ->
       let parse_pair = function
         | `List [k; v] ->
           (match parse_value k, parse_value v with
            | Ok k', Ok v'   -> Ok (k', v')
            | Error e, _     -> Error e
            | _,       Error e -> Error e)
         | _ -> Error "malformed #map entry"
       in
       let rec go acc = function
         | []      -> Ok (Value.Map (List.rev acc))
         | p :: ps ->
           (match parse_pair p with
            | Error e   -> Error e
            | Ok pair   -> go (pair :: acc) ps)
       in
       go [] pairs
     | _ ->
       let rec go acc = function
         | []           -> Ok (Value.Record (List.rev acc))
         | (k, v) :: fs ->
           (match parse_value v with
            | Error e -> Error e
            | Ok v'   -> go ((k, v') :: acc) fs)
       in
       go [] fields)

(* ---- Step parser ---- *)

let parse_value_list f lst =
  let rec go acc = function
    | []       -> Ok (List.rev acc)
    | x :: xs  ->
      (match f x with
       | Error e -> Error e
       | Ok v    -> go (v :: acc) xs)
  in
  go [] lst

let parse_step (j : Yojson.Basic.t) : (Step.t, string) result =
  match j with
  | `Assoc fields ->
    let meta_opt =
      match List.assoc_opt "#meta" fields with
      | Some (`Assoc m) -> Some m
      | _               -> None
    in
    let action_name =
      match meta_opt with
      | Some m ->
        (match List.assoc_opt "actionTaken" m with
         | Some (`String s) -> Some s
         | _                -> None)
      | None -> None
    in
    let nondet_pairs =
      match meta_opt with
      | Some m ->
        (match List.assoc_opt "nondetPicks" m with
         | Some (`Assoc picks) -> picks
         | _                   -> [])
      | None -> []
    in
    (match parse_value_list (fun (k, v) ->
       match parse_value v with Ok v' -> Ok (k, v') | Error e -> Error e
     ) nondet_pairs with
     | Error e -> Error e
     | Ok nondet_picks ->
       let binding_pairs =
         List.filter (fun (k, _) -> k <> "#meta") fields
       in
       (match parse_value_list (fun (k, v) ->
          match parse_value v with Ok v' -> Ok (k, v') | Error e -> Error e
        ) binding_pairs with
        | Error e -> Error e
        | Ok bindings ->
          Ok { Step.action_name; bindings; nondet_picks }))
  | _ -> Error "step must be a JSON object"

(* ---- Trace parser ---- *)

let parse_trace (j : Yojson.Basic.t) : (Trace.t, string) result =
  match j with
  | `Assoc fields ->
    (* #meta is required *)
    (match List.assoc_opt "#meta" fields with
     | None -> Error "missing required #meta field at trace root"
     | Some _ ->
       (match List.assoc_opt "states" fields with
        | Some (`List states) ->
          let results = List.map parse_step states in
          let rec collect acc = function
            | []              -> Ok (List.rev acc)
            | Error e :: _    -> Error e
            | Ok s    :: rest -> collect (s :: acc) rest
          in
          (match collect [] results with
           | Ok steps -> Ok steps
           | Error e  -> Error e)
        | _ -> Error "missing or malformed 'states' array"))
  | _ -> Error "ITF JSON must be a top-level object"

(* ---- Public API ---- *)

let parse_string (s : string) : (Trace.t list, string) result =
  match Yojson.Basic.from_string s with
  | exception Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
  | j ->
    (match parse_trace j with
     | Ok trace -> Ok [trace]
     | Error e  -> Error e)

let parse_file (path : string) : (Trace.t list, string) result =
  match Yojson.Basic.from_file path with
  | exception Sys_error msg         -> Error ("File error: " ^ msg)
  | exception Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
  | j ->
    (match parse_trace j with
     | Ok trace -> Ok [trace]
     | Error e  -> Error e)
