open Ocaml_quint_connect

(* ===== Minimal in-memory SQLite story store ===== *)

type story = { id: int; status: int; was_accepted: bool }

module Story_store = struct
  type t = Sqlite3.db

  let create () =
    let db = Sqlite3.db_open ":memory:" in
    (match Sqlite3.exec db
       "CREATE TABLE stories \
        (id INTEGER PRIMARY KEY, \
         status INTEGER NOT NULL DEFAULT 0, \
         was_accepted INTEGER NOT NULL DEFAULT 0)"
    with
    | Sqlite3.Rc.OK -> ()
    | rc -> failwith ("Story_store.create: " ^ Sqlite3.Rc.to_string rc));
    db

  let exec_unit db sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> Ok ()
    | rc -> Error (Sqlite3.Rc.to_string rc ^ " executing: " ^ sql)

  let create_story db id =
    exec_unit db
      (Printf.sprintf
         "INSERT INTO stories (id, status, was_accepted) VALUES (%d, 0, 0)" id)

  let update_story_status db story_id new_status =
    let extra = if new_status = 2 then ", was_accepted = 1" else "" in
    exec_unit db
      (Printf.sprintf "UPDATE stories SET status = %d%s WHERE id = %d"
         new_status extra story_id)

  let reject_story db story_id =
    exec_unit db
      (Printf.sprintf "UPDATE stories SET status = 0 WHERE id = %d" story_id)

  let reset_story_for_retry db story_id =
    exec_unit db
      (Printf.sprintf
         "UPDATE stories SET status = 2, was_accepted = 1 WHERE id = %d"
         story_id)

  let list_stories db =
    let rows = ref [] in
    let cb row _headers =
      match row with
      | [| Some id_s; Some status_s; Some was_s |] ->
        let id           = int_of_string id_s in
        let status       = int_of_string status_s in
        let was_accepted = int_of_string was_s <> 0 in
        rows := { id; status; was_accepted } :: !rows
      | _ -> ()
    in
    ignore
      (Sqlite3.exec db ~cb
         "SELECT id, status, was_accepted FROM stories ORDER BY id");
    List.rev !rows
end

(* ===== DRIVER ===== *)

module Story_store_driver = struct
  type t = { db : Story_store.t }

  let create () = { db = Story_store.create () }

  let bigint_or_int = function
    | Itf.Value.BigInt s ->
      (try Ok (int_of_string s)
       with _ -> Error ("bad bigint: " ^ s))
    | Itf.Value.Int n -> Ok n
    | _ -> Error "expected bigint or int"

  let step d s =
    match s.Itf.Step.action_name with
    | None -> Ok ()
    | Some "create_story" ->
      (match Itf.Switch.param s "id" with
      | Error e -> Error e
      | Ok v ->
        (match bigint_or_int v with
        | Error e -> Error ("create_story id: " ^ e)
        | Ok id   -> Story_store.create_story d.db id))
    | Some "update_story_status" ->
      (match Itf.Switch.param s "sid", Itf.Switch.param s "ns" with
      | Error e, _ | _, Error e -> Error e
      | Ok sid_v, Ok ns_v ->
        (match bigint_or_int sid_v, bigint_or_int ns_v with
        | Error e, _ | _, Error e -> Error ("update_story_status: " ^ e)
        | Ok sid, Ok ns           -> Story_store.update_story_status d.db sid ns))
    | Some "reject_story" ->
      (match Itf.Switch.param s "rid" with
      | Error e -> Error e
      | Ok v ->
        (match bigint_or_int v with
        | Error e -> Error ("reject_story rid: " ^ e)
        | Ok rid  -> Story_store.reject_story d.db rid))
    | Some "reset_story_for_retry" ->
      (match Itf.Switch.param s "retid" with
      | Error e -> Error e
      | Ok v ->
        (match bigint_or_int v with
        | Error e   -> Error ("reset_story_for_retry retid: " ^ e)
        | Ok retid  -> Story_store.reset_story_for_retry d.db retid))
    | Some _ -> Ok ()
end

(* ===== STATE ===== *)

module Story_store_state = struct
  type t      = story list
  type driver = Story_store_driver.t

  let parse_int_field fields name =
    match List.assoc_opt name fields with
    | None -> Error ("missing field: " ^ name)
    | Some (`Assoc [("#bigint", `String s)]) ->
      (try Ok (int_of_string s)
       with _ -> Error ("bad bigint in " ^ name ^ ": " ^ s))
    | Some (`Int n) -> Ok n
    | Some j ->
      Error
        ("unexpected int value for " ^ name ^ ": "
        ^ Yojson.Basic.to_string j)

  let parse_story = function
    | `Assoc fields -> (
      match
        parse_int_field fields "id",
        parse_int_field fields "status",
        List.assoc_opt "was_accepted" fields
      with
      | Ok id, Ok status, Some (`Bool was_accepted) ->
        Ok { id; status; was_accepted }
      | Ok id, Ok status, Some (`Int n) ->
        Ok { id; status; was_accepted = n <> 0 }
      | Error e, _, _ | _, Error e, _ -> Error e
      | _, _, None -> Error "missing field: was_accepted"
      | _, _, Some j ->
        Error ("unexpected was_accepted: " ^ Yojson.Basic.to_string j))
    | j -> Error ("expected story record: " ^ Yojson.Basic.to_string j)

  let of_yojson = function
    | `Assoc fields -> (
      match List.assoc_opt "stories" fields with
      | None -> Error "missing 'stories' field"
      | Some (`Assoc [("#set", `List items)]) ->
        let rec go acc = function
          | [] ->
            Ok (List.sort (fun a b -> compare a.id b.id) (List.rev acc))
          | item :: rest -> (
            match parse_story item with
            | Error e -> Error e
            | Ok s    -> go (s :: acc) rest)
        in
        go [] items
      | Some j ->
        Error ("unexpected stories value: " ^ Yojson.Basic.to_string j))
    | j -> Error ("unexpected state json: " ^ Yojson.Basic.to_string j)

  let story_to_yojson s =
    `Assoc
      [ ("id",          `Assoc [("#bigint", `String (string_of_int s.id))])
      ; ("status",      `Assoc [("#bigint", `String (string_of_int s.status))])
      ; ("was_accepted", `Bool s.was_accepted)
      ]

  let to_yojson stories =
    `Assoc
      [("stories", `Assoc [("#set", `List (List.map story_to_yojson stories))])]

  let equal a b =
    let sort = List.sort (fun x y -> compare x.id y.id) in
    let a' = sort a and b' = sort b in
    List.length a' = List.length b'
    && List.for_all2
         (fun x y ->
           x.id = y.id && x.status = y.status
           && x.was_accepted = y.was_accepted)
         a' b'

  let of_driver d = Story_store.list_stories d.Story_store_driver.db
end

module R = Replay.Make (Story_store_driver) (Story_store_state)

let fixture_path = "fixtures/story_store_trace.itf.json"

(* AC1: step called with create_story → store contains the story with correct params *)
let test_create_story_dispatch () =
  let d    = Story_store_driver.create () in
  let step : Itf.Step.t =
    { action_name  = Some "create_story"
    ; bindings     = []
    ; nondet_picks = [("id", Itf.Value.BigInt "42")]
    }
  in
  (match Story_store_driver.step d step with
  | Error e -> Alcotest.fail ("step returned Error: " ^ e)
  | Ok () ->
    let stories = Story_store.list_stories d.Story_store_driver.db in
    Alcotest.(check int)  "one story created"   1     (List.length stories);
    let s = List.hd stories in
    Alcotest.(check int)  "id = 42"             42    s.id;
    Alcotest.(check int)  "status = DRAFT (0)"  0     s.status;
    Alcotest.(check bool) "was_accepted = false" false s.was_accepted)

(* AC2: Replay.run with Story_store_driver — all invariants hold *)
let test_replay_holds_invariants () =
  match Itf.parse_file fixture_path with
  | Error e    -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok []      -> Alcotest.fail "no traces in fixture"
  | Ok (tr :: _) ->
    (match R.run tr with
    | Ok ()          -> ()
    | Error (i, msg) ->
      Alcotest.fail (Printf.sprintf "replay failed at step %d: %s" i msg))

(* AC3: test passes using only pre-generated fixtures (no live Quint) *)
let test_no_live_quint_required () =
  match Itf.parse_file fixture_path with
  | Error e    -> Alcotest.fail ("parse_file failed: " ^ e)
  | Ok []      -> Alcotest.fail "empty trace list"
  | Ok (tr :: _) ->
    Alcotest.(check bool) "trace has steps" true (List.length tr > 0)

let () =
  Alcotest.run "Story #8 \xe2\x80\x93 Story_store_driver"
    [ "driver",
      [ Alcotest.test_case
          "create_story dispatches to store with correct params"
          `Quick test_create_story_dispatch
      ; Alcotest.test_case
          "Replay.run invariants hold across trace"
          `Quick test_replay_holds_invariants
      ; Alcotest.test_case
          "fixture-based: no live Quint install required"
          `Quick test_no_live_quint_required
      ]
    ]
