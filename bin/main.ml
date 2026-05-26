open Ocaml_quint_connect

(* Usage:
   quint-connect run SPEC [TEST_NAME] [--driver FILE.cma] [--verbose] [--traces N]
*)

let usage =
  "Usage: quint-connect run SPEC [TEST_NAME] [--driver FILE] [--verbose] [--traces N]"

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "run" :: rest ->
    let spec     = ref "" in
    let test_name = ref None in
    let driver_path = ref None in
    let verbose  = ref false in
    let traces   = ref None in
    let positional = ref [] in
    let rec parse = function
      | [] -> ()
      | "--driver" :: v :: tl -> driver_path := Some v; parse tl
      | "--verbose" :: tl     -> verbose := true; parse tl
      | "--traces" :: v :: tl ->
        (match int_of_string_opt v with
         | Some n -> traces := Some n
         | None   -> Printf.eprintf "error: --traces requires an integer\n"; exit 2);
        parse tl
      | s :: _ when String.length s > 0 && s.[0] = '-' ->
        Printf.eprintf "unknown flag: %s\n" s; exit 2
      | s :: tl -> positional := s :: !positional; parse tl
    in
    parse rest;
    (match List.rev !positional with
     | [] -> Printf.eprintf "%s\n" usage; exit 2
     | s :: rest ->
       spec := s;
       (match rest with
        | [] -> ()
        | name :: _ -> test_name := Some name));
    (* Load driver .cma if provided *)
    (match !driver_path with
     | None -> ()
     | Some path ->
       (try Dynlink.loadfile path
        with Dynlink.Error e ->
          Printf.eprintf "error loading driver %s: %s\n" path (Dynlink.error_message e);
          exit 2));
    let driver =
      match Quint_cli.registered_driver () with
      | Some d -> d
      | None ->
        (* No driver registered — provide a no-op pass-through driver *)
        (module struct
           type t = unit
           let create () = ()
           let step () _s = Ok ()
         end : Itf.DRIVER)
    in
    let code =
      match !test_name, !traces with
      | Some name, _ ->
        Quint_cli.run_test ~verbose:!verbose !spec name driver
      | None, Some n ->
        Quint_cli.run_simulation ~verbose:!verbose ~traces:n !spec driver
      | None, None ->
        Quint_cli.run_simulation ~verbose:!verbose !spec driver
    in
    exit code
  | _ ->
    Printf.eprintf "%s\n" usage;
    exit 2
