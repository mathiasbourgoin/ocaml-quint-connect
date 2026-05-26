let _driver : (module Itf.DRIVER) option ref = ref None

let register_driver d = _driver := Some d
let registered_driver () = !_driver

let default_print s = print_endline s

let trace_path_of_spec spec =
  (if Filename.check_suffix spec ".qnt"
   then Filename.chop_suffix spec ".qnt"
   else spec) ^ ".itf.json"

let run_test ?(verbose = false) ?(print = default_print) spec test_name (module D : Itf.DRIVER) =
  if not (Sys.file_exists spec) then begin
    print (Printf.sprintf "FAIL: %s \xe2\x80\x94 spec file not found: %s" test_name spec);
    1
  end else
    let tp = trace_path_of_spec spec in
    if not (Sys.file_exists tp) then begin
      print (Printf.sprintf "FAIL: %s \xe2\x80\x94 trace file not found: %s" test_name tp);
      1
    end else
      match Itf.parse_file tp with
      | Error e ->
        print (Printf.sprintf "FAIL: %s \xe2\x80\x94 parse error: %s" test_name e);
        1
      | Ok [] ->
        print (Printf.sprintf "FAIL: %s \xe2\x80\x94 no traces in file" test_name);
        1
      | Ok (trace :: _) ->
        let driver = D.create () in
        let rec loop i = function
          | [] ->
            print (Printf.sprintf "PASS: %s (%d steps)" test_name i);
            0
          | step :: rest ->
            if verbose then
              print (Printf.sprintf "  step %d: %s" i
                (Option.value ~default:"(none)" step.Itf.Step.action_name));
            (match D.step driver step with
             | Ok () -> loop (i + 1) rest
             | Error e ->
               print (Printf.sprintf "FAIL: %s at step %d: %s" test_name i e);
               1)
        in
        loop 0 trace

let run_simulation ?(verbose = false) ?(traces = 5) ?(print = default_print) spec (module D : Itf.DRIVER) =
  if not (Sys.file_exists spec) then begin
    print (Printf.sprintf "FAIL: simulation \xe2\x80\x94 spec file not found: %s" spec);
    1
  end else
    let tp = trace_path_of_spec spec in
    if not (Sys.file_exists tp) then begin
      print (Printf.sprintf "FAIL: simulation \xe2\x80\x94 trace file not found: %s" tp);
      1
    end else
      match Itf.parse_file tp with
      | Error e ->
        print (Printf.sprintf "FAIL: simulation \xe2\x80\x94 parse error: %s" e);
        1
      | Ok [] ->
        print (Printf.sprintf "FAIL: simulation \xe2\x80\x94 no traces in file");
        1
      | Ok (trace :: _) ->
        let rec loop_traces i =
          if i >= traces then begin
            print (Printf.sprintf "PASS: simulation (%d traces)" traces);
            0
          end else
            let driver = D.create () in
            let rec loop_steps j = function
              | [] -> loop_traces (i + 1)
              | step :: rest ->
                if verbose then
                  print (Printf.sprintf "  trace %d step %d: %s" i j
                    (Option.value ~default:"(none)" step.Itf.Step.action_name));
                (match D.step driver step with
                 | Ok () -> loop_steps (j + 1) rest
                 | Error e ->
                   print (Printf.sprintf "FAIL: simulation trace %d step %d: %s" i j e);
                   1)
            in
            loop_steps 0 trace
        in
        loop_traces 0
