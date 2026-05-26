let run_simulation ?(traces = 5) spec_path (module D : Itf.DRIVER) =
  if not (Sys.file_exists spec_path) then
    Error ("spec file not found: " ^ spec_path)
  else
    let trace_path =
      (if Filename.check_suffix spec_path ".qnt"
       then Filename.chop_suffix spec_path ".qnt"
       else spec_path) ^ ".itf.json"
    in
    if not (Sys.file_exists trace_path) then
      Error (Printf.sprintf "trace file not found for simulation: %s" trace_path)
    else
      match Itf.parse_file trace_path with
      | Error e -> Error (Printf.sprintf "simulation: parse error: %s" e)
      | Ok [] -> Error "simulation: no traces in file"
      | Ok (trace :: _) ->
        let rec loop_traces i =
          if i >= traces then Ok ()
          else
            let driver = D.create () in
            let rec loop_steps j = function
              | [] -> loop_traces (i + 1)
              | step :: rest ->
                (match D.step driver step with
                 | Ok () -> loop_steps (j + 1) rest
                 | Error e ->
                   Error (Printf.sprintf
                     "simulation trace %d step %d failed: %s" i j e))
            in
            loop_steps 0 trace
        in
        loop_traces 0

let run_test spec_path test_name (module D : Itf.DRIVER) =
  if not (Sys.file_exists spec_path) then
    Error ("spec file not found: " ^ spec_path)
  else
    let trace_path =
      (if Filename.check_suffix spec_path ".qnt"
       then Filename.chop_suffix spec_path ".qnt"
       else spec_path) ^ ".itf.json"
    in
    if not (Sys.file_exists trace_path) then
      Error (Printf.sprintf "trace file not found for test '%s': %s" test_name trace_path)
    else
      match Itf.parse_file trace_path with
      | Error e -> Error (Printf.sprintf "test '%s': parse error: %s" test_name e)
      | Ok [] -> Error (Printf.sprintf "test '%s': no traces in file" test_name)
      | Ok (trace :: _) ->
        let driver = D.create () in
        let rec loop i = function
          | [] -> Ok ()
          | step :: rest ->
            (match D.step driver step with
             | Ok () -> loop (i + 1) rest
             | Error e ->
               Error (Printf.sprintf "test '%s' step %d failed: %s" test_name i e))
        in
        loop 0 trace
