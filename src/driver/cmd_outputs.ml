type log_dest =
  [ `Compile
  | `Link
  | `Count_occurrences
  | `Generate
  | `Index
  | `Sherlodoc
  | `Classify ]

type log_line = { log_dest : log_dest; prefix : string; run : Run.t }

let outputs : log_line list ref = ref []

let maybe_log log_dest run =
  match log_dest with
  | Some (log_dest, prefix) ->
      outputs := !outputs @ [ { log_dest; run; prefix } ]
  | None -> ()

let submit log_dest desc cmd output_file =
  match Worker_pool.submit desc cmd output_file with
  | Ok x ->
      maybe_log log_dest x;
      String.split_on_char '\n' x.output
  | Error exn -> raise exn

let submit_ignore_failures log_dest desc cmd output_file =
  match Worker_pool.submit desc cmd output_file with
  | Ok x ->
      maybe_log log_dest x;
      ()
  | Error exn ->
      Logs.err (fun m -> m "Error: %s" (Printexc.to_string exn));
      ()

type action = {
  log : (log_dest * string) option;
  desc : string;
  cmd : Bos.Cmd.t;
  output : Fpath.t option;
  ignore_failures : bool;
}

let v ?log ?output ?(ignore_failures = false) ~desc cmd =
  { log; desc; cmd; output; ignore_failures }

let run { log; desc; cmd; output; ignore_failures } =
  if ignore_failures then submit_ignore_failures log desc cmd output
  else ignore (submit log desc cmd output)
