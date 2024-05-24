(* Worker pool *)
open Eio

let stream : ((string * Bos.Cmd.t * Fpath.t option) * _) Eio.Stream.t =
  Eio.Stream.create 0

let handle_job env request output_file = Run.run env request output_file

let rec run_worker env id : unit =
  let (_, request, output_file), reply = Eio.Stream.take stream in
  Atomic.incr Stats.stats.processes;
  (try
     let result = handle_job env request output_file in
     Atomic.decr Stats.stats.processes;
     Promise.resolve reply (Ok result)
   with e -> Promise.resolve_error reply e);
  run_worker env id

let submit desc request output_file =
  let reply, resolve_reply = Promise.create () in
  Eio.Stream.add stream ((desc, request, output_file), resolve_reply);
  Promise.await reply

let start_workers env sw n =
  let spawn_worker name =
    Fiber.fork_daemon ~sw (fun () ->
        try
          run_worker env name;
          `Stop_daemon
        with Stdlib.Exit -> `Stop_daemon)
  in
  for i = 1 to n do
    spawn_worker (Printf.sprintf "%d" i)
  done;
  stream
