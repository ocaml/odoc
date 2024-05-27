(* Worker pool *)
open Eio

type request = {
  description : string;
  request : Bos.Cmd.t;
  output_file : Fpath.t option;
}

type response = (string list, exn) result
type resolver = response Eio.Promise.u

type t = (request * resolver) Eio.Stream.t

let stream : t = Eio.Stream.create 0

let handle_job env request output_file = Run.run env request output_file

let rec run_worker env id : unit =
  let { request; output_file; description = _ }, reply =
    Eio.Stream.take stream
  in
  Atomic.incr Stats.stats.processes;
  (try
     let result = handle_job env request output_file in
     Atomic.decr Stats.stats.processes;
     Promise.resolve reply (Ok result)
   with e -> Promise.resolve_error reply e);
  run_worker env id

let submit description request output_file =
  let reply, resolve_reply = Promise.create () in
  Eio.Stream.add stream ({ description; request; output_file }, resolve_reply);
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
  ()
