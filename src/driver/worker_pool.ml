(* Worker pool *)
open Eio

type request = {
  description : string;
  request : Bos.Cmd.t;
  output_file : Fpath.t option;
}

type response = (Run.t, exn) result
type resolver = response Eio.Promise.u

type t = (request * resolver) Eio.Stream.t

let stream : t = Eio.Stream.create 0

let handle_job env request output_file = Run.run env request output_file

exception Worker_failure of Run.t

let rec run_worker env id : unit =
  let { request; output_file; description }, reply = Eio.Stream.take stream in
  Atomic.incr Stats.stats.processes;
  Atomic.set Stats.stats.process_activity.(id) description;
  (try
     let result = handle_job env request output_file in
     match result.status with
     | `Exited 0 ->
         Atomic.decr Stats.stats.processes;
         Atomic.set Stats.stats.process_activity.(id) "idle";
         Promise.resolve reply (Ok result)
     | _ -> Promise.resolve_error reply (Worker_failure result)
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
  for i = 0 to n - 1 do
    spawn_worker i
  done;
  ()
