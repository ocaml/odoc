let instrument = false

open Bos

let instrument_dir =
  lazy
    (let dir = Fpath.v "landmarks" in
     OS.Dir.delete dir |> Result.get_ok;
     OS.Dir.create dir |> Result.get_ok |> ignore;
     dir)

type executed_command = {
  cmd : string list;
  time : float;  (** Running time in seconds. *)
  output_file : Fpath.t option;
  errors : string;
}

(* Environment variables passed to commands. *)

(* Record the commands executed, their running time and optionally the path to
   the produced file. *)
let commands = ref []

let n = Atomic.make 0

(** Return the list of executed commands where the first argument was [cmd]. *)
let run env cmd output_file =
  let cmd = Bos.Cmd.to_list cmd in
  let myn = Atomic.fetch_and_add n 1 in
  Logs.debug (fun m -> m "%d - Executing: %s" myn (String.concat " " cmd));
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let t_start = Unix.gettimeofday () in
  let env =
    let env = OS.Env.current () |> Result.get_ok in
    env
  in
  let env =
    Astring.String.Map.fold
      (fun k v env -> Astring.String.concat [ k; "="; v ] :: env)
      env []
    |> Array.of_list
  in
  (* Logs.debug (fun m -> m "Running cmd %a" Fmt.(list ~sep:sp string) cmd); *)
  let r, errors =
    Eio.Switch.run ~name:"Process.parse_out" @@ fun sw ->
    let r, w = Eio.Process.pipe proc_mgr ~sw in
    let re, we = Eio.Process.pipe proc_mgr ~sw in
    try
      let child =
        Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:we ~env cmd
      in
      Eio.Flow.close w;
      Eio.Flow.close we;
      let output, err =
        Eio.Fiber.pair
          (fun () ->
            Eio.Buf_read.parse_exn Eio.Buf_read.take_all r ~max_size:max_int)
          (fun () ->
            Eio.Buf_read.parse_exn Eio.Buf_read.take_all re ~max_size:max_int)
      in
      Eio.Flow.close r;
      Eio.Flow.close re;
      match Eio.Process.await child with
      | `Exited 0 -> (output, err)
      | `Exited n ->
          Logs.err (fun m -> m "%d - Process exitted %d: stderr=%s" myn n err);
          failwith "Error"
      | `Signaled n ->
          Logs.err (fun m -> m "%d - Signalled %d: stderr=%s" myn n err);
          failwith ("Signaled " ^ string_of_int n)
    with Eio.Exn.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "%d - running command: %a" myn
        Eio.Process.pp_args cmd
  in
  (* Logs.debug (fun m ->
      m "Finished running cmd %a" Fmt.(list ~sep:sp string) cmd); *)
  let t_end = Unix.gettimeofday () in
  let r = String.split_on_char '\n' r in
  let time = t_end -. t_start in
  commands := { cmd; time; output_file; errors } :: !commands;
  r

(** Print an executed command and its time. *)

let filter_commands cmd =
  match
    List.filter
      (fun c -> match c.cmd with _ :: cmd' :: _ -> cmd = cmd' | _ -> false)
      !commands
  with
  | [] -> []
  | _ :: _ as cmds -> cmds

let print_cmd c =
  Printf.printf "[%4.2f] $ %s\n" c.time (String.concat " " c.cmd)

(** Returns the [k] commands that took the most time for a given subcommand. *)
let k_longest_commands cmd k =
  filter_commands cmd
  |> List.sort (fun a b -> Float.compare b.time a.time)
  |> List.filteri (fun i _ -> i < k)
