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

(** Return the list of executed commands where the first argument was [cmd]. *)
let run env cmd output_file =
  let cmd = Bos.Cmd.to_list cmd in
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
  let stderr = Buffer.create 1024 in
  let err = Eio.Flow.buffer_sink stderr in
  (* Logs.debug (fun m -> m "Running cmd %a" Fmt.(list ~sep:sp string) cmd); *)
  let r =
    Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all ~env ~stderr:err cmd
  in
  (* Logs.debug (fun m ->
      m "Finished running cmd %a" Fmt.(list ~sep:sp string) cmd); *)
  let t_end = Unix.gettimeofday () in
  let r = String.split_on_char '\n' r in
  let time = t_end -. t_start in
  let errors = Buffer.contents stderr in
  commands := { cmd; time; output_file; errors } :: !commands;
  r

(** Print an executed command and its time. *)

let filter_commands cmd =
  match
    List.filter
      (fun c -> match c.cmd with _ :: cmd' :: _ -> cmd = cmd' | _ -> false)
      !commands
  with
  | [] -> failwith ("No commands run for " ^ cmd)
  | _ :: _ as cmds -> cmds

let print_cmd c =
  Printf.printf "[%4.2f] $ %s\n" c.time (String.concat " " c.cmd)

(** Returns the [k] commands that took the most time for a given subcommand. *)
let k_longest_commands cmd k =
  filter_commands cmd
  |> List.sort (fun a b -> Float.compare b.time a.time)
  |> List.filteri (fun i _ -> i < k)
