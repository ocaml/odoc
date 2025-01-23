let instrument = false

open Bos

let instrument_dir =
  lazy
    (let dir = Fpath.v "landmarks" in
     OS.Dir.delete dir |> Result.get_ok;
     OS.Dir.create dir |> Result.get_ok |> ignore;
     dir)

type t = {
  cmd : string list;
  time : float;  (** Running time in seconds. *)
  output_file : Fpath.t option;
  output : string;
  errors : string;
  status : [ `Exited of int | `Signaled of int ];
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
  let output, errors, status =
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
      let status = Eio.Process.await child in
      (output, err, status)
    with Eio.Exn.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "%d - running command: %a" myn
        Eio.Process.pp_args cmd
  in
  (* Logs.debug (fun m ->
      m "Finished running cmd %a" Fmt.(list ~sep:sp string) cmd); *)
  let t_end = Unix.gettimeofday () in
  let time = t_end -. t_start in
  let result = { cmd; time; output_file; output; errors; status } in
  commands := result :: !commands;
  (match result.status with
  | `Exited 0 -> ()
  | _ ->
      let verb, n =
        match result.status with
        | `Exited n -> ("exited", n)
        | `Signaled n -> ("signaled", n)
      in
      Logs.err (fun m ->
          m
            "@[<2>Process %s with %d:@ '@[%a'@]@]@\n\n\
             Stdout:\n\
             %s\n\n\
             Stderr:\n\
             %s"
            verb n
            Fmt.(list ~sep:sp string)
            result.cmd result.output result.errors));
  result

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
