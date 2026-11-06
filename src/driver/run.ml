open Bos

type t = {
  cmd : string list;
  time : float;  (** Running time in seconds. *)
  output_file : Fpath.t option;
  output : string;
  errors : string;
  status : [ `Exited of int | `Signaled of int ];
}

(* Record the commands executed, their running time and optionally the path to
   the produced file. *)
let commands = ref []

let n = Atomic.make 0

let run env cmd output_file =
  let cmd = Bos.Cmd.to_list cmd in
  let myn = Atomic.fetch_and_add n 1 in
  Logs.debug (fun m -> m "%d - Executing: %s" myn (String.concat " " cmd));
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let t_start = Unix.gettimeofday () in
  let env =
    Astring.String.Map.fold
      (fun k v env -> Astring.String.concat [ k; "="; v ] :: env)
      (OS.Env.current () |> Result.get_ok)
      []
    |> Array.of_list
  in
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

let filter_commands cmd =
  List.filter
    (fun c -> match c.cmd with _ :: cmd' :: _ -> cmd = cmd' | _ -> false)
    !commands
