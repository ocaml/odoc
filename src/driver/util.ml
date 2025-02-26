open Bos

let ( >>= ) = Result.bind

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let with_dir dir pat f =
  match dir with
  | None -> OS.Dir.with_tmp pat f () |> Result.get_ok
  | Some dir -> f dir ()

let lines_of_channel ic =
  let rec inner acc =
    try
      let l = input_line ic in
      inner (l :: acc)
    with End_of_file -> List.rev acc
  in
  inner []

let lines_of_process cmd =
  match OS.Cmd.(run_out ~err:err_null cmd |> to_lines) with
  | Ok x -> x
  | Error (`Msg e) -> failwith ("Error: " ^ e)

(** Opens a file for writing and calls [f]. The destination directory is created
    if needed. *)
let with_out_to filename f =
  let filename = Fpath.normalize filename in
  OS.Dir.create (Fpath.parent filename) >>= fun _ ->
  OS.File.with_oc filename
    (fun oc () ->
      f oc;
      Ok ())
    ()
  |> Result.join

let cp src dst = assert (lines_of_process Cmd.(v "cp" % src % dst) = [])
