open Bos

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

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

let mkdir_p d =
  let segs =
    Fpath.segs (Fpath.normalize d) |> List.filter (fun s -> String.length s > 0)
  in
  let _ =
    List.fold_left
      (fun path seg ->
        let d = Fpath.(path // v seg) in
        try
          Unix.mkdir (Fpath.to_string d) 0o755;
          d
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> d
        | exn -> raise exn)
      (Fpath.v ".") segs
  in
  ()

let write_file filename lines =
  let dir = fst (Fpath.split_base filename) in
  mkdir_p dir;
  let oc = open_out (Fpath.to_string filename) in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) lines;
  close_out oc

let cp src dst = assert (lines_of_process Cmd.(v "cp" % src % dst) = [])
