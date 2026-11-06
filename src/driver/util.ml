open Bos

let ( >>= ) = Result.bind

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let with_dir dir pat f =
  match dir with
  | None -> OS.Dir.with_tmp pat f () |> Result.get_ok
  | Some dir -> f dir ()

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

let memo ~key f =
  let started = Hashtbl.create 100 in
  let rec g x =
    let k = key x in
    match Hashtbl.find_opt started k with
    | Some p -> Eio.Promise.await_exn p
    | None ->
        let p, r = Eio.Promise.create () in
        Hashtbl.add started k p;
        (match f g x with
        | y -> Eio.Promise.resolve_ok r y
        | exception e -> Eio.Promise.resolve_error r e);
        Eio.Promise.await_exn p
  in
  g
