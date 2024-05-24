(* ocamlobjinfo *)

open Bos
let ocamlobjinfo = Cmd.v "ocamlobjinfo"

let source_possibilities file =
  let default = [ file ] in
  let generated =
    if Astring.String.is_suffix ~affix:"-gen" file then
      let pos = String.length file - 4 in
      [ Astring.String.take ~max:pos file ]
    else []
  in
  let pp =
    if Astring.String.is_suffix ~affix:".pp.ml" file then
      let pos = String.length file - 5 in
      [ Astring.String.take ~max:pos file ^ "ml" ]
    else []
  in
  default @ generated @ pp

let get_source file =
  let cmd = Cmd.(ocamlobjinfo % p file) in
  let lines_res =
    Worker_pool.submit ("Ocamlobjinfo " ^ Fpath.to_string file) cmd None
  in
  let lines =
    match lines_res with
    | Ok l -> l
    | Error e ->
        Logs.err (fun m ->
            m "Error finding source for module %a: %s" Fpath.pp file
              (Printexc.to_string e));
        []
  in
  let f =
    List.filter_map
      (fun line ->
        let affix = "Source file: " in
        if Astring.String.is_prefix ~affix line then
          let name =
            String.sub line (String.length affix)
              (String.length line - String.length affix)
          in
          let name = Fpath.(filename (v name)) in
          let dir, _ = Fpath.split_base file in
          let possibilities =
            List.map
              (fun poss -> Fpath.(dir / poss))
              (source_possibilities name)
          in
          List.find_opt
            (fun f -> Sys.file_exists (Fpath.to_string f))
            possibilities
        else None)
      lines
  in
  match f with
  | [] -> None
  | x :: _ :: _ ->
      Logs.warn (fun m -> m "Multiple source files found for %a" Fpath.pp file);
      Some x
  | x :: _ -> Some x
