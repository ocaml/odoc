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
  pp @ default @ generated

let get_source file srcdirs =
  let cmd = Cmd.(ocamlobjinfo % p file) in
  let lines_res =
    Worker_pool.submit ("Ocamlobjinfo " ^ Fpath.to_string file) cmd None
  in
  let lines =
    match lines_res with
    | Ok l -> String.split_on_char '\n' l.output
    | Error e ->
        Logs.err (fun m ->
            m "Error finding source for module %a: %s" Fpath.pp file
              (Printexc.to_string e));
        []
  in
  let f =
    List.filter_map
      (fun line ->
        match Astring.String.cut ~sep:"Source file: " line with
        | Some ("", name) ->
            let name = Fpath.(filename (v name)) in
            let possibilities =
              List.concat_map
                (fun dir ->
                  List.map
                    (fun poss -> Fpath.(dir / poss))
                    (source_possibilities name))
                srcdirs
            in
            List.find_opt
              (fun f ->
                Logs.debug (fun m -> m "src: checking %a" Fpath.pp f);
                Sys.file_exists (Fpath.to_string f))
              possibilities
        | _ -> None)
      lines
  in
  match f with
  | [] -> None
  | x :: _ :: _ ->
      Logs.warn (fun m -> m "Multiple source files found for %a" Fpath.pp file);
      Some x
  | x :: _ -> Some x
