type empty = |

let never_returns : empty -> 'a = function _ -> .

let die =
  Printf.ksprintf (fun s ->
      prerr_endline s;
      (exit 1 :> empty))

let run_command cmd args =
  let r = Sys.command (Filename.quote_command cmd args) in
  if r <> 0 then never_returns (die "Command failed: %s" cmd)

let () =
  if Array.length Sys.argv <> 2 then
    never_returns (die "Usage: %s <input_file>" Sys.argv.(0));
  let input_file = Sys.argv.(1) in
  let cmt_file, odoc_file =
    let base = Filename.remove_extension input_file in
    match Filename.extension input_file with
    | ".ml" -> (base ^ ".cmt", base ^ ".odoc")
    | ".mli" -> (base ^ ".cmti", base ^ ".odoc")
    | e ->
        never_returns
          (die "Don't know what to do with file with extension '%s'" e)
  in
  run_command "ocamlc" [ "-bin-annot"; "-c"; input_file ];
  run_command "odoc" [ "compile"; "--package"; "test"; cmt_file ];
  run_command "odoc" [ "link"; odoc_file ]
