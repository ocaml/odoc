(** Expect the output of [odoc] on standard input. Called like that, Odoc will
    output the list of subcommands. *)

open Astring

let gen_preamble _cmds =
  Printf.printf "{0 Odoc}\n\nOdoc is made of several sub-commands."

let gen_subcommand cmd =
  Printf.printf "\n{1 odoc %s}\n\n{@man[\n%!" cmd;
  ignore (Sys.command (Filename.quote_command "odoc" [ cmd; "--help" ]));
  Printf.printf "]}\n"

let () =
  let line0 = input_line stdin in
  let subcommands =
    match String.cut ~sep:":" line0 with
    | Some (_, s) -> String.cuts ~sep:"," s |> List.map String.trim
    | None -> assert false
  in
  gen_preamble subcommands;
  List.iter gen_subcommand subcommands
