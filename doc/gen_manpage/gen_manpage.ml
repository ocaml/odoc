(** Expect the output of [odoc] on standard input. Called like that, Odoc will
    output the list of subcommands. *)

open Astring

let with_process_in cmd args f =
  let inp = Unix.open_process_in (Filename.quote_command cmd args) in
  let finally () = ignore (Unix.close_process_in inp) in
  Fun.protect ~finally (fun () -> f inp)

let cat_command cmd args =
  with_process_in cmd args (fun inp ->
      try
        while true do
          Printf.printf "%s\n" (input_line inp)
        done
      with End_of_file -> ())

type cmd = { name : string; section : string; summary : string }

let section_prefix = "COMMANDS: "

let parse_man' =
  let rec collect acc kind = function
    | (kind', line) :: tl when kind = kind' -> collect (line :: acc) kind tl
    | tl -> (List.rev acc, tl)
  in
  let rec commands ~section = function
    | (`Command, line) :: tl ->
        let name = List.hd (String.fields ~empty:false line) in
        let _, tl = collect [] `Command tl in
        let summary, tl = collect [] `Summary tl in
        { name; section; summary = String.concat ~sep:" " summary }
        :: commands ~section tl
    | tl -> sections tl
  and sections = function
    | (`Section, line) :: tl when String.is_prefix ~affix:section_prefix line ->
        let first = String.length section_prefix in
        let section = String.with_range ~first line in
        commands ~section tl
    | _ :: tl -> sections tl
    | [] -> []
  in
  sections

let parse_man inp =
  let lines = ref [] in
  (try
     while true do
       let line = input_line inp in
       if line = "" then ()
       else
         let kind =
           if String.is_prefix ~affix:"           " line then `Summary
           else if String.is_prefix ~affix:"       " line then `Command
           else `Section
         in
         lines := (kind, String.trim line) :: !lines
     done
   with End_of_file -> ());
  parse_man' (List.rev !lines)

let gen_preamble cmds =
  Printf.printf "{0 Odoc}\n\n{1 odoc}\nOdoc is made of several sub-commands.\n";
  List.iter
    (fun { name; summary; _ } ->
      Printf.printf "- {!\"odoc-%s\"} %s\n" name summary)
    cmds

let gen_subcommand { name; _ } =
  Printf.printf "\n{1 odoc %s}\n\n{@man[\n%!" name;
  cat_command "odoc" [ name; "--help" ];
  Printf.printf "]}\n"

let () =
  let subcommands = with_process_in "odoc" [ "--help" ] parse_man in
  gen_preamble subcommands;
  List.iter gen_subcommand subcommands
