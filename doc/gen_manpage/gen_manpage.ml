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

type cmd = { name : string; summary : string }

let section_prefix = "COMMANDS: "

let parse_man' =
  let rec collect acc kind = function
    | (kind', line) :: tl when kind = kind' -> collect (line :: acc) kind tl
    | tl -> (List.rev acc, tl)
  in
  let rec commands acc = function
    | (`Command, line) :: tl ->
        let name = List.hd (String.fields ~empty:false line) in
        let _, tl = collect [] `Command tl in
        let summary, tl = collect [] `Summary tl in
        commands ({ name; summary = String.concat ~sep:" " summary } :: acc) tl
    | tl -> (List.rev acc, tl)
  and sections = function
    | (`Section, line) :: tl when String.is_prefix ~affix:section_prefix line ->
        let first = String.length section_prefix in
        let section = String.with_range ~first line in
        let cmds, tl = commands [] tl in
        (section, cmds) :: sections tl
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

open Printf

let gen_preamble sections =
  printf "{0 Odoc}\n\n{1 odoc}\nOdoc is made of several sub-commands.\n";
  List.iter
    (fun (section, cmds) ->
      printf "\n%s:\n\n" section;
      List.iter
        (fun { name; summary; _ } ->
          printf "- {!\"odoc-%s\"} %s\n" name summary)
        cmds)
    sections

let gen_manpages sections =
  List.iter
    (fun (section, cmds) ->
      printf "\n{1 %s}\n" section;
      List.iter
        (fun { name; _ } ->
          printf "\n{2 odoc %s}\n\n{@man[\n%!" name;
          cat_command "odoc" [ name; "--help" ];
          printf "]}\n")
        cmds)
    sections

let () =
  let sections = with_process_in "odoc" [ "--help" ] parse_man in
  gen_preamble sections;
  gen_manpages sections
