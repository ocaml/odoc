(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOck

exception Bad_string of Xmlm.pos

let root_fold = {DocOckXmlFold.f = fun f a s -> f a (`Data s)}

let input_string input =
  let pos = Xmlm.pos input in
    match Xmlm.input input with
    | `Data s -> s
    | _ -> raise (Bad_string pos)

let parser = DocOckXmlParse.build input_string

exception Bad_lookup

exception Bad_fetch of string

let test silent cmti =
  match read_cmti (fun _ _ -> cmti) cmti with
  | Not_an_interface ->
      prerr_endline (cmti ^ ": not an interface");
      1
  | Wrong_version ->
      prerr_endline (cmti ^ ": wrong OCaml version");
      1
  | Corrupted ->
      prerr_endline (cmti ^ ": corrupted");
      1
  | Not_a_typedtree ->
      prerr_endline (cmti ^ ": not a typedtree");
      1
  | Not_an_implementation ->
      prerr_endline (cmti ^ ": not an implementation");
      1
  | exception Bad_string (line, column) ->
      prerr_endline
        (cmti ^ ":"
         ^ (string_of_int line) ^ "." ^ (string_of_int column)
         ^ ": expected string");
      1
  | Ok intf ->
      let lookup u s =
        if u != intf then raise Bad_lookup;
        if s = cmti then Found { root = cmti; hidden = false }
        else Not_found
      in
      let fetch s =
        if s = cmti then intf
        else raise (Bad_fetch s)
      in
      let resolver = build_resolver lookup fetch in
        try
          let intf = resolve resolver intf in
          let buf = Buffer.create 1024 in
          let output = Xmlm.make_output (`Buffer buf) in
          let fold_file = (DocOckXmlFold.file root_fold).DocOckXmlFold.f in
          fold_file (fun () signal -> Xmlm.output output signal) () intf;
          if not silent then begin
            Buffer.output_buffer stdout buf;
            print_newline ();
          end;
          let input = Xmlm.make_input (`String(0, Buffer.contents buf)) in
          match DocOckXmlParse.file parser input with
          | DocOckXmlParse.Error(start, (fline, fcolumn), msg) ->
              let start =
                match start with
                | None -> ""
                | Some (sline, scolumn) ->
                    let sline = string_of_int sline in
                    let scolumn = string_of_int scolumn in
                      ":" ^ sline ^ "." ^ scolumn
              in
              let finish =
                let fline = string_of_int fline in
                let fcolumn = string_of_int fcolumn in
                  ":" ^ fline ^ "." ^ fcolumn
              in
                prerr_endline (cmti ^ start ^ finish ^ ": " ^ msg);
                1
          | DocOckXmlParse.Ok intf2 ->
              let buf2 = Buffer.create 1024 in
              let output2 = Xmlm.make_output (`Buffer buf2) in
              fold_file (fun () signal -> Xmlm.output output2 signal) () intf2;
              if Buffer.contents buf <> Buffer.contents buf2 then begin
                prerr_endline (cmti ^ ": parsing does not match printing");
                Buffer.output_buffer stderr buf2;
                prerr_newline ();
                1
              end else 0
        with
        | Bad_lookup->
            prerr_endline (cmti ^ ": bad lookup during resolution");
            1
        | Bad_fetch s ->
            prerr_endline (cmti ^ ": bad fetch of " ^ s ^ " during resolution");
            1

let main () =
  let silent = ref false in
  let cmtis = ref [] in
  let add_cmt cmti = cmtis := cmti :: !cmtis in
  let args = ["-s", Arg.Set silent, "Do not output the XML" ] in
  Arg.parse args add_cmt "Test XML parser and printer on cmti files";
  exit @@
  List.fold_left (fun acc cmti -> acc + test !silent cmti) 0 (List.rev !cmtis)

let () = main ()
