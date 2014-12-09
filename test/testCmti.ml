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

let output_string output s =
  Xmlm.output output (`Data s)

let printer = DocOckXmlPrint.build output_string

let input_string input =
  let pos = Xmlm.pos input in
    match Xmlm.input input with
    | `Data s -> s
    | _ -> raise (Bad_string pos)

let parser = DocOckXmlParse.build input_string

exception Bad_lookup

exception Bad_fetch of string

let test cmti =
  match read_cmti cmti cmti with
  | Not_an_interface ->
      prerr_endline (cmti ^ ": not an interface");
      1
  | Wrong_version_interface ->
      prerr_endline (cmti ^ ": interface has wrong OCaml version");
      1
  | Corrupted_interface ->
      prerr_endline (cmti ^ ": corrupted interface");
      1
  | Not_a_typedtree ->
      prerr_endline (cmti ^ ": not a typedtree");
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
        if s = cmti then Some cmti
        else None
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
          DocOckXmlPrint.file printer output intf;
          Buffer.output_buffer stdout buf;
          print_newline ();
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
              DocOckXmlPrint.file printer output2 intf2;
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
  let code = ref 0 in
  let test cmti =
    code := !code + (test cmti)
  in
    Arg.parse [] test "Test XML parser and printer on cmti files";
    exit !code

let () = main ()
