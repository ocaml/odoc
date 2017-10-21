(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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

type t = Root.t Types.Page.t

let root (t : Root.t Types.Page.t) =
  match t.Types.Page.name with
  | Paths.Identifier.Page (root, _) -> root

let save_xml file page =
  let xml_folder = DocOckXmlFold.file_page Root.Xml.fold in
  Fs.Directory.mkdir_p (Fs.File.dirname file);
  let oc = open_out (Fs.File.to_string file) in
  let output = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel oc) in
  xml_folder.DocOckXmlFold.f (fun () -> Xmlm.output output) () page;
  close_out oc

let load_xml file =
  let xml_parser = DocOckXmlParse.build Root.Xml.parse in
  let ic = open_in (Fs.File.to_string file) in
  let input = Xmlm.make_input (`Channel ic) in
  let result = DocOckXmlParse.page_file xml_parser input in
  close_in ic;
  match result with
  | DocOckXmlParse.Ok page -> page
  | DocOckXmlParse.Error (_, (line, col), error_msg) ->
    let msg = Printf.sprintf "File %s, Line %d, column %d: %s"
                (Fs.File.to_string file) line col error_msg in
    failwith msg

let save file t =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"page-" base then
      file
    else
      Fs.File.create ~directory:dir ~name:("page-" ^ base)
  in
  Fs.Directory.mkdir_p dir;
  let oc = open_out (Fs.File.to_string file) in
  Root.save oc (root t);
  Marshal.to_channel oc t [];
  close_out oc

let load =
  let pages = Hashtbl.create 23 (* because. *) in
  fun file ->
    let file = Fs.File.to_string file in
    match Hashtbl.find pages file with
    | page -> page
    | exception Not_found ->
      try
        let ic = open_in file in
        let _root = Root.load file ic in
        let res = Marshal.from_channel ic in
        close_in ic;
        Hashtbl.add pages file res;
        res
      with exn ->
        Printf.eprintf "Error while unmarshalling %S: %s\n%!" file
          (match exn with
           | Failure s -> s
           | _ -> Printexc.to_string exn);
        exit 2
