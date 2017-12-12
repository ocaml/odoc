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



type t = Model.Lang.Page.t

let root (t : Model.Lang.Page.t) =
  match t.Model.Lang.Page.name with
  | Model.Paths.Identifier.Page (root, _) -> root

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
