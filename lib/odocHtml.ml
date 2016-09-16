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

open DocOckHtml

module Env = OdocEnv
module Unit = OdocUnit
module Root = OdocRoot
module Fs = OdocFs

let unit ~env ~output:root_dir input =
  let unit = Unit.load input in
  let env = Env.build env unit in
  let odoctree =
    DocOck.resolve (Env.resolver env) unit
    |> DocOck.expand (Env.expander env)
  in
  let get_package root = Root.Package.to_string (Root.package root) in
  let pkg_dir =
    let pkg_name = get_package (Unit.root unit) in
    Fs.Directory.reach_from ~dir:root_dir pkg_name
  in
  let pages = To_html_tree.unit ~get_package odoctree in
  Html_tree.traverse pages ~f:(fun ~parents name content ->
    let directory =
      let dir =
        List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
          parents ~init:pkg_dir
      in
      Fs.Directory.reach_from ~dir name
    in
    let oc =
      Fs.Directory.mkdir_p directory;
      let file = Fs.File.create ~directory ~name:"index.html" in
      open_out (Fs.File.to_string file)
    in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a" (Tyxml.Html.pp ()) content;
    close_out oc
  )
