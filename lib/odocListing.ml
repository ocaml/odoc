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
open Tyxml.Html
open DocOckHtml

class listing_page_creator ~name ~global content =
  let path = if global then [] else [ name ] in
  object
    inherit Html_tree.page_creator ~path content

    val! has_parent = not global

    method! title_string = name

    method! heading =
      if global then [
        Markup.keyword "Packages"
      ] else [
        Markup.keyword "Package " ;
        a ~a:[ a_href ("../#/" ^ name)] [ pcdata name ]
      ]
  end

let mk_html_list lst =
  ul @@ List.map lst ~f:(fun s -> li [ a ~a:[ a_href s ] [ pcdata s ] ])

let for_package ~root_dir ~pkg_name units =
  let pkg_dir = OdocFs.Directory.create ~parent:root_dir ~name:pkg_name in
  let output  = OdocFs.File.create ~directory:pkg_dir ~name:"index.html" in (* TODO: allow different names *)
  let oc = open_out (OdocFs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let content = mk_html_list units in
  let listing = new listing_page_creator ~name:pkg_name ~global:false content in
  Format.fprintf fmt "%a" (Tyxml.Html.pp ()) listing#html;
  close_out oc

let global ~root_dir packages =
  let output  = OdocFs.File.create ~directory:root_dir ~name:"index.html" in (* TODO: allow different names *)
  let oc = open_out (OdocFs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let c = mk_html_list packages in
  let listing = new listing_page_creator ~name:"Package list" ~global:true c in
  Format.fprintf fmt "%a" (Tyxml.Html.pp ()) listing#html;
  close_out oc
