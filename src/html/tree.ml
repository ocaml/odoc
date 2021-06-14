(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

module Html = Tyxml.Html

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

let page_creator ?(theme_uri = Relative None) ?(support_uri = Relative None)
    ~url name header toc content =
  let path = Link.Path.for_printing url in

  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let file_uri base file =
      match base with
      | Absolute uri -> uri ^ "/" ^ file
      | Relative uri ->
          let page =
            Odoc_document.Url.Path.{ kind = "file"; parent = uri; name = file }
          in
          Link.href ~resolve:(Current url)
            Odoc_document.Url.Anchor.{ page; anchor = ""; kind = "file" }
    in

    let odoc_css_uri = file_uri theme_uri "odoc.css" in
    let highlight_js_uri = file_uri support_uri "highlight.pack.js" in

    Html.head
      (Html.title (Html.txt title_string))
      [
        Html.link ~rel:[ `Stylesheet ] ~href:odoc_css_uri ();
        Html.meta ~a:[ Html.a_charset "utf-8" ] ();
        Html.meta
          ~a:[ Html.a_name "generator"; Html.a_content "odoc %%VERSION%%" ]
          ();
        Html.meta
          ~a:
            [
              Html.a_name "viewport";
              Html.a_content "width=device-width,initial-scale=1.0";
            ]
          ();
        Html.script ~a:[ Html.a_src highlight_js_uri ] (Html.txt "");
        Html.script (Html.txt "hljs.initHighlightingOnLoad();");
      ]
  in

  let breadcrumbs =
    let rec get_parents x =
      match x with
      | [] -> []
      | x :: xs -> (
          match Link.Path.of_list (List.rev (x :: xs)) with
          | Some x -> x :: get_parents xs
          | None -> get_parents xs)
    in
    let parents = get_parents (List.rev (Link.Path.to_list url)) |> List.rev in
    let has_parent = List.length parents > 1 in
    let href page =
      Link.href ~resolve:(Current url)
        Odoc_document.Url.Anchor.{ page; anchor = ""; kind = "" }
    in
    if has_parent then
      let up_url = List.hd (List.tl (List.rev parents)) in
      let l =
        [
          Html.a ~a:[ Html.a_href (href up_url) ] [ Html.txt "Up" ];
          Html.txt " – ";
        ]
        @
        (* Create breadcrumbs *)
        let space = Html.txt " " in
        parents
        |> Utils.list_concat_map
             ?sep:(Some [ space; Html.entity "#x00BB"; space ])
             ~f:(fun url' ->
               [
                 [
                   (if url = url' then Html.txt url.name
                   else
                     Html.a
                       ~a:[ Html.a_href (href url') ]
                       [ Html.txt url'.name ]);
                 ];
               ])
        |> List.flatten
      in
      [ Html.nav ~a:[ Html.a_class [ "odoc-nav" ] ] l ]
    else []
  in

  let body =
    breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in
  Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body)

let make ?theme_uri ?support_uri ~indent ~url ~header ~extra_suffix ~toc title
    content children =
  let filename = Fpath.add_ext extra_suffix (Link.Path.as_filename url) in
  let html =
    page_creator ?theme_uri ?support_uri ~url title header toc content
  in
  let content ppf = (Html.pp ~indent ()) ppf html in
  { Odoc_document.Renderer.filename; content; children }

let open_details = ref true
