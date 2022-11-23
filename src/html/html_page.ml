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

let html_of_toc toc =
  let open Types in
  let rec section (section : toc) =
    let link = Html.a ~a:[ Html.a_href section.href ] section.title in
    match section.children with [] -> [ link ] | cs -> [ link; sections cs ]
  and sections the_sections =
    the_sections
    |> List.map (fun the_section -> Html.li (section the_section))
    |> Html.ul
  in
  match toc with
  | [] -> []
  | _ -> [ Html.nav ~a:[ Html.a_class [ "odoc-toc" ] ] [ sections toc ] ]

let html_of_breadcrumbs (breadcrumbs : Types.breadcrumb list) =
  let make_navigation ~up_url rest =
    [
      Html.nav
        ~a:[ Html.a_class [ "odoc-nav" ] ]
        ([ Html.a ~a:[ Html.a_href up_url ] [ Html.txt "Up" ]; Html.txt " – " ]
        @ rest);
    ]
  in
  match List.rev breadcrumbs with
  | [] -> [] (* Can't happen - there's always the current page's breadcrumb. *)
  | [ _ ] -> [] (* No parents *)
  | [ { name = "index"; _ }; x ] ->
      (* Special case leaf pages called 'index' with one parent. This is for files called
          index.mld that would otherwise clash with their parent. In particular,
          dune and odig both cause this situation right now. *)
      let up_url = "../index.html" in
      let parent_name = x.name in
      make_navigation ~up_url [ Html.txt parent_name ]
  | current :: up :: bs ->
      let space = Html.txt " " in
      let sep = [ space; Html.entity "#x00BB"; space ] in
      let html =
        (* Create breadcrumbs *)
        Utils.list_concat_map ?sep:(Some sep)
          ~f:(fun (breadcrumb : Types.breadcrumb) ->
            [
              [
                Html.a
                  ~a:[ Html.a_href breadcrumb.href ]
                  [ Html.txt breadcrumb.name ];
              ];
            ])
          (up :: bs)
        |> List.flatten
      in
      make_navigation ~up_url:up.href
        (List.rev html @ sep @ [ Html.txt current.name ])

let page_creator ~config ~url ~uses_katex header breadcrumbs toc content =
  let theme_uri = Config.theme_uri config in
  let support_uri = Config.support_uri config in
  let path = Link.Path.for_printing url in

  let head : Html_types.head Html.elt =
    let title_string =
      Printf.sprintf "%s (%s)" url.name (String.concat "." path)
    in

    let file_uri base file =
      match base with
      | Types.Absolute uri -> uri ^ "/" ^ file
      | Relative uri ->
          let page =
            Odoc_document.Url.Path.{ kind = `File; parent = uri; name = file }
          in
          Link.href ~config ~resolve:(Current url)
            (Odoc_document.Url.from_path page)
    in

    let odoc_css_uri = file_uri theme_uri "odoc.css" in
    let highlight_js_uri = file_uri support_uri "highlight.pack.js" in
    let default_meta_elements =
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
    let meta_elements =
      if uses_katex then
        let katex_css_uri = file_uri theme_uri "katex.min.css" in
        let katex_js_uri = file_uri support_uri "katex.min.js" in
        default_meta_elements
        @ [
            Html.link ~rel:[ `Stylesheet ] ~href:katex_css_uri ();
            Html.script ~a:[ Html.a_src katex_js_uri ] (Html.txt "");
            Html.script
              (Html.cdata_script
                 {|
          document.addEventListener("DOMContentLoaded", function () {
            var elements = Array.from(document.getElementsByClassName("odoc-katex-math"));
            for (var i = 0; i < elements.length; i++) {
              var el = elements[i];
              var content = el.textContent;
              var new_el = document.createElement("span");
              new_el.setAttribute("class", "odoc-katex-math-rendered");
              var display = el.classList.contains("display");
              katex.render(content, new_el, { throwOnError: false, displayMode: display });
              el.replaceWith(new_el);
            }
          });
        |});
          ]
      else default_meta_elements
    in
    Html.head (Html.title (Html.txt title_string)) meta_elements
  in

  let body =
    html_of_breadcrumbs breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ html_of_toc toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in
  let htmlpp = Html.pp ~indent:(Config.indent config) () in
  let html = Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body) in
  let content ppf = htmlpp ppf html in
  content

let make ~config ~url ~header ~breadcrumbs ~toc ~uses_katex content children =
  let filename = Link.Path.as_filename ~is_flat:(Config.flat config) url in
  let content =
    page_creator ~config ~url ~uses_katex header breadcrumbs toc content
  in
  [ { Odoc_document.Renderer.filename; content; children } ]
