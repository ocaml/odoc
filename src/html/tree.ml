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
            Odoc_document.Url.Path.{ kind = `File; parent = uri; name = file }
          in
          Link.href ~resolve:(Current url) (Odoc_document.Url.from_path page)
    in

    let odoc_css_uri = file_uri theme_uri "odoc.css" in
    let highlight_js_uri = file_uri support_uri "highlight.pack.js" in
    let katex_css_uri = {|https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css" integrity="sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ|} in
    let katex_js_uri = {|https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.js" integrity="sha384-VQ8d8WVFw0yHhCk5E8I86oOhv48xLpnDZx5T9GogA/Y84DcCKWXDmSDfn13bzFZY|} in

    Html.head
      (Html.title (Html.txt title_string))
      [
        Html.link ~rel:[ `Stylesheet ] ~href:odoc_css_uri ();
        Html.link ~rel:[ `Stylesheet ] ~href:katex_css_uri ();
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
        Html.script ~a:[ Html.a_src katex_js_uri ] (Html.txt "");
        (* Unsafe is necessary to avoid escaping parts of the following code. *)
        Html.script
          (Html.Unsafe.data
             {|
          document.addEventListener("DOMContentLoaded", function () {
            var elements = document.getElementsByClassName("odoc-katex-math");
            for (var i = 0; i < elements.length; i++) {
              var el = elements[i];
              var content = el.textContent;
              var new_el = document.createElement("span");
              new_el.setAttribute("class", "odoc-katex-math-rendered");
              katex.render(content, new_el, { throwOnError: false });
              el.replaceWith(new_el);
            }
          });
        |});
      ]
  in

  let breadcrumbs =
    let rec get_parents x =
      match x with
      | [] -> []
      | x :: xs -> (
          match Odoc_document.Url.Path.of_list (List.rev (x :: xs)) with
          | Some x -> x :: get_parents xs
          | None -> get_parents xs)
    in
    let parents =
      get_parents (List.rev (Odoc_document.Url.Path.to_list url)) |> List.rev
    in
    let href page =
      Link.href ~resolve:(Current url) (Odoc_document.Url.from_path page)
    in
    let make_navigation ~up_url breadcrumbs =
      [
        Html.nav
          ~a:[ Html.a_class [ "odoc-nav" ] ]
          ([
             Html.a ~a:[ Html.a_href up_url ] [ Html.txt "Up" ]; Html.txt " – ";
           ]
          @ breadcrumbs);
      ]
    in
    match parents with
    | [] -> [] (* Can't happen - Url.Path.to_list returns a non-empty list *)
    | [ _ ] -> [] (* No parents *)
    | [ x; { name = "index"; _ } ] ->
        (* Special case leaf pages called 'index' with one parent. This is for files called
           index.mld that would otherwise clash with their parent. In particular,
           dune and odig both cause this situation right now. *)
        let up_url = "../index.html" in
        let parent_name = x.name in
        make_navigation ~up_url [ Html.txt parent_name ]
    | _ ->
        let up_url = href (List.hd (List.tl (List.rev parents))) in
        let l =
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
        make_navigation ~up_url l
  in

  let body =
    breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in
  Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body)

let make ?theme_uri ?support_uri ~indent ~url ~header ~toc title content
    children =
  let filename = Link.Path.as_filename url in
  let html =
    page_creator ?theme_uri ?support_uri ~url title header toc content
  in
  let content ppf = (Html.pp ~indent ()) ppf html in
  { Odoc_document.Renderer.filename; content; children }

let open_details = ref true
