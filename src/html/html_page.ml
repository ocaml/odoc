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

module Url = Odoc_document.Url
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
  match toc with [] -> [] | _ -> [ sections toc ]

let html_of_search () =
  let search_bar =
    Html.(
      input
        ~a:[ a_class [ "search-bar" ]; a_placeholder "🔎 Type '/' to search..." ]
        ())
  in
  let snake = Html.(div ~a:[ a_class [ "search-snake" ] ] []) in
  let search_result = Html.div ~a:[ Html.a_class [ "search-result" ] ] [] in
  Html.(
    div ~a:[ a_class [ "search-inner" ] ] [ search_bar; snake; search_result ])

let sidebars ~global_toc ~local_toc =
  let local_toc =
    match local_toc with
    | [] -> []
    | _ :: _ ->
        [
          Html.nav
            ~a:[ Html.a_class [ "odoc-toc"; "odoc-local-toc" ] ]
            (html_of_toc local_toc);
        ]
  in
  let global_toc =
    match global_toc with
    | None -> []
    | Some c ->
        [ Html.nav ~a:[ Html.a_class [ "odoc-toc"; "odoc-global-toc" ] ] c ]
  in
  match local_toc @ global_toc with
  | [] -> []
  | tocs -> [ Html.div ~a:[ Html.a_class [ "odoc-tocs" ] ] tocs ]

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
        Odoc_utils.List.concat_map ?sep:(Some sep)
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

let file_uri ~config ~url (base : Types.uri) file =
  match base with
  | Types.Absolute uri -> uri ^ "/" ^ file
  | Relative uri ->
      let page = Url.Path.{ kind = `File; parent = uri; name = file } in
      Link.href ~config ~resolve:(Current url) (Url.from_path page)

let default_meta_elements ~config ~url =
  let theme_uri = Config.theme_uri config in
  let odoc_css_uri = file_uri ~config ~url theme_uri "odoc.css" in
  [
    Html.meta ~a:[ Html.a_charset "utf-8" ] ();
    Html.link ~rel:[ `Stylesheet ] ~href:odoc_css_uri ();
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
  ]

let page_creator ~config ~url ~uses_katex ~global_toc header breadcrumbs
    local_toc content =
  let theme_uri = Config.theme_uri config in
  let support_uri = Config.support_uri config in
  let search_uris = Config.search_uris config in
  let path = Link.Path.for_printing url in

  let head : Html_types.head Html.elt =
    let title_string =
      Printf.sprintf "%s (%s)" url.name (String.concat "." path)
    in

    let file_uri = file_uri ~config ~url in
    let search_uri uri =
      match uri with
      | Types.Absolute uri -> uri
      | Relative uri ->
          Link.href ~config ~resolve:(Current url) (Url.from_path uri)
    in
    let search_scripts =
      match search_uris with
      | [] -> []
      | _ ->
          let search_urls = List.map search_uri search_uris in
          let search_urls =
            let search_url name = Printf.sprintf "'%s'" name in
            let search_urls = List.map search_url search_urls in
            "[" ^ String.concat "," search_urls ^ "]"
          in
          (* The names of the search scripts are put into a js variable. Then
             the code in [odoc_search.js] load them into a webworker. *)
          [
            Html.script ~a:[]
              (Html.txt
                 (Format.asprintf
                    {|let base_url = '%s';
let search_urls = %s;
|}
                    (let page =
                       Url.Path.{ kind = `File; parent = None; name = "" }
                     in
                     Link.href ~config ~resolve:(Current url)
                       (Url.from_path page))
                    search_urls));
            Html.script
              ~a:
                [
                  Html.a_src (file_uri support_uri "odoc_search.js");
                  Html.a_defer ();
                ]
              (Html.txt "");
          ]
    in
    let meta_elements =
      let highlightjs_meta =
        let highlight_js_uri = file_uri support_uri "highlight.pack.js" in
        [
          Html.script ~a:[ Html.a_src highlight_js_uri ] (Html.txt "");
          Html.script (Html.txt "hljs.initHighlightingOnLoad();");
        ]
      in
      let katex_meta =
        if uses_katex then
          let katex_css_uri = file_uri theme_uri "katex.min.css" in
          let katex_js_uri = file_uri support_uri "katex.min.js" in
          [
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
        else []
      in
      default_meta_elements ~config ~url @ highlightjs_meta @ katex_meta
    in
    let meta_elements = meta_elements @ search_scripts in
    Html.head (Html.title (Html.txt title_string)) meta_elements
  in
  let search_bar =
    match search_uris with
    | [] -> []
    | _ ->
        [ Html.div ~a:[ Html.a_class [ "odoc-search" ] ] [ html_of_search () ] ]
  in

  let body =
    html_of_breadcrumbs breadcrumbs
    @ search_bar
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ sidebars ~global_toc ~local_toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in

  let htmlpp = Html.pp ~indent:(Config.indent config) () in
  let html = Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body) in
  let content ppf =
    htmlpp ppf html;
    (* Tyxml's pp doesn't output a newline a the end, so we force one *)
    Format.pp_force_newline ppf ()
  in
  content

let make ~config ~url ~header ~breadcrumbs ~sidebar ~toc ~uses_katex content
    children =
  let filename = Link.Path.as_filename ~config url in
  let content =
    page_creator ~config ~url ~uses_katex ~global_toc:sidebar header breadcrumbs
      toc content
  in
  { Odoc_document.Renderer.filename; content; children; path = url }

let path_of_module_of_source ppf url =
  match url.Url.Path.parent with
  | Some parent ->
      let path = Link.Path.for_printing parent in
      Format.fprintf ppf " (%s)" (String.concat "." path)
  | None -> ()

let src_page_creator ~breadcrumbs ~config ~url ~header name content =
  let head : Html_types.head Html.elt =
    let title_string =
      Format.asprintf "Source: %s%a" name path_of_module_of_source url
    in
    let meta_elements = default_meta_elements ~config ~url in
    Html.head (Html.title (Html.txt title_string)) meta_elements
  in
  let body =
    html_of_breadcrumbs breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ content
  in
  (* We never indent as there is a bug in tyxml and it would break lines inside
     a [pre] *)
  let htmlpp = Html.pp ~indent:false () in
  let html =
    Html.html head (Html.body ~a:[ Html.a_class [ "odoc-src" ] ] body)
  in
  let content ppf =
    htmlpp ppf html;
    (* Tyxml's pp doesn't output a newline a the end, so we force one *)
    Format.pp_force_newline ppf ()
  in
  content

let make_src ~config ~url ~breadcrumbs ~header title content =
  let filename = Link.Path.as_filename ~config url in
  let content =
    src_page_creator ~breadcrumbs ~config ~url ~header title content
  in
  { Odoc_document.Renderer.filename; content; children = []; path = url }
