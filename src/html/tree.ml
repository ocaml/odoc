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
module Url = Odoc_document.Url

type syntax = OCaml | Reason

let string_of_syntax = function
  | OCaml -> "ml"
  | Reason -> "re"

type uri =
  | Absolute of string
  | Relative of string

type t = {
  filename : string;
  content : Html.doc;
  children : t list
}

(* Translation of Url.Path *)
module Path = struct

  let to_list url =
    let rec loop acc {Url.Path. parent ; name ; kind } =
      match parent with
      | None -> (kind,name) :: acc
      | Some p -> loop ((kind, name) :: acc) p
    in
    loop [] url

  let for_printing url = List.map snd @@ to_list url

  let segment_to_string (kind, name) =
    if kind = "module" || kind = "package"
    then name
    else Printf.sprintf "%s-%s" kind name
  let for_linking url = List.map segment_to_string @@ to_list url
  let filename (url : Url.Path.t) = segment_to_string (url.kind, url.name)

  let is_page url = (url.Url.Path.kind = "page")

end

module Link = struct
  let semantic_uris = ref false

  type t =
    | Current of Url.Path.t
    | Base of string
  
  let rec drop_shared_prefix l1 l2 =
    match l1, l2 with
    | l1 :: l1s, l2 :: l2s when l1 = l2 ->
      drop_shared_prefix l1s l2s
    | _, _ -> l1, l2

  let href ~resolve { Url.Anchor. page; anchor; kind } =
    let leaf = if !semantic_uris || kind = "page" then [] else ["index.html"] in
    let target = Path.for_linking page @ leaf in
    match resolve with
    (* If xref_base_uri is defined, do not perform relative URI resolution. *)
    | Base xref_base_uri ->
      let page = xref_base_uri ^ String.concat "/" target in
      begin match anchor with
      | "" -> page
      | anchor -> page ^ "#" ^ anchor
      end
    | Current path ->
      let current_loc =
        let l = Path.for_linking path in
        if Path.is_page path then
          (* Sadness. *)
          List.tl l
        else l
      in
      let current_from_common_ancestor, target_from_common_ancestor =
        drop_shared_prefix current_loc target
      in
      let relative_target =
        List.map (fun _ -> "..") current_from_common_ancestor
        @ target_from_common_ancestor
      in
      let page = String.concat "/" relative_target in
      begin match anchor with
      | "" -> page
      | anchor -> page ^ "#" ^ anchor
      end
end

let page_creator ?(theme_uri = Relative "./") ~url name header_docs content =
  let is_page = Path.is_page url in
  let path = Path.for_printing url in
  let rec add_dotdot ~n acc =
    if n <= 0 then
      acc
    else
      add_dotdot ~n:(n - 1) ("../" ^ acc)
  in
  let resolve_relative_uri uri =
    (* Remove the first "dot segment". *)
    let uri =
      if String.length uri >= 2 && String.sub uri 0 2 = "./" then
        String.sub uri 2 (String.length uri - 2)
      else uri
    in
    (* How deep is this page? *)
    let n =
      List.length path - (
        (* This is just horrible. *)
        if is_page then 1 else 0)
    in
    add_dotdot uri ~n
  in

  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let theme_uri =
      match theme_uri with
      | Absolute uri -> uri
      | Relative uri -> resolve_relative_uri uri
    in

    let support_files_uri = resolve_relative_uri "./" in

    let odoc_css_uri = theme_uri ^ "odoc.css" in
    let highlight_js_uri = support_files_uri ^ "highlight.pack.js" in

    Html.head (Html.title (Html.txt title_string)) [
      Html.link ~rel:[`Stylesheet] ~href:odoc_css_uri () ;
      Html.meta ~a:[ Html.a_charset "utf-8" ] () ;
      Html.meta ~a:[ Html.a_name "generator";
                     Html.a_content "odoc %%VERSION%%" ] ();
      Html.meta ~a:[ Html.a_name "viewport";
                  Html.a_content "width=device-width,initial-scale=1.0"; ] ();
      Html.script ~a:[Html.a_src highlight_js_uri] (Html.txt "");
      Html.script (Html.txt "hljs.initHighlightingOnLoad();");
    ]
  in

  let wrapped_content : (Html_types.div_content Html.elt) list =

    let header_content =
      let dot = if !Link.semantic_uris then "" else "index.html" in
      let dotdot = add_dotdot ~n:1 dot in
      let up_href = if is_page && name <> "index" then dot else dotdot in
      let has_parent = List.length path > 1 in
      if has_parent then
        let nav =
          Html.nav @@ [
            Html.a ~a:[Html.a_href up_href] [
              Html.txt "Up"
            ];
            Html.txt " – "
          ] @
            (* Create breadcrumbs *)
            let space = Html.txt " " in
            let breadcrumb_spec =
              if is_page
              then (fun n x -> n, dot, x)
              else (fun n x -> n, add_dotdot ~n dot, x)
            in
            let rev_path = if is_page && name = "index"
              then List.tl (List.rev path)
              else List.rev path
            in
            rev_path |>
            List.mapi breadcrumb_spec |>
            List.rev |>
            Utils.list_concat_map ?sep:(Some([space; Html.entity "#x00BB"; space]))
              ~f:(fun (n, addr, lbl) ->
                if n > 0 then
                  [[Html.a ~a:[Html.a_href addr] [Html.txt lbl]]]
                else
                  [[Html.txt lbl]]
                ) |>
            List.flatten
        in
        nav::header_docs
      else
        header_docs
    in

    let header = Html.header header_content in

    [Html.div ~a:[Html.a_class ["content"]] (header::content)]
  in

  let html : [ `Html ] Html.elt = Html.html head (Html.body wrapped_content) in

  html

let make ?(header_docs = []) ?theme_uri ~url title content children =
  let filename = Path.filename url in
  let content = page_creator ?theme_uri ~url title header_docs content in
  { filename; content; children }

let traverse ~f t =
  let rec aux parents node =
    f ~parents node.filename node.content;
    List.iter (aux (node.filename :: parents)) node.children
  in
  aux [] t

let open_details = ref true
