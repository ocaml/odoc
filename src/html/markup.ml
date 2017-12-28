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



let keyword keyword =
  Html.span ~a:[ Html.a_class ["keyword"] ] [ Html.pcdata keyword ]

let module_path ids =
  Html.span
    ~a:[ Html.a_class ["module-path"] ] [Html.pcdata (String.concat "." ids)]

module Type = struct
  let path p = Html.span ~a:[ Html.a_class ["type-id"] ] p
  let var tv = Html.span ~a:[ Html.a_class ["type-var"] ] [ Html.pcdata tv ]
end

let def_div lst = Html.div ~a:[ Html.a_class ["def" ] ] [ Html.code lst ]

let def_summary lst = Html.summary [ Html.span ~a:[ Html.a_class ["def"] ] lst ]

let make_def ~id ~code:def ~doc =
  match Url.from_identifier ~stop_before:true id with
  | Error e -> failwith (Url.Error.to_string e)
  | Ok { anchor; kind; _ } ->
    Html.div ~a:[ Html.a_class ["spec"; kind] ; Html.a_id anchor ] [
      Html.a ~a:[ Html.a_href ("#" ^ anchor); Html.a_class ["anchor"] ] [];
      Html.div ~a:[ Html.a_class ["def"; kind] ] [ Html.code def ];
      Html.div ~a:[ Html.a_class ["doc"] ] doc;
    ]

let make_spec ~id ?doc code =
  match Url.from_identifier ~stop_before:true id with
  | Error e -> failwith (Url.Error.to_string e)
  | Ok { anchor; kind; _ } ->
    Html.div ~a:[ Html.a_class ["spec"; kind] ; Html.a_id anchor ] (
      Html.a ~a:[ Html.a_href ("#" ^ anchor); Html.a_class ["anchor"] ] [] ::
      Html.div ~a:[ Html.a_class ["def"; kind] ] code ::
      (match doc with
       | None -> []
       | Some doc -> [Html.div ~a:[ Html.a_class ["doc"] ] doc])
    )

let arrow =
  Html.span
    ~a:[ Html.a_class ["keyword" ] ] [ Html.entity "#8209"; Html.entity "gt" ]

let label = function
  | Model.Lang.TypeExpr.Label s -> [ Html.pcdata s ]
  | Optional s -> [ Html.pcdata "?"; Html.entity "#8288"; Html.pcdata s ]
