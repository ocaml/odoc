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

let make ~config ~url doc children =
  let filename = Link.Path.as_filename ~config url in
  let content ppf =
    let renderer = Cmarkit_commonmark.renderer () in
    Format.fprintf ppf "%s" (Cmarkit_renderer.doc_to_string renderer doc)
  in
  { Odoc_document.Renderer.filename; content; children; path = url }

let make_src ~config ~url _title block_list =
  let filename = Link.Path.as_filename ~config url in
  let content (ppf : Format.formatter) =
    let renderer = Cmarkit_commonmark.renderer () in
    let root_block = Cmarkit.Block.Blocks (block_list, Cmarkit.Meta.none) in
    let doc = Cmarkit.Doc.make root_block in
    Format.fprintf ppf "%s" (Cmarkit_renderer.doc_to_string renderer doc)
  in
  { Odoc_document.Renderer.filename; content; children = []; path = url }
