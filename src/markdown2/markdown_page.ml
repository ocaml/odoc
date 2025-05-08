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
