module Url = Odoc_document.Url

let make ~config ~url doc children =
  let filename = Link.Path.as_filename ~config url in
  let content ppf = Format.fprintf ppf "%s" (Renderer.to_string doc) in
  { Odoc_document.Renderer.filename; content; children; path = url }

let make_src ~config ~url _title block_list =
  let filename = Link.Path.as_filename ~config url in
  let content (ppf : Format.formatter) =
    let root_block = Renderer.Block.Blocks block_list in
    let doc = root_block in
    Format.fprintf ppf "%s" (Renderer.to_string doc)
  in
  { Odoc_document.Renderer.filename; content; children = []; path = url }
