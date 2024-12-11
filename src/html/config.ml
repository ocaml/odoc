(* HTML output configuration *)

type t = {
  theme_uri : Types.uri option;
  support_uri : Types.uri option;
  search_uris : Types.file_uri list;
  remap : (string * string) list;
  semantic_uris : bool;
  search_result : bool;
      (* Used to not render links, for summary in search results *)
  indent : bool;
  flat : bool;
  open_details : bool;
  as_json : bool;
  home_breadcrumb : bool;
}

let v ?(search_result = false) ?theme_uri ?support_uri ?(search_uris = [])
    ~semantic_uris ~indent ~flat ~open_details ~as_json ~remap
    ?(home_breadcrumb = true) () =
  {
    semantic_uris;
    indent;
    flat;
    open_details;
    theme_uri;
    support_uri;
    search_uris;
    as_json;
    search_result;
    remap;
    home_breadcrumb;
  }

let theme_uri config : Types.uri =
  match config.theme_uri with None -> Types.Relative None | Some uri -> uri

let support_uri config : Types.uri =
  match config.support_uri with None -> Types.Relative None | Some uri -> uri

let search_uris config = config.search_uris

let semantic_uris config = config.semantic_uris

let indent config = config.indent

let flat config = config.flat

let open_details config = config.open_details

let as_json config = config.as_json

let search_result config = config.search_result

let remap config = config.remap

let home_breadcrumb config = config.home_breadcrumb
