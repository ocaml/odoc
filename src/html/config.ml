(* HTML output configuration *)

type t = {
  theme_uri : Types.uri option;
  support_uri : Types.uri option;
  semantic_uris : bool;
  indent : bool;
  flat : bool;
  open_details : bool;
  omit_breadcrumbs : bool;
}

let v ?theme_uri ?support_uri ~semantic_uris ~indent ~flat ~open_details ~omit_breadcrumbs () =
  { theme_uri; support_uri; semantic_uris; indent; flat; open_details; omit_breadcrumbs }

let theme_uri config =
  match config.theme_uri with None -> Types.Relative None | Some uri -> uri

let support_uri config =
  match config.support_uri with None -> Types.Relative None | Some uri -> uri

let semantic_uris config = config.semantic_uris

let indent config = config.indent

let flat config = config.flat

let open_details config = config.open_details

let omit_breadcrumbs config = config.omit_breadcrumbs