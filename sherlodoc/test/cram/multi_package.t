  $ ODOCLS=$(find ../docs/odoc/ -name '*.odocl'  | grep -v "__" | sort)
  $ echo "$ODOCLS" | awk 'END { print NR }'
  16
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index --index-docstring=false $ODOCLS
  $ sherlodoc search --print-cost --limit 100 "S_poly"
  [No results]
  $ sherlodoc search --print-cost --no-rhs "group b"
  247 type Html_types.hgroup_attrib
  257 type Html_types.colgroup_attrib
  270 type Html_types.optgroup_attrib
  $ sherlodoc search --no-rhs "group by"
  [No results]
  $ sherlodoc search --print-cost "map2"
  [No results]

  $ sherlodoc search --print-cost --static-sort "List map2"
  [No results]

  $ sherlodoc search --print-cost "List map2"
  [No results]

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  [No results]
  $ sherlodoc search --print-cost "list"
  147 type 'a Tyxml_xml.list_wrap = 'a W.tlist
  151 type 'a Tyxml_svg.list_wrap = 'a Xml.W.tlist
  152 type 'a Tyxml_html.list_wrap = 'a Xml.W.tlist
  156 type 'a Tyxml_svg.Xml.list_wrap = 'a W.tlist
  157 type 'a Svg_f.Make.list_wrap = 'a Xml.W.tlist
  157 type 'a Tyxml_html.Xml.list_wrap = 'a W.tlist
  158 type 'a Html_f.Make.list_wrap = 'a Xml.W.tlist
  162 type 'a Svg_f.Make.Xml.list_wrap = 'a W.tlist
  163 type 'a Html_f.Make.Xml.list_wrap = 'a W.tlist
  180 type 'a Svg_f.Make_with_wrapped_functions.list_wrap = 'a Xml.W.tlist
  181 type 'a Html_f.Make_with_wrapped_functions.list_wrap = 'a Xml.W.tlist
  183 type Html_types.listed = [ resetable | submitable | `Fieldset ]
  185 type 'a Svg_f.Make_with_wrapped_functions.Xml.list_wrap = 'a W.tlist
  186 type 'a Html_f.Make_with_wrapped_functions.Xml.list_wrap = 'a W.tlist
  190 type Html_types.datalist = [ `Datalist ]
  191 type 'a Xml_wrap.NoWrap.tlist = 'a list
  195 type Html_types.datalist_content = notag
  199 type Html_types.datalist_attrib = [ common ]
  199 type Html_types.datalist_content_fun = notag
  209 type 'a Svg_f.Make.Xml.W.tlist = 'a Xml.W.tlist
  210 type 'a Html_f.Make.Xml.W.tlist = 'a Xml.W.tlist
  232 type 'a Svg_f.Make_with_wrapped_functions.Xml.W.tlist = 'a Xml.W.tlist
  233 type 'a Html_f.Make_with_wrapped_functions.Xml.W.tlist = 'a Xml.W.tlist
  253 type 'a Xml_sigs.T.list_wrap = 'a W.tlist
  257 type 'a Svg_sigs.T.list_wrap = 'a Xml.W.tlist
  $ sherlodoc search --print-cost ": list"
  199 val Tyxml_svg.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  200 val Tyxml_html.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  205 val Svg_f.Make.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  206 val Html_f.Make.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  228 val Svg_f.Make_with_wrapped_functions.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  229 val Html_f.Make_with_wrapped_functions.of_seq : Xml_stream.signal Stdlib.Seq.t -> 'a elt list_wrap
  248 val Tyxml_svg.Info.emptytags : string list
  249 val Tyxml_html.Info.emptytags : string list
  254 val Svg_f.Make.Info.emptytags : string list
  255 val Html_f.Make.Info.emptytags : string list
  264 val Tyxml_svg.Info.alternative_content_types : string list
  265 val Tyxml_html.Info.alternative_content_types : string list
  273 val Tyxml_xml.all_entities : elt -> string list
  283 val Tyxml_xml.amap : (ename -> attrib list -> attrib list) -> elt -> elt
  284 val Tyxml_svg.totl : Xml.elt list_wrap -> 'a elt list_wrap
  284 val Tyxml_xml.amap1 : (ename -> attrib list -> attrib list) -> elt -> elt
  285 val Tyxml_html.totl : Xml.elt list_wrap -> 'a elt list_wrap
  286 val Tyxml_svg.toeltl : 'a elt list_wrap -> Xml.elt list_wrap
  286 val Xml_iter.Make.all_entities : Xml.elt -> string list
  287 val Tyxml_html.toeltl : 'a elt list_wrap -> Xml.elt list_wrap
  288 val Xml_wrap.NoWrap.nil : unit -> 'a tlist
  289 val Tyxml_svg.to_xmlattribs : 'a attrib list -> Xml.attrib list
  290 val Svg_f.Make.totl : Xml.elt list_wrap -> 'a elt list_wrap
  290 val Tyxml_html.to_xmlattribs : 'a attrib list -> Xml.attrib list
  291 val Tyxml_xml.of_seq : Xml_stream.signal Stdlib.Seq.t -> elt list

Partial name search:
  $ sherlodoc search --print-cost "strin"
  156 type Svg_types.strings = string list
  169 val Xml_print.string_of_number : float -> string
  197 val Tyxml_svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  198 val Tyxml_html.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  203 val Svg_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  204 val Html_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  206 type Svg_types.commastrings = string list
  206 type Svg_types.spacestrings = string list
  226 val Svg_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  227 val Html_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  269 val Tyxml_xml.string_of_uri : (uri, string) W.ft
  273 val Tyxml_svg.string_of_uri : (uri, string) Xml.W.ft
  274 val Tyxml_html.string_of_uri : (uri, string) Xml.W.ft
  278 val Tyxml_svg.Xml.string_of_uri : (uri, string) W.ft
  279 val Tyxml_xml.uri_of_string : (string, uri) W.ft
  279 val Svg_f.Make.string_of_uri : (uri, string) Xml.W.ft
  279 val Tyxml_html.Xml.string_of_uri : (uri, string) W.ft
  280 val Html_f.Make.string_of_uri : (uri, string) Xml.W.ft
  281 val Tyxml_xml.string_attrib : aname -> string wrap -> attrib
  283 val Tyxml_svg.uri_of_string : (string, uri) Xml.W.ft
  284 val Tyxml_html.uri_of_string : (string, uri) Xml.W.ft
  284 val Svg_f.Make.Xml.string_of_uri : (uri, string) W.ft
  285 val Html_f.Make.Xml.string_of_uri : (uri, string) W.ft
  288 val Tyxml_svg.Xml.uri_of_string : (string, uri) W.ft
  289 val Svg_f.Make.uri_of_string : (string, uri) Xml.W.ft
  $ sherlodoc search --print-cost "base strin"
  415 val Svg_f.Wrapped_functions.string_of_dominant_baseline : ([< Svg_types.dominant_baseline ], string) Xml.W.ft
  417 val Svg_f.Wrapped_functions.string_of_alignment_baseline : ([< Svg_types.alignment_baseline ], string) Xml.W.ft
  518 val Svg_sigs.Wrapped_functions.string_of_dominant_baseline : ([< Svg_types.dominant_baseline ], string) Xml.W.ft
  520 val Svg_sigs.Wrapped_functions.string_of_alignment_baseline : ([< Svg_types.alignment_baseline ], string) Xml.W.ft

  $ sherlodoc search --print-cost "tring"
  201 type Svg_types.strings = string list
  204 val Xml_print.string_of_number : float -> string
  206 type Svg_types.commastrings = string list
  206 type Svg_types.spacestrings = string list
  232 val Tyxml_svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  233 val Tyxml_html.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  238 val Svg_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  239 val Html_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  261 val Svg_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  262 val Html_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  299 val Tyxml_xml.uri_of_string : (string, uri) W.ft
  303 val Tyxml_svg.uri_of_string : (string, uri) Xml.W.ft
  304 val Tyxml_xml.string_of_uri : (uri, string) W.ft
  304 val Tyxml_html.uri_of_string : (string, uri) Xml.W.ft
  308 val Tyxml_svg.string_of_uri : (uri, string) Xml.W.ft
  308 val Tyxml_svg.Xml.uri_of_string : (string, uri) W.ft
  309 val Svg_f.Make.uri_of_string : (string, uri) Xml.W.ft
  309 val Tyxml_html.string_of_uri : (uri, string) Xml.W.ft
  309 val Tyxml_html.Xml.uri_of_string : (string, uri) W.ft
  310 val Tyxml_svg.a_string : string wrap -> [> `String ] attrib
  310 val Html_f.Make.uri_of_string : (string, uri) Xml.W.ft
  313 val Tyxml_svg.Xml.string_of_uri : (uri, string) W.ft
  314 val Svg_f.Make.string_of_uri : (uri, string) Xml.W.ft
  314 val Svg_f.Make.Xml.uri_of_string : (string, uri) W.ft
  314 val Tyxml_html.Xml.string_of_uri : (uri, string) W.ft
  $ sherlodoc search --print-cost "base tring"
  450 val Svg_f.Wrapped_functions.string_of_dominant_baseline : ([< Svg_types.dominant_baseline ], string) Xml.W.ft
  452 val Svg_f.Wrapped_functions.string_of_alignment_baseline : ([< Svg_types.alignment_baseline ], string) Xml.W.ft
  553 val Svg_sigs.Wrapped_functions.string_of_dominant_baseline : ([< Svg_types.dominant_baseline ], string) Xml.W.ft
  555 val Svg_sigs.Wrapped_functions.string_of_alignment_baseline : ([< Svg_types.alignment_baseline ], string) Xml.W.ft

