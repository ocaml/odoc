  $ ODOCLS=$(find ../docs/odoc/tyxml/ -name '*.odocl' | grep -v "__")
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index --index-docstring=false $ODOCLS
  $ sherlodoc search --print-cost --limit 100 "attrib"
  145 type +'a Svg_f.Make.attrib = Xml.attrib
  146 type +'a Tyxml_svg.attrib
  146 type Tyxml_xml.attrib
  146 type +'a Html_f.Make.attrib = Xml.attrib
  147 type +'a Tyxml_html.attrib
  152 type Html_types.b_attrib = [ common ]
  152 type Html_types.i_attrib = [ common ]
  152 type Html_types.p_attrib = [ common ]
  152 type Html_types.u_attrib = [ common ]
  153 type Html_types.br_attrib = [ common ]
  153 type Html_types.dd_attrib = [ common ]
  153 type Html_types.dl_attrib = [ common ]
  153 type Html_types.dt_attrib = [ common ]
  153 type Html_types.em_attrib = [ common ]
  153 type Html_types.h1_attrib = [ common ]
  153 type Html_types.h2_attrib = [ common ]
  153 type Html_types.h3_attrib = [ common ]
  153 type Html_types.h4_attrib = [ common ]
  153 type Html_types.h5_attrib = [ common ]
  153 type Html_types.h6_attrib = [ common ]
  153 type Html_types.hr_attrib = [ common ]
  153 type Html_types.rp_attrib = [ common ]
  153 type Html_types.rt_attrib = [ common ]
  153 type Html_types.tr_attrib = [ common ]
  153 type Html_types.ul_attrib = [ common ]
  154 type Tyxml_svg.Xml.attrib = Tyxml_xml.attrib
  154 type Html_types.bdo_attrib = [ common ]
  154 type Html_types.dfn_attrib = [ common ]
  154 type Html_types.div_attrib = [ common ]
  154 type Html_types.kbd_attrib = [ common ]
  154 type Html_types.nav_attrib = [ common ]
  154 type Html_types.pre_attrib = [ common ]
  154 type Html_types.sub_attrib = [ common ]
  154 type Html_types.sup_attrib = [ common ]
  154 type Html_types.var_attrib = [ common ]
  154 type Html_types.wbr_attrib = [ common ]
  154 type Svg_f.Make.Xml.attrib = Xml.attrib
  154 type Html_types.title_attrib = noattrib
  155 type Tyxml_html.Xml.attrib = Tyxml_xml.attrib
  155 type Html_f.Make.Xml.attrib = Xml.attrib
  155 type Html_types.abbr_attrib = [ common ]
  155 type Html_types.cite_attrib = [ common ]
  155 type Html_types.code_attrib = [ common ]
  155 type Html_types.head_attrib = [ common ]
  155 type Html_types.main_attrib = [ common ]
  155 type Html_types.mark_attrib = [ common ]
  155 type Html_types.ruby_attrib = [ common ]
  155 type Html_types.samp_attrib = [ common ]
  155 type Html_types.span_attrib = [ common ]
  156 type Html_types.aside_attrib = [ common ]
  156 type Html_types.small_attrib = [ common ]
  156 type Html_types.tbody_attrib = [ common ]
  156 type Html_types.tfoot_attrib = [ common ]
  156 type Html_types.thead_attrib = [ common ]
  157 type Html_types.figure_attrib = [ common ]
  157 type Html_types.footer_attrib = [ common ]
  157 type Html_types.header_attrib = [ common ]
  157 type Html_types.hgroup_attrib = [ common ]
  157 type Html_types.legend_attrib = [ common ]
  157 type Html_types.strong_attrib = [ common ]
  158 type Html_types.address_attrib = [ common ]
  158 type Html_types.article_attrib = [ common ]
  158 type Html_types.caption_attrib = [ common ]
  158 type Html_types.picture_attrib = [ common ]
  158 type Html_types.section_attrib = [ common ]
  158 type Html_types.summary_attrib = [ common ]
  159 type Html_types.datalist_attrib = [ common ]
  159 type Html_types.noscript_attrib = [ common ]
  159 type Html_types.template_attrib = [ common ]
  160 type Html_types.q_attrib = [ common | `Cite ]
  161 type Html_types.figcaption_attrib = [ common ]
  162 type Html_types.col_attrib = [ common | `Span ]
  162 type Html_types.map_attrib = [ common | `Name ]
  162 type Html_types.svg_attrib = Svg_types.svg_attr
  165 type Html_types.dialog_attrib = [ common | `Open ]
  166 type Html_types.li_attrib = [ common | `Int_Value ]
  166 type Html_types.details_attrib = [ common | `Open ]
  167 type Html_types.html_attrib = [ common | `Manifest ]
  167 type Html_types.table_attrib = [ common | `Summary ]
  167 type Html_types.colgroup_attrib = [ common | `Span ]
  168 type Html_types.tablex_attrib = [ common | `Summary ]
  168 type +'a Svg_f.Make_with_wrapped_functions.attrib = Xml.attrib
  169 type Html_types.blockquote_attrib = [ common | `Cite ]
  169 type +'a Html_f.Make_with_wrapped_functions.attrib = Xml.attrib
  171 type Html_types.audio_attrib = [ common | media_attrib ]
  173 type Html_types.base_attrib = [ common | `Href | `Target ]
  174 type Html_types.ol_attrib = [ common | `Reversed | `Start ]
  174 type Html_types.del_attrib = [ common | `Cite | `Datetime ]
  174 type Html_types.ins_attrib = [ common | `Cite | `Datetime ]
  176 type Html_types.canvas_attrib = [ common | `Width | `Height ]
  177 type Html_types.menu_attrib = [ common | `Label | `Menu_Type ]
  177 type Html_types.label_attrib = [ common | `Label_for | `Form ]
  177 type Svg_f.Make_with_wrapped_functions.Xml.attrib = Xml.attrib
  178 type Html_types.time_attrib = [ common | `Datetime | `Pubdate ]
  178 type Html_types.param_attrib = [ common | `Name | `Text_Value ]
  178 type Html_f.Make_with_wrapped_functions.Xml.attrib = Xml.attrib
  180 type Html_types.optgroup_attrib = [ common | `Disabled | `Label ]
  186 type Html_types.td_attrib = [ common | `Colspan | `Headers | `Rowspan ]
  186 val Tyxml_svg.Unsafe.int_attrib : string -> int wrap -> 'a attrib
  186 val Tyxml_svg.Unsafe.uri_attrib : string -> uri wrap -> 'a attrib
  $ sherlodoc search --print-cost --no-rhs "group"
  186 type Html_types.hgroup
  190 type Html_types.colgroup
  190 type Html_types.optgroup
  197 type Html_types.hgroup_attrib
  198 type Html_types.colgroup_content
  201 type Html_types.optgroup_content
  202 type Html_types.colgroup_content_fun
  205 type Html_types.optgroup_content_fun
  207 type Html_types.colgroup_attrib
  220 type Html_types.optgroup_attrib
  225 type Html_types.hgroup_content
  229 type Html_types.hgroup_content_fun
  319 val Tyxml_html.a_mediagroup
  325 val Html_f.Make.a_mediagroup
  328 val Tyxml_html.a_radiogroup
  334 val Html_f.Make.a_radiogroup
  348 val Html_f.Make_with_wrapped_functions.a_mediagroup
  357 val Html_f.Make_with_wrapped_functions.a_radiogroup
  377 val Tyxml_html.hgroup
  383 val Html_f.Make.hgroup
  385 val Tyxml_html.colgroup
  391 val Html_f.Make.colgroup
  406 val Html_f.Make_with_wrapped_functions.hgroup
  414 val Html_f.Make_with_wrapped_functions.colgroup
  423 val Tyxml_html.optgroup
  $ sherlodoc search --no-rhs "group b"
  type Html_types.hgroup_attrib
  type Html_types.colgroup_attrib
  type Html_types.optgroup_attrib
  $ sherlodoc search --print-cost "event_handler"
  142 type Tyxml_xml.event_handler = string
  158 type Tyxml_xml.mouse_event_handler = string
  158 type Tyxml_xml.touch_event_handler = string
  161 type Tyxml_xml.keyboard_event_handler = string
  168 type Tyxml_svg.Xml.event_handler = Tyxml_xml.event_handler
  168 type Svg_f.Make.Xml.event_handler = Xml.event_handler
  169 type Tyxml_html.Xml.event_handler = Tyxml_xml.event_handler
  169 type Html_f.Make.Xml.event_handler = Xml.event_handler
  190 type Tyxml_svg.Xml.mouse_event_handler = Tyxml_xml.mouse_event_handler
  190 type Tyxml_svg.Xml.touch_event_handler = Tyxml_xml.touch_event_handler
  190 type Svg_f.Make.Xml.mouse_event_handler = Xml.mouse_event_handler
  190 type Svg_f.Make.Xml.touch_event_handler = Xml.touch_event_handler
  191 type Tyxml_html.Xml.mouse_event_handler = Tyxml_xml.mouse_event_handler
  191 type Tyxml_html.Xml.touch_event_handler = Tyxml_xml.touch_event_handler
  191 type Html_f.Make.Xml.mouse_event_handler = Xml.mouse_event_handler
  191 type Html_f.Make.Xml.touch_event_handler = Xml.touch_event_handler
  191 type Svg_f.Make_with_wrapped_functions.Xml.event_handler = Xml.event_handler
  192 type Html_f.Make_with_wrapped_functions.Xml.event_handler = Xml.event_handler
  196 type Tyxml_svg.Xml.keyboard_event_handler = Tyxml_xml.keyboard_event_handler
  196 type Svg_f.Make.Xml.keyboard_event_handler = Xml.keyboard_event_handler
  197 type Tyxml_html.Xml.keyboard_event_handler = Tyxml_xml.keyboard_event_handler
  197 type Html_f.Make.Xml.keyboard_event_handler = Xml.keyboard_event_handler
  213 type Svg_f.Make_with_wrapped_functions.Xml.mouse_event_handler = Xml.mouse_event_handler
  213 type Svg_f.Make_with_wrapped_functions.Xml.touch_event_handler = Xml.touch_event_handler
  214 type Html_f.Make_with_wrapped_functions.Xml.touch_event_handler = Xml.touch_event_handler

  $ sherlodoc search --print-cost --static-sort "Svg event_handler"
  163 type Tyxml_svg.Xml.event_handler = Tyxml_xml.event_handler
  163 type Svg_f.Make.Xml.event_handler = Xml.event_handler
  175 type Tyxml_svg.Xml.mouse_event_handler = Tyxml_xml.mouse_event_handler
  175 type Tyxml_svg.Xml.touch_event_handler = Tyxml_xml.touch_event_handler
  175 type Svg_f.Make.Xml.mouse_event_handler = Xml.mouse_event_handler
  175 type Svg_f.Make.Xml.touch_event_handler = Xml.touch_event_handler
  181 type Tyxml_svg.Xml.keyboard_event_handler = Tyxml_xml.keyboard_event_handler
  181 type Svg_f.Make.Xml.keyboard_event_handler = Xml.keyboard_event_handler
  186 type Svg_f.Make_with_wrapped_functions.Xml.event_handler = Xml.event_handler
  198 type Svg_f.Make_with_wrapped_functions.Xml.mouse_event_handler = Xml.mouse_event_handler
  198 type Svg_f.Make_with_wrapped_functions.Xml.touch_event_handler = Xml.touch_event_handler
  204 type Svg_f.Make_with_wrapped_functions.Xml.keyboard_event_handler = Xml.keyboard_event_handler
  263 type Svg_sigs.T.Xml.event_handler
  268 type Svg_sigs.NoWrap.Xml.event_handler
  269 type Svg_sigs.T.Xml.mouse_event_handler
  269 type Svg_sigs.T.Xml.touch_event_handler
  272 type Svg_sigs.T.Xml.keyboard_event_handler
  273 type Svg_sigs.Make.T.Xml.event_handler = Xml.event_handler
  274 type Svg_sigs.NoWrap.Xml.mouse_event_handler
  274 type Svg_sigs.NoWrap.Xml.touch_event_handler
  277 type Svg_sigs.NoWrap.Xml.keyboard_event_handler
  279 val Tyxml_svg.Xml.event_handler_attrib : aname -> event_handler -> attrib
  279 type Svg_sigs.Wrapped_functions.Xml.event_handler
  285 val Svg_f.Make.Xml.event_handler_attrib : aname -> event_handler -> attrib
  285 type Svg_sigs.Make.T.Xml.mouse_event_handler = Xml.mouse_event_handler

  $ sherlodoc search --print-cost "string_attrib"
  182 val Tyxml_svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  183 val Tyxml_html.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  188 val Svg_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  189 val Html_f.Make.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  211 val Svg_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  212 val Html_f.Make_with_wrapped_functions.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  266 val Tyxml_xml.string_attrib : aname -> string wrap -> attrib
  275 val Tyxml_svg.Xml.string_attrib : aname -> string wrap -> attrib
  276 val Tyxml_html.Xml.string_attrib : aname -> string wrap -> attrib
  281 val Svg_f.Make.Xml.string_attrib : aname -> string wrap -> attrib
  282 val Html_f.Make.Xml.string_attrib : aname -> string wrap -> attrib
  288 val Svg_sigs.T.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  289 val Html_sigs.T.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  293 val Svg_sigs.NoWrap.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  294 val Html_sigs.NoWrap.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  295 val Tyxml_xml.add_string_attrib : aname -> string -> attrib list -> attrib list
  298 val Html_sigs.T.Svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  298 val Svg_sigs.Make.T.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  299 val Html_sigs.Make.T.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  303 val Html_sigs.NoWrap.Svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  304 val Svg_f.Make_with_wrapped_functions.Xml.string_attrib : aname -> string wrap -> attrib
  305 val Html_f.Make_with_wrapped_functions.Xml.string_attrib : aname -> string wrap -> attrib
  316 val Xml_iter.Make.add_string_attrib : Xml.aname -> string -> Xml.attrib list -> Xml.attrib list
  317 val Tyxml_xml.map_string_attrib : (aname -> bool) -> (string -> string) -> attrib list -> attrib list
  330 val Tyxml_xml.map_string_attrib_in_list : (aname -> bool) -> (string -> string) -> attrib list -> attrib list


  $ sherlodoc search --no-rhs "Tyxml_html.Xml.string_attrib"
  val Tyxml_html.Xml.string_attrib
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
