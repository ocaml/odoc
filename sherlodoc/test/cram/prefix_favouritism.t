  $ ODOCLS=$(find ../docs/odoc/tyxml/ -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "attrib"
  145 type +'a Svg_f.Make.attrib = Xml.attrib
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
  154 type Html_types.bdo_attrib = [ common ]
  154 type Html_types.dfn_attrib = [ common ]
  154 type Html_types.div_attrib = [ common ]
  154 type Html_types.kbd_attrib = [ common ]
  $ sherlodoc index --favoured-prefixes=Tyxml_xml $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "attrib"
  96 type Tyxml_xml.attrib
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
  154 type Html_types.bdo_attrib = [ common ]
  154 type Html_types.dfn_attrib = [ common ]
  154 type Html_types.div_attrib = [ common ]
  154 type Html_types.wbr_attrib = [ common ]
  $ sherlodoc index --favoured-prefixes=Tyxml_svg $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "attrib"
  96 type +'a Tyxml_svg.attrib
  104 type Tyxml_svg.Xml.attrib = Tyxml_xml.attrib
  136 val Tyxml_svg.Unsafe.int_attrib : string -> int wrap -> 'a attrib
  136 val Tyxml_svg.Unsafe.uri_attrib : string -> uri wrap -> 'a attrib
  140 val Tyxml_svg.Unsafe.float_attrib : string -> float wrap -> 'a attrib
  142 val Tyxml_svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  150 val Tyxml_svg.Unsafe.comma_sep_attrib : string -> string list wrap -> 'a attrib
  150 val Tyxml_svg.Unsafe.space_sep_attrib : string -> string list wrap -> 'a attrib
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
  153 type Html_types.ul_attrib = [ common ]
  $ sherlodoc index --favoured-prefixes=Tyxml_xml,Tyxml_svg $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "attrib"
  96 type +'a Tyxml_svg.attrib
  96 type Tyxml_xml.attrib
  104 type Tyxml_svg.Xml.attrib = Tyxml_xml.attrib
  136 val Tyxml_svg.Unsafe.int_attrib : string -> int wrap -> 'a attrib
  136 val Tyxml_svg.Unsafe.uri_attrib : string -> uri wrap -> 'a attrib
  140 val Tyxml_svg.Unsafe.float_attrib : string -> float wrap -> 'a attrib
  142 val Tyxml_svg.Unsafe.string_attrib : string -> string wrap -> 'a attrib
  150 val Tyxml_svg.Unsafe.comma_sep_attrib : string -> string list wrap -> 'a attrib
  150 val Tyxml_svg.Unsafe.space_sep_attrib : string -> string list wrap -> 'a attrib
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
  153 type Html_types.ul_attrib = [ common ]
  $ sherlodoc index $ODOCLS --favoured-prefixes "" > /dev/null
  $ sherlodoc search --print-cost "attrib"
  145 type +'a Svg_f.Make.attrib = Xml.attrib
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
  154 type Html_types.bdo_attrib = [ common ]
  154 type Html_types.dfn_attrib = [ common ]
  154 type Html_types.div_attrib = [ common ]
  154 type Html_types.kbd_attrib = [ common ]

Partial name search:
