  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 0.000959s
  "" {page page page page page page Main Main.foo Main.unique_name Main.multiple_hit_1
     Main.multiple_hit_2 Main.multiple_hit_3 Main.name_conflict Main.name_conflict Main.Nest
     Main.Nest.nesting_priority Main.nesting_priority Main.Map Main.Map.to_list Main.list Main.List
     Main.List.t Main.List.map Main.foo Main.moo Main.t Main.value Main.consume Main.consume_2
     Main.consume_2_other Main.produce Main.produce_2' Main.Modtype Main.Modtype.v_modtype Main.S
     Main.S_to_S1 Main.poly_1 Main.poly_2 Main.boo Main.poly_param Main.extensible_type
     Main.MyExtension}
  
      "" {Main.produce_2'}
      "" {}
  
          "oo" {Main.boo}
          "onsume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
          "xtensible_type" {Main.extensible_type}
          "oo" {Main.foo Main.foo}
          "ist" {Main.list Main.List}  "" {}  "ap" {Main.List.map}  "" {Main.List.t}
          "" {}
  
              "p" {Main.Map Main.List.map}  "to_list" {Main.Map.to_list}
              "" {}  "type" {Main.Modtype}  "v_modtype" {Main.Modtype.v_modtype}  "" {Main.moo}
              "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
              "extension" {Main.MyExtension}
          "" {}
  
              "me_conflict" {Main.name_conflict Main.name_conflict}
              "st" {Main.Nest}
  
                  "nesting_priority" {Main.Nest.nesting_priority}
                  "ng_priority" {Main.Nest.nesting_priority Main.nesting_priority}
          "" {}
  
              "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
              "oduce" {Main.produce}  "2'" {Main.produce_2'}
          "" {Main.S}  "to_s1" {Main.S_to_S1}
          "" {Main.List.t Main.t}  "_list" {Main.Map.to_list}
          "nique_name" {Main.unique_name}
          "" {}  "modtype" {Main.Modtype.v_modtype}  "lue" {Main.value}
      "" {Main.multiple_hit_1 Main.S_to_S1 Main.poly_1}
      "" {Main.multiple_hit_2 Main.consume_2 Main.poly_2}
       "" {Main.produce_2'}  "other" {Main.consume_2_other}
      "" {Main.multiple_hit_3}
      "" {}
  
          "" {Main.multiple_hit_1 Main.poly_1}
          "" {Main.multiple_hit_2 Main.consume_2 Main.poly_2}
           "" {Main.produce_2'}  "other" {Main.consume_2_other}
          "" {Main.multiple_hit_3}
          "onflict" {Main.name_conflict Main.name_conflict}
          "it_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "ist" {Main.Map.to_list}
          "odtype" {Main.Modtype.v_modtype}
          "ame" {Main.unique_name}
          "ther" {Main.consume_2_other}
          "" {}  "ram" {Main.poly_param}  "iority" {Main.Nest.nesting_priority Main.nesting_priority}
          "1" {Main.S_to_S1}
          "" {}  "_s1" {Main.S_to_S1}  "pe" {Main.extensible_type}
      "" {page page page Main.foo}
  
          "raph" {page}
          "n" {Main}
  
              "" {}
  
                  "oo" {Main.boo}
                  "onsume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
                  "xtensible_type" {Main.extensible_type}
                  "oo" {Main.foo Main.foo}
                  "ist" {Main.list Main.List}  "" {}  "ap" {Main.List.map}  "" {Main.List.t}
                  "" {}
  
                      "p" {Main.Map}  "to_list" {Main.Map.to_list}
                      "" {}  "type" {Main.Modtype}  "v_modtype" {Main.Modtype.v_modtype}  "" {Main.moo}
                      "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
                      "extension" {Main.MyExtension}
                  "" {}
  
                      "me_conflict" {Main.name_conflict Main.name_conflict}
                      "st" {Main.Nest}
                       "nesting_priority" {Main.Nest.nesting_priority}  "ng_priority" {Main.nesting_priority}
                  "" {}
  
                      "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
                      "oduce" {Main.produce}  "2'" {Main.produce_2'}
                  "" {Main.S}  "to_s1" {Main.S_to_S1}
                  "" {Main.t}
                  "nique_name" {Main.unique_name}
                  "alue" {Main.value}
          "ue" {Main.value}
          "" {Main.poly_param}  "" {Main.unique_name}  "conflict" {Main.name_conflict Main.name_conflict}
          "d" {page}
          "" {Main.Map Main.List.map Main.foo}  "to_list" {Main.Map.to_list}  "" {page}
          "a" {}  "raph" {page}  "" {Main.poly_param}
          "im" {page}
      "" {page}
       "tim" {page}  "" {}  "_type" {Main.extensible_type}  "b" {page}  "ib" {page}  "o" {Main.boo}
      "" {}
  
          "" {Main.produce}  "2'" {Main.produce_2'}
          "" {}
  
              "e" {page}
              "" {}
  
                  "lict" {Main.name_conflict Main.name_conflict}
                  "ume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
          "" {Main.name_conflict Main.name_conflict}
      "" {page}
  
          "" {page}
          "ype" {Main.Modtype Main.Modtype.v_modtype}  "v_modtype" {Main.Modtype.v_modtype}
          "ce" {Main.produce}  "2'" {Main.produce_2'}
      "" {page page page Main.unique_name Main.value Main.consume Main.produce Main.Modtype
         Main.Modtype.v_modtype Main.extensible_type}
  
          "v_modtype" {Main.Modtype.v_modtype}
          "" {}
  
              "" {Main.consume_2}  "" {Main.produce_2'}  "other" {Main.consume_2_other}
              "onflict" {Main.name_conflict Main.name_conflict}
              "it_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
              "ame" {Main.unique_name}
              "ype" {Main.extensible_type}
          "si" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
          "" {Main.consume_2_other}  "atim" {page}
          "t" {Main.Nest}
  
              "nesting_priority" {Main.Nest.nesting_priority}
              "ng_priority" {Main.Nest.nesting_priority Main.nesting_priority}
          "tensi" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
      "" {page}  "ict" {Main.name_conflict Main.name_conflict}  "o" {Main.foo Main.foo}
      "" {}  "priority" {Main.Nest.nesting_priority Main.nesting_priority}  "aph" {page}  "" {page}
      "" {page}
  
          "r" {Main.consume_2_other}
          "" {}
  
              "gs" {page}
              "" {Main.foo}
              "_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
      "" {}
  
          "" {page}  "" {}  "_type" {Main.extensible_type}  "b" {page}
          "t" {Main.name_conflict Main.name_conflict}
          "" {page}
          "" {Main}
  
              "" {}
  
                  "oo" {Main.boo}
                  "onsume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
                  "xtensible_type" {Main.extensible_type}
                  "oo" {Main.foo Main.foo}
                  "ist" {Main.list Main.List}  "" {}  "ap" {Main.List.map}  "" {Main.List.t}
                  "" {}
  
                      "p" {Main.Map}  "to_list" {Main.Map.to_list}
                      "" {}  "type" {Main.Modtype}  "v_modtype" {Main.Modtype.v_modtype}  "" {Main.moo}
                      "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
                      "extension" {Main.MyExtension}
                  "" {}
  
                      "me_conflict" {Main.name_conflict Main.name_conflict}
                      "st" {Main.Nest}
                       "nesting_priority" {Main.Nest.nesting_priority}  "ng_priority" {Main.nesting_priority}
                  "" {}
  
                      "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
                      "oduce" {Main.produce}  "2'" {Main.produce_2'}
                  "" {Main.S}  "to_s1" {Main.S_to_S1}
                  "" {Main.t}
                  "nique_name" {Main.unique_name}
                  "alue" {Main.value}
              "" {}  "priority" {Main.Nest.nesting_priority Main.nesting_priority}  "" {page}
          "" {}  "" {Main.MyExtension}  "ity" {Main.Nest.nesting_priority Main.nesting_priority}
          "le_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "ue_name" {Main.unique_name}
          "" {Main.foo}
  
              "" {page Main.Map.to_list Main.list Main.List Main.foo}
               "" {}  "ap" {Main.List.map}  "" {Main.List.t}
          "" {}
  
              "" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
              "e" {page}
              "" {Main.Nest.nesting_priority Main.nesting_priority}
      "" {}
  
          "" {page}
  
              "" {}
  
                  "it_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
                  "ype" {Main.extensible_type}
          "" {}
  
              "" {page}  "ib" {page}
              "t" {Main.name_conflict Main.name_conflict}
              "t" {page Main.Map.to_list Main.list Main.List Main.foo}
               "" {}  "ap" {Main.List.map}  "" {Main.List.t}
          "iple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "e" {Main.value}
          "_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
      "" {page Main.poly_param}
  
          "" {}
  
              "n" {Main}
  
                  "" {}
  
                      "oo" {Main.boo}
                      "onsume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
                      "xtensible_type" {Main.extensible_type}
                      "oo" {Main.foo Main.foo}
                      "ist" {Main.list Main.List}  "" {}  "ap" {Main.List.map}  "" {Main.List.t}
                      "" {}
  
                          "p" {Main.Map}  "to_list" {Main.Map.to_list}
                          "" {}  "type" {Main.Modtype}  "v_modtype" {Main.Modtype.v_modtype}  "" {Main.moo}
                          "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
                          "extension" {Main.MyExtension}
                      "" {}
  
                          "me_conflict" {Main.name_conflict Main.name_conflict}
                          "st" {Main.Nest}
                           "nesting_priority" {Main.Nest.nesting_priority}  "ng_priority" {Main.nesting_priority}
                      "" {}
  
                          "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
                          "oduce" {Main.produce}  "2'" {Main.produce_2'}
                      "" {Main.S}  "to_s1" {Main.S_to_S1}
                      "" {Main.t}
                      "nique_name" {Main.unique_name}
                      "alue" {Main.value}
              "" {Main.Map Main.List.map Main.foo}  "to_list" {Main.Map.to_list}
          "" {page Main.unique_name Main.consume}
  
              "" {}
  
                  "" {Main.consume_2}  "other" {Main.consume_2_other}
                  "onflict" {Main.name_conflict Main.name_conflict}
          "" {}
  
              "type" {Main.Modtype Main.Modtype.v_modtype}  "v_modtype" {Main.Modtype.v_modtype}
              "" {Main.moo}
          "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "extension" {Main.MyExtension}
      "" {Main Main.MyExtension}
  
          "" {}
  
              "oo" {Main.boo}
              "onsume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
              "xtensible_type" {Main.extensible_type}
              "oo" {Main.foo Main.foo}
              "ist" {Main.list Main.List}  "" {}  "ap" {Main.List.map}  "" {Main.List.t}
              "" {}
  
                  "p" {Main.Map}  "to_list" {Main.Map.to_list}
                  "" {}  "type" {Main.Modtype}  "v_modtype" {Main.Modtype.v_modtype}  "" {Main.moo}
                  "ltiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
                  "extension" {Main.MyExtension}
              "" {}
  
                  "me_conflict" {Main.name_conflict Main.name_conflict}
                  "st" {Main.Nest}
                   "nesting_priority" {Main.Nest.nesting_priority}  "ng_priority" {Main.nesting_priority}
              "" {}
  
                  "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
                  "oduce" {Main.produce}  "2'" {Main.produce_2'}
              "" {Main.S}  "to_s1" {Main.S_to_S1}
              "" {Main.t}
              "nique_name" {Main.unique_name}
              "alue" {Main.value}
          "me" {Main.unique_name}  "conflict" {Main.name_conflict Main.name_conflict}
          "" {page}
          "st" {Main.Nest}
  
              "nesting_priority" {Main.Nest.nesting_priority}
              "ng_priority" {Main.Nest.nesting_priority Main.nesting_priority}
          "lict" {Main.name_conflict Main.name_conflict}
          "" {}  "priority" {Main.Nest.nesting_priority Main.nesting_priority}  "" {page}
          "que_name" {Main.unique_name}
          "" {}  "" {Main.foo}  "" {Main.foo}
          "" {}
  
              "" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
              "me" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
      "" {Main.foo Main.foo Main.moo Main.boo}
  
          "" {}  "ist" {Main.Map.to_list}  "1" {Main.S_to_S1}
          "" {}
  
              "" {page}
              "ype" {Main.Modtype Main.Modtype.v_modtype}  "v_modtype" {Main.Modtype.v_modtype}
              "ce" {Main.produce}  "2'" {Main.produce_2'}
          "" {page}
          "y_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
          "e" {page}
          "" {Main.MyExtension}
  
              "lict" {Main.name_conflict Main.name_conflict}
              "ume" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
          "" {Main.foo Main.foo Main.moo Main.boo}
          "" {Main.foo}  "ty" {Main.Nest.nesting_priority Main.nesting_priority}
          "" {Main.foo}  "er" {Main.consume_2_other}
      "" {Main.Map Main.List.map Main.foo}
  
          "to_list" {Main.Map.to_list}
          "ra" {}  "raph" {page}  "" {Main.poly_param}
          "" {Main.Modtype Main.Modtype.v_modtype Main.extensible_type}
           "v_modtype" {Main.Modtype.v_modtype}
          "" {page}
          "e_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "ly_" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
          "" {}
  
              "ority" {Main.Nest.nesting_priority Main.nesting_priority}
              "duce" {Main.produce}  "2'" {Main.produce_2'}
      "ue_name" {Main.unique_name}
      "" {Main.foo Main.consume_2_other}
  
          "" {}  "raph" {page}  "" {Main.poly_param}  "h" {page}
          "atim" {page}
          "" {}
  
              "rity" {Main.Nest.nesting_priority Main.nesting_priority}
              "y" {Main.Nest.nesting_priority Main.nesting_priority}
          "duce" {Main.produce}  "2'" {Main.produce_2'}
      "" {page Main.foo Main.S}
  
          "" {Main.S_to_S1}
          "to_s1" {Main.S_to_S1}
          "" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
          "me" {page}
          "" {page Main.Nest Main.Map.to_list Main.list Main.List Main.foo}
  
              "" {}  "ap" {Main.List.map}  "esting_priority" {Main.Nest.nesting_priority}  "" {Main.List.t}
              "ng_priority" {Main.Nest.nesting_priority Main.nesting_priority}
          "me" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
      "" {page Main.name_conflict Main.name_conflict Main.Nest Main.Map.to_list Main.list Main.List
         Main.List.t Main.foo Main.t}
  
          "" {}  "ap" {Main.List.map}  "esting_priority" {Main.Nest.nesting_priority}  "" {Main.List.t}
          "" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "nsi" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
          "" {}  "r" {Main.consume_2_other}  "" {}  "gs" {page}  "" {Main.foo}
          "" {}
  
              "" {page}
              "g_priority" {Main.Nest.nesting_priority Main.nesting_priority}
              "le_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
              "le" {page}
          "e" {page}
          "_" {}  "ist" {Main.Map.to_list}  "1" {Main.S_to_S1}
          "" {Main.Nest.nesting_priority Main.nesting_priority}
  
              "e" {Main.Modtype Main.Modtype.v_modtype Main.extensible_type}
               "v_modtype" {Main.Modtype.v_modtype}
      "" {}
  
          "e" {Main.produce}  "2'" {Main.produce_2'}
          "" {Main.value}  "name" {Main.unique_name}
          "tiple_hit_" {}  "" {Main.multiple_hit_1}  "" {Main.multiple_hit_2}  "" {Main.multiple_hit_3}
          "e" {Main.consume}  "2" {Main.consume_2}  "other" {Main.consume_2_other}
          "ique_name" {Main.unique_name}
      "" {}  "modtype" {Main.Modtype.v_modtype}  "lue" {Main.value}  "rbatim" {page}
      "tensi" {}  "le_type" {Main.extensible_type}  "n" {Main.MyExtension}
      "" {Main.Nest.nesting_priority Main.nesting_priority}
  
          "" {}  "" {Main.poly_1}  "" {Main.poly_2}  "aram" {Main.poly_param}
          "xtension" {Main.MyExtension}
          "e" {Main.Modtype Main.Modtype.v_modtype Main.extensible_type}
           "v_modtype" {Main.Modtype.v_modtype}
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc "unique_name"
  val Main.unique_name : foo
  $ sherlodoc "multiple_hit"
  val Main.multiple_hit_1 : foo
  val Main.multiple_hit_2 : foo
  val Main.multiple_hit_3 : foo
  $ sherlodoc "name_conflict"
  type Main.name_conflict = foo
  val Main.name_conflict : foo
  $ sherlodoc "nesting_priority"
  [No results]
  $ sherlodoc --print-cost "list"
  315 type Main.List.t = 'a list
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  $ sherlodoc --print-cost "map"
  108 mod Main.Map
  320 val Main.Map.to_list : foo
  $ sherlodoc --print-cost "list map"
  [No results]
  $ sherlodoc --print-cost ":moo"
  210 val Main.value : moo
  213 val Main.produce : unit -> moo
  217 val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc --print-cost ":moo -> _"
  212 val Main.consume : moo -> unit
  215 val Main.consume_2 : moo -> moo -> unit
  221 val Main.consume_2_other : moo -> t -> unit
  266 cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc --print-cost "modtype"
  112 sig Main.Modtype
  325 val Main.Modtype.v_modtype : foo
  $ sherlodoc --print-cost "S"
  216 mod Main.S_to_S1
  318 type Main.List.t = 'a list
  319 val Main.consume : moo -> unit
  320 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  321 val Main.consume_2 : moo -> moo -> unit
  327 val Main.consume_2_other : moo -> t -> unit
  327 type Main.extensible_type = ..
  333 val Main.Nest.nesting_priority : foo
  373 cons Main.MyExtension : moo -> extensible_type
  1154 doc page
  $ sherlodoc --print-cost "qwertyuiopasdfghjklzxcvbnm"
  [No results]
TODO : get a result for the query bellow
  $ sherlodoc --print-cost "hidden"
  [No results]
  $ sherlodoc --print-cost ":mo"
  [No results]
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.poly_param : 'a boo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  $ sherlodoc ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  $ sherlodoc ": ('a -> 'b) -> 'a t -> 'b t"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
TODO : get a result for the query bellow
  $ sherlodoc ": 'a bo"
  [No results]
  $ sherlodoc ":extensible_type"
  cons Main.MyExtension : moo -> extensible_type
