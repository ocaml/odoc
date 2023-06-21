  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 0.000103s
  "" {Main Main.List Main.list Main.List.t Main.List.map}
  
      "." {}
  
          "list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
          "map" {Main.List.map}
          "t" {Main.List.t}
      "a" {}
  
          "in" {Main}  ".list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
          "p" {Main.List.map}
      "i" {}
  
          "n" {Main}  ".list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
          "st" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
      "list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
      "ma" {}
  
          "in" {Main}  ".list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
          "p" {Main.List.map}
      "n" {Main}  ".list" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
      "p" {Main.List.map}
      "st" {Main.List Main.list}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
      "t" {Main.List Main.list Main.List.t}  "." {}  "map" {Main.List.map}  "t" {Main.List.t}
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc --print-cost "list"
  list
  109 mod Main.List
  209 type Main.list
  315 type Main.List.t = 'a list
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
