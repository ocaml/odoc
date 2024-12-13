  $ ocamlc -c a.mli -bin-annot -I .
  $ odoc compile -I . a.cmti
  $ ocamlc -c b.mli -bin-annot -I .
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc

  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index a.odocl b.odocl
  $ sherlodoc search --print-cost "unique_name"
  229 val A.unique_name : int
  229 val B.unique_name : int
  $ sherlodoc index --favoured a.odocl b.odocl
  $ sherlodoc search --print-cost "unique_name"
  179 val A.unique_name : int
  229 val B.unique_name : int
  $ sherlodoc index a.odocl --favoured b.odocl
  $ sherlodoc search --print-cost "unique_name"
  179 val B.unique_name : int
  229 val A.unique_name : int
  $ sherlodoc index --favoured a.odocl --favoured b.odocl
  $ sherlodoc search --print-cost "unique_name"
  179 val A.unique_name : int
  179 val B.unique_name : int
