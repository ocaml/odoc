  $ ocamlc -c a.mli -bin-annot -I .
  $ odoc compile -I . a.cmti
  $ odoc link -I . a.odoc
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $(find . -name '*.odocl')
  $ sherlodoc search --print-docstring "foo"
  Usage: sherlodoc search [--help] [OPTION]… [QUERY]
  sherlodoc: unknown option --print-docstring
  [124]
  $ sherlodoc search --print-docstring "bar"
  Usage: sherlodoc search [--help] [OPTION]… [QUERY]
  sherlodoc: unknown option --print-docstring
  [124]
