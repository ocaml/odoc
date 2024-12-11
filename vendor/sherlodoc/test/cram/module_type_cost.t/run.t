  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
$ du -sh megaodocl
4.0K	megaodocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $(find . -name '*.odocl')
Here we expect to have the `my_function` from the module be above the one from
the module type.
  $ sherlodoc search --print-cost --no-rhs "my_function"
  246 val Main.M.my_function
  249 val Main.Make.my_function
  346 val Main.S.my_function
Here we expect both the module type and the module to be ranked the same
  $ sherlodoc search --print-cost "module"
  166 mod Main.Module_nype
  216 sig Main.Module_type
