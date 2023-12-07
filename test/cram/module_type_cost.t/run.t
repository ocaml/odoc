  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
Here we expect to have the `my_function` from the module be above the one from
the module type.
  $ sherlodoc --print-cost --no-rhs "my_function"
  218 val Main.M.my_function
  221 val Main.Make.my_function
  223 val Main.Make.M.my_function
  618 val Main.S.my_function
Here we expect both the module type and the module to be ranked the same
  $ sherlodoc --print-cost "module"
  220 mod Main.Module_nype
  220 sig Main.Module_type
