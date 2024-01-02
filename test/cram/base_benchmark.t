This test will fail, it is not deterministic. Please just check that the values
are not crazy and discard the changes
  $ find . -name '*.odocl' | sort
  ./base_odocls/base.odocl
  ./base_odocls/base_internalhash_types.odocl
  ./base_odocls/caml.odocl
  ./base_odocls/md5_lib.odocl
  ./base_odocls/page-index.odocl
  ./base_odocls/shadow_stdlib.odocl
  $ sherlodoc index --format=js --db=db.js $(find . -name '*.odocl')








