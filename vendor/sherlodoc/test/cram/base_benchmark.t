This test will fail, it is not deterministic. Please just check that the values
are not crazy and discard the changes
  $ ODOCLS=$(find ../docs/odoc/base/ -name '*.odocl' | grep -v "__")
  $ sherlodoc index --format=js --db=db.js $ODOCLS
