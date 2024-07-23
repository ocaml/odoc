
Compile the module first

  $ ocamlc -c -bin-annot test.mli

Then we need to odoc-compile the package mld file, listing its children

  $ odoc compile index.mld --child module-test --child asset-img.jpg
  Warning: Potential name clash - child page named 'index'

  $ odoc compile-asset --name img --parent-id page-test --output-dir odoc

  $ tree
  .
  |-- index.mld -> ../../../../../../default/test/pages/new_assets.t/index.mld
  |-- odoc
  |   `-- page-test
  |       `-- asset-img.odoc
  |-- page-index.odoc
  |-- test.cmi
  |-- test.cmti
  `-- test.mli -> ../../../../../../default/test/pages/new_assets.t/test.mli
  
  3 directories, 6 files
