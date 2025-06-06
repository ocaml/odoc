Test compile-impl with markdown source generation:

  $ cat simple_test.ml
  let x = 42

Compile the OCaml source file:

  $ ocamlc -c simple_test.ml -bin-annot

Compile the implementation with source-id:

  $ odoc compile-impl -I . --source-id src/simple_test.ml simple_test.cmt

Compile the interface documentation:

  $ odoc compile -I . simple_test.cmt

Link both documentation and implementation:

  $ odoc link -I . simple_test.odoc
  $ odoc link -I . impl-simple_test.odoc

Generate markdown documentation:

  $ odoc markdown-generate simple_test.odocl -o markdown

Generate markdown source documentation:

  $ odoc markdown-generate-source --impl impl-simple_test.odocl -o markdown simple_test.ml

Check that markdown files were generated:

  $ find markdown -name "*.md" | sort
  markdown/Simple_test.md
  markdown/src/simple_test.ml.md

Check the generated markdown source file:

  $ cat markdown/src/simple_test.ml.md
  
  # Source file `simple_test.ml`
  
  ```ocaml
  let x = 42
  ```
