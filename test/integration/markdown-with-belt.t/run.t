  $ ocamlc -c -bin-annot Belt_Id.mli
  $ ocamlc -c -bin-annot Belt.mli
  $ odoc compile --package Belt -I . Belt.cmti
  $ odoc compile --package Belt -I . Belt_Id.cmti

  $ odoc link Belt.odoc
  $ odoc link Belt_Id.odoc

  $ odoc markdown-generate Belt.odocl -o markdown
  $ odoc markdown-generate Belt_Id.odocl -o markdown

  $ tree markdown
  markdown
  `-- Belt
      |-- Belt.md
      `-- Belt_Id.md
  
  1 directory, 2 files

  $ cat markdown/Belt/Belt.md
  ```
  module Id = Belt_Id
  ```
  [`Belt.Id`](./Belt_Id.md)
  Provide utilities to create identified comparators or hashes for data structures used below.
  It create a unique identifier per module of functions so that different data structures with slightly different comparison functions won't mix
