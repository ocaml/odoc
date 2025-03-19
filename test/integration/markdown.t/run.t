  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ odoc compile --package test -I . page.mld
  File "page.mld", line 51, characters 24-25:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "page.mld", line 68, characters 2-3:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "page.mld", line 100, characters 19-20:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "page.mld", line 115, characters 2-3:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "page.mld", line 119, characters 0-11:
  Warning: Tags are not allowed in pages.
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc link page-page.odoc
  File "page.mld", line 84, characters 0-33:
  Warning: Failed to resolve reference ./odoc_logo_placeholder.jpg Path 'odoc_logo_placeholder.jpg' not found
  File "page.mld", line 31, characters 4-49:
  Warning: Failed to resolve reference ./test.mli Path 'test' not found
  File "page.mld", line 29, characters 4-50:
  Warning: Failed to resolve reference /test.v Path '/test' not found
  $ odoc markdown-generate test.odocl -o markdown
  ## Section 1
  type t = int
  A very important type
  ### Section 2
  val v : [t](#type-t)
  A very important value
  $ odoc markdown-generate test2.odocl -o markdown
  val v : [Test.t](Test.html#type-t)
  $ odoc markdown-generate page-page.odocl -o markdown
  ## Title
  ### Subtitle
  #### Referenceable title
  See [Referenceable title](#my_id).
  #### Styled
  **bold** text, *italic* text, *emphasized* text
  H2O and 1st
  #### Link
  Here is a link: [https://www.example.com](https://www.example.com).
  You can also click [here](https://www.example.com).
  #### References
  See `Odoc_odoc.Compile.compile`.
  See `Odoc_odoc.Compile.compile`.
  See [this function from another library]().
  See [this page from another package]().
  See [this section](#styled) for the syntax of references.
  #### Lists
  - First item
  - Second item
  0. First ordered item
  1. Second numbered item
  - First item
  - Second item
  - can also be used
  0. First numbered item
  1. Second numbered item
  2. can also be used
  }
  #### Code blocks
  Inline `code`.
  ```ocaml
  let _ = "Block code"
  ```
  ```text
  Code block with {[inner code block syntax]}
  ```
  ```python
  [i+1 for i in xrange(2)]
  ```
  }
  #### Verbatim
  ```
  verbatim text
  ```
  #### Math
  For inline math: \\sqrt 2.
  For display math:
  \\sqrt 2
  #### Images
  ![./odoc\_logo\_placeholder.jpg]()
  ![https://picsum.photos/200/100](https://picsum.photos/200/100)
  #### Table
  ##### Explicit syntax
  \| Header   1 \| Header   2 \|
  \| --- \| --- \|
  \| Cell   1 \| Cell   2 \|
  \| Cell   3 \| Cell   4 \|
  }
  ##### Light syntax
  \| Header   1 \| Header   2 \|
  \| --- \| --- \|
  \| Cell   1 \| Cell   2 \|
  \| Cell   3 \| Cell   4 \|
  #### HTML
  
    <blockquote>
      Odoc language lack support for quotation!
    </blockquote>
  
  }
  #### Tags
  since 4\.08
  Tags are explained in this section.
$ cat markdown/test/Test.html
