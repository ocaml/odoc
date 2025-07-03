  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ ocamlc -c -bin-annot list.mli
  $ odoc compile --package test -I . page.mld
  File "page.mld", line 25, characters 23-34:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 35, characters 23-32:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 35, characters 49-59:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 125, characters 0-11:
  Warning: Tags are not allowed in pages.
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc compile --package list -I . list.cmti
  File "list.mli", line 1, characters 4-12:
  Warning: '{0': heading level should be lower than top heading level '0'.
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc link list.odoc
  File "list.mli", line 37, characters 12-19:
  Warning: Reference to 'head' is ambiguous. Please specify its kind: section-head, val-head.
  $ odoc link page-page.odoc
  File "page.mld", line 85, characters 0-33:
  Warning: Failed to resolve reference ./odoc_logo_placeholder.jpg Path 'odoc_logo_placeholder.jpg' not found
  File "page.mld", line 29, characters 4-49:
  Warning: Failed to resolve reference ./test.mli Path 'test' not found
  $ odoc markdown-generate test.odocl -o markdown
  $ odoc markdown-generate test2.odocl -o markdown
  $ odoc markdown-generate page-page.odocl -o markdown
  $ odoc markdown-generate list.odocl -o markdown

  $ cat markdown/test/Test.md
  
  # Module `Test`
  
  
  ## Section 1
  
  ```
  type t = int
  ```
  A very important type
  
  
  ### Section 2
  
  ```
  val v : t
  ```
  A very important value
  
  ```
  module List : sig ... end
  ```
  ```
  module type X = sig ... end
  ```
  ```
  module type T = sig ... end
  ```

  $ cat markdown/test/Test-module-type-X.md
  
  # Module type `Test.X`
  
  ```
  type t = int
  ```

  $ cat markdown/test/Test-module-type-T.md
  
  # Module type `Test.T`
  
  ```
  type t = int
  ```

  $ cat markdown/test/Test-List.md
  
  # Module `Test.List`
  
  ```
  type 'a t = 'a list
  ```
  ```
  val head : 'a t -> 'a option
  ```
  ```
  val headExn : 'a t -> 'a
  ```

  $ cat markdown/test/page.md
  
  # The title
  
  Quick reference for the odoc language rendering markdown
  
  
  ## Title
  
  
  ### Subtitle
  
  
  #### Referenceable title
  
  See [Referenceable title](./#my_id).
  
  
  #### Styled
  
  **bold** text, *italic* text, *emphasized* text
  
  H2O and 1st
  
  
  #### Link
  
  Here is a link: [https://www.example.com](https://www.example.com).
  
  You can also click [here](https://www.example.com).
  
  
  #### References
  
  See an empty reference [`Test.v`](./Test.md#val-v).
  
  See [this function from another library](./Test.md#val-v).
  
  See `this page from another package`.
  
  See [this section](./#styled) for the syntax of references.
  
  
  ##### Subpages
  
  There's a subpage here [`Test`](./Test.md) and another one [`Test2`](./Test2.md)
  
  
  #### Lists
  
  - First item
  - Second item
  1. First ordered item
  2. Second numbered item
  - First item
  - Second item
  - can also be used
  1. First numbered item
  2. Second numbered item
  3. can also be used
  
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
  
  #### Verbatim
  
  ```
  verbatim text
  ```
  
  #### Math
  
  For inline math: `\sqrt 2`.
  
  For display math:
  
  ```
  \sqrt 2
  ```
  
  #### Images
  
  ![./odoc\_logo\_placeholder.jpg]()
  ![https://picsum.photos/200/100](https://picsum.photos/200/100)
  
  #### Table
  
  
  ##### Explicit syntax
  
  | Header 1 | Header 2 |
  | --- | --- |
  | Cell 1 | Cell 2 |
  | Cell 3 | Cell 4 |
  
  ##### Light syntax
  
  | Header 1 | Header 2 |
  | --- | --- |
  | Cell 1 | Cell 2 |
  | Cell 3 | Cell 4 |
  
  #### HTML
  
  This is a strong tag:  <strong> Odoc language lack support for quotation! </strong>
  
  
  
    <div>
      <blockquote>
        Odoc language lack support for quotation!
      </blockquote>
    </div>
  
  
  #### Tags
  
  since 4\.08
  Tags are explained in this section.
