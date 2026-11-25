Rendering of inline and block markup by the markdown backend.

  $ gen () {
  >   odoc compile --package test "$1.mld" &&
  >   odoc link "page-$1.odoc" &&
  >   odoc markdown-generate "page-$1.odocl" -o markdown &&
  >   cat "markdown/test/$1.md"
  > }

Code spans whose content contains backticks use a longer delimiter, and are
padded with a space when the content starts or ends with a backtick.

  $ cat > code_spans.mld <<'MLD'
  > {0 Code spans}
  > Polymorphic variant: [`Partial].
  > Labelled argument: [~consume:`All].
  > Character literal: ['`'].
  > Double backtick: [a``b].
  > Both: [``a`b].
  > Plain: [plain].
  > MLD
  $ gen code_spans
  # Code spans
  
  Polymorphic variant: `` `Partial ``. Labelled argument: ``~consume:`All``. Character literal: ``'`'``. Double backtick: `a``b`. Both: ``` ``a`b ```. Plain: `plain`.

A list ends with a blank line, so that a following paragraph does not become a
lazy continuation of the last item. Nested lists stay tight.

  $ cat > lists.mld <<'MLD'
  > {0 Lists}
  > {ul {li first} {li second}}
  > Paragraph after an unordered list.
  > {ol {li one} {li two}}
  > Paragraph after an ordered list.
  > {ul {li outer
  > {ul {li inner} {li inner too}}} {li outer again}}
  > Paragraph after a nested list.
  > {ul {li before code}}
  > {[let x = 1]}
  > {ul {li before heading}}
  > {1 Heading}
  > {ul {li last}}
  > MLD
  $ gen lists
  # Lists
  
  - first
  - second
  
  Paragraph after an unordered list.
  
  1. one
  2. two
  
  Paragraph after an ordered list.
  
  - outer
    - inner
    - inner too
  - outer again
  
  Paragraph after a nested list.
  
  - before code
  
  ```ocaml
  let x = 1
  ```
  
  - before heading
  
  ## Heading
  
  - last

A link to an anchor on the current page is a fragment-only link, as in the
HTML backend.

  $ cat > anchors.mld <<'MLD'
  > {0 Anchors}
  > See {!section-details}, or {{!section-details}there}.
  > {1:details Details}
  > Text.
  > MLD
  $ gen anchors
  # Anchors
  
  See [Details](#details), or [there](#details).
  
  ## Details
  
  Text.

Characters that could start a block are escaped only at the start of a line.

  $ cat > escaping.mld <<'MLD'
  > {0 Escaping}
  > Operators: a + b, a - b, a = b, 1. 2) and x_y.
  > 
  > Separate words: {e a} + {e b} - c = d.
  > 
  > After code: [x] + y, [n + 1]-th, [x]=y, {e a}+b, {e a}_b.
  > 
  > = at the start of a paragraph.
  > 
  > 1. at the start of a paragraph.
  > MLD
  $ gen escaping
  # Escaping
  
  Operators: a + b, a - b, a = b, 1. 2) and x\_y.
  
  Separate words: *a* + *b* - c = d.
  
  After code: `x` + y, `n + 1`-th, `x`=y, *a*+b, *a*\_b.
  
  \= at the start of a paragraph.
  
  1\. at the start of a paragraph.

Blocks are separated by exactly one blank line, including after a code block
and before a heading. The page starts with its first block and ends with a
newline.

  $ cat > spacing.mld <<'MLD'
  > {0 Spacing}
  > First paragraph.
  > {[let x = 1]}
  > Paragraph after code.
  > {1 Heading after a paragraph}
  > {[let y = 2]}
  > {[let z = 3]}
  > {ul {li item
  > 
  > second paragraph of the item
  > }}
  > {table {tr {td cell}}}
  > Last paragraph.
  > MLD
  $ gen spacing
  # Spacing
  
  First paragraph.
  
  ```ocaml
  let x = 1
  ```
  
  Paragraph after code.
  
  ## Heading after a paragraph
  
  ```ocaml
  let y = 2
  ```
  
  ```ocaml
  let z = 3
  ```
  
  - item
  
    second paragraph of the item
  
  | --- |
  | cell |
  
  Last paragraph.

Adjacent lists of the same kind use alternate markers, so that they stay two
lists instead of merging into one.

  $ cat > adjacent_lists.mld <<'MLD'
  > {0 Adjacent lists}
  > {ul {li a}}
  > {ul {li b}}
  > {ul {li c}}
  > {ol {li one}}
  > {ol {li two}}
  > MLD
  $ gen adjacent_lists
  # Adjacent lists
  
  - a
  
  * b
  
  - c
  
  1. one
  
  1) two

In a list item, a nested list follows a paragraph directly, but other blocks
are separated from it by a blank line: an HTML block would otherwise absorb
the list. An empty table and a list of empty items produce no block.

  $ cat > item_blocks.mld <<'MLD'
  > {0 Item blocks}
  > {ul {li {%html:<div>x</div>%}
  > {ul {li nested}}}}
  > 
  > {table }
  > 
  > {ol {li }}
  > 
  > {ul {li after the empty table}}
  > MLD
  $ gen item_blocks
  File "item_blocks.mld", line 7, characters 4-7:
  Warning: '{li ...}' (list item) should not be empty.
  # Item blocks
  
  - <div>x</div>
  
    - nested
  
  * after the empty table
