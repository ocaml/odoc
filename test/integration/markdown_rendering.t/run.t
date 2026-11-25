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
