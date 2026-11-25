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
