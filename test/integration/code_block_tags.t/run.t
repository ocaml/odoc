The bare tags of a code block become classes of the block in the HTML,
prefixed with tag-. Tags with values are not rendered.

  $ cat > index.mld << EOF
  > {0 Code block tags}
  > {@ocaml server[let x = 1]}
  > {@ocaml client shared env=f1[let y = 2]}
  > {@ocaml[let z = 3]}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc link _odoc/pkg/page-index.odoc
  $ odoc html-generate -o html _odoc/pkg/page-index.odocl
  $ grep -o '<pre[^>]*>' html/pkg/index.html
  <pre class="language-ocaml tag-server">
  <pre class="language-ocaml tag-client tag-shared">
  <pre class="language-ocaml">
