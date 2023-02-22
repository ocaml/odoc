Checking that source links exists inside recursive modules.

  $ odoc compile -c module-main -C main.ml root.mld
  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --source-name main.ml --source-parent-file page-root.odoc -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc html-generate --source main.ml --indent -o html main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/main.ml.html#def-0" class="source_link">Source
      </a>
     </h1>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../../root/source/main.ml.html#def-1" class="source_link">Source
      </a>
     </h1>
