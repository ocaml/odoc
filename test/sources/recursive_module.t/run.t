Checking that source links exists inside recursive modules.

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --impl main.ml -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../Main.ml.html#def-Main0" class="source_link">Source</a>
     </h1>
    </header>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../Main.ml.html#def-Main1" class="source_link">Source</a>
     </h1>
    </header>
