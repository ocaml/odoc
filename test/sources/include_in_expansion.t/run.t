Checking that source parents are kept, using include.

  $ odoc compile -c module-a root.mld

  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile-impl --source-id src/b.m -I . b.cmt --output-dir .
  $ odoc compile -I . b.cmt
  $ odoc compile-impl --source-id src/a.ml -I . main__A.cmt --output-dir .
  $ odoc compile -I . main__A.cmt
  $ odoc compile-impl --source-id src/main.ml -I . main.cmt --output-dir .
  $ odoc compile -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . impl-main.odoc
  $ odoc link -I . impl-main__A.odoc

  $ odoc html-generate-source --impl impl-main.odocl --indent -o html main.ml
  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate-source --impl impl-main__A.odocl --indent -o html a.ml
  $ odoc html-generate --hidden --indent -o html main__A.odocl

In Main.A, the source parent of value x should be to Main__A, while the
source parent of value y should be left to B.

  $ grep source_link html/Main/A/index.html -C 1
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../src/a.ml.html" class="source_link">Source</a>
     </h1>
  --
         <a href="#val-y" class="anchor"></a>
         <a href="../../src/b.m.html#val-y" class="source_link">Source</a>
         <code><span><span class="keyword">val</span> y : int</span></code>
  --
       <a href="#val-x" class="anchor"></a>
       <a href="../../src/a.ml.html#val-x" class="source_link">Source</a>
       <code><span><span class="keyword">val</span> x : int</span></code>
