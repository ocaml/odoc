Checking that source links exists inside recursive modules.

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --impl main.ml -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../Main.ml.html#def-0" class="source_link">Source</a>
     </h1>
    </header>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../Main.ml.html" class="source_link">Source</a>
       <code><span><span class="keyword">type</span> t</span>
        <span> = <a href="../B/index.html#type-t">B.t</a></span>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../Main.ml.html#def-1" class="source_link">Source</a>
     </h1>
    </header>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../Main.ml.html" class="source_link">Source</a>
       <code><span><span class="keyword">type</span> t</span><span> = </span>
       </code>
