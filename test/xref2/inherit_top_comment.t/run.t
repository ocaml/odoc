  $ ocamlc -bin-annot file.ml
  $ odoc compile file.cmt
  $ odoc link file.odoc
  $ odoc html-generate --indent -o html file.odocl

Module X inherits from X__Hidden's top comment:

  $ cat html/File/index.html | grep -B 10 X__Hidden
      <div class="spec module anchored" id="module-X">
       <a href="#module-X" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="X/index.html">X</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
      <div class="spec-doc"><p>Top comment of <code>X__Hidden</code></p></div>
