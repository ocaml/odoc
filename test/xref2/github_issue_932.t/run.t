A quick test to repro the issue found in #941

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc

  $ odoc html-generate --indent -o html/ foo.odocl

The rendered html

  $ cat html/Foo/index.html | grep "extension" -A 3
      <div class="spec type extension anchored" id="extension-decl-A">
       <a href="#extension-decl-A" class="anchor"></a>
       <code>
        <span><span class="keyword">type</span> <a href="#type-t">t</a> += 
        </span>
  --
        <li id="extension-A" class="def variant extension anchored">
         <a href="#extension-A" class="anchor"></a>
         <code><span>| </span><span><span class="extension">A</span></span>
         </code>
        </li>
        <li id="extension-B" class="def variant extension anchored">
         <a href="#extension-B" class="anchor"></a>
         <code><span>| </span><span><span class="extension">B</span></span>
         </code>
        </li>
       </ol>
  --
      <div class="spec type extension anchored" id="extension-decl-C">
       <a href="#extension-decl-C" class="anchor"></a>
       <code>
        <span><span class="keyword">type</span> 
         <a href="M/index.html#type-t">M.t</a> += 
  --
        <li id="extension-C" class="def variant extension anchored">
         <a href="#extension-C" class="anchor"></a>
         <code><span>| </span><span><span class="extension">C</span></span>
         </code>
        </li>
       </ol>
  --
      <li>extension-decl-A : <a href="#extension-decl-A"><code>A</code></a>
      </li>
      <li>extension-decl-B : <a href="#extension-decl-A"><code>B</code></a>
      </li><li>extension-A : <a href="#extension-A"><code>A</code></a></li>
      <li>extension-B : <a href="#extension-B"><code>B</code></a></li>
      <li>A : <a href="#extension-A"><code>A</code></a></li>
     </ul>
     <ul><li>M.t : <a href="M/index.html#type-t"><code>M.t</code></a></li>
      <li>M.extension-decl-A : 
       <a href="M/index.html#extension-decl-A"><code>M.A</code></a>
      </li>
      <li>M.extension-decl-B : 
       <a href="M/index.html#extension-decl-A"><code>M.B</code></a>
      </li>
      <li>M.extension-A : 
       <a href="M/index.html#extension-A"><code>M.A</code></a>
      </li>
      <li>M.extension-B : 
       <a href="M/index.html#extension-B"><code>M.B</code></a>
      </li>
      <li>M.A : <a href="M/index.html#extension-A"><code>M.A</code></a></li>
     </ul>
    </div>
   </body>
