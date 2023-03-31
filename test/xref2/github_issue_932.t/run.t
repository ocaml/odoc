A quick test to repro the issue found in #941

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  File "foo.mli", line 10, characters 8-24:
  Warning: Unknown reference qualifier 'extension-decl'.
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
        <li id="extension-A" class="def extension anchored">
         <a href="#extension-A" class="anchor"></a>
         <code><span>| </span><span><span class="extension">A</span></span>
         </code>
        </li>
        <li id="extension-B" class="def extension anchored">
         <a href="#extension-B" class="anchor"></a>
         <code><span>| </span><span><span class="extension">B</span></span>
         </code>
        </li>
       </ol>
  --
      <li><code>extension-decl-A</code></li>
      <li><a href="#extension-A"><code>A</code></a></li>
      <li><a href="#extension-B"><code>B</code></a></li>
     </ul>
    </div>
   </body>
