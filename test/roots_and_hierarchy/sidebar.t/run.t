  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname unit.cmt

  $ odoc link -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname _odoc/pkg/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/unit.odoc

  $ odoc compile-index -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname -o sidebar.odoc-index
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/page-file.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/dir1/page-my_page.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/libname/unit.odocl

  $ cat html/pkg/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul class="odoc-modules">
       <li><b>Library <code>libname</code></b>
        <ul><li><a href="libname/Unit/index.html">Unit</a></li></ul>
       </li>
      </ul><b>Documentation</b>
      <ul class="odoc-pages">
       <li><a href="#" class="current_unit">Package <code>pkg</code></a>
        <ul>
         <li><a href="dir1/index.html">A directory</a>
          <ul><li><a href="dir1/my_page.html">My page</a></li></ul>
         </li><li><a href="file.html">File</a></li>
        </ul>
       </li>
      </ul>
     </nav>

  $ cat html/pkg/libname/Unit/X/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul class="odoc-modules">
       <li><b>Library <code>libname</code></b>
        <ul>
         <li><a href="../index.html">Unit</a>
          <ul>
           <li><a href="#" class="current_unit">X</a>
            <ul><li><a href="Y/index.html">Y</a></li>
             <li><a href="#module-Z">Z</a></li>
            </ul>
           </li>
          </ul>
         </li>
        </ul>
       </li>
      </ul><b>Documentation</b>

  $ odoc support-files -o html
