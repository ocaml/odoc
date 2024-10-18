  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/lib/libname unit.cmt

  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/lib/libname/unit.odoc

  $ odoc compile-index -P pkg:_odoc/pkg/doc/ -L libname:_odoc/pkg/lib/libname -o sidebar.odoc-index
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/doc/page-file.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/doc/dir1/page-my_page.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/lib/libname/unit.odocl

  $ cat html/pkg/doc/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc"><b>pkg's Pages</b>
      <ul>
       <li><a href="#" class="current_unit">Package <code>pkg</code></a>
        <ul>
         <li><a href="dir1/index.html">A directory</a>
          <ul><li><a href="dir1/my_page.html">My page</a></li></ul>
         </li><li><a href="file.html">File</a></li>
        </ul>
       </li>
      </ul><b>Libraries</b>
      <ul>
       <li><b>libname's Units</b>
        <ul><li><a href="../lib/libname/Unit/index.html">Unit</a></li></ul>
       </li>
      </ul>
     </nav>

  $ cat html/pkg/lib/libname/Unit/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc"><b>pkg's Pages</b>
      <ul>
       <li><a href="../../../doc/index.html">Package <code>pkg</code></a>
        <ul>
         <li><a href="../../../doc/dir1/index.html">A directory</a>
          <ul><li><a href="../../../doc/dir1/my_page.html">My page</a></li>
          </ul>
         </li><li><a href="../../../doc/file.html">File</a></li>
        </ul>
       </li>
      </ul><b>Libraries</b>
      <ul>
       <li><b>libname's Units</b>
        <ul><li><a href="#" class="current_unit">Unit</a></li></ul>
       </li>
      </ul>

  $ odoc support-files -o html
$ cp -r html /tmp/html
