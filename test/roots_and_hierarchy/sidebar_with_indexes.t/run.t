This is the same example as `sidebar.t`, but with an additional index page for the library.

Since -L subfolders are omitted from -P roots, the index page should not be added to the sidebar

  $ ocamlc -c -bin-annot libname/unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname libname/unit.cmt
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname libname/index.mld

  $ odoc link -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname _odoc/pkg/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/unit.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/page-index.odoc

  $ odoc compile-index --root _odoc/pkg/
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-file.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-my_page.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/unit.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/page-index.odocl

  $ cat html/pkg/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul>
       <li><a href="#" class="current_unit">Package <code>pkg</code></a>
        <ul><li><a href="dir1/index.html">A directory</a></li>
         <li><a href="file.html">File</a></li>
         <li><a href="libname/index.html">Library landing page</a></li>
        </ul>
       </li>
      </ul>
     </nav>
    </div><div class="odoc-content"></div>
   </body>
  </html>

As we can see, "Library landing page" has NOT been added to the sidebar, in the "documentation" side.
