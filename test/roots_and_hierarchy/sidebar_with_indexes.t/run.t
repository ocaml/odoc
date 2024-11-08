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

  $ odoc compile-index -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname -o sidebar.odoc-index
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/page-file.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/dir1/page-my_page.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/page-index.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/libname/unit.odocl
  $ odoc html-generate --indent --index sidebar.odoc-index -o html _odoc/pkg/libname/page-index.odocl

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

As we can see, "Library landing page" has NOT been added to the sidebar, in the "documentation" side.
