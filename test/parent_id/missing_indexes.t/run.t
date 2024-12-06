  $ echo "{0 Foo}" > foo.mld
  $ echo "{0 Bar}" > bar.mld
  $ echo "{0 Bli}" > bli.mld
  $ odoc compile --parent-id "" --output-dir _odoc foo.mld
  $ odoc compile --parent-id "" --output-dir _odoc bar.mld
  $ odoc compile --parent-id "baz" --output-dir _odoc bli.mld
  $ odoc link _odoc/page-foo.odoc
  $ odoc link _odoc/page-bar.odoc
  $ odoc link _odoc/baz/page-bli.odoc
  $ odoc compile-index -P _:_odoc
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --sidebar sidebar.odoc-sidebar --indent --output-dir _html _odoc/page-foo.odocl

Missing index for Baz makes it unclickable but use the ID for the name.
Root is used for the missing index in the unnamed root directory.
  $ cat _html/foo.html | grep Documentation -A 8
      <b>Documentation</b>
      <ul class="odoc-pages">
       <li>Root
        <ul><li><a href="bar.html">Bar</a></li>
         <li>baz<ul><li><a href="baz/bli.html">Bli</a></li></ul></li>
         <li><a href="#" class="current_unit">Foo</a></li>
        </ul>
       </li>
      </ul>
