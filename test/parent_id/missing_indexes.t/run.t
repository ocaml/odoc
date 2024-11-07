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

  $ odoc html-generate --index index.odoc-index --indent --output-dir _html _odoc/page-foo.odocl

Baz is missing.
  $ cat _html/foo.html | grep Documentation -A 7
      <b>Documentation</b>
      <ul class="odoc-pages">
       <li>root
        <ul><li><a href="bar.html">Bar</a></li>
         <li><a href="#" class="current_unit">Foo</a></li>
        </ul>
       </li>
      </ul>
