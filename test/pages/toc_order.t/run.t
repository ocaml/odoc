  $ odoc compile --parent-id pkg/doc --output-dir _odoc index.mld
  $ odoc compile --parent-id pkg/doc --output-dir _odoc content.mld
  $ odoc compile --parent-id pkg/doc --output-dir _odoc omitted.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/index.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/content_in_dir.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/dontent.mld

  $ odoc link _odoc/pkg/doc/page-index.odoc
  $ odoc link _odoc/pkg/doc/page-content.odoc
  $ odoc link _odoc/pkg/doc/page-omitted.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-content_in_dir.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-dontent.odoc

  $ odoc compile-index -P test:_odoc/pkg/doc

  $ odoc html-generate --indent --index index.odoc-index -o _html  _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-content.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-omitted.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-content_in_dir.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-dontent.odocl
  $ odoc support-files -o _html

  $ odoc_print _odoc/pkg/doc/page-index.odocl | jq .frontmatter
  {
    "children": {
      "Some": [
        {
          "Page": "content"
        },
        {
          "Dir": "dir1"
        },
        {
          "Page": "typo"
        }
      ]
    }
  }


$ cp -r _html /tmp/html

The order in toplevel should be as given by the children field, and by
alphabetical order on the filename in dir1.
Omitted has been omitted in the children of index, so it does not appear.
Typo is in the children field of index, but does not exist. It is omitted to,
but this should be a warning!

  $ cat _html/pkg/doc/index.html | grep odoc-global-toc -A 11
     <nav class="odoc-toc odoc-global-toc"><b>test's Pages</b>
      <ul>
       <li><a href="#" class="current_unit">This is the main index</a>
        <ul><li><a href="content.html">This is top level content</a></li>
         <li><a href="dir1/index.html">This is dir1's index</a>
          <ul>
           <li>
            <a href="dir1/content_in_dir.html">This is some content in dir1</a>
           </li><li><a href="dir1/dontent.html">The name is dontent</a></li>
          </ul>
         </li>
        </ul>
