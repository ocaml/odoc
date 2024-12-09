  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --parent-id pkg --output-dir _odoc unit.cmt
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc compile --parent-id pkg --output-dir _odoc content.mld
  $ odoc compile --parent-id pkg --output-dir _odoc omitted.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/index.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/content_in_dir.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/dontent.mld

  $ odoc link _odoc/pkg/page-index.odoc
  $ odoc link _odoc/pkg/unit.odoc
  $ odoc link _odoc/pkg/page-content.odoc
  $ odoc link _odoc/pkg/page-omitted.odoc
  $ odoc link _odoc/pkg/dir1/page-index.odoc
  $ odoc link _odoc/pkg/dir1/page-content_in_dir.odoc
  $ odoc link _odoc/pkg/dir1/page-dontent.odoc

  $ odoc compile-index --root _odoc/pkg
  File "index.mld", line 1, characters 42-47:
  Warning: Duplicate 'dir1/' in (children).
  File "index.mld", line 1, characters 48-52:
  Warning: 'typo' in (children) does not correspond to anything.
  File "index.mld", line 1, characters 0-52:
  Warning: (children) doesn't include 'omitted'.

Turn the index into a sidebar (removes all unnecessary entries)
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/page-index.odocl
  $ odoc html-generate --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/page-content.odocl
  $ odoc html-generate --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/page-omitted.odocl
  $ odoc html-generate --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/dir1/page-content_in_dir.odocl
  $ odoc html-generate --sidebar sidebar.odoc-sidebar -o _html  _odoc/pkg/dir1/page-dontent.odocl
  $ odoc support-files -o _html

  $ odoc_print _odoc/pkg/page-index.odocl | jq .frontmatter
  {
    "children": {
      "Some": [
        {
          "Page": "content"
        },
        {
          "Module": "Unit"
        },
        {
          "Dir": "dir1"
        },
        {
          "Dir": "dir1"
        },
        {
          "Page": "typo"
        }
      ]
    },
    "short_title": "None",
    "toc_status": "None",
    "order_category": "None"
  }


$ cp -r _html /tmp/html

The order in toplevel should be as given by the children field, and by
alphabetical order on the filename in dir1.
Omitted has been added in the children of index, after the ones that were ordered.
Typo is in the children field of index, but does not exist. It is omitted to,
but this should be a warning!

  $ cat _html/pkg/index.html | grep odoc-global-toc -A 11
     <nav class="odoc-toc odoc-global-toc">
      <ul>
       <li><a href="#" class="current_unit">This is the main index</a>
        <ul><li><a href="content.html">This is top level content</a></li>
         <li><a href="Unit/index.html">Unit</a></li>
         <li><a href="dir1/index.html">This is dir1's index</a></li>
         <li><a href="omitted.html">This one is omitted</a></li>
        </ul>
       </li>
      </ul>
     </nav>
    </div><div class="odoc-content"></div>


Some more parsing test:

  $ mkdir errors
  $ cat << EOF >> errors/index.mld
  > @children_order [Some wrong content]
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg/doc --output-dir _odoc errors/index.mld
  File "errors/index.mld", line 1, characters 16-36:
  Warning: Only words are accepted when specifying children order

  $ mkdir valid
  $ cat << EOF > valid/index.mld
  > @children_order a
  >            b                     c
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg/doc --output-dir _odoc valid/index.mld

  $ cat << EOF > errors/index.mld
  > {0 Test1}
  > @children_order a
  > EOF
  $ odoc compile --parent-id pkg/doc --output-dir _odoc errors/index.mld
  File "errors/index.mld", line 2, characters 0-17:
  Warning: @children_order tag has to be before any content

  $ cat << EOF > errors/not_index.mld
  > @children_order a
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg/doc --output-dir _odoc errors/not_index.mld
  File "errors/not_index.mld":
  Warning: Non-index page cannot specify @children_order.
