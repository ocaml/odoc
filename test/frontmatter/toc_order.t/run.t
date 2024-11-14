  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc compile --parent-id pkg --output-dir _odoc content.mld
  $ odoc compile --parent-id pkg --output-dir _odoc omitted.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/index.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/content_in_dir.mld
  $ odoc compile --parent-id pkg/dir1 --output-dir _odoc dir1/dontent.mld

  $ odoc link _odoc/pkg/page-index.odoc
  $ odoc link _odoc/pkg/page-content.odoc
  $ odoc link _odoc/pkg/page-omitted.odoc
  $ odoc link _odoc/pkg/dir1/page-index.odoc
  $ odoc link _odoc/pkg/dir1/page-content_in_dir.odoc
  $ odoc link _odoc/pkg/dir1/page-dontent.odoc

  $ odoc compile-index -P test:_odoc/pkg
  File "index.mld", line 1, characters 30-35:
  Warning: Duplicate 'dir1/' in (children).
  File "index.mld", line 1, characters 36-40:
  Warning: 'typo' in (children) does not correspond to anything.
  File "index.mld", line 1, characters 0-40:
  Warning: (children) doesn't include 'omitted'.

  $ odoc html-generate --indent --index index.odoc-index -o _html  _odoc/pkg/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/page-content.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/page-omitted.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/dir1/page-content_in_dir.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/dir1/page-dontent.odocl
  $ odoc support-files -o _html

  $ odoc_print _odoc/pkg/page-index.odocl | jq .frontmatter
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
          "Dir": "dir1"
        },
        {
          "Page": "typo"
        }
      ]
    },
    "short_title": "None"
  }


$ cp -r _html /tmp/html

The order in toplevel should be as given by the children field, and by
alphabetical order on the filename in dir1.
Omitted has been added in the children of index, after the ones that were ordered.
Typo is in the children field of index, but does not exist. It is omitted to,
but this should be a warning!

  $ cat _html/pkg/index.html | grep odoc-global-toc -A 11
     <nav class="odoc-toc odoc-global-toc"><ul class="odoc-modules"></ul>
      <b>Documentation</b>
      <ul class="odoc-pages">
       <li><a href="#" class="current_unit">This is the main index</a>
        <ul><li><a href="content.html">This is top level content</a></li>
         <li><a href="dir1/index.html">This is dir1's index</a>
          <ul>
           <li>
            <a href="dir1/content_in_dir.html">This is some content in dir1</a>
           </li><li><a href="dir1/dontent.html">The name is dontent</a></li>
          </ul>
         </li><li><a href="omitted.html">This one is omitted</a></li>


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
