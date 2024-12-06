The "canonical" hierarchy is to put pages in `pkgname/` and libraries in
`pkgname/libraryname/`.

While it can create name clashes, the generated hierarchy is more natural.

  $ ocamlc -c -bin-annot libname/unit.ml

Let's generate the hierarchy:

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname libname/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname libname/unit.cmt

Let's link it:

  $ odoc link -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname _odoc/pkg/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/unit.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/page-index.odoc

Let's html-generate it (with a sidebar), separating the sidebar for pages and the library root:

  $ odoc compile-index --root _odoc/pkg/ --root _odoc/pkg/libname

For manual inspection:
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-file.odocl
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-my_page.odocl
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-index.odocl
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-index.odocl
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/unit.odocl
$ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/page-index.odocl

Now, let's see the result
  $ print_index index.odoc-index | jq
  {
    "node": "{\"url\":\"pkg/index.html\",\"html\":\"\"}",
    "children": [
      {
        "node": "{\"url\":\"pkg/file.html\",\"html\":\"\"}",
        "children": []
      },
      {
        "node": "{\"url\":\"pkg/dir1/index.html\",\"html\":\"\"}",
        "children": [
          {
            "node": "{\"url\":\"pkg/dir1/my_page.html\",\"html\":\"\"}",
            "children": []
          }
        ]
      }
    ]
  }
  {
    "node": "{\"url\":\"pkg/libname/index.html\",\"html\":\"\"}",
    "children": [
      {
        "node": "{\"url\":\"pkg/libname/Unit/index.html\",\"html\":\"\"}",
        "children": [
          {
            "node": "{\"url\":\"pkg/libname/Unit/index.html#val-x\",\"html\":\"\"}",
            "children": []
          }
        ]
      }
    ]
  }
