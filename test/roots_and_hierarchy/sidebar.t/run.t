  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname unit.cmt
  $ odoc compile-impl --output-dir _odoc/ --source-id pkg/src/libname/unit.ml --parent-id pkg/libname unit.cmt

  $ odoc link -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname _odoc/pkg/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/unit.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/impl-unit.odoc

  $ odoc compile-index --root _odoc/pkg/
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-file.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-my_page.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/unit.odocl
  $ odoc html-generate-source --indent --impl _odoc/pkg/libname/impl-unit.odocl --sidebar sidebar.odoc-sidebar -o html unit.ml

A json version of a sidebar can be obtained using the sidebar-generate command:

  $ odoc sidebar-generate --json index.odoc-index
  $ cat sidebar.json | jq
  [
    {
      "node": {
        "url": "pkg/index.html",
        "kind": "leaf-page",
        "content": "Package <code>pkg</code>"
      },
      "children": [
        {
          "node": {
            "url": "pkg/dir1/index.html",
            "kind": "leaf-page",
            "content": "A directory"
          },
          "children": [
            {
              "node": {
                "url": "pkg/dir1/my_page.html",
                "kind": "leaf-page",
                "content": "My page"
              },
              "children": []
            }
          ]
        },
        {
          "node": {
            "url": "pkg/file.html",
            "kind": "leaf-page",
            "content": "File"
          },
          "children": []
        },
        {
          "node": {
            "url": null,
            "kind": null,
            "content": "libname"
          },
          "children": [
            {
              "node": {
                "url": "pkg/libname/Unit/index.html",
                "kind": "module",
                "content": "Unit"
              },
              "children": [
                {
                  "node": {
                    "url": "pkg/libname/Unit/X/index.html",
                    "kind": "module",
                    "content": "X"
                  },
                  "children": [
                    {
                      "node": {
                        "url": "pkg/libname/Unit/X/Y/index.html",
                        "kind": "module",
                        "content": "Y"
                      },
                      "children": []
                    },
                    {
                      "node": {
                        "url": "pkg/libname/Unit/X/index.html#module-Z",
                        "kind": "module",
                        "content": "Z"
                      },
                      "children": []
                    }
                  ]
                },
                {
                  "node": {
                    "url": "pkg/libname/Unit/module-type-Foo/index.html",
                    "kind": "module-type",
                    "content": "Foo"
                  },
                  "children": []
                }
              ]
            }
          ]
        },
        {
          "node": {
            "url": null,
            "kind": null,
            "content": "src"
          },
          "children": [
            {
              "node": {
                "url": null,
                "kind": null,
                "content": "libname"
              },
              "children": [
                {
                  "node": {
                    "url": "pkg/src/libname/unit.ml.html",
                    "kind": "source",
                    "content": "unit.ml"
                  },
                  "children": []
                }
              ]
            }
          ]
        }
      ]
    }
  ]

  $ cat html/pkg/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul>
       <li><a href="#" class="current_unit">Package <code>pkg</code></a>
        <ul><li><a href="dir1/index.html">A directory</a></li>
         <li><a href="file.html">File</a></li>
         <li>libname
          <ul><li><a href="libname/Unit/index.html">Unit</a></li></ul>
         </li>
         <li>src
          <ul>
           <li>libname
            <ul><li><a href="src/libname/unit.ml.html">unit.ml</a></li></ul>
           </li>
          </ul>
         </li>
        </ul>

  $ cat html/pkg/libname/Unit/X/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul>
       <li><a href="../../../index.html">Package <code>pkg</code></a>
        <ul><li><a href="../../../dir1/index.html">A directory</a></li>
         <li><a href="../../../file.html">File</a></li>
         <li>libname
          <ul>
           <li><a href="../index.html">Unit</a>
            <ul>
             <li><a href="#" class="current_unit">X</a>
              <ul><li><a href="Y/index.html">Y</a></li>
               <li><a href="#module-Z">Z</a></li>
              </ul>
             </li><li><a href="../module-type-Foo/index.html">Foo</a></li>
            </ul>
           </li>

  $ odoc support-files -o html
