Check resolution works

  $ cat top1.mld
  {0 Top1}
  {!childpage-sub1}
  {!childpage:sub2}
  $ cat sub1.mld
  {0 Sub1}
  {!childmodule:M1}
  $ cat sub2.mld
  {0 Sub2}
  {!childpage:m1}
  
  $ cat m1.mld
  {0 M1}

  $ ocamlc -c -bin-annot m1.mli
  $ odoc compile top1.mld --child page-sub1 --child page-sub2
  $ odoc compile sub1.mld -I . --parent top1 --child m1
  $ odoc compile sub2.mld -I . --parent top1 --child page-m1
  $ odoc compile m1.cmti -I . --parent sub1
  $ odoc compile m1.mld -I . --parent sub2
  $ for i in *.odoc; do odoc link -I . $i; done
  $ for i in *.odocl; do odoc html-generate -o html $i; done

If everything has worked to plan, we'll have resolved references for all of the 'child' refs in the various pages. Additionally, the
references should be to the correct identifiers - so top1 should be a RootPage, sub1 is a Page, sub2 is a LeafPage, and m1 is a Root.

This is the '{!childpage-sub1}' reference
  $ odoc_print page-top1.odocl | jq '.content[1]["`Paragraph"][0]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Page": [
          {
            "Some": {
              "`Page": [
                "None",
                "top1"
              ]
            }
          },
          "sub1"
        ]
      }
    }
  }

This is the '{!childpage:sub2}' reference
  $ odoc_print page-top1.odocl | jq '.content[1]["`Paragraph"][2]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Page": [
          {
            "Some": {
              "`Page": [
                "None",
                "top1"
              ]
            }
          },
          "sub2"
        ]
      }
    }
  }

This is the '{!childmodule:M1}' reference
  $ odoc_print page-sub1.odocl | jq '.content[1]["`Paragraph"][0]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Root": [
          {
            "Some": {
              "`Page": [
                {
                  "Some": {
                    "`Page": [
                      "None",
                      "top1"
                    ]
                  }
                },
                "sub1"
              ]
            }
          },
          "M1"
        ]
      }
    }
  }

Let's also check the hierarchy of files produced:

  $ odoc support-files -o html
  $ find html -type f | sort
  html/highlight.pack.js
  html/odoc.css
  html/top1/index.html
  html/top1/sub1/M1/index.html
  html/top1/sub1/index.html
  html/top1/sub2/index.html
  html/top1/sub2/m1.html
