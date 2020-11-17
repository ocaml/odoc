Check resolution works

  $ cat top1.mld
  {0 Top1}
  {!child-sub1}
  {!child:sub2}
  $ cat sub1.mld
  {0 Sub1}
  {!child:M1}
  $ cat sub2.mld
  {0 Sub2}

  $ ocamlc -c -bin-annot m1.mli
  $ odoc compile top1.mld --child sub1 --child sub2
  $ odoc compile sub1.mld -I . --parent top1 --child m1
  $ odoc compile sub2.mld -I . --parent top1
  $ odoc compile m1.cmti -I . --parent sub1
  $ for i in *.odoc; do odoc link -I . $i; done
  $ for i in *.odocl; do odoc html-generate -o html $i; done

If everything has worked to plan, we'll have resolved references for all of the 'child' refs in the various pages. Additionally, the
references should be to the correct identifiers - so top1 should be a RootPage, sub1 is a Page, sub2 is a LeafPage, and m1 is a Root.

This is the '{!child-sub1}' reference
  $ odoc_print page-top1.odocl | jq '.content[1][1]["`Paragraph"][0][1]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Page": [
          {
            "`RootPage": "top1"
          },
          "sub1"
        ]
      }
    }
  }

This is the '{!child:sub2}' reference
  $ odoc_print page-top1.odocl | jq '.content[1][1]["`Paragraph"][2][1]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`LeafPage": [
          {
            "`RootPage": "top1"
          },
          "sub2"
        ]
      }
    }
  }

This is the '{!child:M1}' reference
  $ odoc_print page-sub1.odocl | jq '.content[1][1]["`Paragraph"][0][1]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Root": [
          {
            "`Page": [
              {
                "`RootPage": "top1"
              },
              "sub1"
            ]
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
  html/top1/sub2.html
