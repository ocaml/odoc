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
  $ odoc compile sub1.mld -I . --parent top1 --child M1
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
  html/fonts/KaTeX_AMS-Regular.woff2
  html/fonts/KaTeX_Caligraphic-Bold.woff2
  html/fonts/KaTeX_Caligraphic-Regular.woff2
  html/fonts/KaTeX_Fraktur-Bold.woff2
  html/fonts/KaTeX_Fraktur-Regular.woff2
  html/fonts/KaTeX_Main-Bold.woff2
  html/fonts/KaTeX_Main-BoldItalic.woff2
  html/fonts/KaTeX_Main-Italic.woff2
  html/fonts/KaTeX_Main-Regular.woff2
  html/fonts/KaTeX_Math-BoldItalic.woff2
  html/fonts/KaTeX_Math-Italic.woff2
  html/fonts/KaTeX_SansSerif-Bold.woff2
  html/fonts/KaTeX_SansSerif-Italic.woff2
  html/fonts/KaTeX_SansSerif-Regular.woff2
  html/fonts/KaTeX_Script-Regular.woff2
  html/fonts/KaTeX_Size1-Regular.woff2
  html/fonts/KaTeX_Size2-Regular.woff2
  html/fonts/KaTeX_Size3-Regular.woff2
  html/fonts/KaTeX_Size4-Regular.woff2
  html/fonts/KaTeX_Typewriter-Regular.woff2
  html/fonts/fira-mono-v14-latin-500.woff2
  html/fonts/fira-mono-v14-latin-regular.woff2
  html/fonts/fira-sans-v17-latin-500.woff2
  html/fonts/fira-sans-v17-latin-500italic.woff2
  html/fonts/fira-sans-v17-latin-700.woff2
  html/fonts/fira-sans-v17-latin-700italic.woff2
  html/fonts/fira-sans-v17-latin-italic.woff2
  html/fonts/fira-sans-v17-latin-regular.woff2
  html/fonts/noticia-text-v15-latin-700.woff2
  html/fonts/noticia-text-v15-latin-italic.woff2
  html/fonts/noticia-text-v15-latin-regular.woff2
  html/highlight.pack.js
  html/katex.min.css
  html/katex.min.js
  html/odoc.css
  html/odoc_search.js
  html/top1/index.html
  html/top1/sub1/M1/index.html
  html/top1/sub1/index.html
  html/top1/sub2/index.html
  html/top1/sub2/m1.html
