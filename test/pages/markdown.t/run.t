Here is a markdown file

  $ cat file.md
  # Some title
  
  A little text
  
  - An item
  - list
  
  1. And an ordered
  2. one
  
  Some **emphasized** and _italic_ text.
  
  [text][!val-x] and [!val-y]

Compile and link:

  $ odoc compile file.md
  $ odoc link page-file.odoc
  File "file.md", line 13, characters 19-27:
  Warning: Failed to resolve reference unresolvedroot(y) Couldn't find "y"
  File "file.md", line 13, characters 0-14:
  Warning: Failed to resolve reference unresolvedroot(x) Couldn't find "x"

The locations seem correct.

The content of the markdown file after translation:

  $ odoc_print page-file.odocl
  {
    "name": { "`LeafPage": [ "None", "file" ] },
    "root": "<root>",
    "content": [
      {
        "`Heading": [
          { "heading_level": "`Section", "heading_label_explicit": "false" },
          { "`Label": [ { "`LeafPage": [ "None", "file" ] }, "some-title" ] },
          [ { "`Word": "Some" }, "`Space", { "`Word": "title" } ]
        ]
      },
      {
        "`Paragraph": [
          { "`Word": "A" },
          "`Space",
          { "`Word": "little" },
          "`Space",
          { "`Word": "text" }
        ]
      },
      {
        "`List": [
          "`Unordered",
          [
            [
              {
                "`Paragraph": [
                  { "`Word": "An" }, "`Space", { "`Word": "item" }
                ]
              }
            ],
            [ { "`Paragraph": [ { "`Word": "list" } ] } ]
          ]
        ]
      },
      {
        "`List": [
          "`Ordered",
          [
            [
              {
                "`Paragraph": [
                  { "`Word": "And" },
                  "`Space",
                  { "`Word": "an" },
                  "`Space",
                  { "`Word": "ordered" }
                ]
              }
            ],
            [ { "`Paragraph": [ { "`Word": "one" } ] } ]
          ]
        ]
      },
      {
        "`Paragraph": [
          { "`Word": "Some" },
          "`Space",
          { "`Styled": [ "`Bold", [ { "`Word": "emphasized" } ] ] },
          "`Space",
          { "`Word": "and" },
          "`Space",
          { "`Styled": [ "`Emphasis", [ { "`Word": "italic" } ] ] },
          "`Space",
          { "`Word": "text." }
        ]
      },
      {
        "`Paragraph": [
          {
            "`Reference": [
              { "`Root": [ "x", "`TValue" ] }, [ { "`Word": "text" } ]
            ]
          },
          "`Space",
          { "`Word": "and" },
          "`Space",
          { "`Reference": [ { "`Root": [ "y", "`TValue" ] }, [] ] }
        ]
      }
    ],
    "digest": "<digest>"
  }

Since it might be easier to read, here is the html generated:

  $ odoc html-generate -o html --indent page-file.odocl

  $ cat html/file.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>file (file)</title><meta charset="utf-8"/>
    <link rel="stylesheet" href="odoc.css"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <header class="odoc-preamble">
     <h2 id="some-title"><a href="#some-title" class="anchor"></a>Some title
     </h2><p>A little text</p><ul><li>An item</li><li>list</li></ul>
     <ol><li>And an ordered</li><li>one</li></ol>
     <p>Some <b>emphasized</b> and <em>italic</em> text.</p>
     <p><span class="xref-unresolved" title="x">text</span> and <code>y</code>
     </p>
    </header><div class="odoc-content"></div>
   </body>
  </html>
