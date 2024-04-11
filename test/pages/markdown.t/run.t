Here is a markdown file

  $ cat file.md
  # Some title
  
  A little text
  
  - An item
  - list
  
  1. And an ordered
  2. one
  
  Some **emphasized** and _italic_ text.
  
  Some $E=mc^2$ inline math!
  
  Absurd but true:
  
  ```math
  \bot\vdash\top
  ```
  
  [text][!val-x] and [!val-y]
  
  | Un  | Deux | Trois | Quatre |
  |-----|:-----|------:|:------:|
  | One | Two  | Three | Four   |
  | Un  | Dos  |  Tres | Cuatro |
  
  | Un  | Deux |
  | One | Two  |
  | Un  | Dos  |
  
  | Un  | Deux | Trois |
  | One | Two  |
  | Un  |
  
  
  # A heading {#with-a-label}
  
  I'm referencing the [label][!"with-a-label"].

Compile and link:

  $ odoc compile file.md
  File "file.md", line 37, characters 0-27:
  Warning: '{1': heading level should be lower than top heading level '1'.
  $ odoc link page-file.odoc
  File "file.md", line 21, characters 19-27:
  Warning: Failed to resolve reference unresolvedroot(y) Couldn't find "y"
  File "file.md", line 21, characters 0-14:
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
          { "`Word": "Some" },
          "`Space",
          { "`Math_span": "E=mc^2" },
          "`Space",
          { "`Word": "inline" },
          "`Space",
          { "`Word": "math!" }
        ]
      },
      {
        "`Paragraph": [
          { "`Word": "Absurd" },
          "`Space",
          { "`Word": "but" },
          "`Space",
          { "`Word": "true:" }
        ]
      },
      { "`Math_block": "\\bot\\vdash\\top" },
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
      },
      {
        "`Table": [
          [
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Header" ],
              [ [ { "`Paragraph": [ { "`Word": "Deux" } ] } ], "`Header" ],
              [ [ { "`Paragraph": [ { "`Word": "Trois" } ] } ], "`Header" ],
              [ [ { "`Paragraph": [ { "`Word": "Quatre" } ] } ], "`Header" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "One" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Two" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Three" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Four" } ] } ], "`Data" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Dos" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Tres" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Cuatro" } ] } ], "`Data" ]
            ]
          ],
          {
            "Some": [
              "None",
              { "Some": "`Left" },
              { "Some": "`Right" },
              { "Some": "`Center" }
            ]
          }
        ]
      },
      {
        "`Table": [
          [
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Deux" } ] } ], "`Data" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "One" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Two" } ] } ], "`Data" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Dos" } ] } ], "`Data" ]
            ]
          ],
          "None"
        ]
      },
      {
        "`Table": [
          [
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Deux" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Trois" } ] } ], "`Data" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "One" } ] } ], "`Data" ],
              [ [ { "`Paragraph": [ { "`Word": "Two" } ] } ], "`Data" ],
              [ [], "`Data" ]
            ],
            [
              [ [ { "`Paragraph": [ { "`Word": "Un" } ] } ], "`Data" ],
              [ [], "`Data" ],
              [ [], "`Data" ]
            ]
          ],
          "None"
        ]
      },
      {
        "`Heading": [
          { "heading_level": "`Section", "heading_label_explicit": "true" },
          { "`Label": [ { "`LeafPage": [ "None", "file" ] }, "with-a-label" ] },
          [ { "`Word": "A" }, "`Space", { "`Word": "heading" }, "`Space" ]
        ]
      },
      {
        "`Paragraph": [
          { "`Word": "I'm" },
          "`Space",
          { "`Word": "referencing" },
          "`Space",
          { "`Word": "the" },
          "`Space",
          {
            "`Reference": [
              {
                "`Resolved": {
                  "`Identifier": {
                    "`Label": [
                      { "`LeafPage": [ "None", "file" ] }, "with-a-label"
                    ]
                  }
                }
              },
              [ { "`Word": "label" } ]
            ]
          },
          { "`Word": "." }
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
    <link rel="stylesheet" href="katex.min.css"/>
    <script src="katex.min.js"></script>
    <script>
     
  //<![CDATA[
  
            document.addEventListener("DOMContentLoaded", function () {
              var elements = Array.from(document.getElementsByClassName("odoc-katex-math"));
              for (var i = 0; i < elements.length; i++) {
                var el = elements[i];
                var content = el.textContent;
                var new_el = document.createElement("span");
                new_el.setAttribute("class", "odoc-katex-math-rendered");
                var display = el.classList.contains("display");
                katex.render(content, new_el, { throwOnError: false, displayMode: display });
                el.replaceWith(new_el);
              }
            });
          
  //]]>
  
    </script>
   </head>
   <body class="odoc">
    <header class="odoc-preamble">
     <h2 id="some-title"><a href="#some-title" class="anchor"></a>Some title
     </h2><p>A little text</p><ul><li>An item</li><li>list</li></ul>
     <ol><li>And an ordered</li><li>one</li></ol>
     <p>Some <b>emphasized</b> and <em>italic</em> text.</p>
     <p>Some <code class="odoc-katex-math">E=mc^2</code> inline math!</p>
     <p>Absurd but true:</p>
     <div><pre class="odoc-katex-math display">\bot\vdash\top</pre></div>
     <p><span class="xref-unresolved" title="x">text</span> and <code>y</code>
     </p>
     <table class="odoc-table">
      <tr><th><p>Un</p></th><th style="text-align:left"><p>Deux</p></th>
       <th style="text-align:right"><p>Trois</p></th>
       <th style="text-align:center"><p>Quatre</p></th>
      </tr>
      <tr><td><p>One</p></td><td style="text-align:left"><p>Two</p></td>
       <td style="text-align:right"><p>Three</p></td>
       <td style="text-align:center"><p>Four</p></td>
      </tr>
      <tr><td><p>Un</p></td><td style="text-align:left"><p>Dos</p></td>
       <td style="text-align:right"><p>Tres</p></td>
       <td style="text-align:center"><p>Cuatro</p></td>
      </tr>
     </table>
     <table class="odoc-table"><tr><td><p>Un</p></td><td><p>Deux</p></td></tr>
      <tr><td><p>One</p></td><td><p>Two</p></td></tr>
      <tr><td><p>Un</p></td><td><p>Dos</p></td></tr>
     </table>
     <table class="odoc-table">
      <tr><td><p>Un</p></td><td><p>Deux</p></td><td><p>Trois</p></td></tr>
      <tr><td><p>One</p></td><td><p>Two</p></td><td></td></tr>
      <tr><td><p>Un</p></td><td></td><td></td></tr>
     </table>
     <h2 id="with-a-label"><a href="#with-a-label" class="anchor"></a>A heading 
     </h2>
     <p>I'm referencing the 
      <a href="#with-a-label" title="with-a-label">label</a>.
     </p>
    </header><div class="odoc-content"></div>
   </body>
  </html>
