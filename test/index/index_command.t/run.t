Make sure wrapped libraries don't interfere with generating the source code.
Test both canonical paths and hidden units.
It's a simpler case than Dune's wrapping.

$ odoc compile -c module-main -c src-source root.mld

  $ ocamlc -c j.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --search-asset index.js -I . --child asset-index.js --child module-main --child module-j page.mld

Search scripts are given as a reference to an asset.

An example with an error during the resolving of the reference:
  $ odoc compile --parent page --search-asset index2.js -I . j.cmt
  $ odoc link -I . j.odoc
  File "j.odoc":
  Warning: Failed to resolve asset reference unresolvedroot(index2.js) Couldn't find asset "index2.js"

Without error during resolving
  $ odoc compile --parent page --search-asset index.js -I . j.cmt
  $ odoc compile --parent page --search-asset index.js -I . main.cmt

  $ odoc link -I . j.odoc
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc

  $ odoc compile-index -I .

  $ cat index.json | jq
  [
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Module"
      },
      "display": {
        "url": "page/Main/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">module</div><div>module Main = struct ... end</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Type",
          "name": "t"
        }
      ],
      "doc": "A comment",
      "extra": {
        "kind": "TypeDecl",
        "private": false,
        "manifest": "int",
        "constraints": []
      },
      "display": {
        "url": "page/Main/index.html#type-t",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">type</div><div>type t</div><div><p>A comment</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Type",
          "name": "tdzdz"
        }
      ],
      "doc": "A comment aaaaaaaaaa",
      "extra": {
        "kind": "TypeDecl",
        "private": false,
        "manifest": null,
        "constraints": []
      },
      "display": {
        "url": "page/Main/index.html#type-tdzdz",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">type</div><div>type tdzdz = &lt;variant&gt;</div><div><p>A comment aaaaaaaaaa</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Type",
          "name": "tdzdz"
        },
        {
          "kind": "Constructor",
          "name": "A"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Constructor",
        "args": {
          "kind": "Tuple",
          "vals": [
            "int",
            "int"
          ]
        },
        "res": "tdzdz"
      },
      "display": {
        "url": "page/Main/index.html#type-tdzdz.A",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">constructor</div><div>constructor A : </div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Type",
          "name": "tdzdz"
        },
        {
          "kind": "Constructor",
          "name": "B"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Constructor",
        "args": {
          "kind": "Tuple",
          "vals": [
            "int list",
            "int"
          ]
        },
        "res": "tdzdz"
      },
      "display": {
        "url": "page/Main/index.html#type-tdzdz.B",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">constructor</div><div>constructor B : </div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Label",
          "name": "this-is-a-title"
        }
      ],
      "doc": "this is a title",
      "extra": {
        "kind": "Doc",
        "subkind": "Heading"
      },
      "display": {
        "url": "page/Main/index.html#this-is-a-title",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>this is a title</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        }
      ],
      "doc": "and this is a paragraph",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>and this is a paragraph</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "M"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Module"
      },
      "display": {
        "url": "page/Main/M/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">module</div><div>module M : sig ... end</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "M"
        },
        {
          "kind": "Type",
          "name": "t"
        }
      ],
      "doc": "dsdsd",
      "extra": {
        "kind": "TypeDecl",
        "private": false,
        "manifest": null,
        "constraints": []
      },
      "display": {
        "url": "page/Main/M/index.html#type-t",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">type</div><div>type t</div><div><p>dsdsd</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "v"
        }
      ],
      "doc": "a reference , and some formatted content with code and\ncode blocks",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-v",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val v : int</div><div><p>a reference <span><code>t</code></span>, and some <em>formatted</em> <b>content</b> with <code>code</code> and</p><pre class=\"language-ocaml\"><code>code blocks</code></pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "lorem"
        }
      ],
      "doc": "lorem 1 and a link",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-lorem",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val lorem : int</div><div><p>lorem 1 and a <span>link</span></p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "lorem2"
        }
      ],
      "doc": "lorem 2",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-lorem2",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val lorem2 : int</div><div><p>lorem 2</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "lorem3"
        }
      ],
      "doc": "lorem 3",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-lorem3",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val lorem3 : int</div><div><p>lorem 3</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "lorem4"
        }
      ],
      "doc": "lorem 4",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-lorem4",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val lorem4 : int</div><div><p>lorem 4</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Module"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">module</div><div>module I : sig ... end</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        },
        {
          "kind": "Value",
          "name": "x"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/I/index.html#val-x",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val x : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "a paragraph",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>a paragraph</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "and another",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>and another</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "verbatim",
      "extra": {
        "kind": "Doc",
        "subkind": "Verbatim"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre>verbatim</pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "x + 1",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "blibli",
      "extra": {
        "kind": "Doc",
        "subkind": "CodeBlock"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        },
        {
          "kind": "Value",
          "name": "y"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/I/index.html#val-y",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val y : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "x"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-x",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val x : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "a paragraph",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>a paragraph</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "and another",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>and another</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "verbatim",
      "extra": {
        "kind": "Doc",
        "subkind": "Verbatim"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre>verbatim</pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "x + 1",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "blibli",
      "extra": {
        "kind": "Doc",
        "subkind": "CodeBlock"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "y"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-y",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val y : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Value",
          "name": "uu"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/Main/index.html#val-uu",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val uu : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "Main"
        },
        {
          "kind": "Module",
          "name": "I"
        }
      ],
      "doc": "a paragraph two",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/Main/I/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>a paragraph two</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "J"
        }
      ],
      "doc": "a paragraph one",
      "extra": {
        "kind": "Module"
      },
      "display": {
        "url": "page/J/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">module</div><div>module J = struct ... end</div><div><p>a paragraph one</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "J"
        },
        {
          "kind": "Value",
          "name": "uu"
        }
      ],
      "doc": "",
      "extra": {
        "kind": "Value",
        "type": "int"
      },
      "display": {
        "url": "page/J/index.html#val-uu",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">value</div><div>val uu : int</div><div></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Root",
          "name": "J"
        }
      ],
      "doc": "a paragraph two",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/J/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>a paragraph two</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        },
        {
          "kind": "Label",
          "name": "a-title"
        }
      ],
      "doc": "A title",
      "extra": {
        "kind": "Doc",
        "subkind": "Heading"
      },
      "display": {
        "url": "page/index.html#a-title",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>A title</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        }
      ],
      "doc": "A paragraph",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>A paragraph</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        }
      ],
      "doc": "some verbatim",
      "extra": {
        "kind": "Doc",
        "subkind": "Verbatim"
      },
      "display": {
        "url": "page/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre>some verbatim</pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        }
      ],
      "doc": "and code",
      "extra": {
        "kind": "Doc",
        "subkind": "CodeBlock"
      },
      "display": {
        "url": "page/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><pre class=\"language-ocaml\"><code>and code</code></pre></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        }
      ],
      "doc": "a list of things",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>a list <em>of</em> things</p></div></div>"
      }
    },
    {
      "id": [
        {
          "kind": "Page",
          "name": "page"
        }
      ],
      "doc": "bliblib",
      "extra": {
        "kind": "Doc",
        "subkind": "Paragraph"
      },
      "display": {
        "url": "page/index.html",
        "html": "<div class=\"search-entry\"><div class=\"entry-kind\">doc</div><div></div><div><p>bliblib</p></div></div>"
      }
    }
  ]

The index.js file need to provide a odoc_search command, from a 

  $ cat fuse.js.js > index.js
  $ echo "\n\nlet documents = " >> index.js
  $ cat index.json >> index.js

  $ echo "\n\nconst options = { keys: ['id', 'doc'] };" >> index.js
  $ echo "\nvar idx_fuse = new Fuse(documents, options);" >> index.js
  $ echo "\nonmessage = (m) => {\n  let query = m.data;\n  let result = idx_fuse.search(query);\n  postMessage(result.slice(0,200).map(a => a.item.display));};" >> index.js

  $ odoc html-generate -o html j.odocl
  $ odoc html-generate -o html main.odocl
  $ odoc html-generate --asset index.js -o html page-page.odocl
  $ odoc support-files --search-file index.js -o html

  $ find html | sort
  html
  html/fonts
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
  html/index.js
  html/katex.min.css
  html/katex.min.js
  html/odoc.css
  html/odoc_search.js
  html/page
  html/page/J
  html/page/J/index.html
  html/page/Main
  html/page/Main/I
  html/page/Main/I/index.html
  html/page/Main/M
  html/page/Main/M/index.html
  html/page/Main/index.html
  html/page/index.html
  html/page/index.js

Run
$ firefox html/page/Main/index.html
to manually test the search
