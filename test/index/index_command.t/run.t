Make sure wrapped libraries don't interfere with generating the source code.
Test both canonical paths and hidden units.
It's a simpler case than Dune's wrapping.

$ odoc compile -c module-main -c src-source root.mld

  $ ocamlc -c j.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile -I . j.cmt
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld

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
        "html": "<div class=\"search-entry module\"><code class=\"entry-title\"><span class=\"entry-kind\">module</span><span class=\"prefix-name\"></span><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/index.html"
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
        "html": "<div class=\"search-entry type\"><code class=\"entry-title\"><span class=\"entry-kind\">type</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">t</span><code class=\"entry-rhs\"> = int</code></code><div class=\"entry-comment\"><div><p>A comment</p></div></div></div>",
        "url": "Main/index.html#type-t"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">this-is-a-title</span></code><div class=\"entry-comment\"><div><p>this is a title</p></div></div></div>",
        "url": "Main/index.html#this-is-a-title"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div><p>and this is a paragraph</p></div></div></div>",
        "url": "Main/index.html"
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
        "html": "<div class=\"search-entry module\"><code class=\"entry-title\"><span class=\"entry-kind\">module</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">M</span></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/M/index.html"
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
        "html": "<div class=\"search-entry type\"><code class=\"entry-title\"><span class=\"entry-kind\">type</span><span class=\"prefix-name\">Main.M.</span><span class=\"entry-name\">t</span></code><div class=\"entry-comment\"><div><p>dsdsd</p></div></div></div>",
        "url": "Main/M/index.html#type-t"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">v</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>a reference <span><code>t</code></span>, and some <em>formatted</em> <b>content</b> with <code>code</code> and</p><pre class=\"language-ocaml\"><code>code blocks</code></pre></div></div></div>",
        "url": "Main/index.html#val-v"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 1 and a <span>link</span></p></div></div></div>",
        "url": "Main/index.html#val-lorem"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem2</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 2</p></div></div></div>",
        "url": "Main/index.html#val-lorem2"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem3</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 3</p></div></div></div>",
        "url": "Main/index.html#val-lorem3"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem4</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 4</p></div></div></div>",
        "url": "Main/index.html#val-lorem4"
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
        "html": "<div class=\"search-entry module\"><code class=\"entry-title\"><span class=\"entry-kind\">module</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">x</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/I/index.html#val-x"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph</p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>and another</p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre>verbatim</pre></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">y</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/I/index.html#val-y"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">x</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/index.html#val-x"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph</p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>and another</p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre>verbatim</pre></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">y</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/index.html#val-y"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">uu</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "Main/index.html#val-uu"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div></div>",
        "url": "Main/I/index.html"
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
        "html": "<div class=\"search-entry module\"><code class=\"entry-title\"><span class=\"entry-kind\">module</span><span class=\"prefix-name\"></span><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph one</p></div></div></div>",
        "url": "J/index.html"
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
        "html": "<div class=\"search-entry val\"><code class=\"entry-title\"><span class=\"entry-kind\">val</span><span class=\"prefix-name\">J.</span><span class=\"entry-name\">uu</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>",
        "url": "J/index.html#val-uu"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div></div>",
        "url": "J/index.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\">page.</span><span class=\"entry-name\">a-title</span></code><div class=\"entry-comment\"><div><p>A title</p></div></div></div>",
        "url": "page.html#a-title"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>A paragraph</p></div></div></div>",
        "url": "page.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><pre>some verbatim</pre></div></div></div>",
        "url": "page.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>and code</code></pre></div></div></div>",
        "url": "page.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>a list <em>of</em> things</p></div></div></div>",
        "url": "page.html"
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
        "html": "<div class=\"search-entry doc\"><code class=\"entry-title\"><span class=\"entry-kind\">doc</span><span class=\"prefix-name\"></span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>bliblib</p></div></div></div>",
        "url": "page.html"
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

  $ odoc html-generate --search-file index.js -o html j.odocl
  $ odoc html-generate --search-file index.js -o html main.odocl
  $ odoc html-generate --search-file index.js -o html page-page.odocl
  $ odoc support-files --search-file index.js -o html

  $ find html | sort
  html
  html/J
  html/J/index.html
  html/Main
  html/Main/I
  html/Main/I/index.html
  html/Main/M
  html/Main/M/index.html
  html/Main/index.html
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
  html/page.html

Run
 $ firefox html/Main/index.html
to manually test the search
