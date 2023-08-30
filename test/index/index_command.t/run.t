Compile the files

  $ ocamlc -c j.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

When compiling with odoc, if we want support for search, we need to pass the --search-index argument.
This argument takes a reference to an asset, taken "litterary": index.js will be interpreted as asset-"index.js".
(As a consequence, the page or one of its parent must have the asset declared as child)

  $ odoc compile --search-asset fuse.js.js --search-asset index.js -I . --child asset-index.js --child asset-fuse.js.js --child module-main --child module-j page.mld

An example with an error during the resolving of the reference. Here, neither page nor j has index2.js as declared asset.
  $ odoc compile --parent page --search-asset index2.js -I . j.cmt
  $ odoc link -I . j.odoc
  File "j.odoc":
  Warning: Failed to resolve asset reference unresolvedroot(index2.js) Couldn't find asset "index2.js"

With the right command line arguments:
  $ odoc compile --parent page --search-asset fuse.js.js --search-asset index.js -I . j.cmt
  $ odoc compile --parent page --search-asset fuse.js.js --search-asset index.js -I . main.cmt

  $ odoc link -I . j.odoc
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc

This command generates a json index containing all .odocl files found in the search path (-I). If -o is not provided, the file is saved as index.json.
  $ odoc compile-index -I .

The index file, one entry per file
  $ cat index.json | jq sort | jq '.[]' -c
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"},{"kind":"Constructor","name":"A"}],"doc":"","kind":{"kind":"Constructor","args":{"kind":"Tuple","vals":["int","int"]},"res":"tdzdz"},"display":{"url":"page/Main/index.html#type-tdzdz.A","html":"<div class=\"search-entry\"><code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">A</span><code class=\"entry_rhs\"> : int * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"},{"kind":"Constructor","name":"B"}],"doc":"Bliiiiiiiiiii","kind":{"kind":"Constructor","args":{"kind":"Tuple","vals":["int list","int"]},"res":"tdzdz"},"display":{"url":"page/Main/index.html#type-tdzdz.B","html":"<div class=\"search-entry\"><code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">B</span><code class=\"entry_rhs\"> : int list * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div><p>Bliiiiiiiiiii</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"J"}],"doc":"a paragraph two","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/J/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"A paragraph","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>A paragraph</p></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"a list of things","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>a list <em>of</em> things</p></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"bliblib","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>bliblib</p></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"and code","kind":{"kind":"Doc","subkind":"CodeBlock"},"display":{"url":"page/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>and code</code></pre></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"some verbatim","kind":{"kind":"Doc","subkind":"Verbatim"},"display":{"url":"page/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><pre>some verbatim</pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"x + 1","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"x + 1","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p><code class=\"odoc-katex-math\">x + 1</code></p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"a paragraph two","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"a paragraph","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"a paragraph","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"and another","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>and another</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"and another","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>and another</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"blibli","kind":{"kind":"Doc","subkind":"CodeBlock"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"blibli","kind":{"kind":"Doc","subkind":"CodeBlock"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"verbatim","kind":{"kind":"Doc","subkind":"Verbatim"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre>verbatim</pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"verbatim","kind":{"kind":"Doc","subkind":"Verbatim"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><pre>verbatim</pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"X"}],"doc":"and this is a paragraph","kind":{"kind":"Doc","subkind":"Paragraph"},"display":{"url":"page/Main/X/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">X</span></code><div class=\"entry-comment\"><div><p>and this is a paragraph</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Label","name":"this-is-a-title"}],"doc":"this is a title","kind":{"kind":"Doc","subkind":"Heading"},"display":{"url":"page/Main/index.html#this-is-a-title","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">this-is-a-title</span></code><div class=\"entry-comment\"><div><p>this is a title</p></div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"},{"kind":"Label","name":"a-title"}],"doc":"A title","kind":{"kind":"Doc","subkind":"Heading"},"display":{"url":"page/index.html#a-title","html":"<div class=\"search-entry\"><code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">page.</span><span class=\"entry-name\">a-title</span></code><div class=\"entry-comment\"><div><p>A title</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"J"}],"doc":"a paragraph one","kind":{"kind":"Module"},"display":{"url":"page/J/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph one</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">.</span><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/I/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"M"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/M/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">M</span></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"X"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/X/index.html","html":"<div class=\"search-entry\"><code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">X</span></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"t"}],"doc":"A comment","kind":{"kind":"TypeDecl","private":false,"manifest":"int","constraints":[]},"display":{"url":"page/Main/index.html#type-t","html":"<div class=\"search-entry\"><code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">t</span><code class=\"entry_rhs\"> = int</code></code><div class=\"entry-comment\"><div><p>A comment</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"}],"doc":"A comment aaaaaaaaaa","kind":{"kind":"TypeDecl","private":false,"manifest":null,"constraints":[]},"display":{"url":"page/Main/index.html#type-tdzdz","html":"<div class=\"search-entry\"><code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">tdzdz</span><code class=\"entry_rhs\">= A of int * int | B of int list * int</code></code><div class=\"entry-comment\"><div><p>A comment aaaaaaaaaa</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"M"},{"kind":"Type","name":"t"}],"doc":"dsdsd","kind":{"kind":"TypeDecl","private":false,"manifest":null,"constraints":[]},"display":{"url":"page/Main/M/index.html#type-t","html":"<div class=\"search-entry\"><code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.M.</span><span class=\"entry-name\">t</span></code><div class=\"entry-comment\"><div><p>dsdsd</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"J"},{"kind":"Value","name":"uu"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/J/index.html#val-uu","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">J.</span><span class=\"entry-name\">uu</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem2"}],"doc":"lorem 2","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem2","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem2</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 2</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem3"}],"doc":"lorem 3","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem3","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem3</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 3</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem4"}],"doc":"lorem 4","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem4","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem4</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 4</p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem"}],"doc":"lorem 1 and a link","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 1 and a <span>link</span></p></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"uu"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-uu","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">uu</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"v"}],"doc":"a reference , and some formatted content with code and\ncode blocks","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-v","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">v</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>a reference <span><code>t</code></span>, and some <em>formatted</em> <b>content</b> with <code>code</code> and</p><pre class=\"language-ocaml\"><code>code blocks</code></pre></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"x"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-x","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">x</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"y"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-y","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">y</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"},{"kind":"Value","name":"x"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/I/index.html#val-x","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">x</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"},{"kind":"Value","name":"y"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/I/index.html#val-y","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">y</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"X"},{"kind":"Value","name":"c"}],"doc":"A value inside a module","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/X/index.html#val-c","html":"<div class=\"search-entry\"><code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.X.</span><span class=\"entry-name\">c</span><code class=\"entry_rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>A value inside a module</p></div></div></div>"}}

An index entry contains:
- an ID,
- the docstring associated (in text format),
- its kind, and some additional information (that are specific for each kind of entry)
- Information on how to render it: the link, and some html. (The link cannot be embedded in the html, it is relative to the "root" of the page, and thus may have to be modified)

  $ cat index.json | jq sort | head -n 33
  [
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
      "kind": {
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
        "html": "<div class=\"search-entry\"><code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">A</span><code class=\"entry_rhs\"> : int * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div></div></div></div>"
      }
    },

Here is the list of ids for entries. Multiple IDs exists as standalone
paragraphs/codeblocks/... use their parent ID (they don't have one).

  $ cat index.json | jq -r '.[].id | map(.kind + "-" + .name) | join(".")' | sort
  Page-page
  Page-page
  Page-page
  Page-page
  Page-page
  Page-page.Label-a-title
  Root-J
  Root-J
  Root-J.Value-uu
  Root-Main
  Root-Main.Label-this-is-a-title
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I.Value-x
  Root-Main.Module-I.Value-y
  Root-Main.Module-M
  Root-Main.Module-M.Type-t
  Root-Main.Module-X
  Root-Main.Module-X
  Root-Main.Module-X.Value-c
  Root-Main.Type-t
  Root-Main.Type-tdzdz
  Root-Main.Type-tdzdz.Constructor-A
  Root-Main.Type-tdzdz.Constructor-B
  Root-Main.Value-lorem
  Root-Main.Value-lorem2
  Root-Main.Value-lorem3
  Root-Main.Value-lorem4
  Root-Main.Value-uu
  Root-Main.Value-v
  Root-Main.Value-x
  Root-Main.Value-y

Now, from the index.json file, we need to create the assets that we declared as --search-assets.
Those should be javascript file. They will be run in their declared order, in a webworker (so as not to block UI).
They take their input (a string, the query) as a message sent to the webworker (so they have to listen to it). They answer their result to the query by sending a message.
This response should be a JSON entry of the form of the [display] field of a index.json entry, for odoc to be able to print it.

Here, we use fuse.js to generate such an asset.

  $ echo "\n\nlet documents = " > index.js
  $ cat index.json >> index.js

  $ echo "\n\nconst options = { keys: ['id', 'doc'] };" >> index.js
  $ echo "\nvar idx_fuse = new Fuse(documents, options);" >> index.js
  $ echo "\nonmessage = (m) => {\n  let query = m.data;\n  let result = idx_fuse.search(query);\n  postMessage(result.slice(0,200).map(a => a.item.display));};" >> index.js

We now generate the html, passing the assets:

  $ odoc html-generate -o html j.odocl
  $ odoc html-generate -o html main.odocl
  $ odoc html-generate --asset index.js --asset fuse.js.js -o html page-page.odocl
  $ odoc support-files -o html

The assets are put as child of their parent

  $ find html/page | sort
  html/page
  html/page/J
  html/page/J/index.html
  html/page/Main
  html/page/Main/I
  html/page/Main/I/index.html
  html/page/Main/M
  html/page/Main/M/index.html
  html/page/Main/X
  html/page/Main/X/index.html
  html/page/Main/index.html
  html/page/fuse.js.js
  html/page/index.html
  html/page/index.js
Checks that the asset is correctly referenced to in the html.
  $ grep -E -o "'[\./]*fuse\.js\.js" html/page/index.html
  'fuse.js.js

One way to visually try the search is to indent
$ cp -r html /tmp/
$ firefox /tmp/html/page/Main/index.html
and run `dune test`.
