Compile the files

  $ ocamlc -c j.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile -I . --child module-main --child module-j page.mld
  $ odoc compile --parent page -I . j.cmt
  $ odoc compile --parent page -I . main.cmt

  $ odoc link -I . j.odoc
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc

When html-generating a page, we can provide, through --search-uri flags, uris to
search scripts that will be used to answer search queries. The uris can be
absolute (`https://...` or `/tmp/...` for instance, or relative. If they are
relative, they are interpreted as relative to the `-o` option).

Let us check that `--search-uri` works well:

  $ odoc html-generate --search-uri test.js -o html page-page.odocl
  $ grep -E "test\.js" html/page/index.html
  let search_urls = ['../test.js'];

  $ odoc html-generate --search-uri page/test.js -o html page-page.odocl
  $ grep -E "test\.js" html/page/index.html
  let search_urls = ['test.js'];

  $ odoc html-generate --search-uri search_scripts/test.js -o html page-page.odocl
  $ grep -E "test\.js" html/page/index.html
  let search_urls = ['../search_scripts/test.js'];

  $ odoc html-generate --search-uri /tmp/test.js -o html page-page.odocl
  $ grep -E "test\.js" html/page/index.html
  let search_urls = ['/tmp/test.js'];

  $ odoc html-generate --search-uri https://example.org/test.js -o html page-page.odocl
  $ grep -E "test\.js" html/page/index.html
  let search_urls = ['https://example.org/test.js'];

In this test, we use `fuse.js.js` (a search engine) combined to `index.js`, a file that
we will generate.

  $ odoc html-generate --search-uri fuse.js.js --search-uri index.js -o html j.odocl
  $ odoc html-generate --search-uri fuse.js.js --search-uri index.js -o html main.odocl
  $ odoc html-generate --search-uri fuse.js.js --search-uri index.js -o html page-page.odocl
  $ odoc support-files -o html

We now focus on how to generate the index.js file.

For this, we compute an index of all the values contained in a given list of
odoc files, using the `compile-index` command.

This command generates has two output format: a json output for consumption by
external search engine, and an `odoc` specific extension.  The odoc file is
meant to be consumed either by search engine written in OCaml, which would
depend on `odoc` as a library, or by `odoc` itself to build a global index
incrementally: the `compile-index` command can take indexes as input!

If -o is not provided, the file is saved as index.json, or index-index.odoc if
the --marshall flag is passed.  Odocl files can be given either in a list (using
--file-list, passing a file with the list of line-separated files), or by
passing directly the name of the files.

  $ printf "main.odocl\npage-page.odocl\nj.odocl\n" > index_map
  $ odoc compile-index --json -o index.json --root .

  $ odoc compile-index -o index-main.odoc-index --root .

The json index file contains a json array, each element of the array corresponding to
a search entry.
An index entry contains:
- an ID,
- the docstring associated (in text format),
- its kind, and some additional information (that are specific for each kind of entry)
- Information on how to render it: the link, and some html. (The link cannot be embedded in the html, it is relative to the "root" of the page, and thus may have to be modified). This also corresponds to the json that should be output to odoc in case the entry is selected by the query.

The index file, one entry per line:
  $ cat index.json | jq sort | jq '.[]' -c
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"},{"kind":"Constructor","name":"A"}],"doc":"","kind":{"kind":"Constructor","args":{"kind":"Tuple","vals":["int","int"]},"res":"tdzdz"},"display":{"url":"page/Main/index.html#type-tdzdz.A","html":"<code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">A</span><code class=\"entry-rhs\"> : int * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"},{"kind":"Constructor","name":"B"}],"doc":"Bliiiiiiiiiii","kind":{"kind":"Constructor","args":{"kind":"Tuple","vals":["int list","int"]},"res":"tdzdz"},"display":{"url":"page/Main/index.html#type-tdzdz.B","html":"<code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">B</span><code class=\"entry-rhs\"> : int list * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div><p>Bliiiiiiiiiii</p></div></div>"}}
  {"id":[{"kind":"Root","name":"J"}],"doc":"a paragraph two","kind":{"kind":"Doc"},"display":{"url":"page/J/index.html","html":"<code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"}],"doc":"a paragraph two","kind":{"kind":"Doc"},"display":{"url":"page/Main/index.html","html":"<code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div><p>a paragraph two</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"}],"doc":"a paragraph\nand another\nverbatim\nx + 1\nblibli","kind":{"kind":"Doc"},"display":{"url":"page/Main/index.html","html":"<code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div><p>a paragraph</p><p>and another</p><pre>verbatim</pre><p><code class=\"odoc-katex-math\">x + 1</code></p><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"}],"doc":"this is a title\nand this is a paragraph","kind":{"kind":"Doc"},"display":{"url":"page/Main/index.html","html":"<code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div><p>this is a title</p><p>and this is a paragraph</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"a paragraph\nand another\nverbatim\nx + 1\nblibli","kind":{"kind":"Doc"},"display":{"url":"page/Main/I/index.html","html":"<code class=\"entry-kind\">doc</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div><p>a paragraph</p><p>and another</p><pre>verbatim</pre><p><code class=\"odoc-katex-math\">x + 1</code></p><pre class=\"language-ocaml\"><code>blibli</code></pre></div></div>"}}
  {"id":[{"kind":"Root","name":"J"}],"doc":"a paragraph one","kind":{"kind":"Module"},"display":{"url":"page/J/index.html","html":"<code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"entry-name\">J</span></code><div class=\"entry-comment\"><div><p>a paragraph one</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/index.html","html":"<code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"entry-name\">Main</span></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/index.html#module-I","html":"<code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">I</span></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"M"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/index.html#module-M","html":"<code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">M</span></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"X"}],"doc":"","kind":{"kind":"Module"},"display":{"url":"page/Main/index.html#module-X","html":"<code class=\"entry-kind\">mod</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">X</span></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Page","name":"page"}],"doc":"A title\nA paragraph\nsome verbatim\nand code\na list of things bliblib","kind":{"kind":"Page"},"display":{"url":"page/index.html","html":"<code class=\"entry-kind\">page</code><code class=\"entry-title\"><span class=\"entry-name\">page</span></code><div class=\"entry-comment\"><div><p>A title</p><p>A paragraph</p><pre>some verbatim</pre><pre class=\"language-ocaml\"><code>and code</code></pre><ul><li>a list <em>of</em> things</li><li>bliblib</li></ul></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"t"}],"doc":"A comment","kind":{"kind":"TypeDecl","private":false,"manifest":"int","constraints":[]},"display":{"url":"page/Main/index.html#type-t","html":"<code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">t</span><code class=\"entry-rhs\"> = int</code></code><div class=\"entry-comment\"><div><p>A comment</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Type","name":"tdzdz"}],"doc":"A comment aaaaaaaaaa","kind":{"kind":"TypeDecl","private":false,"manifest":null,"constraints":[]},"display":{"url":"page/Main/index.html#type-tdzdz","html":"<code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">tdzdz</span><code class=\"entry-rhs\"> = A of int * int | B of int list * int</code></code><div class=\"entry-comment\"><div><p>A comment aaaaaaaaaa</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"M"},{"kind":"Type","name":"t"}],"doc":"dsdsd","kind":{"kind":"TypeDecl","private":false,"manifest":null,"constraints":[]},"display":{"url":"page/Main/M/index.html#type-t","html":"<code class=\"entry-kind\">type</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.M.</span><span class=\"entry-name\">t</span></code><div class=\"entry-comment\"><div><p>dsdsd</p></div></div>"}}
  {"id":[{"kind":"Root","name":"J"},{"kind":"Value","name":"uu"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/J/index.html#val-uu","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">J.</span><span class=\"entry-name\">uu</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem2"}],"doc":"lorem 2","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem2","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem2</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 2</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem3"}],"doc":"lorem 3","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem3","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem3</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 3</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem4"}],"doc":"lorem 4","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem4","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem4</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 4</p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"lorem"}],"doc":"lorem 1 and a link","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-lorem","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">lorem</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>lorem 1 and a <span>link</span></p></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"uu"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-uu","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">uu</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"v"}],"doc":"a reference , and some formatted content with code and\n  code blocks","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-v","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">v</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>a reference <span><code>t</code></span>, and some <em>formatted</em> <b>content</b> with <code>code</code> and</p><pre class=\"language-ocaml\"><code>  code blocks</code></pre></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"x"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-x","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">x</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Value","name":"y"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/index.html#val-y","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.</span><span class=\"entry-name\">y</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"},{"kind":"Value","name":"x"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/I/index.html#val-x","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">x</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"I"},{"kind":"Value","name":"y"}],"doc":"","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/I/index.html#val-y","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.I.</span><span class=\"entry-name\">y</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div></div></div>"}}
  {"id":[{"kind":"Root","name":"Main"},{"kind":"Module","name":"X"},{"kind":"Value","name":"c"}],"doc":"A value inside a module","kind":{"kind":"Value","type":"int"},"display":{"url":"page/Main/X/index.html#val-c","html":"<code class=\"entry-kind\">val</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.X.</span><span class=\"entry-name\">c</span><code class=\"entry-rhs\"> : int</code></code><div class=\"entry-comment\"><div><p>A value inside a module</p></div></div>"}}

and the first entries formatted:

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
        "html": "<code class=\"entry-kind\">cons</code><code class=\"entry-title\"><span class=\"prefix-name\">Main.tdzdz.</span><span class=\"entry-name\">A</span><code class=\"entry-rhs\"> : int * int -&gt; tdzdz</code></code><div class=\"entry-comment\"><div></div></div>"
      }
    },

Here is the list of ids for entries. Multiple IDs exists as standalone
paragraphs, codeblocks, etc. use their parent ID (they don't have one for
themselves).

  $ cat index.json | jq -r '.[].id | map(.kind + "-" + .name) | join(".")' | sort
  Page-page
  Root-J
  Root-J
  Root-J.Value-uu
  Root-Main
  Root-Main
  Root-Main
  Root-Main
  Root-Main.Module-I
  Root-Main.Module-I
  Root-Main.Module-I.Value-x
  Root-Main.Module-I.Value-y
  Root-Main.Module-M
  Root-Main.Module-M.Type-t
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

Now, from the index.json file, we need to create the scripts and put them as specified with --search-uri
Those should be javascript file. They will be run in their declared order, in a webworker (so as not to block UI).
They take their input (a string, the query) as a message sent to the webworker (so they have to listen to it). They answer their result to the query by sending a message.
This response should be a JSON entry of the form of the [display] field of a index.json entry, for odoc to be able to print it.

Here is an example of such search script generation, using the fuse.js search engine.

  $ printf "let documents = " > index.js
  $ cat index.json >> index.js

  $ cat << EOF >> index.js
  > 
  > const options = { keys: ['id', 'doc'] };
  > var idx_fuse = new Fuse(documents, options);
  > onmessage = (m) => {
  >   let query = m.data;
  >   let result = idx_fuse.search(query);
  >   postMessage(result.slice(0,200).map(a => a.item.display));
  > };
  > EOF

We should now put the scripts where it was:

  $ cp index.js html/
  $ cp fuse.js.js html/

One way to visually try the search is to indent
$ cp -r html /tmp/
$ firefox /tmp/html/page/Main/index.html
and run `dune test`.

Testing the warnings/errors for the `compile-index` command:

Passing an inexistent file:

  $ odoc compile-index --root babar
  $ odoc compile-index --file-list babar
  odoc: option '--file-list': no 'babar' file or directory
  Usage: odoc compile-index [OPTION]… [FILE]…
  Try 'odoc compile-index --help' or 'odoc --help' for more information.
  [2]

Passing an empty folder is allowed:

  $ mkdir foo
  $ odoc compile-index --root foo

Wrong file extensions:

  $ odoc compile-index -o index.odoc
  ERROR: When generating a binary index, the output must have a .odoc-index file extension
  [1]
  $ odoc compile-index -o index.json
  ERROR: When generating a binary index, the output must have a .odoc-index file extension
  [1]
  $ odoc compile-index  --json  -o index.odoc-index
  ERROR: When generating a json index, the output must have a .json or .js file extension
  [1]

Passing a file which is not a correctly marshalled one:

  $ echo hello > my_file.odocl
  $ odoc compile-index --root .


Passing no file is allowed, generating an empty index:

  $ odoc compile-index
