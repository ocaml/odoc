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

A hash of the file, to detect changes
  $ cat index.json | jq sort | md5sum
  0403b8ca55bebecc2880dcda30c71edd  -

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

One way to visually try the search is to indent
$ cp -r html /tmp/
$ firefox /tmp/html/page/Main/index.html
and run `dune test`.
