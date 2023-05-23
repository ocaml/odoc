Checking that source links exists inside recursive modules.

  $ odoc compile -c module-main -c src-source root.mld

  $ printf "main.ml" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --source-name main.ml --source-parent-file src-source.odoc -I . main.cmt
  $ odoc link -I . main.odoc
  File "main.odoc":
  Warning: Failed to lookup type identifier((root Main).A, false).t Parent_module: Lookup failure (module): (root Main).A
  File "main.odoc":
  Warning: Failed to resolve module path identifier((root Main).B, false) Lookup failure (module): (root Main).B
  File "main.odoc":
  Warning: Failed to lookup type identifier((root Main).B, false).t Parent_module: Lookup failure (module): (root Main).B
  File "main.odoc":
  Warning: Failed to resolve module path identifier((root Main).A, false) Lookup failure (module): (root Main).A
  $ odoc html-generate --source main.ml --indent -o html main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/main.ml.html#module-A" class="source_link">
       Source
      </a>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../../root/source/main.ml.html#module-B" class="source_link">
       Source
      </a>
