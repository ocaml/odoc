Checking that source links exists inside recursive modules.

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile-impl --source-id src/main.ml -I . main.cmt
  $ odoc compile -I . main.cmt
  $ odoc link -I . impl-main.odoc
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate-impl --source main.ml --indent -o html impl-main.odocl
  odoc: unknown command 'html-generate-impl', must be one of 'aggregate-occurrences', 'classify', 'compile', 'compile-asset', 'compile-deps', 'compile-impl', 'compile-index', 'compile-targets', 'count-occurrences', 'css', 'errors', 'html', 'html-deps', 'html-fragment', 'html-generate', 'html-generate-source', 'html-targets', 'html-targets-source', 'html-url', 'latex', 'latex-generate', 'latex-targets', 'latex-url', 'link', 'link-deps', 'man', 'man-generate', 'man-targets', 'support-files' or 'support-files-targets'.
  Usage: odoc [COMMAND] …
  Try 'odoc --help' for more information.
  [2]

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../src/main.ml.html#module-A" class="source_link">Source</a>
     </h1>
    </header>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../../src/main.ml.html#module-B" class="source_link">Source</a>
     </h1>
    </header>
