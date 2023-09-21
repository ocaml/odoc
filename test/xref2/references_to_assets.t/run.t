In this file, we test the resolving of asset references.

More precisely we test resolving an an asset reference where the asset lives:
- in the current page (index.mld references caml.gif)
- in a parent page (test.mli references caml.gif)
- in a sibling page (test.mli references caml_not.gif, through page-other_page.caml_not.gif)
- in a child page (index.mld references caml_not.gif, through page-other_page.caml_not.gif)

Compile the module first

  $ ocamlc -c -bin-annot test.mli

Then we need to odoc-compile the package mld file, listing its
children. If we omit the asset child, all assets reference resolving fail:

  $ odoc compile index.mld --child module-test --child page-other_page
  $ odoc compile other_page.mld -I . --parent index
  $ odoc compile test.cmti -I . --parent index
  $ for i in *.odoc; do odoc link -I . $i; done
  File "index.mld", line 10, characters 2-15:
  Warning: Failed to resolve reference unresolvedroot(caml.gif) Couldn't find "caml.gif"
  File "index.mld", line 9, characters 2-36:
  Warning: Failed to resolve reference unresolvedroot(other_page).caml_not.gif Couldn't find asset "caml_not.gif"
  File "index.mld", line 8, characters 2-30:
  Warning: Failed to resolve reference unresolvedroot(other_page).caml_not.gif Couldn't find asset or label "caml_not.gif"
  File "index.mld", line 7, characters 2-35:
  Warning: Failed to resolve reference unresolvedroot(other_page).caml_not.gif Couldn't find asset or label "caml_not.gif"
  File "index.mld", line 6, characters 2-41:
  Warning: Failed to resolve reference unresolvedroot(other_page).caml_not.gif Couldn't find asset "caml_not.gif"
  File "index.mld", line 3, characters 2-32:
  Warning: Failed to resolve reference unresolvedroot(caml.gif) Couldn't find asset "caml.gif"
  File "other_page.mld", line 3, characters 22-41:
  Warning: Failed to resolve reference unresolvedroot(caml.gif) Couldn't find asset "caml.gif"
  File "test.mli", line 4, characters 39-78:
  Warning: Failed to resolve reference unresolvedroot(other_page).caml_not.gif Couldn't find asset "caml_not.gif"
  File "test.mli", line 2, characters 4-34:
  Warning: Failed to resolve reference unresolvedroot(caml.gif) Couldn't find asset "caml.gif"

We should pass the asset as child of a page.

  $ odoc compile index.mld --child module-test --child asset-caml.gif --child page-other_page
  $ odoc compile test.cmti -I . --parent index
  $ odoc compile other_page.mld --child asset-caml_not.gif -I . --parent index

Link and generate the HTML (forgetting the asset!):

  $ for i in *.odoc; do odoc link -I . $i; done
  $ for i in *.odocl; do odoc html-generate --indent $i -o html; done
  File "caml.gif":
  Warning: asset is missing.
  File "caml_not.gif":
  Warning: asset is missing.

Note that the html links are correct (there are dead links due to missing assets)

  $ grep caml.gif html/index/index.html
     <p>A <a href="caml.gif" title="caml.gif">reference</a> to an asset.</p>
      <li><a href="caml.gif"><code>caml.gif</code></a></li>
  $ grep caml_not.gif html/index/index.html
      <li><a href="other_page/caml_not.gif"><code>caml_not.gif</code></a></li>
      <li><a href="other_page/caml_not.gif"><code>caml_not.gif</code></a></li>
      <li><a href="other_page/caml_not.gif"><code>caml_not.gif</code></a></li>
      <li><a href="other_page/caml_not.gif"><code>caml_not.gif</code></a></li>
  $ grep caml.gif html/index/Test/index.html
     <p>A <a href="../caml.gif" title="caml.gif">reference</a> to an asset</p>
  $ grep caml_not.gif html/index/Test/index.html
      <a href="../other_page/caml_not.gif"><code>caml_not.gif</code></a>
  $ grep caml.gif html/index/other_page/index.html
     <p>Hello darkness my old <a href="../caml.gif"><code>caml.gif</code></a>.
