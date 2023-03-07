A quick test to repro the issue found in #857

  $ ocamlc -bin-annot -c a.mli

  $ odoc compile a.cmti
  $ odoc link a.odoc

  $ odoc html-generate --indent -o html/ a.odocl
  $ odoc latex-generate -o latex/ a.odocl

In latex, labels in subpages should be disambiguated since the subpage is inlined inside the generated latex source.
  $ cat latex/A.tex | sed 's/\\/\n\\/g' | grep label
  \label{module-A}%
  \label{module-A-module-type-A}
  \label{first}}%
  \label{first_2}}%

In html, labels in subpages should not be disambiguated since they won't have the same URL.

  $ cat html/A/index.html | grep 'id='
      <div class="spec module-type anchored" id="module-type-A">
     <h2 id="first"><a href="#first" class="anchor"></a>First outer section

  $ cat html/A/module-type-A/index.html | grep 'id='
     <h2 id="first"><a href="#first" class="anchor"></a>First inner section
