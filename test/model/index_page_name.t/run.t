Both odig and dune generate or use files called 'index.mld' for which
the breadcrumbs are broken.

A leaf page called 'index.mld' generates the file 'parent/index.html'.
This clashes with the semantics of the parent/child behaviour, where the
parent page of 'index.mld' also generates the file 'parent/index.html'.

  $ odoc compile --package test index.mld
  $ odoc link page-index.odoc
  $ odoc html-generate -o . --indent page-index.odocl

The breadcrumbs shouldn't show the name of the page, "index".
Expected to fail:

  $ grep odoc-nav test/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – test</nav>
