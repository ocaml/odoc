Index pages in a directory might specify a 'short_title' that must appear in the breadcrumbs.
This applies to package and library index pages.

  $ LINK_OPTS="-P pkg:_odoc/pkg/doc -L lname:_odoc/pkg/lib/lname"

It's not possible to link a page that is not part of a package's doc hierarchy.
This restriction should be lifted in the future.

$ LINK_OPTS="$LINK_OPTS -P root_of_pkg:_odoc/pkg"

  $ ocamlc -c -bin-annot pkg/lib/lname/lname.mli

  $ alias compile="odoc compile --output-dir _odoc/ --parent-id"
  $ compile pkg pkg/index.mld
  $ compile pkg/lib pkg/lib/index.mld
  $ compile pkg/lib/lname pkg/lib/lname/index.mld
  $ compile pkg/lib/lname pkg/lib/lname/lname.cmti
  $ compile pkg/doc pkg/doc/index.mld
  $ compile pkg/doc/subdir pkg/doc/subdir/index.mld
  $ compile pkg/doc/subdir pkg/doc/subdir/foo.mld

  $ find _odoc -name '*.odoc' -exec odoc link $LINK_OPTS {} ';'
  $ odoc compile-index $LINK_OPTS -o _odoc/pkg/package-index.odoc-index
  $ find _odoc -name '*.odocl' -exec odoc html-generate --indent --index _odoc/pkg/package-index.odoc-index -o html {} ';'

  $ nav() {  sed -n '\#<nav class="odoc-nav">#,\#<header class="odoc-preamble">#p' "$1"; }

  $ nav html/pkg/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – Package pkg</nav>
    <header class="odoc-preamble">
  $ nav html/pkg/doc/index.html
    <nav class="odoc-nav"><a href="#">Up</a> – 
     <a href="../index.html">Package pkg</a> &#x00BB; <a href="#">doc</a>
      &#x00BB; index
    </nav>
    <header class="odoc-preamble">
  $ nav html/pkg/doc/subdir/index.html
    <nav class="odoc-nav"><a href="#">Up</a> – 
     <a href="../../index.html">Package pkg</a> &#x00BB; 
     <a href="../index.html">doc</a> &#x00BB; <a href="#">subdir</a> &#x00BB;
      index
    </nav>
    <header class="odoc-preamble">
  $ nav html/pkg/doc/subdir/foo.html
    <nav class="odoc-nav"><a href="index.html">Up</a> – 
     <a href="../../index.html">Package pkg</a> &#x00BB; 
     <a href="../index.html">doc</a> &#x00BB; <a href="index.html">subdir</a>
      &#x00BB; foo
    </nav>
    <header class="odoc-preamble">
  $ nav html/pkg/lib/index.html
    <nav class="odoc-nav"><a href="#">Up</a> – 
     <a href="../index.html">Package pkg</a> &#x00BB; <a href="#">lib</a>
      &#x00BB; index
    </nav>
    <header class="odoc-preamble">
  $ nav html/pkg/lib/lname/index.html
    <nav class="odoc-nav"><a href="#">Up</a> – 
     <a href="../../index.html">Package pkg</a> &#x00BB; 
     <a href="../index.html">lib</a> &#x00BB; <a href="#">lname</a> &#x00BB;
      index
    </nav>
    <header class="odoc-preamble">
  $ nav html/pkg/lib/lname/Lname/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../../index.html">Package pkg</a> &#x00BB; 
     <a href="../index.html">Library lname</a> &#x00BB; Lname
    </nav>
    <header class="odoc-preamble">
