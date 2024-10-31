  $ ocamlc -c -bin-annot otherlib.mli
  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --parent-id prefix/otherpkg --output-dir _odoc otherlib.cmti
  $ odoc compile --parent-id prefix/mypkg --output-dir _odoc test.cmti
  $ odoc link _odoc/prefix/otherpkg/otherlib.odoc
  $ odoc link -I _odoc/prefix/otherpkg _odoc/prefix/mypkg/test.odoc

We should be able to remap the links to one of the packages:

  $ odoc html-generate -o _html --indent _odoc/prefix/mypkg/test.odocl
  $ odoc html-generate -o _html2 --indent _odoc/prefix/mypkg/test.odocl -R prefix/otherpkg/:https://mysite.org/p/otherpkg/1.2.3/

  $ grep Otherlib/index.html _html/prefix/mypkg/Test/index.html _html2/prefix/mypkg/Test/index.html
  _html/prefix/mypkg/Test/index.html:       <a href="../../otherpkg/Otherlib/index.html#type-t">Otherlib.t</a>
  _html2/prefix/mypkg/Test/index.html:        href="https://mysite.org/p/otherpkg/1.2.3/Otherlib/index.html#type-t">

This shouldn't stop us from outputting the remapped package though, and the following should complete without error

  $ odoc html-generate -o _html3 _odoc/prefix/otherpkg/otherlib.odocl -R prefix/otherpkg/:https://mysite.org/p/otherpkg/1.2.3/

