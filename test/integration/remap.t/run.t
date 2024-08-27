  $ ocamlc -c -bin-annot otherlib.mli
  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --parent-id prefix/otherpkg/doc --output-dir _odoc otherlib.cmti
  $ odoc compile --parent-id prefix/mypkg/doc --output-dir _odoc test.cmti
  $ odoc link _odoc/prefix/otherpkg/doc/otherlib.odoc
  $ odoc link -I _odoc/prefix/otherpkg/doc _odoc/prefix/mypkg/doc/test.odoc

We should be able to remap the links to one of the packages:

  $ odoc html-generate -o _html --indent _odoc/prefix/mypkg/doc/test.odocl
  $ odoc html-generate -o _html2 --indent _odoc/prefix/mypkg/doc/test.odocl -R prefix/otherpkg/:https://mysite.org/p/otherpkg/1.2.3/

  $ grep Otherlib/index.html _html/prefix/mypkg/doc/Test/index.html _html2/prefix/mypkg/doc/Test/index.html
  _html/prefix/mypkg/doc/Test/index.html:       <a href="../../../otherpkg/doc/Otherlib/index.html#type-t">Otherlib.t
  _html2/prefix/mypkg/doc/Test/index.html:        href="https://mysite.org/p/otherpkg/1.2.3/doc/Otherlib/index.html#type-t"

This shouldn't stop us from outputting the remapped package though, and the following should complete without error

  $ odoc html-generate -o _html3 _odoc/prefix/otherpkg/doc/otherlib.odocl -R prefix/otherpkg/:https://mysite.org/p/otherpkg/1.2.3/

