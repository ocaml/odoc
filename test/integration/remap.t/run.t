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

The order shouldn't matter, the longest prefix ought to win
  $ odoc html-generate -o _html3 --indent _odoc/prefix/mypkg/test.odocl -R prefix/:https://mysite.org/foo/ -R prefix/otherpkg/:https://mysite.org/bar/
  $ odoc html-generate -o _html4 --indent _odoc/prefix/mypkg/test.odocl -R prefix/otherpkg/:https://mysite.org/bar/ -R prefix/:https://mysite.org/foo/ 

  $ grep Otherlib/index.html _html3/prefix/mypkg/Test/index.html _html4/prefix/mypkg/Test/index.html
  _html3/prefix/mypkg/Test/index.html:       <a href="https://mysite.org/bar/Otherlib/index.html#type-t">Otherlib.t
  _html4/prefix/mypkg/Test/index.html:       <a href="https://mysite.org/bar/Otherlib/index.html#type-t">Otherlib.t

  $ odoc html-generate -o _html5 --indent _odoc/prefix/mypkg/test.odocl --remap-file remap.txt
  $ grep Otherlib/index.html _html5/prefix/mypkg/Test/index.html
         <a href="https://mysite.org/bar/Otherlib/index.html#type-t">Otherlib.t


