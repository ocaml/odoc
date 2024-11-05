  $ ocamlc -bin-annot test.ml
  $ mkdir h
  $ odoc compile --output-dir h --parent-id pkg page.mld
  $ odoc compile --output-dir h --parent-id pkg/libname test.cmt

No -P or -L passed, no error. This ensures compatibility with Odoc 2.

  $ odoc link h/pkg/libname/test.odoc
  $ odoc link h/pkg/page-page.odoc
  $ odoc link h/pkg/libname/test.odoc
  $ odoc link h/pkg/page-page.odoc

The correct -P and -L will be used several times, we store them to make it more readable.

  $ P="-P pkg:h/pkg"
  $ L="-L libname:h/pkg/libname"

Including all options:

  $ odoc link $P $L h/pkg/libname/test.odoc
  $ odoc link $P $L h/pkg/page-page.odoc

A package can be passed, either with `--current-package` or by being below a
`-P`.

 For modules:

  $ odoc link -P pkg:h/pkg2 $L h/pkg/libname/test.odoc --current-package pkg
  $ odoc link $P            $L h/pkg/libname/test.odoc

 For pages:

  $ odoc link -P pkg:h/pkg2 $L h/pkg/page-page.odoc --current-package pkg
  $ odoc link $P            $L h/pkg/page-page.odoc

It is not required to be below a `-L`, even for modules:

  $ odoc link $P -L otherlib:h/otherpkg/otherlib h/pkg/libname/test.odoc

For both pages and modules, --current-package has to correspond to a -P:

  $ odoc link --current-package wrong -P pkg:h/pkg2 $L h/pkg/libname/test.odoc
  ERROR: The package name specified with --current-package do not match any package passed as a -P
  [1]
  $ odoc link --current-package wrong h/pkg/page-page.odoc
  ERROR: The package name specified with --current-package do not match any package passed as a -P
  [1]

For both pages and modules, --current-package has to correspond to the same -P as the one that is a root of the page, when there is one:

  $ odoc link --current-package wrong $P $L h/pkg/page-page.odoc
  ERROR: The package name specified with --current-package is not consistent with the packages passed as a -P
  [1]
  $ odoc link --current-package otherpkg $P -P otherpkg:h/otherpkg $L h/pkg/page-page.odoc
  ERROR: The package name specified with --current-package is not consistent with the packages passed as a -P
  [1]

Packages and libraries overlap do not pose a problem:

  $ odoc link $P $L h/pkg/libname/test.odoc
  $ odoc link $P $L h/pkg/page-page.odoc
