  $ ocamlc -bin-annot test.ml
  $ mkdir h
  $ odoc compile --output-dir h --parent-id pkg/doc page.mld
  $ odoc compile --output-dir h --parent-id pkg/lib/libname test.cmt

No library or package are passed, no error. This ensures compatibility with Odoc 2.

  $ odoc link -P pkg:h/pkg/doc h/pkg/lib/libname/test.odoc
  $ odoc link -P pkg:h/pkg/doc h/pkg/doc/page-page.odoc
  $ odoc link -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  $ odoc link -L libname:h/pkg/lib/libname h/pkg/doc/page-page.odoc

Current library is not passed:

  $ odoc link -P pkg:h/pkg/doc -L otherlib:h/otherpkg h/pkg/lib/libname/test.odoc
  ERROR: The output file must be part of a directory passed as -L
  [1]
  $ odoc link -P pkg:h/pkg/doc -L otherlib:h/otherpkg h/pkg/doc/page-page.odoc

Current package is not passed:

  $ odoc link -P otherpkg:h/otherpkg/doc -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  $ odoc link -P otherpkg:h/otherpkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-page.odoc
  ERROR: The output file must be part of a directory passed as -P
  [1]

Specified current package is wrong:

  $ odoc link --current-package wrong -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  ERROR: The package name specified with --current-package do not match any package passed as a -P
  [1]
  $ odoc link --current-package wrong -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-page.odoc
  ERROR: The package name specified with --current-package do not match any package passed as a -P
  [1]

Specified current package is inconsistent:

  $ odoc link --current-package otherpkg -P pkg:h/pkg/doc -P otherpkg:h/otherpkg/doc -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  $ odoc link --current-package otherpkg -P pkg:h/pkg/doc -P otherpkg:h/otherpkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-page.odoc
  ERROR: The package name specified with --current-package is not consistent with the packages passed as a -P
  [1]

Packages and libraries overlap:

  $ odoc link -P pkg:h/pkg/doc -P otherpkg:h/pkg/lib/libname -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]
  $ odoc link -P pkg:h/pkg/doc -P otherpkg:h/pkg/lib/libname -L libname:h/pkg/lib/libname h/pkg/doc/page-page.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]
