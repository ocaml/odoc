  $ compile foo.mli

Generating url for a reference uses html by default
  $ odoc uri -I . Foo.t
  test/Foo/index.html#type-t

Generates url for the html backend
  $ odoc uri --html -I . Foo.t
  test/Foo/index.html#type-t
  $ odoc uri -h -I . Foo.t
  test/Foo/index.html#type-t

Generates url for the latex backend
  $ odoc uri --latex -I . Foo.t
  page-test-module-Foo-type-t
  $ odoc uri -l -I . Foo.t
  page-test-module-Foo-type-t

When the reference cannot be resolved.
  $ odoc uri -I . Foo.u
  ERROR: Couldn't find "u"
  [1]

When the reference cannot be parsed.
  $ odoc uri -I . ""
  ERROR: Identifier in reference should not be empty.
  [1]

