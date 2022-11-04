  $ compile foo.mli

Generating html url for a reference
  $ odoc html-url -I . Foo.t
  test/Foo/index.html#type-t

The root-url argument prepends a string to the html url
  $ odoc html-url -r /base -I . Foo.t
  /base/test/Foo/index.html#type-t

  $ odoc html-url --root-url=/base/ -I . Foo.t
  /base/test/Foo/index.html#type-t

Generate latex url
  $ odoc latex-url -I . Foo.t
  page-test-module-Foo-type-t

When the reference cannot be resolved.
  $ odoc html-url -I . Foo.u
  ERROR: Couldn't find "u"
  [1]

When the reference cannot be parsed.
  $ odoc html-url -I . ""
  ERROR: Identifier in reference should not be empty.
  [1]

