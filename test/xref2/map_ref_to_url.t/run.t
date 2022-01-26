  $ compile foo.mli

Generating html uri for a reference
  $ odoc html-uri -I . Foo.t
  test/Foo/index.html#type-t

The prefix argument prepend a string to the html uri
  $ odoc html-uri -r /root-dir -I . Foo.t
  /root-dir/test/Foo/index.html#type-t

  $ odoc html-uri --root-dir=/Root-dir/ -I . Foo.t
  /Root-dir/test/Foo/index.html#type-t

Generate latex uri
  $ odoc latex-uri -I . Foo.t
  page-test-module-Foo-type-t

When the reference cannot be resolved.
  $ odoc html-uri -I . Foo.u
  ERROR: Couldn't find "u"
  [1]

When the reference cannot be parsed.
  $ odoc html-uri -I . ""
  ERROR: Identifier in reference should not be empty.
  [1]

