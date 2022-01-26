  $ compile foo.mli

Generates url for a corresponding reference
  $ odoc uri -I . Foo.t
  test/Foo/index.html#type-t

When the reference is can not be resolved.
  $ odoc uri -I . Foo.u
  ERROR: Couldn't find "u"
  [1]

When the reference can not be parsed.
  $ odoc uri -I . "" 
  ERROR: Identifier in reference should not be empty.
  [1]

