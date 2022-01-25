g=Generates documentation using odoc for a library: 
  $ dune build @doc

Generates url for a corresponding reference
  $ odoc uri -I _build/default/.foo.objs/byte Foo.t
  foo/Foo/index.html#type-t
