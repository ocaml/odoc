Notes
=====

Functors
--------

Functors usually constrain the output:

```ocaml env=e1
module type T = sig type t end
module F = functor(X:T) -> struct include X end
module Y = struct
  type t
  type u
end
```

so we have no access to type `u` in the result of applying
`F` to `Y`.

```ocaml env=e1
# module FY = F(Y) ;;
module FY : sig type t = Y.t end
```

However, we do add equations so that within the body of
`F` above, the signature of `X` is `T` strengthened with
`Y`. Hence in this case, the signature of `X` within `F(Y)`
is

```ocaml skip
sig
    type t = Y.t
end
```

When we can't strengthen (for example, because the module
argument isn't bound) the equalities aren't present:

```ocaml env=e1
# module F_ = F(struct type t end);;
module F_ : sig type t end
```

Note though that we can still add equations in this way:

```ocaml env=e1
# module F_ = F(struct type t = int end);;
module F_ : sig type t = int end
```

We can construct an example where the resulting signature is dependent on the argument - i.e. there are values and types
exposed on the result of the functor that aren't present in 
the functors definition.

```ocaml env=e1
module type T = sig module type U module N : U end
module F = functor(X:T) -> struct include X end
module Test = struct
    module type U = sig type t end
    module N = struct type t = int end
end
```

so even through the declaration of `F` contains no reference to
any type `t`, we can still do:

```ocaml env=e1
# module M = F(Test);;
module M : sig module type U = Test.U module N : Test.U end
```

```ocaml env=e1
# type t = F(Test).N.t;;
type t = F(Test).N.t
```

This is all important for odoc since we would like to add links to
paths, so when we see `F(Test).N.t` where should we link? In this
case we should be linking to `Test.U.t` (Note that this _isn't_ a
valid path, since paths should only contain modules before the 
dots, not module _types_).

Note that we can't refer to modules like this (since module paths
aren't [extended](https://caml.inria.fr/pub/docs/manual-ocaml/names.html#extended-module-path) ).

```ocaml env=e1
# module N = F(Test);;
module N : sig module type U = Test.U module N : Test.U end
```



