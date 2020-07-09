Resolution is the process by which we take a value of type `Lang.t` and look up
details of the internal cross references and make sure we know exactly which
component is being referred to. Much of the work has been done by the compiler
but we need to do a little more. For example, given

```ocaml skip
module M : sig
  type t
end
type u = M.t 
```

in the definition of `u`, the compiler tells us precisely which `M` is on the
right hand side but doesn't we need to which which `t` it is referring to.

```ocaml
(* Prelude *)
#require "odoc.xref2";;
#require "odoc.xref_test";;
open Odoc_xref2
open Odoc_xref_test

let test_resolve test_data =
    Odoc_xref2.Tools.reset_caches ();
    let _, _, sg = Common.model_of_string test_data in
    let open Format in
    fprintf std_formatter "BEFORE\n======\n%!%a\n%!" Common.LangUtils.Fmt.signature sg;
    let sg' = Compile.signature Env.empty Common.root_with_name sg in
    fprintf std_formatter "AFTER \n===== \n%!%a\n%!" Common.LangUtils.Fmt.signature sg'
```

The simplest resolution is where we simply look up a type and check it's there.
Given the signature
```ocaml skip
type t
type u = t
```

the resolution process will check the right-hand side of the `type u`
declaration and look up the definition of `t`.

The first thing that happens is that we construct an `Env.t`, which is a
mapping of `Model.Path.Identifier.t` to `Component`, where the component is a
value of type specific to the type of component (e.g. `Component.Module.t` or
`Component.Type.t`).
For the case above, the environment looks like this:

```ocaml skip
{ ident_max = 0
; modules = []
; module_types = []
; types =
    [(`Type (`Root root, "u"),
       { id = ("u", 1)
       ; manifest = Some (Constr (`Global (`Resolved (`Identifier (`Type (`Root root, "t")))), []))});
     (`Type (`Root root, "t"),
       { id = ("t", 0)
       ; manifest = None})
    ]
}
```

here we can see there are two types in the environment and nothing else. `u`
has identifier `` `Type (`Root root, "u") ``, and has an `Ident.t` (internal
identifier) of `("u",1)`. `t` has no manifest, as it's abstract, but `u` has a
manifest that points to the `` `Global `` path that has already been ``
`Resolved `` to `` `Identifier `` `` `Type (`Root root, "u") ``. So there won't
be much for us to do.

Once we've set up the environment we enter the function `Resolve.signature`
which takes a `Model.Lang.Signature.t` and runs the resolve mapping functions
over the tree. For this example the interesting point comes when we get to
looking at the manifest for type `u`. We see that we have a `Constr` that has a
path in it, so we look up the component from the path via the function
`Tools.lookup_type_from_model_path`. This returns us a `Result.result`
containing the resolved path and the `Component.Type.t` that represents the
type `t`. We don't particularly care about this, but the returned path we use
in place of the path we had before.

Simplest possible resolution:

```ocaml
# test_resolve {|
  type t
  type u = t
  |}
BEFORE
======
type (root Root).t
type (root Root).u = resolved[global((root Root).t)]

AFTER
=====
type (root Root).t
type (root Root).u = resolved[global((root Root).t)]

- : unit = ()
```

Let's look at a marginally more complicated example. In this case, our type `t`
is now inside a module:

```ocaml skip
module M : sig
    type t
end
type u = M.t  
```

The OCaml compiler find the module `M` exactly, but everything after that is
left to us to identify precisely. So the manifest of `u` is now:

```ocaml skip
Some (Constr (`Dot (`Resolved (`Identifier (`Module (`Root root, "M"))), "t"))
```

Note that this is the manifest from the `Lang.TypeDecl.t` rather than the
`Component.Type.t`, and so the argument to `Constr` is a `Model.Paths.Path.t`
rather than a `Cpath.t`, but this distinction is unimportant here and amounts
to the reason why there is no `` `Global `` here where there is above.

What _is_ important is that the path is not completely resolved. The `M` bit is
resolved, but the `t` bit is not. So we have to do a bit more work when we look
up the type in `lookup_type_from_model_path`.

Let's look in more detail at that process. The first thing that happens is that
the path is matched the `` `Dot `` is found. This implies that the thing before
the dot is a module, so we call `lookup_module_from_model_path`.  This is a
resolved identifier so we can simply look this up from the environment. This
gets us back the path and `Component.Module.t` representing the module `M`,
which are:

```ocaml skip
  `Identifier (`Module (`Root root, "M")))  
```

and

```ocaml skip
    { id = ("M", 0)
    ; type_ = ModuleType (Signature [ Type {id = ("t", 1); manifest = None } ])
```

we then convert this into a signature, which in this case is simply extrating
the `Signature` from within the `type_` field, and then we can call
`Component.Find.type_in_sig` to find the representation of the type `t`. Now we
know it's there we can construct a resolved path that points precisely at this
type declaration. We take the path returned when looking up [M], and use that
as the parent in the declaration of the new path for `t`. As a comparison, we
were called with  

```ocaml skip
    `Dot (`Resolved (`Identifier (`Module (`Root root, "M"))), "t"))
```

but we return

```ocaml skip
    `Resolved (`Type (`Identifier (`Module (`Root root, "M")), "t"))
```

and this is replaced as the path in the representation of the signature.

Basic resolution 2, environment lookup:

```ocaml
# test_resolve {|
  module M : sig
      type t
  end
  type u = M.t
    |}
BEFORE
======
module (root Root).M : sig
  type (root Root).M.t
  end
type (root Root).u = resolved[global((root Root).M)].t

AFTER
=====
module (root Root).M : sig
  type (root Root).M.t
  end
type (root Root).u = resolved[global((root Root).M).t]

- : unit = ()
```


Now we have a bit of indirection. Take the example:

```ocaml skip
    module type M = sig
        type t
    end
    module N : M
    type u = N.t
```

This follows a similar pattern to the above, but now when we look up `N` from
the environment we get back

```ocaml skip
    { id = ("N", 2)
    ; type_ = ModuleType (Path (`Global (`Resolved (`Identifier (`ModuleType root, "M"))))))}
```

so we can't directly extract the signature from this module to look up the type
`t` as before.  Instead we must look up the module type "M" from the
environment, then convert that into a signature, then look up the type in that.
when we look up "M" we get back

```ocaml skip
{ id = ("M", 9)
   ; expr = Some (Signature
              [ Module
                  { id = ("N", 10)
                  ; type_ = ModuleType (Signature [Type
                         { id = ("t", 11);
                           manifest = None}])}])})
```

We can turn this into a signature as before, and then we proceed in a similar fashion to the previous example.

Basic resolution 3, module type:

```ocaml
# test_resolve {|
  module type M = sig
      type t
  end
  module N : M
  type u = N.t
    |}
BEFORE
======
module type (root Root).M = sig
  type (root Root).M.t
  end
module (root Root).N : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).N)].t

AFTER
=====
module type (root Root).M = sig
  type (root Root).M.t
  end
module (root Root).N : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).N).t]

- : unit = ()
```

This example is very similar but there is one more level of nesting of the modules:

```ocaml skip
    module type M = sig
        module N : sig
            type t
        end
    end
    module A : M
    type u = A.N.t
```

Basic resolution 4, module type:

```ocaml
# test_resolve {|
  module type M = sig
      module N : sig
          type t
      end
  end
  module A : M
  type u = A.N.t
    |}
BEFORE
======
module type (root Root).M = sig
  module (root Root).M.N : sig
    type (root Root).M.N.t
    end
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A)].N.t

AFTER
=====
module type (root Root).M = sig
  module (root Root).M.N : sig
    type (root Root).M.N.t
    end
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A).N.t]

- : unit = ()
```

This example is rather more interesting:

```ocaml skip
module type M = sig
    module type N = sig
        type t
    end
    module B : N
end
module A : M
type u = A.B.t
```

Once again we look up `A`, then look up `M`. The representation of `M` is as follows:

```ocaml skip
{ id = ("M", 0);
; expr = Some (Signature
    [ ModuleType
        { id = ("N", 1)
        ; expr = Some (Signature [Type {Type.id = ("t", 3); manifest = None}])};
      Module
        { id = ("B", 2)
        ; type_ = ModuleType (Path (`Local ("N", 1)))}])})];
```

The interesting thing here is that the type of "B" has a *Local* path in it.
Eventually we need to have fully qualified paths for all items so this needs some work.
The way this is handled is that when we want to look up an element within a module,
we don't just convert it blindly to a signature. Since we have the fully qualified
path to the module, we prefix all the identifiers bound in that signature
with that path to turn them into global paths.

In the above example, wherever we see `` `Local ("N",1) ``, we substitute instead
`` `Global (`Resolved (`ModuleType (a, "N"))) `` where `a` is the path to `A` that we
found when looking up the module `A` previously.

Once this is done when we look up the module `B` we discover a module whose
type is 

```ocaml skip
    `Global (`Resolved (`ModuleType (`Identifier (`Module (`Root root, "A")), "N")))
```

So once again we call lookup_module_type_from_model_path, which recurses down 
until it finds the identifier. We look up `A`, find the module type "N" within
it, then convert _that_ into a signature, prefixing it with the path `A.B`, 
and then we can look up the type `t`. 

```ocaml
# test_resolve {|
  module type M = sig
      module type N = sig
          type t
      end
      module B : N
  end
  module A : M
  type u = A.B.t
    |}
BEFORE
======
module type (root Root).M = sig
  module type (root Root).M.N = sig
    type (root Root).M.N.t
    end
  module (root Root).M.B : resolved[global((root Root).M.N)]
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A)].B.t

AFTER
=====
module type (root Root).M = sig
  module type (root Root).M.N = sig
    type (root Root).M.N.t
    end
  module (root Root).M.B : resolved[global((root Root).M.N)]
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A).B.t]

- : unit = ()
```

```ocaml
# test_resolve {|
  module type M = sig
      module type N = sig
          type t
      end
      module X : sig
          module B : N
      end
  end
  module A : M
  type u = A.X.B.t
    |}
BEFORE
======
module type (root Root).M = sig
  module type (root Root).M.N = sig
    type (root Root).M.N.t
    end
  module (root Root).M.X : sig
    module (root Root).M.X.B : resolved[global((root Root).M.N)]
    end
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A)].X.B.t

AFTER
=====
module type (root Root).M = sig
  module type (root Root).M.N = sig
    type (root Root).M.N.t
    end
  module (root Root).M.X : sig
    module (root Root).M.X.B : resolved[global((root Root).M.N)]
    end
  end
module (root Root).A : resolved[global((root Root).M)]
type (root Root).u = resolved[global((root Root).A).X.B.t]

- : unit = ()
```

Ensure a substitution is taken into account during resolution:

```ocaml
# test_resolve {|
  module type A = sig
  module M : sig module type S end
  module N : M.S
  end

  module B : sig module type S = sig type t end end

  module C : A with module M = B

  type t = C.N.t
    |}
BEFORE
======
module type (root Root).A = sig
  module (root Root).A.M : sig
    module type (root Root).A.M.S
    end
  module (root Root).A.N : resolved[global((root Root).A.M)].S
  end
module (root Root).B : sig
  module type (root Root).B.S = sig
    type (root Root).B.S.t
    end
  end
module (root Root).C : resolved[global((root Root).A)] with [*.M = = resolved[global((root Root).B)]]
type (root Root).t = resolved[global((root Root).C)].N.t

AFTER
=====
module type (root Root).A = sig
  module (root Root).A.M : sig
    module type (root Root).A.M.S
    end
  module (root Root).A.N : resolved[opaquemoduletype(global((root Root).A.M).S)]
  end
module (root Root).B : sig
  module type (root Root).B.S = sig
    type (root Root).B.S.t
    end
  end
module (root Root).C : resolved[global((root Root).A)] with [root_module_type(global((root Root).A)).M = = resolved[global((root Root).B)]]
type (root Root).t = resolved[global((root Root).C).N.t]

- : unit = ()
```

Ensure a destructive substitution is taken into account during resolution:

```ocaml
# test_resolve {|
  module type A = sig
  module M : sig module type S end
  module N : M.S
  end

  module B : sig module type S = sig type t end end

  module C : A with module M := B

  type t = C.N.t
    |}
BEFORE
======
module type (root Root).A = sig
  module (root Root).A.M : sig
    module type (root Root).A.M.S
    end
  module (root Root).A.N : resolved[global((root Root).A.M)].S
  end
module (root Root).B : sig
  module type (root Root).B.S = sig
    type (root Root).B.S.t
    end
  end
module (root Root).C : resolved[global((root Root).A)] with [*.M := resolved[global((root Root).B)]]
type (root Root).t = resolved[global((root Root).C)].N.t

AFTER
=====
module type (root Root).A = sig
  module (root Root).A.M : sig
    module type (root Root).A.M.S
    end
  module (root Root).A.N : resolved[opaquemoduletype(global((root Root).A.M).S)]
  end
module (root Root).B : sig
  module type (root Root).B.S = sig
    type (root Root).B.S.t
    end
  end
module (root Root).C : resolved[global((root Root).A)] with [root_module_type(global((root Root).A)).M := resolved[global((root Root).B)]]
type (root Root).t = resolved[global((root Root).C).N.t]

- : unit = ()
```

Resolve a module alias:

```ocaml
# test_resolve {|
  module A : sig
      type t
  end
  module B = A
  type t = B.t
    |}
BEFORE
======
module (root Root).A : sig
  type (root Root).A.t
  end
module (root Root).B = resolved[global((root Root).A)]
type (root Root).t = resolved[global((root Root).B)].t

AFTER
=====
module (root Root).A : sig
  type (root Root).A.t
  end
module (root Root).B = resolved[global((root Root).A)]
type (root Root).t = resolved[(global((root Root).A) -> global((root Root).B)).t]

- : unit = ()
```

Resolve a module alias:

```ocaml
# test_resolve {|
  module A : sig
      type t
  end
  module B = A
  module C = B
  type t = C.t
    |}
BEFORE
======
module (root Root).A : sig
  type (root Root).A.t
  end
module (root Root).B = resolved[global((root Root).A)]
module (root Root).C = resolved[global((root Root).B)]
type (root Root).t = resolved[global((root Root).C)].t

AFTER
=====
module (root Root).A : sig
  type (root Root).A.t
  end
module (root Root).B = resolved[global((root Root).A)]
module (root Root).C = resolved[(global((root Root).A) -> global((root Root).B))]
type (root Root).t = resolved[((global((root Root).A) -> global((root Root).B)) -> global((root Root).C)).t]

- : unit = ()
```

Resolve a functor:

```ocaml
# test_resolve {|
  module type S = sig
    type t
  end

  module F ( X : S ) ( Y : S ) : sig
    type x_t = X.t
    type y_t = Y.t
    type f_t = x_t
  end
  |}
BEFORE
======
module type (root Root).S = sig
  type (root Root).S.t
  end
module (root Root).F : ((param (root Root).F X) : resolved[global((root Root).S)]) -> ((param (root Root).F.result Y) : resolved[global((root Root).S)]) -> sig
  type (root Root).F.result.result.x_t = resolved[global((param (root Root).F X))].t
  type (root Root).F.result.result.y_t = resolved[global((param (root Root).F.result Y))].t
  type (root Root).F.result.result.f_t = resolved[global((root Root).F.result.result.x_t)]
  end

AFTER
=====
module type (root Root).S = sig
  type (root Root).S.t
  end
module (root Root).F : ((param (root Root).F X) : resolved[global((root Root).S)]) -> ((param (root Root).F.result Y) : resolved[global((root Root).S)]) -> sig
  type (root Root).F.result.result.x_t = resolved[global((param (root Root).F X)).t]
  type (root Root).F.result.result.y_t = resolved[global((param (root Root).F.result Y)).t]
  type (root Root).F.result.result.f_t = resolved[global((root Root).F.result.result.x_t)]
  end

- : unit = ()
```

Resolve a functor:

```ocaml
# test_resolve {|
  module type S =
  sig
    type t
  end

  module type S1 = functor (_ : S) -> S

  module F1 : functor (Arg : S) -> S

  module F2 : functor (Arg : S) -> (S with type t = Arg.t)

  module F3 : functor (Arg : S) ->
  sig
    type t = Arg.t
  end

  module F4 (Arg : S) : S

  module F5 (Arg1 : S) (Arg2 : S) (Arg3 : S) : sig
          type t = Arg1.t
          type u = Arg2.t
          type v = Arg3.t
          type z = t
  end

  module F6 : S1

  module type F7 = functor (Arg : S) -> sig
    type t = Arg.t
    type u = t
  end
    |}
BEFORE
======
module type (root Root).S = sig
  type (root Root).S.t
  end
module type (root Root).S1 = ((param (root Root).S1 _) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F1 : ((param (root Root).F1 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F2 : ((param (root Root).F2 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)] with [*.t = resolved[global((param (root Root).F2 Arg))].t]
module (root Root).F3 : ((param (root Root).F3 Arg) : resolved[global((root Root).S)]) -> sig
  type (root Root).F3.result.t = resolved[global((param (root Root).F3 Arg))].t
  end
module (root Root).F4 : ((param (root Root).F4 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F5 : ((param (root Root).F5 Arg1) : resolved[global((root Root).S)]) -> ((param (root Root).F5.result Arg2) : resolved[global((root Root).S)]) -> ((param (root Root).F5.result.result Arg3) : resolved[global((root Root).S)]) -> sig
  type (root Root).F5.result.result.result.t = resolved[global((param (root Root).F5 Arg1))].t
  type (root Root).F5.result.result.result.u = resolved[global((param (root Root).F5.result Arg2))].t
  type (root Root).F5.result.result.result.v = resolved[global((param (root Root).F5.result.result Arg3))].t
  type (root Root).F5.result.result.result.z = resolved[global((root Root).F5.result.result.result.t)]
  end
module (root Root).F6 : resolved[global((root Root).S1)]
module type (root Root).F7 = ((param (root Root).F7 Arg) : resolved[global((root Root).S)]) -> sig
  type (root Root).F7.result.t = resolved[global((param (root Root).F7 Arg))].t
  type (root Root).F7.result.u = resolved[global((root Root).F7.result.t)]
  end

AFTER
=====
module type (root Root).S = sig
  type (root Root).S.t
  end
module type (root Root).S1 = ((param (root Root).S1 _) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F1 : ((param (root Root).F1 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F2 : ((param (root Root).F2 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)] with [root_module_type(global((root Root).S)).t = resolved[global((param (root Root).F2 Arg)).t]]
module (root Root).F3 : ((param (root Root).F3 Arg) : resolved[global((root Root).S)]) -> sig
  type (root Root).F3.result.t = resolved[global((param (root Root).F3 Arg)).t]
  end
module (root Root).F4 : ((param (root Root).F4 Arg) : resolved[global((root Root).S)]) -> resolved[global((root Root).S)]
module (root Root).F5 : ((param (root Root).F5 Arg1) : resolved[global((root Root).S)]) -> ((param (root Root).F5.result Arg2) : resolved[global((root Root).S)]) -> ((param (root Root).F5.result.result Arg3) : resolved[global((root Root).S)]) -> sig
  type (root Root).F5.result.result.result.t = resolved[global((param (root Root).F5 Arg1)).t]
  type (root Root).F5.result.result.result.u = resolved[global((param (root Root).F5.result Arg2)).t]
  type (root Root).F5.result.result.result.v = resolved[global((param (root Root).F5.result.result Arg3)).t]
  type (root Root).F5.result.result.result.z = resolved[global((root Root).F5.result.result.result.t)]
  end
module (root Root).F6 : resolved[global((root Root).S1)]
module type (root Root).F7 = ((param (root Root).F7 Arg) : resolved[global((root Root).S)]) -> sig
  type (root Root).F7.result.t = resolved[global((param (root Root).F7 Arg)).t]
  type (root Root).F7.result.u = resolved[global((root Root).F7.result.t)]
  end

- : unit = ()
```

```ocaml skip
let functor_app =
  { name = "Functor"
  ; description = "Resolve a functor"
  ; test_data = {|
module type ARG = sig
  module type S
end

module F : functor (X : ARG) -> sig
  module N : X.S 
end

module M : sig
  module type S = sig
    type t
  end
end

type t = F(M).N.t
|}
```

Identifier of F:

```
  ident_f = `Identifier (`Module (`Root (Common.root, "Root"), "F")))
```

Identifier of M:

```
  ident_m = `Identifier (`Module (`Root (Common.root, "Root"), "M")))
```

```
type t = `Type (`Subst (`ModuleType (ident_m, "S"), `Module (`Apply (ident_f, ident_m), "N"))), "t")
```

Functor app nightmare:

```ocaml
# test_resolve {|
  module type Type = sig module type T end
  module App : functor (T : Type) (F : Type -> Type) (M : F(T).T) -> F(T).T
  module Bar : sig module type T = sig type bar end end
  module Foo :
    functor (T : Type) -> sig module type T = sig module Foo : T.T end end
  module FooBarInt : sig module Foo : sig type bar = int end end
  type t = App(Bar)(Foo)(FooBarInt).Foo.bar
  (* Note: I think correct result is:
  type t = resolved[(global(Bar).T subst-> global(App)(resolved[global(Bar)])(resolved[global(Foo)])(resolved[global(FooBarInt)]).Foo).bar]
  *)
    |}
BEFORE
======
module type (root Root).Type = sig
  module type (root Root).Type.T
  end
module (root Root).App : ((param (root Root).App T) : resolved[global((root Root).Type)]) -> ((param (root Root).App.result F) : ((param (param (root Root).App.result F) _) : resolved[global((root Root).Type)]) -> resolved[global((root Root).Type)]) -> ((param (root Root).App.result.result M) : resolved[global((param (root Root).App.result F))](resolved[global((param (root Root).App T))]).T) -> resolved[global((param (root Root).App.result F))](resolved[global((param (root Root).App T))]).T
module (root Root).Bar : sig
  module type (root Root).Bar.T = sig
    type (root Root).Bar.T.bar
    end
  end
module (root Root).Foo : ((param (root Root).Foo T) : resolved[global((root Root).Type)]) -> sig
  module type (root Root).Foo.result.T = sig
    module (root Root).Foo.result.T.Foo : resolved[global((param (root Root).Foo T))].T
    end
  end
module (root Root).FooBarInt : sig
  module (root Root).FooBarInt.Foo : sig
    type (root Root).FooBarInt.Foo.bar = resolved[global(int)]
    end
  end
type (root Root).t = resolved[global((root Root).App)](resolved[global((root Root).Bar)])(resolved[global((root Root).Foo)])(resolved[global((root Root).FooBarInt)]).Foo.bar

AFTER
=====
module type (root Root).Type = sig
  module type (root Root).Type.T
  end
module (root Root).App : ((param (root Root).App T) : resolved[global((root Root).Type)]) -> ((param (root Root).App.result F) : ((param (param (root Root).App.result F) _) : resolved[global((root Root).Type)]) -> resolved[global((root Root).Type)]) -> ((param (root Root).App.result.result M) : resolved[opaquemoduletype(global((param (root Root).App.result F))(resolved[global((param (root Root).App T))]).T)]) -> resolved[opaquemoduletype(global((param (root Root).App.result F))(resolved[global((param (root Root).App T))]).T)]
module (root Root).Bar : sig
  module type (root Root).Bar.T = sig
    type (root Root).Bar.T.bar
    end
  end
module (root Root).Foo : ((param (root Root).Foo T) : resolved[global((root Root).Type)]) -> sig
  module type (root Root).Foo.result.T = sig
    module (root Root).Foo.result.T.Foo : resolved[opaquemoduletype(global((param (root Root).Foo T)).T)]
    end
  end
module (root Root).FooBarInt : sig
  module (root Root).FooBarInt.Foo : sig
    type (root Root).FooBarInt.Foo.bar = resolved[global(int)]
    end
  end
type (root Root).t = resolved[(global((root Root).Bar).T subst-> global((root Root).App)(resolved[global((root Root).Bar)])(resolved[global((root Root).Foo)])(resolved[global((root Root).FooBarInt)]).Foo).bar]

- : unit = ()
```
