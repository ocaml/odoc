Resolution
==========

Resolution is the process by which we take a value of type `Lang.t` and
look up details of the internal cross references and make sure we know exactly
which component is being referred to. Much of the work has been done by the
compiler but we need to do a little more. For example, given
	        
```
module M : sig
  type t
end
type u = M.t 
```

in the definition of `u`, the compiler tells us precisely which `M` is on the
right hand side but doesn't say to which `t` it is referring to; the representation
of this is simply the string "t". Resolution is the process of finding precise
identifiers that will allow us to construct links.

We'll start with a little preamble, constructing the execution environment in which we can
run through some tests and describe the resolution process.

```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 65536;;
Odoc_xref2__Component.Delayed.eager := true;;
```

Simple resolution
-----------------

We'll start by examining the simple case - how we deal with module hierarchies,
and build up later to the more advanced uses of the module system.

### No modules at all

The simplest possible resolution it simply checking that a resolved path exists.
We'll use a helper function `Common.signature_of_mli_string` to go straight from
a text representation of an mli file to the type we would ordinarily read from a
compiled cmti file.

```ocaml env=e1
let test_data = {|
  type x
  type u = x
|};;
let sg = Common.signature_of_mli_string test_data;;
```

The type of `sg` is:

```ocaml env=e1
# #show_val sg;;
val sg : Odoc_model.Lang.Signature.t
```

This `Signature.t` is a representation of the entire cmti file, and resolution
essentially proceeds as map function over this data type: `Signature.t -> Signature.t`,
with the end result having precise identifiers for all elements in the signature.

The first thing we do is to construct an environment from the signature. The
environment is simply a mapping from identifiers to items representing 
each element in the signature. The representations are types declared in the
module `Component`, and follow quite closely those in module `Lang`, the main
difference being in the types of paths. The environment constructed from the above
signature is as follows:

```ocaml env=e1
# Env.open_signature sg Env.empty;;
- : Env.t = <abstr>
```

here we can see there are two types in the environment and nothing else. `u` has identifier 
`` `Type (`Root (Common.root, "Root"), "u") ``, and has an internal identifier of `("u",17)`. `t` is
abstract and therefore has
no `manifest`, but `u` has a manifest that points to the `` `Identifier `` path that
has already been `` `Resolved `` to `` `Identifier `` `` `Type (`Root (Common.root, "Root"), "u") ``. So there won't be much
for us to do.

The resolution process proceeds starting from the `Lang.Signature.t` going down until it finds
values that are subtypes of `Path.t` - for example, a `Module.decl` is defined as

```
type decl =
  | Alias of Path.Module.t
  | ModuleType of ModuleType.expr
```

This type `Path.Module.t` is a polymorphic variant:

```
type any = [
  | `Resolved of Resolved_path.module_
  | `Root of string
  | `Forward of string
  | `Dot of module_ * string
  | `Apply of module_ * module_
]
```

and `Resolved_path.module_` is:

```
type module_ = [
  | `Identifier of Identifier.path_module
  | `Subst of module_type * module_
  | `SubstAlias of module_ * module_
  | `Hidden of module_
  | `Module of module_ * ModuleName.t
  | `Canonical of module_ * Path.module_
  | `Apply of module_ * Path.module_
  | `Alias of module_ * module_
  ]
```

Once it gets to a `Path.t` (or a `Path.Module.t`, `Path.Type.t` etc.), and looks down the path to
find the element. The aim is to have every path start with `` `Resolved ``.

In our example above, the first instance of a `Path.t` is the right hand side of the type `u`. For
the purposes of examining that value, we can define a `Lens` that extracts just that out of the
`Signature.t`:

```ocaml env=e1
let type_manifest name =
  let open Common.LangUtils.Lens in
  Signature.type_ name |-- TypeDecl.equation |-- TypeDecl.Equation.manifest
let u_manifest = type_manifest "u"
let t_manifest = type_manifest "t"
```

and using this lens on our original signature we obtain:

```ocaml env=e1
# Common.LangUtils.Lens.get u_manifest sg
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved (`Identifier (`Type (`Root (Common.root, "Root"), "x"))), 
   []))
```

This path clearly already begins with `` `Resolved ``, so we don't expect to change it,
but we _are_ going to check it exists. We convert the path into a `Cpath.t` and call
`Tools.lookup_type_from_path`. This function starts approximately:

```
   match p with
    | `Resolved p -> Ok (lookup_type_from_resolved_path env p)
```

and `lookup_type_from_resolved_path` starts:

```
and lookup_type_from_resolved_path : Env.t -> Cpath.resolved -> type_lookup_result = fun env p ->
    match p with
    | `Identifier (#Odoc_model.Paths.Identifier.Type.t as i) ->
        let t = Env.lookup_type i env in
        (false, `Identifier i, t)
```

and so we simply look up the type in the environment and return the same path we
started with alongside the `Component.Type.t` that represents the type (which we
ignore in this case).

```ocaml env=e1
# Compile.signature Env.empty sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "x");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
     manifest = None; constraints = []};
   representation = None});
 Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "u");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
     manifest =
      Some
       (Odoc_model.Lang.TypeExpr.Constr
         (`Resolved (`Identifier (`Type (`Root (Common.root, "Root"), "x"))),
         []));
     constraints = []};
   representation = None})]
```

### One module

Now let's look at a marginally more complicated example. In this case, our type [t] is now
inside a module:

```ocaml env=e1
let test_data = {|
module M : sig
    type t
end
type u = M.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

The OCaml compiler find the module `M` exactly, but everything after that is left to us
to identify precisely. So the manifest of `u` is now:

```ocaml env=e1
# Common.LangUtils.Lens.get u_manifest sg
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Resolved (`Identifier (`Module (`Root (Common.root, "Root"), "M"))),
       "t"),
   []))
```

Here we can see that the path is not completely resolved. The `M` bit is resolved, but the `t`
bit is not. So we have to do a bit more work when we look up the type in
`Tools.lookup_type_from_path`.

Let's look in more detail at that process. The first thing that happens is that the path is matched
the [`Dot] is found:

```
    | `Dot (m, x) -> begin
        match lookup_module_from_path env m with
```

This implies that the thing before the dot is a module, so we call 
`Tools.lookup_module_from_path`.  This is a resolved identifier so we can simply look this up from
the environment. This gets us back the path and a `Component.Module.t` representing the module M,
which are:

```ocaml env=e1
# let get_ok = Tools.ResultMonad.get_resolved
val get_ok : ('a, 'b) Tools.ResultMonad.t -> 'a = <fun>
# let (path, module_) = get_ok @@ Tools.lookup_module_from_path env (`Resolved (`Identifier (`Module (`Root (Common.root, "Root"), "M"))));;
Line 1, characters 33-62:
Error: Unbound value Tools.lookup_module_from_path
```

The three values returned are a boolean representing whether this path is dependent on a module substituted in a functor (see later), the resolved path to the module, and a representation of the module itself. We then turn the module into a signature via `signature_of_module`, which in this case is quite simple since the module contains an explicit signature:

```ocaml env=e1
# Tools.signature_of_module env (path, module_);;
Line 1, characters 1-26:
Error: Unbound value Tools.signature_of_module
```

We're now in a position to verify the existence of the type `t` we're
looking up - and indeed it is there. We therefore construct the
new resolved path to this type : `` `Type (path, "t") ``, and return
this and the definition of the type, which in this case is 
uninteresting.

### Indirection

Let's now examine the case when a module's signature has to be found
from the environment.

```ocaml env=e1
let test_data = {|
module type M = sig
    type t
end
module N : M
type u = N.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

It proceeds much as the previous example until we get the result
of looking up the module `N`:

```ocaml env=e1
# let (path, module_) = get_ok @@ Tools.lookup_module_from_path env (`Resolved (`Identifier (`Module (`Root (Common.root, "Root"), "N"))));;
Line 1, characters 33-62:
Error: Unbound value Tools.lookup_module_from_path
```

This time turning the module into a signature demonstrates why the function `signature_of_module` requires the environment. We need to lookup the module type `M` from the environment to determine the
signature of the module. After this though the resolution is as before.

### Local paths

We now look at an example involving local paths. These paths happen
when a path refers to something that isn't in the toplevel environment. We'll use the following example to explain the idea:

```ocaml env=e1
let test_data = {|
module type M = sig
    module type N = sig
        type t
    end
    module B : N
end
module A : M
type u = A.B.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

The definition of module `B` constrains it to have a signature given
by the module type `N`. However, `N` is not defined at the top level
here, so it has a local identifier. We can see this by looking up module `M` from the environment:

```ocaml env=e1
# let m = Env.lookup_module_type (`ModuleType (`Root (Common.root, "Root"), "M")) env;;
val m : Component.ModuleType.t =
  {Odoc_xref2.Component.ModuleType.doc = [];
   expr =
    Some
     (Odoc_xref2.Component.ModuleType.Signature
       {Odoc_xref2.Component.Signature.items =
         [Odoc_xref2.Component.Signature.ModuleType (`LModuleType ("N", 1),
           {Odoc_xref2.Component.Delayed.v =
             Some
              {Odoc_xref2.Component.ModuleType.doc = [];
               expr =
                Some
                 (Odoc_xref2.Component.ModuleType.Signature
                   {Odoc_xref2.Component.Signature.items =
                     [Odoc_xref2.Component.Signature.Type (`LType ("t", 2),
                       Odoc_model.Lang.Signature.Ordinary,
                       {Odoc_xref2.Component.TypeDecl.doc = [];
                        equation =
                         {Odoc_xref2.Component.TypeDecl.Equation.params = [];
                          private_ = false; manifest = None;
                          constraints = []};
                        representation = None})];
                    removed = []});
               expansion = Some Odoc_xref2.Component.Module.AlreadyASig};
            get = <fun>});
          Odoc_xref2.Component.Signature.Module (`LModule ("B", 0),
           Odoc_model.Lang.Signature.Ordinary,
           {Odoc_xref2.Component.Delayed.v =
             Some
              {Odoc_xref2.Component.Module.doc = [];
               type_ =
                Odoc_xref2.Component.Module.ModuleType
                 (Odoc_xref2.Component.ModuleType.Path
                   (`Resolved (`Local (`LModuleType ("N", 1)))));
               canonical = None; hidden = false; display_type = None;
               expansion = None};
            get = <fun>})];
        removed = []});
   expansion = Some Odoc_xref2.Component.Module.AlreadyASig}
```

We can see here that module `B` has type `` Path (`Local ("N", 1)) `` which refers to the module type defined just above it.

To look up we need to have fully qualified paths for all items so this needs some work.
The way this is handled is that when we want to look up an element within a module,
we don't just convert it blindly to a signature. Since we have the fully qualified
path to the module, we prefix all the identifiers bound in that signature
with that path to turn them into global paths.

Concretely, we start here wanting to resolve the path for type `u`,
which is `A.B.t`. The compiler has started us off by resolving the
`A`:

```ocaml env=e1
# Common.LangUtils.Lens.get u_manifest sg
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Dot
         (`Resolved
            (`Identifier (`Module (`Root (Common.root, "Root"), "A"))),
          "B"),
       "t"),
   []))
```

we look up `A` from the environment:

```ocaml env=e1
# let (p, m) = Tools.lookup_module_from_resolved_path env (`Identifier (`Module (`Root (Common.root, "Root"), "A"))) in
  Tools.signature_of_module env (p, m) |> Tools.prefix_signature;;
Line 1, characters 14-52:
Error: Unbound value Tools.lookup_module_from_resolved_path
```

So before the prefixing operation we had that the type of the module was

```ocaml skip
type_ =
  Odoc_xref2.Component.Module.ModuleType
    (Odoc_xref2.Component.ModuleType.Path (`Local ("N", 26)))
```

and afterwards it is

```ocaml skip
type_ = 
  Odoc_xref2.Component.Module.ModuleType
    (Odoc_xref2.Component.ModuleType.Path
      (`Identifier
          (`Resolved
            (`ModuleType
                (`Identifier (`Module (`Root (Common.root, "Root"), "A")),
                "N")))))
```

We now look up module `B` from this signature, which once again
we need to convert to a signature to find `t`. We find the type above,
which we can then convert in turn into a signature. Once again, 
we go down the path until we find the identifier (`A` in this case), look that up from the environment, then look up the module type `N`.
We can then turn this into the signature for `B`, prefixing local
paths with the resolved path `A.B` (though in this case there are
now no local identifiers.) Finally we can now look up `t`, which
we then return along with the fully resolved identifier.

```ocaml env=e1
# Tools.lookup_type_from_path env
    (`Dot
      (`Dot
         (`Resolved
            (`Identifier (`Module (`Root (Common.root, "Root"), "A"))),
          "B"),
       "t"));;
- : (Tools.type_lookup_result, Cpath.type_) Tools.ResultMonad.t =
Odoc_xref2.Tools.ResultMonad.Resolved
 (`Type
    (`Module
       (`Module
          (`Module (`Identifier (`Module (`Root (Common.root, "Root"), "A"))),
           "B")),
     "t"),
  Odoc_xref2.Find.Found
   (`T
      {Odoc_xref2.Component.TypeDecl.doc = [];
       equation =
        {Odoc_xref2.Component.TypeDecl.Equation.params = [];
         private_ = false; manifest = None; constraints = []};
       representation = None}))
```

### Module aliases

```ocaml env=e1
let test_data = {|
module A : sig
  module M : sig type t end
  module N = M
end

type t = A.N.t
|}
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

Let's look at `t`'s manifest:

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Module
               (`Identifier (`Module (`Root (Common.root, "Root"), "A")),
                "M"),
             `Module
               (`Identifier (`Module (`Root (Common.root, "Root"), "A")),
                "N")),
          "t")),
   []))
```

When we turn `A.N.t` into a link, we need to render `A.N.t` as the link text
(going down the right-hand side of the `Alias` constructor), but link to
`A.M.t` since `A.N` will not have an expansion.

```ocaml env=e1
let test_data = {|
module A : sig
  module M : sig type t end
  module N = M
  module O = N
end

type t = A.O.t
|}
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Module
               (`Identifier (`Module (`Root (Common.root, "Root"), "A")),
                "N"),
             `Module
               (`Identifier (`Module (`Root (Common.root, "Root"), "A")),
                "O")),
          "t")),
   []))
```


### Module substitution

We'll now look at a case where we perform a module substitution
on a signature.

In the following example we have an interesting case where we
have a module with a module type in it, and we make a substitution
of this module.

```ocaml env=e1
let test_data = {|
module type A = sig
module M : sig module type S end
module N : M.S
end

module B : sig module type S = sig type t end end

module C : A with module M = B

type t = C.N.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
let resolved = Compile.signature env sg;;
```

So in module type `A`, module `N` has type `M.S`, which 
does not contain a declaration for type `t`.
When we make the substitution, although we're substituting `M`,
because the signature of `N` is `M.S`, we _also_ change `N`. So
in module `C`, `N` should now contain a type `t`.

Once again, we look at the resolution of `type t = C.N.t`. When
we look up the module C we find that the `type_` field look as
follows:

```ocaml env=e1
# let module_C_lens =
  let open Common.LangUtils.Lens in
  Signature.module_ "C";;
val module_C_lens :
  (Odoc_model.Lang.Signature.t, Odoc_model.Lang.Module.t)
  Common.LangUtils.Lens.lens =
  {Odoc_xref_test.Common.LangUtils.Lens.get = <fun>; set = <fun>}
# Common.LangUtils.Lens.get module_C_lens sg;;
- : Odoc_model.Lang.Module.t =
{Odoc_model.Lang.Module.id = `Module (`Root (Common.root, "Root"), "C");
 doc = [];
 type_ =
  Odoc_model.Lang.Module.ModuleType
   (Odoc_model.Lang.ModuleType.With
     (Odoc_model.Lang.ModuleType.Path
       (`Resolved
          (`Identifier (`ModuleType (`Root (Common.root, "Root"), "A")))),
     [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Root, "M"),
       Odoc_model.Lang.Module.Alias
        (`Resolved (`Identifier (`Module (`Root (Common.root, "Root"), "B")))))]));
 canonical = None; hidden = false; display_type = None; expansion = None}
```

Clearly there is no `type t` declared in here. Let's get the representation
of module `C` we see the following:

```ocaml env=e1
# let (p, m) = Tools.lookup_module_from_resolved_path env (`Identifier (`Module (`Root (Common.root, "Root"), "C")));;
Line 1, characters 14-52:
Error: Unbound value Tools.lookup_module_from_resolved_path
```

now we can ask for the signature of this module:

```ocaml env=e1
# let sg = Tools.signature_of_module env (p, m);;
Line 1, characters 10-35:
Error: Unbound value Tools.signature_of_module
```

and we can see we've picked up the `type t` declaration in `M.S`. If we now ask for the signature of `C.N` we get:

```ocaml env=e1
# let (p, m) = Tools.lookup_module_from_resolved_path env
      (`Module (`Identifier (`Module (`Root (Common.root, "Root"), "C")), "N"));;
Line 1, characters 14-52:
Error: Unbound value Tools.lookup_module_from_resolved_path
# Tools.signature_of_module env (p, m);;
Line 1, characters 1-26:
Error: Unbound value Tools.signature_of_module
```

where we've correctly identified that a type `t` exists in the signature. The path in
type t is resolved as:

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
            (`Identifier (`Module (`Root (Common.root, "Root"), "C")), "N"),
          "t")),
   []))
```

### Interesting functor

```ocaml env=e1
let test_data = {|
module M : sig
  module F(X : sig module type S end) : sig
    module type S = X.S
    module N : X.S
  end
  module T : sig
    module type S = sig type t end
  end
  module O : F(T).S
end
type t = M.O.t
type s = M.F(M.T).N.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
let t_manifest = type_manifest "t";;
let s_manifest = type_manifest "s";;
let resolved = Compile.signature Env.empty sg;;
```

The interesting thing here is the difference between `type t` and `type s`. The module `M.O` has
a concrete representation in the file and the expansion of this will contain a declaration of type
`t` - hence the path representing `M.O.t` does not need a `Subst` constructor in it. However, the
path to `M.F(M.T).N.t` can't point directly at a type within a module as there isn't one - in
some sense `F(M.T)` is making a brand new module on the fly without binding it anywhere, and the
type within this is not within the body of the functor itself.

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
            (`Identifier (`Module (`Root (Common.root, "Root"), "M")), "O"),
          "t")),
   []))
# Common.LangUtils.Lens.get s_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Module
                  (`Identifier (`Module (`Root (Common.root, "Root"), "M")),
                   "T"),
                "S"),
             `Module
               (`Apply
                  (`Module
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "M")),
                      "F"),
                   `Resolved
                     (`Module
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "M")),
                         "T"))),
                "N")),
          "t")),
   []))
```

In the following example, `type t` does not require a `Subst` constructor since although the
pattern is the same as above -- that a new type `t` appears in module `O` despite not being
present in the definition of the functor --  because the module `O` is bound it will be expanded
in the expansion phase of odoc's work, and hence we can simply point right inside the module.

We distinguish between the two cases via the `is_resolve` parameter, which is `true` when we
are directly resolving a path, but `false` when we simply wish to look up the signature of
a module.

```ocaml env=e1
let test_data = {|
module M : sig
  module F(X : sig module type S end) : sig
    module type S = sig
      module N : X.S
    end
  end
  module T : sig
    module type S = sig type t end
  end
  module O : F(T).S
end
type t = M.O.N.t
|}
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

```ocaml env=e1
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
            (`Module
               (`Identifier (`Module (`Root (Common.root, "Root"), "M")),
                "O"),
             "N"),
          "t")),
   []))
```

This following example also shouldn't have a `Subst`, since although as before we're effectively
constructing a new module `M.O(M)` on the fly here, the subsequent path `.N.t` is actually
present in the body of the functor and is independent of the argument (in fact, the argument
in this case is totally ignored). Consequently the resolution proceeds without encountering
a `Substituted` node and we therefore don't end up putting a `Subst` node in the returned 
path.

```ocaml env=e1
let test_data = {|
module M : sig
  module F(X : sig module type S end) : sig
    module type S = sig
      module N : X.S
    end
  end
  module T : sig
    module type S = sig type t end
  end
  module O : functor (X : sig end) -> F(T).S
end
type t = M.O(M).N.t
|}
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
            (`Apply
               (`Module
                  (`Identifier (`Module (`Root (Common.root, "Root"), "M")),
                   "O"),
                `Resolved
                  (`Identifier (`Module (`Root (Common.root, "Root"), "M")))),
             "N"),
          "t")),
   []))
```

# Higher order functors

Functors may take other functors as arguments. In the following example we have
a complex functor where type of the 3rd argument is dependent on the first two:

```ocaml env=e1
let test_data = {|
module type Type = sig module type T end
module App : functor (T : Type) (F : Type -> Type) (M : F(T).T) -> F(T).T
module Bar : sig module type T = sig type bar end end
module Foo :
  functor (T : Type) -> sig module type T = sig module Foo : T.T end end
module FooBarInt : sig module Foo : sig type bar = int end end
type t = App(Bar)(Foo)(FooBarInt).Foo.bar
|}
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

The type path we're trying to look up is:

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "t") sg;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Dot
         (`Apply
            (`Apply
               (`Apply
                  (`Resolved
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "App"))),
                   `Resolved
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Bar")))),
                `Resolved
                  (`Identifier (`Module (`Root (Common.root, "Root"), "Foo")))),
             `Resolved
               (`Identifier
                  (`Module (`Root (Common.root, "Root"), "FooBarInt")))),
          "Foo"),
       "bar"),
   []))
```

Extract the path to the Apply (so `App(Bar)(Foo)(FooBarInt)`)
```ocaml env=e1
let test_path =
  `Apply
    (`Apply
        (`Apply
          (`Resolved
              (`Identifier
                (`Module (`Root (Common.root, "Root"), "App"))),
            `Resolved
              (`Identifier
                (`Module (`Root (Common.root, "Root"), "Bar")))),
        `Resolved
          (`Identifier (`Module (`Root (Common.root, "Root"), "Foo")))),
      `Resolved
        (`Identifier
          (`Module (`Root (Common.root, "Root"), "FooBarInt"))));;
let cp = Component.Of_Lang.(module_path empty test_path);;
```

Now let's lookup that module:

```ocaml env=e1
# let (p, m) = get_ok @@ Tools.lookup_and_resolve_module_from_path true true env cp;;
val p : Cpath.Resolved.module_ =
  `Apply
    (`Apply
       (`Apply
          (`Identifier (`Module (`Root (Common.root, "Root"), "App")),
           `Resolved
             (`Substituted
                (`Identifier (`Module (`Root (Common.root, "Root"), "Bar"))))),
        `Resolved
          (`Substituted
             (`Identifier (`Module (`Root (Common.root, "Root"), "Foo"))))),
     `Resolved
       (`Substituted
          (`Identifier (`Module (`Root (Common.root, "Root"), "FooBarInt")))))
val m : Component.Module.t =
  {Odoc_xref2.Component.Module.doc = [];
   type_ =
    Odoc_xref2.Component.Module.ModuleType
     (Odoc_xref2.Component.ModuleType.Path
       (`Dot
          (`Apply
             (`Resolved
                (`Substituted
                   (`Identifier
                      (`Module (`Root (Common.root, "Root"), "Foo")))),
              `Resolved
                (`Substituted
                   (`Identifier
                      (`Module (`Root (Common.root, "Root"), "Bar"))))),
           "T")));
   canonical = None; hidden = false; display_type = None; expansion = None}
# let (p, sg') = Tools.signature_of_module env (p, m);;
Line 1, characters 16-41:
Error: Unbound value Tools.signature_of_module
```

```ocaml env=e1
let resolved = Compile.signature Env.empty sg;;
```

The resolved path of t is:

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Identifier (`Module (`Root (Common.root, "Root"), "Bar")),
                "T"),
             `Module
               (`Apply
                  (`Apply
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "App")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Bar")))),
                      `Resolved
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Foo")))),
                   `Resolved
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "FooBarInt")))),
                "Foo")),
          "bar")),
   []))
```

### Yet another nasty one


```ocaml env=e1
let test_data = {|
module M : sig
  module F(X : sig module type S end) : sig
    module type S = sig
      module N : X.S
    end
  end
  module T : sig
    module type S = sig type t end
  end
  module O : functor (X : sig end) -> F(T).S
end
type t = M.O(M).N.t
|}
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
            (`Apply
               (`Module
                  (`Identifier (`Module (`Root (Common.root, "Root"), "M")),
                   "O"),
                `Resolved
                  (`Identifier (`Module (`Root (Common.root, "Root"), "M")))),
             "N"),
          "t")),
   []))
```


# More dependently typed modules

```ocaml env=e1
let test_data = {|
module Dep1 : sig
  module type S = sig
    type c = int
  end
  module X : sig
    module Y : S
  end
end 
module Dep2 :
  functor (Arg : sig module type S module X : sig module Y : S end end) ->
    sig
      module A : sig
        module Y : Arg.S
      end
      module B = A.Y
    end
type dep1 = Dep2(Dep1).B.c
|};;

let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

(*let ty_dep1 = {|
type dep1 = Dep2(Dep1).B.c;;
|};;*)


```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep1") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Subst
               (`ModuleType
                  (`Identifier
                     (`Module (`Root (Common.root, "Root"), "Dep1")),
                   "S"),
                `Module
                  (`Module
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Dep2")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Dep1")))),
                      "A"),
                   "Y")),
             `Module
               (`Apply
                  (`Identifier
                     (`Module (`Root (Common.root, "Root"), "Dep2")),
                   `Resolved
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Dep1")))),
                "B")),
          "c")),
   []))
```

```ocaml env=e1
let test_data = {|
module Dep3 : sig type a end
module Dep4 : sig
  module type T = sig type b end
  module type S = sig
    module X : T
    module Y : sig end
  end
  module X : T
end
module Dep5 :
  functor (Arg : sig
                   module type T
                   module type S = sig
                     module X : T
                     module Y : sig end
                   end
                   module X : T
            end) ->
    sig
      module Z : Arg.S with module Y = Dep3
    end 
type dep2 = Dep5(Dep4).Z.X.b
type dep3 = Dep5(Dep4).Z.Y.a
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep2") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Identifier (`Module (`Root (Common.root, "Root"), "Dep4")),
                "T"),
             `Module
               (`Subst
                  (`ModuleType
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Dep4")),
                      "S"),
                   `Module
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Dep5")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Dep4")))),
                      "Z")),
                "X")),
          "b")),
   []))
# Common.LangUtils.Lens.get (type_manifest "dep3") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Identifier (`Module (`Root (Common.root, "Root"), "Dep3")),
             `Module
               (`Subst
                  (`ModuleType
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Dep4")),
                      "S"),
                   `Module
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Dep5")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Dep4")))),
                      "Z")),
                "Y")),
          "a")),
   []))
```

```ocaml env=e1
let test_data = {|
module Dep6 : sig
  module type S = sig type d end
  module type T = sig
    module type R = S
    module Y : R
  end
  module X : T
end

module Dep7 :
  functor (Arg : sig
                   module type S
                   module type T = sig
                     module type R = S
                     module Y : R
                   end
                   module X : T
            end) -> sig
      module M : Arg.T
    end

type dep4 = Dep7(Dep6).M.Y.d
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep4") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Subst
                  (`ModuleType
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Dep6")),
                      "T"),
                   `Module
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Dep7")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Dep6")))),
                      "M")),
                "R"),
             `Module
               (`Subst
                  (`ModuleType
                     (`Identifier
                        (`Module (`Root (Common.root, "Root"), "Dep6")),
                      "T"),
                   `Module
                     (`Apply
                        (`Identifier
                           (`Module (`Root (Common.root, "Root"), "Dep7")),
                         `Resolved
                           (`Identifier
                              (`Module (`Root (Common.root, "Root"), "Dep6")))),
                      "M")),
                "Y")),
          "d")),
   []))
```

```ocaml env=e1
let test_data = {|
module Dep8 : sig module type T = sig type t end end

module Dep9 : functor (X : sig module type T end) -> sig module type T = X.T end

module type Dep10 = Dep9(Dep8).T with type t = int

module Dep11 : sig
  module type S = sig
    type c
  end
end

module Dep12 :
  functor (Arg : sig module type S end) -> sig
      module type T = Arg.S
end

module Dep13 : Dep12(Dep11).T

type dep5 = Dep13.c
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```


```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep5") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Identifier (`Module (`Root (Common.root, "Root"), "Dep13")), "c")),
   []))
```

Let's take a look now at hidden modules

```ocaml env=e1
let test_data = {|
module Hidden__ : sig
  type t
end

module H=Hidden__

type t = Hidden__.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "t") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Hidden
            (`Identifier (`Module (`Root (Common.root, "Root"), "Hidden__"))),
          "t")),
   []))
```


How about references now

```ocaml env=e1
let test_data = {|
type t
(** [t] {!t} *)
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (Common.LangUtils.Lens.Signature.type_ "t") resolved;;
- : Odoc_model.Lang.TypeDecl.t =
{Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "t");
 doc =
  [{Odoc_model.Location_.location =
     {Odoc_model.Location_.file = "";
      start = {Odoc_model.Location_.line = 3; column = 6};
      end_ = {Odoc_model.Location_.line = 3; column = 14}};
    value =
     `Paragraph
       [{Odoc_model.Location_.location =
          {Odoc_model.Location_.file = "";
           start = {Odoc_model.Location_.line = 3; column = 6};
           end_ = {Odoc_model.Location_.line = 3; column = 9}};
         value = `Code_span "t"};
        {Odoc_model.Location_.location =
          {Odoc_model.Location_.file = "";
           start = {Odoc_model.Location_.line = 3; column = 9};
           end_ = {Odoc_model.Location_.line = 3; column = 10}};
         value = `Space};
        {Odoc_model.Location_.location =
          {Odoc_model.Location_.file = "";
           start = {Odoc_model.Location_.line = 3; column = 10};
           end_ = {Odoc_model.Location_.line = 3; column = 14}};
         value = `Reference (`Root ("t", `TUnknown), [])}]}];
 equation =
  {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
   manifest = None; constraints = []};
 representation = None}
```

Check that we can refer to stdlib:

```ocaml env=e1
let test_data = {|
type t = Buffer.t
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (Common.LangUtils.Lens.Signature.type_ "t") resolved;;
- : Odoc_model.Lang.TypeDecl.t =
{Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "t");
 doc = [];
 equation =
  {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
   manifest =
    Some
     (Odoc_model.Lang.TypeExpr.Constr
       (`Dot (`Dot (`Root "Stdlib", "Buffer"), "t"), []));
   constraints = []};
 representation = None}
```

```ocaml env=e1
let test_data = {|
exception Foo of { test : int }
|};;
let sg = Common.signature_of_mli_string test_data;;
```

```ocaml env=e1
# sg
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Exception
  {Odoc_model.Lang.Exception.id =
    `Exception (`Root (Common.root, "Root"), "Foo");
   doc = [];
   args =
    Odoc_model.Lang.TypeDecl.Constructor.Record
     [{Odoc_model.Lang.TypeDecl.Field.id =
        `Field (`Root (Common.root, "Root"), "test");
       doc = []; mutable_ = false;
       type_ =
        Odoc_model.Lang.TypeExpr.Constr
         (`Resolved (`Identifier (`CoreType "int")), [])}];
   res = None}]
```

Expansion test
==============

```ocaml env=e1
let test_data = {|
module type M = sig
  type t
end

type u

module type M1 = M with type t = u
|};;
let sg = Common.signature_of_mli_string test_data;;
```

```ocaml env=e1
# Link.signature Env.empty sg
Not reresolving
type_expression: path
before = global((root Root).u)
after = global((root Root).u)
after lookup = global((root Root).u)
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.ModuleType
  {Odoc_model.Lang.ModuleType.id =
    `ModuleType (`Root (Common.root, "Root"), "M");
   doc = [];
   expr =
    Some
     (Odoc_model.Lang.ModuleType.Signature
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`ModuleType (`Root (Common.root, "Root"), "M"), "t");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None})]);
   display_expr = None; expansion = Some Odoc_model.Lang.Module.AlreadyASig};
 Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "u");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
     manifest = None; constraints = []};
   representation = None});
 Odoc_model.Lang.Signature.ModuleType
  {Odoc_model.Lang.ModuleType.id =
    `ModuleType (`Root (Common.root, "Root"), "M1");
   doc = [];
   expr =
    Some
     (Odoc_model.Lang.ModuleType.With
       (Odoc_model.Lang.ModuleType.Path
         (`Resolved
            (`Identifier (`ModuleType (`Root (Common.root, "Root"), "M")))),
       [Odoc_model.Lang.ModuleType.TypeEq (`Dot (`Root, "t"),
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier (`Type (`Root (Common.root, "Root"), "u"))),
              []));
          constraints = []})]));
   display_expr = None; expansion = None}]
```

# Expansion continued

```ocaml env=e1
let test_data = {|
module Foo (X : sig end) : sig
  type t
  module type S = sig type s = C of t end
end
module Bar : sig end
module M : Foo(Bar).S
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
(*let expanded = Link.signature Env.empty resolved;;*)
(*let module_M_expansion =
  let open Common.LangUtils.Lens in
  Signature.module_ "M" |-- Module.expansion |-~ option |-~ Module.expansion_signature*)
```

TODO: This needs fixing (result ought to be `sig type s = C of Foo(Bar).t end`)
```ocaml env=e1
# Common.LangUtils.Lens.get module_M_expansion expanded
Line 1, characters 27-45:
Error: Unbound value module_M_expansion
```

# Anonymous stuff

```ocaml env=e1
let test_data = {|
module type S =
sig
  type t
end

module type S1 = functor (_ : S) -> S
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Compile.signature Env.empty sg;;
let expanded = Link.signature Env.empty resolved;;
```

# Hidden / Canonical

```ocaml env=e1
let test_data = {|
module CanonicalTest : sig
  module Base__List : sig
    type 'a t

    val id : 'a t -> 'a t
  end

  module Base__ : sig
    (** @canonical Root.CanonicalTest.Base.List *)
    module List = Base__List
  end

  module Base : sig
    module List = Base__.List
  end
end

val test : 'a CanonicalTest.Base__.List.t -> unit
|};;
let sg = Common.signature_of_mli_string test_data;;
let unit = Common.my_compilation_unit (`Root (Common.root, Odoc_model.Names.UnitName.of_string "Root")) [] sg;;
let resolver =
  let env = Odoc_odoc.Env.create ~important_digests:false
    ~directories:[] ~open_modules:[] in
  let resolver = Odoc_odoc.Env.build env (`Unit unit) in
  resolver;;
let resolved = Compile.unit resolver unit;;
let _ = Tools.is_compile := false;;
let linked = Link.unit resolver resolved;;

let _ = Tools.is_compile := true;;
let Odoc_model.Lang.Compilation_unit.Module sg = linked.Odoc_model.Lang.Compilation_unit.content
let value_test_lens =
  let open Common.LangUtils.Lens in
  Signature.value "test";;
```

```ocaml env=e1
# Common.LangUtils.Lens.get value_test_lens sg;;
- : Odoc_model.Lang.Value.t =
{Odoc_model.Lang.Value.id = `Value (`Root (Common.root, "Root"), "test");
 doc = [];
 type_ =
  Odoc_model.Lang.TypeExpr.Arrow (None,
   Odoc_model.Lang.TypeExpr.Constr
    (`Resolved
       (`Type
          (`Canonical
             (`Alias
                (`Hidden
                   (`Module
                      (`Identifier
                         (`Module
                            (`Root (Common.root, "Root"), "CanonicalTest")),
                       "Base__List")),
                 `Module
                   (`Module
                      (`Identifier
                         (`Module
                            (`Root (Common.root, "Root"), "CanonicalTest")),
                       "Base__"),
                    "List")),
              `Resolved
                (`Module
                   (`Module
                      (`Identifier
                         (`Module
                            (`Root (Common.root, "Root"), "CanonicalTest")),
                       "Base"),
                    "List"))),
           "t")),
    [Odoc_model.Lang.TypeExpr.Var "a"]),
   Odoc_model.Lang.TypeExpr.Constr
    (`Resolved (`Identifier (`CoreType "unit")), []))}
# Common.LangUtils.Lens.get value_test_lens resolved |> Component.Of_Lang.(value empty);;
Line 1, characters 43-51:
Error: This expression has type Odoc_odoc.Compilation_unit.t
       but an expression was expected of type
         Odoc_model.Lang.Signature.t = Odoc_model.Lang.Signature.item list
# let expanded = Link.signature Env.empty resolved;;
Line 1, characters 41-49:
Error: This expression has type Odoc_odoc.Compilation_unit.t
       but an expression was expected of type
         Odoc_model.Lang.Signature.t = Odoc_model.Lang.Signature.item list
# let (p, m) = get_ok @@ Tools.lookup_and_resolve_module_from_path true true env (`Resolved
               (`Identifier (`Module (`Root (Common.root, "Root"), "N"))));;
Exception:
Odoc_xref2.Env.MyFailure (`Module (`Root (Common.root, "Root"), "N"),
 <abstr>).
# Tools.signature_of_module env (p, m);;
Line 1, characters 1-26:
Error: Unbound value Tools.signature_of_module
# sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.Module.id =
    `Module (`Root (Common.root, "Root"), "CanonicalTest");
   doc = [];
   type_ =
    Odoc_model.Lang.Module.ModuleType
     (Odoc_model.Lang.ModuleType.Signature
       [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.Module.id =
           `Module
             (`Module (`Root (Common.root, "Root"), "CanonicalTest"),
              "Base__List");
          doc = [];
          type_ =
           Odoc_model.Lang.Module.ModuleType
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Module
                       (`Module
                          (`Root (Common.root, "Root"), "CanonicalTest"),
                        "Base__List"),
                     "t");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params =
                    [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                   private_ = false; manifest = None; constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Value
                {Odoc_model.Lang.Value.id =
                  `Value
                    (`Module
                       (`Module
                          (`Root (Common.root, "Root"), "CanonicalTest"),
                        "Base__List"),
                     "id");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Module
                                   (`Root (Common.root, "Root"),
                                    "CanonicalTest"),
                                 "Base__List"),
                              "t"))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Module
                                   (`Root (Common.root, "Root"),
                                    "CanonicalTest"),
                                 "Base__List"),
                              "t"))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]))}]);
          canonical = None; hidden = true; display_type = None;
          expansion = Some Odoc_model.Lang.Module.AlreadyASig});
        Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.Module.id =
           `Module
             (`Module (`Root (Common.root, "Root"), "CanonicalTest"),
              "Base__");
          doc = [];
          type_ =
           Odoc_model.Lang.Module.ModuleType
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module
                       (`Module
                          (`Root (Common.root, "Root"), "CanonicalTest"),
                        "Base__"),
                     "List");
                 doc =
                  [{Odoc_model.Location_.location =
                     {Odoc_model.Location_.file = "";
                      start = {Odoc_model.Location_.line = 10; column = 10};
                      end_ = {Odoc_model.Location_.line = 10; column = 50}};
                    value =
                     `Tag
                       (`Canonical
                          (`Dot
                             (`Dot
                                (`Dot (`Root "Root", "CanonicalTest"),
                                 "Base"),
                              "List"),
                           `Dot
                             (`Dot
                                (`Dot
                                   (`Root ("Root", `TUnknown),
                                    "CanonicalTest"),
                                 "Base"),
                              "List")))}];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Resolved
                      (`Canonical
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (Common.root, "Root"),
                                      "CanonicalTest"),
                                   "Base__List"))),
                          `Resolved
                            (`Module
                               (`Identifier
                                  (`Module
                                     (`Module
                                        (`Root (Common.root, "Root"),
                                         "CanonicalTest"),
                                      "Base")),
                                "List")))));
                 canonical =
                  Some
                   (`Dot
                      (`Dot (`Dot (`Root "Root", "CanonicalTest"), "Base"),
                       "List"),
                    `Dot
                      (`Dot
                         (`Dot (`Root ("Root", `TUnknown), "CanonicalTest"),
                          "Base"),
                       "List"));
                 hidden = false; display_type = None; expansion = None})]);
          canonical = None; hidden = true; display_type = None;
          expansion = Some Odoc_model.Lang.Module.AlreadyASig});
        Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.Module.id =
           `Module
             (`Module (`Root (Common.root, "Root"), "CanonicalTest"), "Base");
          doc = [];
          type_ =
           Odoc_model.Lang.Module.ModuleType
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module
                       (`Module
                          (`Root (Common.root, "Root"), "CanonicalTest"),
                        "Base"),
                     "List");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Resolved
                      (`Canonical
                         (`Alias
                            (`Hidden
                               (`Identifier
                                  (`Module
                                     (`Module
                                        (`Root (Common.root, "Root"),
                                         "CanonicalTest"),
                                      "Base__List"))),
                             `Module
                               (`Hidden
                                  (`Identifier
                                     (`Module
                                        (`Module
                                           (`Root (Common.root, "Root"),
                                            "CanonicalTest"),
                                         "Base__"))),
                                "List")),
                          `Resolved
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Module
                                        (`Root (Common.root, "Root"),
                                         "CanonicalTest"),
                                      "Base"),
                                   "List"))))));
                 canonical = None; hidden = false;
                 display_type =
                  Some
                   (Odoc_model.Lang.Module.ModuleType
                     (Odoc_model.Lang.ModuleType.Signature
                       [Odoc_model.Lang.Signature.Comment
                         (`Docs
                            [{Odoc_model.Location_.location =
                               {Odoc_model.Location_.file = "";
                                start =
                                 {Odoc_model.Location_.line = 0; column = 0};
                                end_ =
                                 {Odoc_model.Location_.line = 0; column = 0}};
                              value =
                               `Tag
                                 (`Canonical
                                    (`Dot
                                       (`Dot
                                          (`Dot
                                             (`Root "Root", "CanonicalTest"),
                                           "Base"),
                                        "List"),
                                     `Dot
                                       (`Dot
                                          (`Dot
                                             (`Root ("Root", `TUnknown),
                                              "CanonicalTest"),
                                           "Base"),
                                        "List")))}]);
                        Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`Module
                                (`Module
                                   (`Module
                                      (`Root (Common.root, "Root"),
                                       "CanonicalTest"),
                                    "Base"),
                                 "List"),
                              "t");
                          doc = [];
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params =
                             [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                            private_ = false;
                            manifest =
                             Some
                              (Odoc_model.Lang.TypeExpr.Constr
                                (`Resolved
                                   (`Type
                                      (`Hidden
                                         (`Identifier
                                            (`Module
                                               (`Module
                                                  (`Root
                                                     (Common.root, "Root"),
                                                   "CanonicalTest"),
                                                "Base__List"))),
                                       "t")),
                                [Odoc_model.Lang.TypeExpr.Var "a"]));
                            constraints = []};
                          representation = None});
                        Odoc_model.Lang.Signature.Value
                         {Odoc_model.Lang.Value.id =
                           `Value
                             (`Module
                                (`Module
                                   (`Module
                                      (`Root (Common.root, "Root"),
                                       "CanonicalTest"),
                                    "Base"),
                                 "List"),
                              "id");
                          doc = [];
                          type_ =
                           Odoc_model.Lang.TypeExpr.Arrow (None,
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved
                                (`Identifier
                                   (`Type
                                      (`Module
                                         (`Module
                                            (`Module
                                               (`Root (Common.root, "Root"),
                                                "CanonicalTest"),
                                             "Base"),
                                          "List"),
                                       "t"))),
                             [Odoc_model.Lang.TypeExpr.Var "a"]),
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved
                                (`Identifier
                                   (`Type
                                      (`Module
                                         (`Module
                                            (`Module
                                               (`Root (Common.root, "Root"),
                                                "CanonicalTest"),
                                             "Base"),
                                          "List"),
                                       "t"))),
                             [Odoc_model.Lang.TypeExpr.Var "a"]))}]));
                 expansion =
                  Some
                   (Odoc_model.Lang.Module.Signature
                     [Odoc_model.Lang.Signature.Comment
                       (`Docs
                          [{Odoc_model.Location_.location =
                             {Odoc_model.Location_.file = "";
                              start =
                               {Odoc_model.Location_.line = 0; column = 0};
                              end_ =
                               {Odoc_model.Location_.line = 0; column = 0}};
                            value =
                             `Tag
                               (`Canonical
                                  (`Dot
                                     (`Dot
                                        (`Dot (`Root "Root", "CanonicalTest"),
                                         "Base"),
                                      "List"),
                                   `Dot
                                     (`Dot
                                        (`Dot
                                           (`Root ("Root", `TUnknown),
                                            "CanonicalTest"),
                                         "Base"),
                                      "List")))}]);
                      Odoc_model.Lang.Signature.Type
                       (Odoc_model.Lang.Signature.Ordinary,
                       {Odoc_model.Lang.TypeDecl.id =
                         `Type
                           (`Module
                              (`Module
                                 (`Module
                                    (`Root (Common.root, "Root"),
                                     "CanonicalTest"),
                                  "Base"),
                               "List"),
                            "t");
                        doc = [];
                        equation =
                         {Odoc_model.Lang.TypeDecl.Equation.params =
                           [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                          private_ = false; manifest = None;
                          constraints = []};
                        representation = None});
                      Odoc_model.Lang.Signature.Value
                       {Odoc_model.Lang.Value.id =
                         `Value
                           (`Module
                              (`Module
                                 (`Module
                                    (`Root (Common.root, "Root"),
                                     "CanonicalTest"),
                                  "Base"),
                               "List"),
                            "id");
                        doc = [];
                        type_ =
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved
                              (`Identifier
                                 (`Type
                                    (`Module
                                       (`Module
                                          (`Module
                                             (`Root (Common.root, "Root"),
                                              "CanonicalTest"),
                                           "Base"),
                                        "List"),
                                     "t"))),
                           [Odoc_model.Lang.TypeExpr.Var "a"]),
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved
                              (`Identifier
                                 (`Type
                                    (`Module
                                       (`Module
                                          (`Module
                                             (`Root (Common.root, "Root"),
                                              "CanonicalTest"),
                                           "Base"),
                                        "List"),
                                     "t"))),
                           [Odoc_model.Lang.TypeExpr.Var "a"]))}])})]);
          canonical = None; hidden = false; display_type = None;
          expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
   canonical = None; hidden = false; display_type = None;
   expansion = Some Odoc_model.Lang.Module.AlreadyASig});
 Odoc_model.Lang.Signature.Value
  {Odoc_model.Lang.Value.id = `Value (`Root (Common.root, "Root"), "test");
   doc = [];
   type_ =
    Odoc_model.Lang.TypeExpr.Arrow (None,
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved
         (`Type
            (`Canonical
               (`Alias
                  (`Hidden
                     (`Module
                        (`Identifier
                           (`Module
                              (`Root (Common.root, "Root"), "CanonicalTest")),
                         "Base__List")),
                   `Module
                     (`Module
                        (`Identifier
                           (`Module
                              (`Root (Common.root, "Root"), "CanonicalTest")),
                         "Base__"),
                      "List")),
                `Resolved
                  (`Module
                     (`Module
                        (`Identifier
                           (`Module
                              (`Root (Common.root, "Root"), "CanonicalTest")),
                         "Base"),
                      "List"))),
             "t")),
      [Odoc_model.Lang.TypeExpr.Var "a"]),
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), []))}]
# resolved;;
- : Odoc_odoc.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id = `Root (Common.root, "Root"); doc = [];
 digest = "nodigest"; imports = []; source = None; interface = true;
 hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
     {Odoc_model.Lang.Module.id =
       `Module (`Root (Common.root, "Root"), "CanonicalTest");
      doc = [];
      type_ =
       Odoc_model.Lang.Module.ModuleType
        (Odoc_model.Lang.ModuleType.Signature
          [Odoc_model.Lang.Signature.Module
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.Module.id =
              `Module
                (`Module (`Root (Common.root, "Root"), "CanonicalTest"),
                 "Base__List");
             doc = [];
             type_ =
              Odoc_model.Lang.Module.ModuleType
               (Odoc_model.Lang.ModuleType.Signature
                 [Odoc_model.Lang.Signature.Type
                   (Odoc_model.Lang.Signature.Ordinary,
                   {Odoc_model.Lang.TypeDecl.id =
                     `Type
                       (`Module
                          (`Module
                             (`Root (Common.root, "Root"), "CanonicalTest"),
                           "Base__List"),
                        "t");
                    doc = [];
                    equation =
                     {Odoc_model.Lang.TypeDecl.Equation.params =
                       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                      private_ = false; manifest = None; constraints = []};
                    representation = None});
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`Module
                          (`Module
                             (`Root (Common.root, "Root"), "CanonicalTest"),
                           "Base__List"),
                        "id");
                    doc = [];
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`Module
                                   (`Module
                                      (`Root (Common.root, "Root"),
                                       "CanonicalTest"),
                                    "Base__List"),
                                 "t"))),
                       [Odoc_model.Lang.TypeExpr.Var "a"]),
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`Module
                                   (`Module
                                      (`Root (Common.root, "Root"),
                                       "CanonicalTest"),
                                    "Base__List"),
                                 "t"))),
                       [Odoc_model.Lang.TypeExpr.Var "a"]))}]);
             canonical = None; hidden = true; display_type = None;
             expansion = Some Odoc_model.Lang.Module.AlreadyASig});
           Odoc_model.Lang.Signature.Module
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.Module.id =
              `Module
                (`Module (`Root (Common.root, "Root"), "CanonicalTest"),
                 "Base__");
             doc = [];
             type_ =
              Odoc_model.Lang.Module.ModuleType
               (Odoc_model.Lang.ModuleType.Signature
                 [Odoc_model.Lang.Signature.Module
                   (Odoc_model.Lang.Signature.Ordinary,
                   {Odoc_model.Lang.Module.id =
                     `Module
                       (`Module
                          (`Module
                             (`Root (Common.root, "Root"), "CanonicalTest"),
                           "Base__"),
                        "List");
                    doc =
                     [{Odoc_model.Location_.location =
                        {Odoc_model.Location_.file = "";
                         start =
                          {Odoc_model.Location_.line = 10; column = 10};
                         end_ = {Odoc_model.Location_.line = 10; column = 50}};
                       value =
                        `Tag
                          (`Canonical
                             (`Dot
                                (`Dot
                                   (`Dot (`Root "Root", "CanonicalTest"),
                                    "Base"),
                                 "List"),
                              `Dot
                                (`Dot
                                   (`Dot
                                      (`Root ("Root", `TUnknown),
                                       "CanonicalTest"),
                                    "Base"),
                                 "List")))}];
                    type_ =
                     Odoc_model.Lang.Module.Alias
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (Common.root, "Root"),
                                      "CanonicalTest"),
                                   "Base__List")))));
                    canonical =
                     Some
                      (`Dot
                         (`Dot (`Dot (`Root "Root", "CanonicalTest"), "Base"),
                          "List"),
                       `Dot
                         (`Dot
                            (`Dot
                               (`Root ("Root", `TUnknown), "CanonicalTest"),
                             "Base"),
                          "List"));
                    hidden = false; display_type = None; expansion = None})]);
             canonical = None; hidden = true; display_type = None;
             expansion = Some Odoc_model.Lang.Module.AlreadyASig});
           Odoc_model.Lang.Signature.Module
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.Module.id =
              `Module
                (`Module (`Root (Common.root, "Root"), "CanonicalTest"),
                 "Base");
             doc = [];
             type_ =
              Odoc_model.Lang.Module.ModuleType
               (Odoc_model.Lang.ModuleType.Signature
                 [Odoc_model.Lang.Signature.Module
                   (Odoc_model.Lang.Signature.Ordinary,
                   {Odoc_model.Lang.Module.id =
                     `Module
                       (`Module
                          (`Module
                             (`Root (Common.root, "Root"), "CanonicalTest"),
                           "Base"),
                        "List");
                    doc = [];
                    type_ =
                     Odoc_model.Lang.Module.Alias
                      (`Resolved
                         (`Canonical
                            (`Alias
                               (`Hidden
                                  (`Identifier
                                     (`Module
                                        (`Module
                                           (`Root (Common.root, "Root"),
                                            "CanonicalTest"),
                                         "Base__List"))),
                                `Module
                                  (`Hidden
                                     (`Identifier
                                        (`Module
                                           (`Module
                                              (`Root (Common.root, "Root"),
                                               "CanonicalTest"),
                                            "Base__"))),
                                   "List")),
                             `Dot
                               (`Dot
                                  (`Dot (`Root "Root", "CanonicalTest"),
                                   "Base"),
                                "List"))));
                    canonical = None; hidden = false; display_type = None;
                    expansion = None})]);
             canonical = None; hidden = false; display_type = None;
             expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
      canonical = None; hidden = false; display_type = None;
      expansion = Some Odoc_model.Lang.Module.AlreadyASig});
    Odoc_model.Lang.Signature.Value
     {Odoc_model.Lang.Value.id = `Value (`Root (Common.root, "Root"), "test");
      doc = [];
      type_ =
       Odoc_model.Lang.TypeExpr.Arrow (None,
        Odoc_model.Lang.TypeExpr.Constr
         (`Resolved
            (`Type
               (`Canonical
                  (`Alias
                     (`Hidden
                        (`Module
                           (`Identifier
                              (`Module
                                 (`Root (Common.root, "Root"),
                                  "CanonicalTest")),
                            "Base__List")),
                      `Module
                        (`Module
                           (`Identifier
                              (`Module
                                 (`Root (Common.root, "Root"),
                                  "CanonicalTest")),
                            "Base__"),
                         "List")),
                   `Dot
                     (`Dot (`Dot (`Root "Root", "CanonicalTest"), "Base"),
                      "List")),
                "t")),
         [Odoc_model.Lang.TypeExpr.Var "a"]),
        Odoc_model.Lang.TypeExpr.Constr
         (`Resolved (`Identifier (`CoreType "unit")), []))}];
 expansion = None}
# Tools.lookup_type_from_path env (`Dot
            (`Resolved
               (`Identifier (`Module (`Root (Common.root, "Root"), "N"))),
             "t"));;
Exception:
Odoc_xref2.Env.MyFailure (`Module (`Root (Common.root, "Root"), "N"),
 <abstr>).
```



# Include

```ocaml env=e1
let test_data = {|

module EEE = struct
  type u
end

module FFF = struct
  module Caml = struct
    include EEE
  end
end

module GGG = struct
  include FFF

  module Caml = struct
    include Caml
    type x = u
  end
end

|};;
let cmt = Common.cmt_of_string test_data |> fst;;
let _, _, sg = Odoc_loader__Cmt.read_implementation Common.root "Root" cmt;;
let resolved = Compile.signature Env.empty sg;;
(*let expanded = Link.signature Env.empty resolved;;*)
```

```ocaml env=e1

```
