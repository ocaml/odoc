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
let test_resolve test_data =
  Tools.reset_caches ();
  Common.resolve_from_string test_data
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
  |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), t);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest = None; constraints = []};
        representation = None});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), u);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (Some (`Page (None, None)), Root), t))),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), M);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Module (`Root (Some (`Page (None, None)), Root), M), t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), u);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Identifier
                       (`Module (`Root (Some (`Page (None, None)), Root), M)),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), M);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), N);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Path
            {Odoc_model.Lang.ModuleType.p_expansion =
              Some
               (Odoc_model.Lang.ModuleType.Signature
                 {Odoc_model.Lang.Signature.items =
                   [Odoc_model.Lang.Signature.Type
                     (Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_model.Lang.TypeDecl.id =
                       `Type
                         (`Module
                            (`Root (Some (`Page (None, None)), Root), N),
                          t);
                      doc = []; canonical = None;
                      equation =
                       {Odoc_model.Lang.TypeDecl.Equation.params = [];
                        private_ = false; manifest = None; constraints = []};
                      representation = None})];
                  compiled = true; doc = []});
             p_path =
              `Resolved
                (`Identifier
                   (`ModuleType (`Root (Some (`Page (None, None)), Root), M)))});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), u);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Identifier
                       (`Module (`Root (Some (`Page (None, None)), Root), N)),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), M);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     N);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`Module
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    M),
                                 N),
                              t);
                          doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), A);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Path
            {Odoc_model.Lang.ModuleType.p_expansion =
              Some
               (Odoc_model.Lang.ModuleType.Signature
                 {Odoc_model.Lang.Signature.items =
                   [Odoc_model.Lang.Signature.Module
                     (Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_model.Lang.Module.id =
                       `Module
                         (`Module
                            (`Root (Some (`Page (None, None)), Root), A),
                          N);
                      doc = [];
                      type_ =
                       Odoc_model.Lang.Module.ModuleType
                        (Odoc_model.Lang.ModuleType.Signature
                          {Odoc_model.Lang.Signature.items =
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                `Type
                                  (`Module
                                     (`Module
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         A),
                                      N),
                                   t);
                               doc = []; canonical = None;
                               equation =
                                {Odoc_model.Lang.TypeDecl.Equation.params =
                                  [];
                                 private_ = false; manifest = None;
                                 constraints = []};
                               representation = None})];
                           compiled = true; doc = []});
                      canonical = None; hidden = false})];
                  compiled = true; doc = []});
             p_path =
              `Resolved
                (`Identifier
                   (`ModuleType (`Root (Some (`Page (None, None)), Root), M)))});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), u);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Module
                       (`Identifier
                          (`Module
                             (`Root (Some (`Page (None, None)), Root), A)),
                        N),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), M);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     N);
                 doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`ModuleType
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    M),
                                 N),
                              t);
                          doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})};
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     B);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion =
                       Some
                        (Odoc_model.Lang.ModuleType.Signature
                          {Odoc_model.Lang.Signature.items =
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                `Type
                                  (`Module
                                     (`ModuleType
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         M),
                                      B),
                                   t);
                               doc = []; canonical = None;
                               equation =
                                {Odoc_model.Lang.TypeDecl.Equation.params =
                                  [];
                                 private_ = false; manifest = None;
                                 constraints = []};
                               representation = None})];
                           compiled = true; doc = []});
                      p_path =
                       `Resolved
                         (`Identifier
                            (`ModuleType
                               (`ModuleType
                                  (`Root (Some (`Page (None, None)), Root),
                                   M),
                                N)))});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), A);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Path
            {Odoc_model.Lang.ModuleType.p_expansion =
              Some
               (Odoc_model.Lang.ModuleType.Signature
                 {Odoc_model.Lang.Signature.items =
                   [Odoc_model.Lang.Signature.ModuleType
                     {Odoc_model.Lang.ModuleType.id =
                       `ModuleType
                         (`Module
                            (`Root (Some (`Page (None, None)), Root), A),
                          N);
                      doc = []; canonical = None;
                      expr =
                       Some
                        (Odoc_model.Lang.ModuleType.Signature
                          {Odoc_model.Lang.Signature.items =
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                `Type
                                  (`ModuleType
                                     (`Module
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         A),
                                      N),
                                   t);
                               doc = []; canonical = None;
                               equation =
                                {Odoc_model.Lang.TypeDecl.Equation.params =
                                  [];
                                 private_ = false; manifest = None;
                                 constraints = []};
                               representation = None})];
                           compiled = ...; doc = ...})};
                    ...];
                  compiled = ...; doc = ...});
             p_path = ...});
        canonical = ...; hidden = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), M);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     N);
                 doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`ModuleType
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    M),
                                 N),
                              t);
                          doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})};
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), M),
                     X);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Module
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.Module.id =
                           `Module
                             (`Module
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    M),
                                 X),
                              B);
                          doc = [];
                          type_ =
                           Odoc_model.Lang.Module.ModuleType
                            (Odoc_model.Lang.ModuleType.Path
                              {Odoc_model.Lang.ModuleType.p_expansion =
                                Some
                                 (Odoc_model.Lang.ModuleType.Signature
                                   {Odoc_model.Lang.Signature.items =
                                     [Odoc_model.Lang.Signature.Type
                                       (Odoc_model.Lang.Signature.Ordinary,
                                       {Odoc_model.Lang.TypeDecl.id =
                                         `Type
                                           (`Module
                                              (`Module
                                                 (`ModuleType
                                                    (`Root
                                                       (Some
                                                         (`Page (None, None)),
                                                        Root),
                                                     M),
                                                  X),
                                               B),
                                            t);
                                        doc = []; canonical = None;
                                        equation =
                                         {Odoc_model.Lang.TypeDecl.Equation.params
                                           = [];
                                          private_ = false; manifest = None;
                                          constraints = []};
                                        representation = None})];
                                    compiled = true; doc = []});
                               p_path =
                                `Resolved
                                  (`Identifier
                                     (`ModuleType
                                        (`ModuleType
                                           (`Root
                                              (Some (`Page (None, None)),
                                               Root),
                                            M),
                                         N)))});
                          canonical = None; hidden = false})];
                      compiled = true; doc = []});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), A);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Path
            {Odoc_model.Lang.ModuleType.p_expansion =
              Some
               (Odoc_model.Lang.ModuleType.Signature
                 {Odoc_model.Lang.Signature.items =
                   [Odoc_model.Lang.Signature.ModuleType
                     {Odoc_model.Lang.ModuleType.id =
                       `ModuleType
                         (`Module
                            (`Root (Some (`Page (None, None)), Root), A),
                          N);
                      doc = ...; canonical = ...; expr = ...};
                    ...];
                  compiled = ...; doc = ...});
             p_path = ...});
        canonical = ...; hidden = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), A);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), A),
                     M);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.ModuleType
                         {Odoc_model.Lang.ModuleType.id =
                           `ModuleType
                             (`Module
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    A),
                                 M),
                              S);
                          doc = []; canonical = None; expr = None}];
                      compiled = true; doc = []});
                 canonical = None; hidden = false});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), A),
                     N);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion = None;
                      p_path =
                       `Resolved
                         (`OpaqueModuleType
                            (`ModuleType
                               (`Identifier
                                  (`Module
                                     (`ModuleType
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         A),
                                      M)),
                                S)))});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), B);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`Module (`Root (Some (`Page (None, None)), Root), B), S);
                 doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`ModuleType
                                (`Module
                                   (`Root (Some (`Page (None, None)), Root),
                                    B),
                                 S),
                              t);
                          doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})}];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), C);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.With
            {Odoc_model.Lang.ModuleType.w_substitutions =
              [Odoc_model.Lang.ModuleType.ModuleEq
                (`Resolved (`Module (`Root (`ModuleType ...), ...)), ...);
                ...];
              w_expansion = ...; w_expr = ...});
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), A);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), A),
                     M);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.ModuleType
                         {Odoc_model.Lang.ModuleType.id =
                           `ModuleType
                             (`Module
                                (`ModuleType
                                   (`Root (Some (`Page (None, None)), Root),
                                    A),
                                 M),
                              S);
                          doc = []; canonical = None; expr = None}];
                      compiled = true; doc = []});
                 canonical = None; hidden = false});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), A),
                     N);
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion = None;
                      p_path =
                       `Resolved
                         (`OpaqueModuleType
                            (`ModuleType
                               (`Identifier
                                  (`Module
                                     (`ModuleType
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         A),
                                      M)),
                                S)))});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), B);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`Module (`Root (Some (`Page (None, None)), Root), B), S);
                 doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`ModuleType
                                (`Module
                                   (`Root (Some (`Page (None, None)), Root),
                                    B),
                                 S),
                              t);
                          doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})}];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), C);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.With
            {Odoc_model.Lang.ModuleType.w_substitutions =
              [Odoc_model.Lang.ModuleType.ModuleSubst
                (`Resolved (`Module (`Root (`ModuleType ...), ...)), ...);
                ...];
              w_expansion = ...; w_expr = ...});
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...}
```

Resolve a module alias:

```ocaml
# test_resolve {|
  module A : sig
      type t
  end
  module B = A
  type t = B.t
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), A);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Module (`Root (Some (`Page (None, None)), Root), A), t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), B);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                (`Module (`Root (Some (`Page (None, None)), Root), A))),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), t);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Alias
                       (`Identifier
                          (`Module
                             (`Root (Some (`Page (None, None)), Root), A)),
                        `Identifier
                          (`Module
                             (`Root (Some (`Page (None, None)), Root), B),
                           false)),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), A);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Module (`Root (Some (`Page (None, None)), Root), A), t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), B);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                (`Module (`Root (Some (`Page (None, None)), Root), A))),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), C);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Alias
                (`Identifier
                   (`Module (`Root (Some (`Page (None, None)), Root), A)),
                 `Identifier
                   (`Module (`Root (Some (`Page (None, None)), Root), B),
                    false))),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         `Type (`Root (Some (`Page (None, None)), Root), t);
        doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Alias
                       (`Alias
                          (`Identifier
                             (`Module
                                (`Root (Some (`Page (None, None)), Root), A)),
                           `Identifier
                             (`Module
                                (`Root (Some (`Page (None, None)), Root), B),
                              false)),
                        `Identifier
                          (`Module
                             (`Root (Some (`Page (None, None)), Root), C),
                           false)),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None}
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
  |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), S);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), S),
                     t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), F);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                `Parameter
                  (`Module (`Root (Some (`Page (None, None)), Root), F), X);
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            `Type
                              (`Parameter
                                 (`Module
                                    (`Root (Some (`Page (None, None)), Root),
                                     F),
                                  X),
                               t);
                           doc = []; canonical = None;
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params = [];
                             private_ = false; manifest = None;
                             constraints = []};
                           representation = None})];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        (`ModuleType
                           (`Root (Some (`Page (None, None)), Root), S)))}},
            Odoc_model.Lang.ModuleType.Functor
             (Odoc_model.Lang.FunctorParameter.Named
               {Odoc_model.Lang.FunctorParameter.id =
                 `Parameter
                   (`Result
                      (`Module (`Root (Some (`Page (None, None)), Root), F)),
                    Y);
                expr =
                 Odoc_model.Lang.ModuleType.Path
                  {Odoc_model.Lang.ModuleType.p_expansion =
                    Some
                     (Odoc_model.Lang.ModuleType.Signature
                       {Odoc_model.Lang.Signature.items =
                         [Odoc_model.Lang.Signature.Type
                           (Odoc_model.Lang.Signature.Ordinary,
                           {Odoc_model.Lang.TypeDecl.id =
                             `Type
                               (`Parameter
                                  (`Result
                                     (`Module
                                        (`Root
                                           (Some (`Page (None, None)), Root),
                                         F)),
                                   Y),
                                t);
                            doc = []; canonical = None;
                            equation =
                             {Odoc_model.Lang.TypeDecl.Equation.params = [];
                              private_ = false; manifest = None;
                              constraints = []};
                            representation = None})];
                        compiled = true; doc = []});
                   p_path =
                    `Resolved
                      (`Identifier
                         (`ModuleType
                            (`Root (Some (`Page (None, None)), Root), S)))}},
             Odoc_model.Lang.ModuleType.Signature
              {Odoc_model.Lang.Signature.items =
                [Odoc_model.Lang.Signature.Type
                  (Odoc_model.Lang.Signature.Ordinary,
                  {Odoc_model.Lang.TypeDecl.id =
                    `Type
                      (`Result
                         (`Result
                            (`Module (`Root (Some (`Page ...), ...), ...))),
                         ...);
                    doc = ...; canonical = ...; equation = ...;
                    representation = ...});
                  ...];
                compiled = ...; doc = ...})));
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), S);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), S),
                     t);
                 doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), S1);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                `Parameter
                  (`ModuleType (`Root (Some (`Page (None, None)), Root), S1),
                   _);
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            `Type
                              (`Parameter
                                 (`ModuleType
                                    (`Root (Some (`Page (None, None)), Root),
                                     S1),
                                  _),
                               t);
                           doc = []; canonical = None;
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params = [];
                             private_ = false; manifest = None;
                             constraints = []};
                           representation = None})];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        (`ModuleType
                           (`Root (Some (`Page (None, None)), Root), S)))}},
            Odoc_model.Lang.ModuleType.Path
             {Odoc_model.Lang.ModuleType.p_expansion =
               Some
                (Odoc_model.Lang.ModuleType.Signature
                  {Odoc_model.Lang.Signature.items =
                    [Odoc_model.Lang.Signature.Type
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.TypeDecl.id =
                        `Type
                          (`Result
                             (`ModuleType
                                (`Root (Some (`Page (None, None)), Root), S1)),
                           t);
                       doc = []; canonical = None;
                       equation =
                        {Odoc_model.Lang.TypeDecl.Equation.params = [];
                         private_ = false; manifest = None; constraints = []};
                       representation = None})];
                   compiled = true; doc = []});
              p_path =
               `Resolved
                 (`Identifier
                    (`ModuleType (`Root (Some (`Page (None, None)), Root), S)))}))};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), F1);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                `Parameter
                  (`Module (`Root (Some (`Page (None, None)), Root), F1),
                   Arg);
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary, ...);
                         ...];
                       compiled = ...; doc = ...});
                  p_path = ...}},
            ...));
        canonical = ...; hidden = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...}
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
    |} ;;
- : Odoc_model.Lang.Compilation_unit.t =
{Odoc_model.Lang.Compilation_unit.id =
  `Root (Some (`Page (None, None)), Root);
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         `ModuleType (`Root (Some (`Page (None, None)), Root), Type);
        doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`ModuleType
                       (`Root (Some (`Page (None, None)), Root), Type),
                     T);
                 doc = []; canonical = None; expr = None}];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         `Module (`Root (Some (`Page (None, None)), Root), App);
        doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                `Parameter
                  (`Module (`Root (Some (`Page (None, None)), Root), App), T);
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.ModuleType
                          {Odoc_model.Lang.ModuleType.id =
                            `ModuleType
                              (`Parameter
                                 (`Module
                                    (`Root (Some (`Page (None, None)), Root),
                                     App),
                                  T),
                               T);
                           doc = []; canonical = None; expr = None}];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        (`ModuleType
                           (`Root (Some (`Page (None, None)), Root), Type)))}},
            Odoc_model.Lang.ModuleType.Functor
             (Odoc_model.Lang.FunctorParameter.Named
               {Odoc_model.Lang.FunctorParameter.id =
                 `Parameter
                   (`Result
                      (`Module (`Root (Some (`Page (None, None)), Root), App)),
                    F);
                expr =
                 Odoc_model.Lang.ModuleType.Functor
                  (Odoc_model.Lang.FunctorParameter.Named
                    {Odoc_model.Lang.FunctorParameter.id =
                      `Parameter
                        (`Parameter
                           (`Result
                              (`Module
                                 (`Root (Some (`Page (None, None)), Root),
                                  App)),
                            F),
                         _);
                     expr =
                      Odoc_model.Lang.ModuleType.Path
                       {Odoc_model.Lang.ModuleType.p_expansion =
                         Some
                          (Odoc_model.Lang.ModuleType.Signature
                            {Odoc_model.Lang.Signature.items =
                              [Odoc_model.Lang.Signature.ModuleType
                                {Odoc_model.Lang.ModuleType.id =
                                  `ModuleType
                                    (`Parameter
                                       (`Parameter
                                          (`Result
                                             (`Module
                                                (`Root
                                                   (Some (`Page (None, None)),
                                                    Root),
                                                 App)),
                                           F),
                                        _),
                                     T);
                                 doc = []; canonical = None; expr = None}];
                             compiled = true; doc = []});
                        p_path =
                         `Resolved
                           (`Identifier
                              (`ModuleType
                                 (`Root (Some (`Page (None, None)), Root),
                                  Type)))}},
                  Odoc_model.Lang.ModuleType.Path
                   {Odoc_model.Lang.ModuleType.p_expansion =
                     Some
                      (Odoc_model.Lang.ModuleType.Signature
                        {Odoc_model.Lang.Signature.items =
                          [Odoc_model.Lang.Signature.ModuleType
                            {Odoc_model.Lang.ModuleType.id = ...; doc = ...;
                             canonical = ...; expr = ...};
                           ...];
                         compiled = ...; doc = ...});
                    p_path = ...})},
             ...)));
        canonical = ...; hidden = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...}
```
