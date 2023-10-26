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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv =
           `Type
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              t);
          ihash = 1016576344; ikey = "t_t.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest = None; constraints = []};
        representation = None});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv =
           `Type
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              u);
          ihash = 15973539; ikey = "t_u.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    {Odoc_model__Paths_types.iv =
                      `Type
                        ({Odoc_model__Paths_types.iv =
                           `Root
                             (Some
                               {Odoc_model__Paths_types.iv =
                                 `Page (None, None);
                                ihash = 236059787; ikey = "p_None"},
                              Root);
                          ihash = 818126955; ikey = "r_Root.p_None"},
                         t);
                     ihash = 1016576344; ikey = "t_t.r_Root.p_None"}),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None; source_info = None;
 shape_info = None}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              M);
          ihash = 716453475; ikey = "m_M.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 716453475; ikey = "m_M.r_Root.p_None"},
                       t);
                   ihash = 746522241; ikey = "t_t.m_M.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv =
           `Type
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              u);
          ihash = 15973539; ikey = "t_u.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Identifier
                       {Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 716453475; ikey = "m_M.r_Root.p_None"},
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = []};
 expansion = None; linked = false; canonical = None; source_info = None;
 shape_info = None}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              M);
          ihash = 459143770; ikey = "mt_M.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       t);
                   ihash = 825731485; ikey = "t_t.mt_M.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              N);
          ihash = 502470005; ikey = "m_N.r_Root.p_None"};
        locs = None; doc = [];
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
                       {Odoc_model__Paths_types.iv =
                         `Type
                           ({Odoc_model__Paths_types.iv =
                              `Module
                                ({Odoc_model__Paths_types.iv =
                                   `Root
                                     (Some
                                       {Odoc_model__Paths_types.iv =
                                         `Page (None, None);
                                        ihash = 236059787; ikey = "p_None"},
                                      Root);
                                  ihash = 818126955; ikey = "r_Root.p_None"},
                                 N);
                             ihash = 502470005; ikey = "m_N.r_Root.p_None"},
                            t);
                        ihash = 598040815; ikey = "t_t.m_N.r_Root.p_None"};
                      locs = None; doc = []; canonical = None;
                      equation =
                       {Odoc_model.Lang.TypeDecl.Equation.params = [];
                        private_ = false; manifest = None; constraints = []};
                      representation = None})];
                  compiled = true; doc = []});
             p_path =
              `Resolved
                (`Identifier
                   {Odoc_model__Paths_types.iv =
                     `ModuleType
                       ({Odoc_model__Paths_types.iv =
                          `Root
                            (Some
                              {Odoc_model__Paths_types.iv =
                                `Page (None, None);
                               ihash = 236059787; ikey = "p_None"},
                             Root);
                         ihash = 818126955; ikey = "r_Root.p_None"},
                        M);
                    ihash = 459143770; ikey = "mt_M.r_Root.p_None"})});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv =
           `Type
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              u);
          ihash = 15973539; ikey = "t_u.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Identifier
                       {Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv = `Root (...);
                             ihash = ...; ikey = ...},
                            ...);
                        ihash = ...; ikey = ...},
                     ...)),
              ...));
          constraints = ...};
        representation = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...; source_info = ...;
 shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              M);
          ihash = 459143770; ikey = "mt_M.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       N);
                   ihash = 998243332; ikey = "m_N.mt_M.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           {Odoc_model__Paths_types.iv =
                             `Type
                               ({Odoc_model__Paths_types.iv =
                                  `Module
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          M);
                                      ihash = 459143770;
                                      ikey = "mt_M.r_Root.p_None"},
                                     N);
                                 ihash = 998243332;
                                 ikey = "m_N.mt_M.r_Root.p_None"},
                                t);
                            ihash = 687003328;
                            ikey = "t_t.m_N.mt_M.r_Root.p_None"};
                          locs = None; doc = []; canonical = None;
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
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              A);
          ihash = 353272258; ikey = "m_A.r_Root.p_None"};
        locs = None; doc = [];
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
                       {Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv =
                              `Module
                                ({Odoc_model__Paths_types.iv =
                                   `Root
                                     (Some
                                       {Odoc_model__Paths_types.iv =
                                         `Page (None, None);
                                        ihash = 236059787; ikey = "p_None"},
                                      Root);
                                  ihash = 818126955; ikey = "r_Root.p_None"},
                                 A);
                             ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                            N);
                        ihash = 456955352; ikey = "m_N.m_A.r_Root.p_None"};
                      locs = None; doc = [];
                      type_ =
                       Odoc_model.Lang.Module.ModuleType
                        (Odoc_model.Lang.ModuleType.Signature
                          {Odoc_model.Lang.Signature.items =
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                {Odoc_model__Paths_types.iv =
                                  `Type
                                    ({Odoc_model__Paths_types.iv =
                                       `Module
                                         ({Odoc_model__Paths_types.iv =
                                            `Module
                                              ({Odoc_model__Paths_types.iv =
                                                 `Root
                                                   (Some
                                                     {Odoc_model__Paths_types.iv
                                                       = `Page (None, None);
                                                      ihash = 236059787;
                                                      ikey = "p_None"},
                                                    Root);
                                                ihash = 818126955;
                                                ikey =
                                                 "r_Root.p"... (* string length 13; truncated *)},
                                               A);
                                           ihash = 353272258; ikey = ...},
                                          ...);
                                      ihash = ...; ikey = ...},
                                     ...);
                                 ihash = ...; ikey = ...};
                               locs = ...; doc = ...; canonical = ...;
                               equation = ...; representation = ...});
                             ...];
                           compiled = ...; doc = ...});
                      canonical = ...; hidden = ...});
                    ...];
                  compiled = ...; doc = ...});
             p_path = ...});
        canonical = ...; hidden = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...; source_info = ...;
 shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              M);
          ihash = 459143770; ikey = "mt_M.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  {Odoc_model__Paths_types.iv =
                    `ModuleType
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       N);
                   ihash = 887387323; ikey = "mt_N.mt_M.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           {Odoc_model__Paths_types.iv =
                             `Type
                               ({Odoc_model__Paths_types.iv =
                                  `ModuleType
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          M);
                                      ihash = 459143770;
                                      ikey = "mt_M.r_Root.p_None"},
                                     N);
                                 ihash = 887387323;
                                 ikey = "mt_N.mt_M.r_Root.p_None"},
                                t);
                            ihash = 652783314;
                            ikey = "t_t.mt_N.mt_M.r_Root.p_None"};
                          locs = None; doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})};
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       B);
                   ihash = 301928208; ikey = "m_B.mt_M.r_Root.p_None"};
                 locs = None; doc = [];
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
                                {Odoc_model__Paths_types.iv =
                                  `Type
                                    ({Odoc_model__Paths_types.iv =
                                       `Module
                                         ({Odoc_model__Paths_types.iv =
                                            `ModuleType
                                              ({Odoc_model__Paths_types.iv =
                                                 `Root
                                                   (Some
                                                     {Odoc_model__Paths_types.iv
                                                       = `Page (None, None);
                                                      ihash = 236059787;
                                                      ikey = "p_None"},
                                                    Root);
                                                ihash = 818126955;
                                                ikey = "r_Root.p_None"},
                                               M);
                                           ihash = 459143770;
                                           ikey = "mt_M.r_Root.p_None"},
                                          B);
                                      ihash = 301928208;
                                      ikey = "m_B.mt_M.r_Root.p_None"},
                                     t);
                                 ihash = 484865120;
                                 ikey = "t_t.m_B.mt_M.r_Root.p_None"};
                               locs = None; doc = []; canonical = None;
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
                            {Odoc_model__Paths_types.iv =
                              `ModuleType
                                ({Odoc_model__Paths_types.iv =
                                   `ModuleType
                                     ({Odoc_model__Paths_types.iv =
                                        `Root (Some ...); ihash = ...;
                                        ikey = ...},
                                       ...);
                                   ihash = ...; ikey = ...},
                                  ...);
                              ihash = ...; ikey = ...})});
                  canonical = ...; hidden = ...});
                ...];
              compiled = ...; doc = ...})};
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              M);
          ihash = 459143770; ikey = "mt_M.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  {Odoc_model__Paths_types.iv =
                    `ModuleType
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       N);
                   ihash = 887387323; ikey = "mt_N.mt_M.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           {Odoc_model__Paths_types.iv =
                             `Type
                               ({Odoc_model__Paths_types.iv =
                                  `ModuleType
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          M);
                                      ihash = 459143770;
                                      ikey = "mt_M.r_Root.p_None"},
                                     N);
                                 ihash = 887387323;
                                 ikey = "mt_N.mt_M.r_Root.p_None"},
                                t);
                            ihash = 652783314;
                            ikey = "t_t.mt_N.mt_M.r_Root.p_None"};
                          locs = None; doc = []; canonical = None;
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params = [];
                            private_ = false; manifest = None;
                            constraints = []};
                          representation = None})];
                      compiled = true; doc = []})};
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            M);
                        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
                       X);
                   ihash = 573009176; ikey = "m_X.mt_M.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.Module
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.Module.id =
                           {Odoc_model__Paths_types.iv =
                             `Module
                               ({Odoc_model__Paths_types.iv =
                                  `Module
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          M);
                                      ihash = 459143770;
                                      ikey = "mt_M.r_Root.p_None"},
                                     X);
                                 ihash = 573009176;
                                 ikey = "m_X.mt_M.r_Root.p_None"},
                                B);
                            ihash = 413241446;
                            ikey = "m_B.m_X.mt_M.r_Root.p_None"};
                          locs = None; doc = [];
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
                                         {Odoc_model__Paths_types.iv =
                                           `Type
                                             ({Odoc_model__Paths_types.iv =
                                                `Module
                                                  ({Odoc_model__Paths_types.iv
                                                     =
                                                     `Module
                                                       ({Odoc_model__Paths_types.iv
                                                          =
                                                          `ModuleType
                                                            ({Odoc_model__Paths_types.iv
                                                               = `Root ...;
                                                               ihash = ...;
                                                               ikey = ...},
                                                              ...);
                                                          ihash = ...;
                                                          ikey = ...},
                                                         ...);
                                                     ihash = ...; ikey = ...},
                                                    ...);
                                                ihash = ...; ikey = ...},
                                               ...);
                                           ihash = ...; ikey = ...};
                                         locs = ...; doc = ...;
                                         canonical = ...; equation = ...;
                                         representation = ...});
                                       ...];
                                     compiled = ...; doc = ...});
                                p_path = ...});
                           canonical = ...; hidden = ...});
                         ...];
                       compiled = ...; doc = ...});
                  canonical = ...; hidden = ...});
                ...];
              compiled = ...; doc = ...})};
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              A);
          ihash = 231492881; ikey = "mt_A.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
                       M);
                   ihash = 564635453; ikey = "m_M.mt_A.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.ModuleType
                         {Odoc_model.Lang.ModuleType.id =
                           {Odoc_model__Paths_types.iv =
                             `ModuleType
                               ({Odoc_model__Paths_types.iv =
                                  `Module
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          A);
                                      ihash = 231492881;
                                      ikey = "mt_A.r_Root.p_None"},
                                     M);
                                 ihash = 564635453;
                                 ikey = "m_M.mt_A.r_Root.p_None"},
                                S);
                            ihash = 3092406;
                            ikey = "mt_S.m_M.mt_A.r_Root.p_None"};
                          locs = None; doc = []; canonical = None;
                          expr = None}];
                      compiled = true; doc = []});
                 canonical = None; hidden = false});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
                       N);
                   ihash = 50158313; ikey = "m_N.mt_A.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion = None;
                      p_path =
                       `Resolved
                         (`OpaqueModuleType
                            (`ModuleType
                               (`Identifier
                                  {Odoc_model__Paths_types.iv =
                                    `Module
                                      ({Odoc_model__Paths_types.iv =
                                         `ModuleType
                                           ({Odoc_model__Paths_types.iv =
                                              `Root
                                                (Some
                                                  {Odoc_model__Paths_types.iv
                                                    = `Page (None, None);
                                                   ihash = 236059787;
                                                   ikey = "p_None"},
                                                 Root);
                                             ihash = 818126955;
                                             ikey = "r_Root.p_None"},
                                            A);
                                        ihash = 231492881;
                                        ikey = "mt_A.r_Root.p_None"},
                                       M);
                                   ihash = 564635453;
                                   ikey = "m_M.mt_A.r_Root.p_None"},
                                S)))});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955;
               ikey = "r_Root.p_No"... (* string length 13; truncated *)},
              B);
          ihash = 814134997;
          ikey = "m_B.r_Ro"... (* string length 17; truncated *)};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType ...]; compiled = ...;
              doc = ...});
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              A);
          ihash = 231492881; ikey = "mt_A.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
                       M);
                   ihash = 564635453; ikey = "m_M.mt_A.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     {Odoc_model.Lang.Signature.items =
                       [Odoc_model.Lang.Signature.ModuleType
                         {Odoc_model.Lang.ModuleType.id =
                           {Odoc_model__Paths_types.iv =
                             `ModuleType
                               ({Odoc_model__Paths_types.iv =
                                  `Module
                                    ({Odoc_model__Paths_types.iv =
                                       `ModuleType
                                         ({Odoc_model__Paths_types.iv =
                                            `Root
                                              (Some
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
                                                 ihash = 236059787;
                                                 ikey = "p_None"},
                                               Root);
                                           ihash = 818126955;
                                           ikey = "r_Root.p_None"},
                                          A);
                                      ihash = 231492881;
                                      ikey = "mt_A.r_Root.p_None"},
                                     M);
                                 ihash = 564635453;
                                 ikey = "m_M.mt_A.r_Root.p_None"},
                                S);
                            ihash = 3092406;
                            ikey = "mt_S.m_M.mt_A.r_Root.p_None"};
                          locs = None; doc = []; canonical = None;
                          expr = None}];
                      compiled = true; doc = []});
                 canonical = None; hidden = false});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
                       N);
                   ihash = 50158313; ikey = "m_N.mt_A.r_Root.p_None"};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion = None;
                      p_path =
                       `Resolved
                         (`OpaqueModuleType
                            (`ModuleType
                               (`Identifier
                                  {Odoc_model__Paths_types.iv =
                                    `Module
                                      ({Odoc_model__Paths_types.iv =
                                         `ModuleType
                                           ({Odoc_model__Paths_types.iv =
                                              `Root
                                                (Some
                                                  {Odoc_model__Paths_types.iv
                                                    = `Page (None, None);
                                                   ihash = 236059787;
                                                   ikey = "p_None"},
                                                 Root);
                                             ihash = 818126955;
                                             ikey = "r_Root.p_None"},
                                            A);
                                        ihash = 231492881;
                                        ikey = "mt_A.r_Root.p_None"},
                                       M);
                                   ihash = 564635453;
                                   ikey = "m_M.mt_A.r_Root.p_None"},
                                S)))});
                 canonical = None; hidden = false})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955;
               ikey = "r_Root.p_No"... (* string length 13; truncated *)},
              B);
          ihash = 814134997;
          ikey = "m_B.r_Ro"... (* string length 17; truncated *)};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType ...]; compiled = ...;
              doc = ...});
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              A);
          ihash = 353272258; ikey = "m_A.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                       t);
                   ihash = 394964294; ikey = "t_t.m_A.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              B);
          ihash = 814134997; ikey = "m_B.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                {Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     A);
                 ihash = 353272258; ikey = "m_A.r_Root.p_None"}),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv =
           `Type
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              t);
          ihash = 1016576344; ikey = "t_t.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        equation =
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Type
                    (`Alias
                       (`Identifier
                          {Odoc_model__Paths_types.iv =
                            `Module
                              ({Odoc_model__Paths_types.iv =
                                 `Root
                                   (Some
                                     {Odoc_model__Paths_types.iv =
                                       `Page (None, None);
                                      ihash = 236059787; ikey = "p_None"},
                                    Root);
                                ihash = 818126955; ikey = "r_Root.p_None"},
                               A);
                           ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                        `Identifier
                          ({Odoc_model__Paths_types.iv =
                             `Module
                               ({Odoc_model__Paths_types.iv =
                                  `Root
                                    (Some
                                      {Odoc_model__Paths_types.iv =
                                        `Page (None, None);
                                       ihash = 236059787; ikey = "p_None"},
                                     Root);
                                 ihash = 818126955;
                                 ikey =
                                  "r_Root.p_"... (* string length 13; truncated *)},
                                B);
                            ihash = 814134997;
                            ikey =
                             "m_B.r_Ro"... (* string length 17; truncated *)},
                           false)),
                     t)),
              []));
          constraints = []};
        representation = None})];
    compiled = true; doc = ...};
 expansion = ...; linked = ...; canonical = ...; source_info = ...;
 shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              A);
          ihash = 353272258; ikey = "m_A.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `Module
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            A);
                        ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                       t);
                   ihash = 394964294; ikey = "t_t.m_A.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []});
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              B);
          ihash = 814134997; ikey = "m_B.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                {Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     A);
                 ihash = 353272258; ikey = "m_A.r_Root.p_None"}),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              C);
          ihash = 43786577; ikey = "m_C.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Alias
                (`Identifier
                   {Odoc_model__Paths_types.iv =
                     `Module
                       ({Odoc_model__Paths_types.iv =
                          `Root
                            (Some
                              {Odoc_model__Paths_types.iv =
                                `Page (None, None);
                               ihash = 236059787; ikey = "p_None"},
                             Root);
                         ihash = 818126955; ikey = "r_Root.p_None"},
                        A);
                    ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                 `Identifier
                   ({Odoc_model__Paths_types.iv =
                      `Module
                        ({Odoc_model__Paths_types.iv =
                           `Root
                             (Some
                               {Odoc_model__Paths_types.iv =
                                 `Page (None, None);
                                ihash = 236059787; ikey = "p_None"},
                              Root);
                          ihash = 818126955; ikey = "r_Root.p_None"},
                         B);
                     ihash = 814134997;
                     ikey =
                      "m_B.r_Root.p_"... (* string length 17; truncated *)},
                    false))),
           None);
        canonical = None; hidden = false});
      Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.TypeDecl.id =
         {Odoc_model__Paths_types.iv = `Type (...); ihash = ...; ikey = ...};
        locs = ...; doc = ...; canonical = ...; equation = ...;
        representation = ...});
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...; source_info = ...;
 shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              S);
          ihash = 527535255; ikey = "mt_S.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            S);
                        ihash = 527535255; ikey = "mt_S.r_Root.p_None"},
                       t);
                   ihash = 130637260; ikey = "t_t.mt_S.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              F);
          ihash = 748202139; ikey = "m_F.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                {Odoc_model__Paths_types.iv =
                  `Parameter
                    ({Odoc_model__Paths_types.iv =
                       `Module
                         ({Odoc_model__Paths_types.iv =
                            `Root
                              (Some
                                {Odoc_model__Paths_types.iv =
                                  `Page (None, None);
                                 ihash = 236059787; ikey = "p_None"},
                               Root);
                           ihash = 818126955; ikey = "r_Root.p_None"},
                          F);
                      ihash = 748202139; ikey = "m_F.r_Root.p_None"},
                     X);
                 ihash = 930266402; ikey = "p_X.m_F.r_Root.p_None"};
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            {Odoc_model__Paths_types.iv =
                              `Type
                                ({Odoc_model__Paths_types.iv =
                                   `Parameter
                                     ({Odoc_model__Paths_types.iv =
                                        `Module
                                          ({Odoc_model__Paths_types.iv =
                                             `Root
                                               (Some
                                                 {Odoc_model__Paths_types.iv
                                                   = `Page (None, None);
                                                  ihash = 236059787;
                                                  ikey = "p_None"},
                                                Root);
                                            ihash = 818126955;
                                            ikey = "r_Root.p_None"},
                                           F);
                                       ihash = 748202139;
                                       ikey = "m_F.r_Root.p_None"},
                                      X);
                                  ihash = 930266402;
                                  ikey = "p_X.m_F.r_Root.p_None"},
                                 t);
                             ihash = 1065278958;
                             ikey = "t_t.p_X.m_F.r_Root.p_None"};
                           locs = None; doc = []; canonical = None;
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params = [];
                             private_ = false; manifest = None;
                             constraints = []};
                           representation = None})];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        {Odoc_model__Paths_types.iv =
                          `ModuleType
                            ({Odoc_model__Paths_types.iv =
                               `Root
                                 (Some
                                   {Odoc_model__Paths_types.iv =
                                     `Page (None, None);
                                    ihash = 236059787; ikey = "p_None"},
                                  Root);
                              ihash = 818126955; ikey = "r_Root.p_None"},
                             S);
                         ihash = 527535255;
                         ikey =
                          "mt_S.r_Root.p"... (* string length 18; truncated *)})}},
            Odoc_model.Lang.ModuleType.Functor
             (Odoc_model.Lang.FunctorParameter.Named
               {Odoc_model.Lang.FunctorParameter.id =
                 {Odoc_model__Paths_types.iv =
                   `Parameter
                     ({Odoc_model__Paths_types.iv = `Result ...; ihash = ...;
                        ikey = ...},
                       ...);
                   ihash = ...; ikey = ...};
                 expr = ...},
               ...)));
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              S);
          ihash = 527535255; ikey = "mt_S.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  {Odoc_model__Paths_types.iv =
                    `Type
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            S);
                        ihash = 527535255; ikey = "mt_S.r_Root.p_None"},
                       t);
                   ihash = 130637260; ikey = "t_t.mt_S.r_Root.p_None"};
                 locs = None; doc = []; canonical = None;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              S1);
          ihash = 289200525; ikey = "mt_S1.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                {Odoc_model__Paths_types.iv =
                  `Parameter
                    ({Odoc_model__Paths_types.iv =
                       `ModuleType
                         ({Odoc_model__Paths_types.iv =
                            `Root
                              (Some
                                {Odoc_model__Paths_types.iv =
                                  `Page (None, None);
                                 ihash = 236059787; ikey = "p_None"},
                               Root);
                           ihash = 818126955; ikey = "r_Root.p_None"},
                          S1);
                      ihash = 289200525; ikey = "mt_S1.r_Root.p_None"},
                     _);
                 ihash = 797224953; ikey = "p__.mt_S1.r_Root.p_None"};
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            {Odoc_model__Paths_types.iv =
                              `Type
                                ({Odoc_model__Paths_types.iv =
                                   `Parameter
                                     ({Odoc_model__Paths_types.iv =
                                        `ModuleType
                                          ({Odoc_model__Paths_types.iv =
                                             `Root
                                               (Some
                                                 {Odoc_model__Paths_types.iv
                                                   = `Page (None, None);
                                                  ihash = 236059787;
                                                  ikey = "p_None"},
                                                Root);
                                            ihash = 818126955;
                                            ikey = "r_Root.p_None"},
                                           S1);
                                       ihash = 289200525;
                                       ikey = "mt_S1.r_Root.p_None"},
                                      _);
                                  ihash = 797224953;
                                  ikey = "p__.mt_S1.r_Root.p_None"},
                                 t);
                             ihash = 993900890;
                             ikey = "t_t.p__.mt_S1.r_Root.p_None"};
                           locs = None; doc = []; canonical = None;
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params = [];
                             private_ = false; manifest = None;
                             constraints = []};
                           representation = None})];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        {Odoc_model__Paths_types.iv =
                          `ModuleType
                            ({Odoc_model__Paths_types.iv =
                               `Root
                                 (Some
                                   {Odoc_model__Paths_types.iv =
                                     `Page (None, None);
                                    ihash = 236059787; ikey = "p_None"},
                                  Root);
                              ihash = 818126955; ikey = "r_Root.p_None"},
                             S);
                         ihash = 527535255;
                         ikey =
                          "mt_S.r_Root.p"... (* string length 18; truncated *)})}},
            Odoc_model.Lang.ModuleType.Path
             {Odoc_model.Lang.ModuleType.p_expansion =
               Some
                (Odoc_model.Lang.ModuleType.Signature
                  {Odoc_model.Lang.Signature.items =
                    [Odoc_model.Lang.Signature.Type
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.TypeDecl.id =
                        {Odoc_model__Paths_types.iv = ...; ihash = ...;
                         ikey = ...};
                       locs = ...; doc = ...; canonical = ...;
                       equation = ...; representation = ...});
                     ...];
                   compiled = ...; doc = ...});
              p_path = ...}))};
      ...];
    compiled = ...; doc = ...};
 expansion = ...; linked = ...; canonical = ...; source_info = ...;
 shape_info = ...}
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
  {Odoc_model__Paths_types.iv =
    `Root
      (Some
        {Odoc_model__Paths_types.iv = `Page (None, None); ihash = 236059787;
         ikey = "p_None"},
       Root);
   ihash = 818126955; ikey = "r_Root.p_None"};
 root = Common.root; digest = "nodigest"; imports = []; source = None;
 interface = true; hidden = false;
 content =
  Odoc_model.Lang.Compilation_unit.Module
   {Odoc_model.Lang.Signature.items =
     [Odoc_model.Lang.Signature.ModuleType
       {Odoc_model.Lang.ModuleType.id =
         {Odoc_model__Paths_types.iv =
           `ModuleType
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              Type);
          ihash = 359972898; ikey = "mt_Type.r_Root.p_None"};
        locs = None; doc = []; canonical = None;
        expr =
         Some
          (Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  {Odoc_model__Paths_types.iv =
                    `ModuleType
                      ({Odoc_model__Paths_types.iv =
                         `ModuleType
                           ({Odoc_model__Paths_types.iv =
                              `Root
                                (Some
                                  {Odoc_model__Paths_types.iv =
                                    `Page (None, None);
                                   ihash = 236059787; ikey = "p_None"},
                                 Root);
                             ihash = 818126955; ikey = "r_Root.p_None"},
                            Type);
                        ihash = 359972898; ikey = "mt_Type.r_Root.p_None"},
                       T);
                   ihash = 1011869183; ikey = "mt_T.mt_Type.r_Root.p_None"};
                 locs = None; doc = []; canonical = None; expr = None}];
             compiled = true; doc = []})};
      Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
       {Odoc_model.Lang.Module.id =
         {Odoc_model__Paths_types.iv =
           `Module
             ({Odoc_model__Paths_types.iv =
                `Root
                  (Some
                    {Odoc_model__Paths_types.iv = `Page (None, None);
                     ihash = 236059787; ikey = "p_None"},
                   Root);
               ihash = 818126955; ikey = "r_Root.p_None"},
              App);
          ihash = 855073208; ikey = "m_App.r_Root.p_None"};
        locs = None; doc = [];
        type_ =
         Odoc_model.Lang.Module.ModuleType
          (Odoc_model.Lang.ModuleType.Functor
            (Odoc_model.Lang.FunctorParameter.Named
              {Odoc_model.Lang.FunctorParameter.id =
                {Odoc_model__Paths_types.iv =
                  `Parameter
                    ({Odoc_model__Paths_types.iv =
                       `Module
                         ({Odoc_model__Paths_types.iv =
                            `Root
                              (Some
                                {Odoc_model__Paths_types.iv =
                                  `Page (None, None);
                                 ihash = 236059787; ikey = "p_None"},
                               Root);
                           ihash = 818126955; ikey = "r_Root.p_None"},
                          App);
                      ihash = 855073208; ikey = "m_App.r_Root.p_None"},
                     T);
                 ihash = 730736887; ikey = "p_T.m_App.r_Root.p_None"};
               expr =
                Odoc_model.Lang.ModuleType.Path
                 {Odoc_model.Lang.ModuleType.p_expansion =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      {Odoc_model.Lang.Signature.items =
                        [Odoc_model.Lang.Signature.ModuleType
                          {Odoc_model.Lang.ModuleType.id =
                            {Odoc_model__Paths_types.iv =
                              `ModuleType
                                ({Odoc_model__Paths_types.iv =
                                   `Parameter
                                     ({Odoc_model__Paths_types.iv =
                                        `Module
                                          ({Odoc_model__Paths_types.iv =
                                             `Root
                                               (Some
                                                 {Odoc_model__Paths_types.iv
                                                   = `Page (None, None);
                                                  ihash = 236059787;
                                                  ikey = "p_None"},
                                                Root);
                                            ihash = 818126955;
                                            ikey = "r_Root.p_None"},
                                           App);
                                       ihash = 855073208;
                                       ikey = "m_App.r_Root.p_None"},
                                      T);
                                  ihash = 730736887;
                                  ikey = "p_T.m_App.r_Root.p_None"},
                                 T);
                             ihash = 167832761;
                             ikey = "mt_T.p_T.m_App.r_Root.p_None"};
                           locs = None; doc = []; canonical = None;
                           expr = None}];
                       compiled = true; doc = []});
                  p_path =
                   `Resolved
                     (`Identifier
                        {Odoc_model__Paths_types.iv =
                          `ModuleType
                            ({Odoc_model__Paths_types.iv =
                               `Root
                                 (Some
                                   {Odoc_model__Paths_types.iv =
                                     `Page (None, None);
                                    ihash = 236059787; ikey = "p_None"},
                                  Root);
                              ihash = 818126955; ikey = "r_Root.p_None"},
                             Type);
                         ihash = 359972898; ikey = "mt_Type.r_Root.p_None"})}},
            Odoc_model.Lang.ModuleType.Functor
             (Odoc_model.Lang.FunctorParameter.Named
               {Odoc_model.Lang.FunctorParameter.id =
                 {Odoc_model__Paths_types.iv =
                   `Parameter
                     ({Odoc_model__Paths_types.iv =
                        `Result
                          {Odoc_model__Paths_types.iv =
                            `Module
                              ({Odoc_model__Paths_types.iv =
                                 `Root (Some ...); ihash = ...; ikey = ...},
                                ...);
                            ihash = ...; ikey = ...};
                        ihash = ...; ikey = ...},
                       ...);
                   ihash = ...; ikey = ...};
                 expr = ...},
               ...)));
         canonical = ...; hidden = ...});
       ...];
     compiled = ...; doc = ...};
  expansion = ...; linked = ...; canonical = ...; source_info = ...;
  shape_info = ...}
```
