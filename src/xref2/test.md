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
open Odoc_model.Names;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ModuleTypeName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ExceptionName.fmt;;
#install_printer Odoc_model.Names.FieldName.fmt;;
#install_printer Odoc_model.Names.PageName.fmt;;
#print_length 65536;;
Odoc_xref2.Component.Delayed.eager := true;;
Odoc_xref2.Tools.disable_all_caches ();;
let id = Common.id;;
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
  | `Alias of module_ * module_
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
let s_manifest = type_manifest "s"
```

and using this lens on our original signature we obtain:

```ocaml env=e1
# Common.LangUtils.Lens.get u_manifest sg ;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Identifier
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            x);
        ihash = 622581103; ikey = "t_x.r_Root.p_None"},
       false),
   []))
```

This path clearly already begins with `` `Resolved ``, so we don't expect to change it,
but we _are_ going to check it exists. We convert the path into a `Cpath.t` and call
`Tools.resolve_type`. This function starts approximately:

```
    | `Resolved r as unresolved ->
        of_result ~unresolved (lookup_type env r) >>=
        fun t -> return (r, t)
```

and `lookup_type` starts:

```

    | `Identifier (`Type _ as i) ->
        of_option ~error:(`Lookup_failure i) (Env.lookup_type i env)
        >>= fun t -> Ok (Find.Found (`T t))
```

and so we simply look up the type in the environment, giving a `Component.Type.t` that represents the type.

```ocaml env=e1
# Common.compile_signature sg;;
- : Odoc_model.Lang.Signature.t =
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
           x);
       ihash = 622581103; ikey = "t_x.r_Root.p_None"};
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
                            {Odoc_model__Paths_types.iv = `Page (None, None);
                             ihash = 236059787; ikey = "p_None"},
                           Root);
                       ihash = 818126955; ikey = "r_Root.p_None"},
                      x);
                  ihash = 622581103; ikey = "t_x.r_Root.p_None"}),
           []));
       constraints = []};
     representation = None})];
 compiled = true; doc = []}
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
# Common.LangUtils.Lens.get u_manifest sg ;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Identifier
         ({Odoc_model__Paths_types.iv =
            `Module
              ({Odoc_model__Paths_types.iv =
                 `Root
                   (Some
                     {Odoc_model__Paths_types.iv = `Page (None, None);
                      ihash = 236059787; ikey = "p_None"},
                    Root);
                ihash = 818126955; ikey = "r_Root.p_None"},
               M);
           ihash = 716453475; ikey = "m_M.r_Root.p_None"},
          false),
       "t"),
   []))
```

Here we can see that the path is not completely resolved. The `M` bit is resolved, but the `t`
bit is not. So we have to do a bit more work when we look up the type in
`Tools.resolve_type`.

Let's look in more detail at that process. The first thing that happens is that the path is matched
the [`Dot] is found:

```
   | `Dot (parent, id) ->
        resolve_module ~mark_substituted:true ~add_canonical:true env parent
        >>= fun (p, m) ->
```

This implies that the thing before the dot is a module, so we call 
`Tools.resolve_module`.  This is a resolved identifier so we can simply look this up from
the environment. This gets us back the path and a `Component.Module.t` representing the module M,
which are:

```ocaml env=e1
# let get_ok = function
    | Ok x -> x
    | Error _ -> failwith "Found error";;
val get_ok : ('a, 'b) result -> 'a = <fun>
# let (path, module_) = get_ok @@ Tools.resolve_module ~mark_substituted:true ~add_canonical:true env (`Resolved (`Gpath (`Identifier (Common.root_module "M"))));;
val path : Cpath.Resolved.module_ =
  `Gpath
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
            M);
        ihash = 716453475; ikey = "m_M.r_Root.p_None"})
val module_ : Component.Module.t Component.Delayed.t =
  {Odoc_xref2.Component.Delayed.v =
    Some
     {Odoc_xref2.Component.Module.locs = None; doc = [];
      type_ =
       Odoc_xref2.Component.Module.ModuleType
        (Odoc_xref2.Component.ModuleType.Signature
          {Odoc_xref2.Component.Signature.items =
            [Odoc_xref2.Component.Signature.Type (`LType (t, 0),
              Odoc_model.Lang.Signature.Ordinary,
              {Odoc_xref2.Component.Delayed.v =
                Some
                 {Odoc_xref2.Component.TypeDecl.locs = None; doc = [];
                  canonical = None;
                  equation =
                   {Odoc_xref2.Component.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None};
               get = None})];
           compiled = false; removed = []; doc = []});
      canonical = None; hidden = false};
   get = None}
```

The values returned are the resolved path to the module, and a representation of the module itself. We then turn the module into a signature via `expansion_of_module`, which in this case is quite simple since the module contains an explicit signature:

```ocaml env=e1
# get_ok @@ Tools.expansion_of_module env (Component.Delayed.get module_);;
- : Tools.expansion =
Odoc_xref2.Tools.Signature
 {Odoc_xref2.Component.Signature.items =
   [Odoc_xref2.Component.Signature.Type (`LType (t, 0),
     Odoc_model.Lang.Signature.Ordinary,
     {Odoc_xref2.Component.Delayed.v =
       Some
        {Odoc_xref2.Component.TypeDecl.locs = None; doc = [];
         canonical = None;
         equation =
          {Odoc_xref2.Component.TypeDecl.Equation.params = [];
           private_ = false; manifest = None; constraints = []};
         representation = None};
      get = None})];
  compiled = false; removed = []; doc = []}
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
# let (path, module_) = get_ok @@ Tools.resolve_module ~mark_substituted:true ~add_canonical:true env (`Resolved (`Gpath (`Identifier (Common.root_module "N"))));;
val path : Cpath.Resolved.module_ =
  `Gpath
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
            N);
        ihash = 502470005; ikey = "m_N.r_Root.p_None"})
val module_ : Component.Module.t Component.Delayed.t =
  {Odoc_xref2.Component.Delayed.v =
    Some
     {Odoc_xref2.Component.Module.locs = None; doc = [];
      type_ =
       Odoc_xref2.Component.Module.ModuleType
        (Odoc_xref2.Component.ModuleType.Path
          {Odoc_xref2.Component.ModuleType.p_expansion = None;
           p_path =
            `Identifier
              ({Odoc_model__Paths_types.iv =
                 `ModuleType
                   ({Odoc_model__Paths_types.iv =
                      `Root
                        (Some
                          {Odoc_model__Paths_types.iv = `Page (None, None);
                           ihash = 236059787; ikey = "p_None"},
                         Root);
                     ihash = 818126955; ikey = "r_Root.p_None"},
                    M);
                ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
               false)});
      canonical = None; hidden = false};
   get = None}
```

This time turning the module into a signature demonstrates why the function `expansion_of_module` requires the environment. We need to lookup the module type `M` from the environment to determine the
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
# let m = Env.(lookup_by_id s_module_type) (Odoc_model.Paths.Identifier.Mk.module_type (Common.id, Odoc_model.Names.ModuleTypeName.make_std "M")) env;;
val m : Component.Element.module_type option =
  Some
   (`ModuleType
      ({Odoc_model__Paths_types.iv =
         `ModuleType
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            M);
        ihash = 459143770; ikey = "mt_M.r_Root.p_None"},
       {Odoc_xref2.Component.ModuleType.locs = None; doc = [];
        canonical = None;
        expr =
         Some
          (Odoc_xref2.Component.ModuleType.Signature
            {Odoc_xref2.Component.Signature.items =
              [Odoc_xref2.Component.Signature.ModuleType
                (`LModuleType (N, 1),
                {Odoc_xref2.Component.Delayed.v =
                  Some
                   {Odoc_xref2.Component.ModuleType.locs = None; doc = [];
                    canonical = None;
                    expr =
                     Some
                      (Odoc_xref2.Component.ModuleType.Signature
                        {Odoc_xref2.Component.Signature.items =
                          [Odoc_xref2.Component.Signature.Type
                            (`LType (t, 2),
                            Odoc_model.Lang.Signature.Ordinary,
                            {Odoc_xref2.Component.Delayed.v =
                              Some
                               {Odoc_xref2.Component.TypeDecl.locs = None;
                                doc = []; canonical = None;
                                equation =
                                 {Odoc_xref2.Component.TypeDecl.Equation.params
                                   = [];
                                  private_ = false; manifest = None;
                                  constraints = []};
                                representation = None};
                             get = None})];
                         compiled = false; removed = []; doc = []})};
                 get = None});
               Odoc_xref2.Component.Signature.Module (`LModule (B, 0),
                Odoc_model.Lang.Signature.Ordinary,
                {Odoc_xref2.Component.Delayed.v =
                  Some
                   {Odoc_xref2.Component.Module.locs = None; doc = [];
                    type_ =
                     Odoc_xref2.Component.Module.ModuleType
                      (Odoc_xref2.Component.ModuleType.Path
                        {Odoc_xref2.Component.ModuleType.p_expansion = None;
                         p_path = `Local (`LModuleType (N, 1), false)});
                    canonical = None; hidden = false};
                 get = None})];
             compiled = false; removed = []; doc = []})}))
```

We can see here that module `B` has type `` Path (`Resolved (`Local (`LModuleType (N, 1)))) `` which refers to the module type defined just above it.

To look up we need to have fully qualified paths for all items so this needs some work.
The way this is handled is that when we want to look up an element within a module,
we don't just convert it blindly to a signature. Since we have the fully qualified
path to the module, we prefix all the identifiers bound in that signature
with that path to turn them into global identifiers.

Concretely, we start here wanting to resolve the path for type `u`,
which is `A.B.t`. The compiler has started us off by resolving the
`A`:

```ocaml env=e1
# Common.LangUtils.Lens.get u_manifest sg ;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Dot
         (`Identifier
            ({Odoc_model__Paths_types.iv =
               `Module
                 ({Odoc_model__Paths_types.iv =
                    `Root
                      (Some
                        {Odoc_model__Paths_types.iv = `Page (None, None);
                         ihash = 236059787; ikey = "p_None"},
                       Root);
                   ihash = 818126955; ikey = "r_Root.p_None"},
                  A);
              ihash = 353272258; ikey = "m_A.r_Root.p_None"},
             false),
          "B"),
       "t"),
   []))
```

we look up `A` from the environment:

```ocaml env=e1
# let p = `Gpath (`Identifier (Common.root_module "A")) in
  let m = get_ok @@ Tools.lookup_module ~mark_substituted:true env p in
  let sg = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m) in
  Tools.prefix_signature (`Module p, sg);;
Line 4, characters 38-40:
Error: This expression has type Tools.expansion
       but an expression was expected of type Component.Signature.t
```

So before the prefixing operation we had that the type of the module was

```ocaml skip
   type_ =
      Odoc_xref2.Component.Module.ModuleType
      (Odoc_xref2.Component.ModuleType.Path
         (`Resolved (`Local (`LModuleType (N, 1)))));
```

and afterwards it is

```ocaml skip
   type_ =
   Odoc_xref2.Component.Module.ModuleType
      (Odoc_xref2.Component.ModuleType.Path
      (`Resolved
         (`ModuleType
            (`Module
               (`Identifier (`Module (`Root (Common.root, Root), A))),
               N))));
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
# fst @@ get_ok @@ Tools.resolve_type env ~add_canonical:false
    (`Dot
      (`Dot
         (`Resolved
            (`Gpath (`Identifier (Common.root_module "A"))),
          "B"),
       "t"));;
- : Cpath.Resolved.type_ =
`Type
  (`Module
     (`Module
        (`Module
           (`Gpath
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
                  ihash = 353272258; ikey = "m_A.r_Root.p_None"})),
         B)),
   t)
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
let resolved = Common.compile_signature sg;;
```

Let's look at `t`'s manifest:

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved ;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Module
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
                   ihash = 353272258; ikey = "m_A.r_Root.p_None"},
                M),
             `Dot
               (`Identifier
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
                   false),
                "N")),
          t)),
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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved ;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Alias
            (`Alias
               (`Module
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
                   M),
                `Dot
                  (`Identifier
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
                      false),
                   "N")),
             `Dot
               (`Identifier
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
                   false),
                "O")),
          t)),
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
let resolved = Common.compile_signature sg;;
let env = Env.open_signature sg Env.empty;;
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
  Odoc_model.Lang.Module.ModuleType
   (Odoc_model.Lang.ModuleType.With
     {Odoc_model.Lang.ModuleType.w_substitutions =
       [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Root, "M"),
         Odoc_model.Lang.Module.Alias
          (`Identifier
             ({Odoc_model__Paths_types.iv =
                `Module
                  ({Odoc_model__Paths_types.iv =
                     `Root
                       (Some
                         {Odoc_model__Paths_types.iv = `Page (None, None);
                          ihash = 236059787; ikey = "p_None"},
                        Root);
                    ihash = 818126955; ikey = "r_Root.p_None"},
                   B);
               ihash = 814134997; ikey = "m_B.r_Root.p_None"},
              false),
           None))];
      w_expansion = None;
      w_expr =
       Odoc_model.Lang.ModuleType.U.Path
        (`Identifier
           ({Odoc_model__Paths_types.iv =
              `ModuleType
                ({Odoc_model__Paths_types.iv =
                   `Root
                     (Some
                       {Odoc_model__Paths_types.iv = `Page (None, None);
                        ihash = 236059787; ikey = "p_None"},
                      Root);
                  ihash = 818126955; ikey = "r_Root.p_None"},
                 A);
             ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
            false))});
 canonical = None; hidden = false}
```

Clearly there is no `type t` declared in here. Let's get the representation
of module `C` we see the following:

```ocaml env=e1
# let m = get_ok @@ Tools.lookup_module ~mark_substituted:true env (`Gpath (`Identifier (Common.root_module "C")));;
val m : Component.Module.t Component.Delayed.t =
  {Odoc_xref2.Component.Delayed.v =
    Some
     {Odoc_xref2.Component.Module.locs = None; doc = [];
      type_ =
       Odoc_xref2.Component.Module.ModuleType
        (Odoc_xref2.Component.ModuleType.With
          {Odoc_xref2.Component.ModuleType.w_substitutions =
            [Odoc_xref2.Component.ModuleType.ModuleEq (`Dot (`Root, "M"),
              Odoc_xref2.Component.Module.Alias
               (`Identifier
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
                    ihash = 814134997; ikey = "m_B.r_Root.p_None"},
                   false),
               None))];
           w_expansion = None;
           w_expr =
            Odoc_xref2.Component.ModuleType.U.Path
             (`Identifier
                ({Odoc_model__Paths_types.iv =
                   `ModuleType
                     ({Odoc_model__Paths_types.iv =
                        `Root
                          (Some
                            {Odoc_model__Paths_types.iv = `Page (None, None);
                             ihash = 236059787; ikey = "p_None"},
                           Root);
                       ihash = 818126955; ikey = "r_Root.p_None"},
                      A);
                  ihash = 231492881; ikey = "mt_A.r_Root.p_None"},
                 false))});
      canonical = None; hidden = false};
   get = None}
```

now we can ask for the signature of this module:

```ocaml env=e1
# let sg = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
val sg : Tools.expansion =
  Odoc_xref2.Tools.Signature
   {Odoc_xref2.Component.Signature.items =
     [Odoc_xref2.Component.Signature.Module (`LModule (M, 37),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.Alias
             (`Identifier
                ({Odoc_model__Paths_types.iv =
                   `Module
                     ({Odoc_model__Paths_types.iv =
                        `Root
                          (Some
                            {Odoc_model__Paths_types.iv = `Page (None, None);
                             ihash = 236059787; ikey = "p_None"},
                           Root);
                       ihash = 818126955; ikey = "r_Root.p_None"},
                      B);
                  ihash = 814134997; ikey = "m_B.r_Root.p_None"},
                 false),
             None);
           canonical = None; hidden = false};
        get = None});
      Odoc_xref2.Component.Signature.Module (`LModule (N, 38),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.ModuleType
             (Odoc_xref2.Component.ModuleType.Path
               {Odoc_xref2.Component.ModuleType.p_expansion = None;
                p_path = `Dot (`Local (`LModule (M, 37), false), "S")});
           canonical = None; hidden = false};
        get = None})];
    compiled = false; removed = []; doc = []}
```

and we can see the module `M` is now an alias of the root module `B`. We can now
look up module `N` from within this and find its signature:

```ocaml env=e1
# let m = get_ok @@ Tools.lookup_module ~mark_substituted:true env
      (`Module (`Module (`Gpath (`Identifier (Common.root_module "C"))), ModuleName.make_std "N"));;
val m : Component.Module.t Component.Delayed.t =
  {Odoc_xref2.Component.Delayed.v =
    Some
     {Odoc_xref2.Component.Module.locs = None; doc = [];
      type_ =
       Odoc_xref2.Component.Module.ModuleType
        (Odoc_xref2.Component.ModuleType.Path
          {Odoc_xref2.Component.ModuleType.p_expansion = None;
           p_path =
            `Dot
              (`Module
                 (`Module
                    (`Gpath
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
                               C);
                           ihash = 43786577; ikey = "m_C.r_Root.p_None"})),
                  M),
               "S")});
      canonical = None; hidden = false};
   get = None}
# get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
- : Tools.expansion =
Odoc_xref2.Tools.Signature
 {Odoc_xref2.Component.Signature.items =
   [Odoc_xref2.Component.Signature.Type (`LType (t, 45),
     Odoc_model.Lang.Signature.Ordinary,
     {Odoc_xref2.Component.Delayed.v =
       Some
        {Odoc_xref2.Component.TypeDecl.locs = None; doc = [];
         canonical = None;
         equation =
          {Odoc_xref2.Component.TypeDecl.Equation.params = [];
           private_ = false; manifest = None; constraints = []};
         representation = None};
      get = None})];
  compiled = false; removed = []; doc = []}
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
                    C);
                ihash = 43786577; ikey = "m_C.r_Root.p_None"},
             N),
          t)),
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
let t_manifest = type_manifest "t";;
let s_manifest = type_manifest "s";;
let resolved = Common.compile_signature sg;;
```

The interesting thing here is the difference between `type t` and `type s`. The module `M.O` has
a concrete representation in the file and the expansion of this will contain a declaration of type
`t` - hence the path representing `M.O.t` does not need a `Subst` constructor in it. However, the
path to `M.F(M.T).N.t` can't point directly at a type within a module as there isn't one - in
some sense `F(M.T)` is making a brand new module on the fly without binding it anywhere, and the
type within this is not within the body of the functor itself.

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "t") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Module
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
                    M);
                ihash = 716453475; ikey = "m_M.r_Root.p_None"},
             O),
          t)),
   []))
# Common.LangUtils.Lens.get (type_manifest "s") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Module
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
                   T),
                S),
             `Module
               (`Apply
                  (`Module
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
                      F),
                   `Module
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
                      T)),
                N)),
          t)),
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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Module
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
                   T),
                S),
             `Module
               (`Module
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
                   O),
                N)),
          t)),
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
type t = M.O(M.T).N.t
|}
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Module
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
                   T),
                S),
             `Module
               (`Apply
                  (`Module
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
                      O),
                   `Module
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
                      T)),
                N)),
          t)),
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
                  (`Identifier
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
                      false),
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
                           Bar);
                       ihash = 608577; ikey = "m_Bar.r_Root.p_None"},
                      false)),
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
                        Foo);
                    ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
                   false)),
             `Identifier
               ({Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     FooBarInt);
                 ihash = 706684202; ikey = "m_FooBarInt.r_Root.p_None"},
                false)),
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
                (Common.root_module "App")),
            `Resolved
              (`Identifier
                (Common.root_module "Bar"))),
        `Resolved
          (`Identifier (Common.root_module "Foo"))),
      `Resolved
        (`Identifier
          (Common.root_module "FooBarInt")));;
let cp = Component.Of_Lang.(module_path (empty ()) test_path);;
```

Now let's lookup that module:

```ocaml env=e1
# let (p,m) = get_ok @@ Tools.resolve_module ~mark_substituted:true ~add_canonical:true env cp;;
val p : Cpath.Resolved.module_ =
  `Apply
    (`Apply
       (`Apply
          (`Gpath
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
                     App);
                 ihash = 855073208; ikey = "m_App.r_Root.p_None"}),
           `Gpath
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
                     Bar);
                 ihash = 608577; ikey = "m_Bar.r_Root.p_None"})),
        `Gpath
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
                  Foo);
              ihash = 249248993; ikey = "m_Foo.r_Root.p_None"})),
     `Gpath
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
               FooBarInt);
           ihash = 706684202; ikey = "m_FooBarInt.r_Root.p_None"}))
val m : Component.Module.t Component.Delayed.t =
  {Odoc_xref2.Component.Delayed.v =
    Some
     {Odoc_xref2.Component.Module.locs = None; doc = [];
      type_ =
       Odoc_xref2.Component.Module.ModuleType
        (Odoc_xref2.Component.ModuleType.Path
          {Odoc_xref2.Component.ModuleType.p_expansion = None;
           p_path =
            `Dot
              (`Apply
                 (`Resolved
                    (`Substituted
                       (`Gpath
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
                                  Foo);
                              ihash = 249248993;
                              ikey = "m_Foo.r_Root.p_None"}))),
                  `Resolved
                    (`Substituted
                       (`Gpath
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
                                  Bar);
                              ihash = 608577; ikey = "m_Bar.r_Root.p_None"})))),
               "T")});
      canonical = None; hidden = false};
   get = None}
# let sg' = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
val sg' : Tools.expansion =
  Odoc_xref2.Tools.Signature
   {Odoc_xref2.Component.Signature.items =
     [Odoc_xref2.Component.Signature.Module (`LModule (Foo, 14),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.ModuleType
             (Odoc_xref2.Component.ModuleType.Path
               {Odoc_xref2.Component.ModuleType.p_expansion = None;
                p_path =
                 `Dot
                   (`Resolved
                      (`Substituted
                         (`Gpath
                            (`Identifier
                               {Odoc_model__Paths_types.iv =
                                 `Module
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
                                    Bar);
                                ihash = 608577; ikey = "m_Bar.r_Root.p_None"}))),
                    "T")});
           canonical = None; hidden = false};
        get = None})];
    compiled = false; removed = []; doc = []}
# let sg' = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
val sg' : Tools.expansion =
  Odoc_xref2.Tools.Signature
   {Odoc_xref2.Component.Signature.items =
     [Odoc_xref2.Component.Signature.Module (`LModule (Foo, 17),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.ModuleType
             (Odoc_xref2.Component.ModuleType.Path
               {Odoc_xref2.Component.ModuleType.p_expansion = None;
                p_path =
                 `Dot
                   (`Resolved
                      (`Substituted
                         (`Gpath
                            (`Identifier
                               {Odoc_model__Paths_types.iv =
                                 `Module
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
                                    Bar);
                                ihash = 608577; ikey = "m_Bar.r_Root.p_None"}))),
                    "T")});
           canonical = None; hidden = false};
        get = None})];
    compiled = false; removed = []; doc = []}
# let sg' = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
val sg' : Tools.expansion =
  Odoc_xref2.Tools.Signature
   {Odoc_xref2.Component.Signature.items =
     [Odoc_xref2.Component.Signature.Module (`LModule (Foo, 20),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.ModuleType
             (Odoc_xref2.Component.ModuleType.Path
               {Odoc_xref2.Component.ModuleType.p_expansion = None;
                p_path =
                 `Dot
                   (`Resolved
                      (`Substituted
                         (`Gpath
                            (`Identifier
                               {Odoc_model__Paths_types.iv =
                                 `Module
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
                                    Bar);
                                ihash = 608577; ikey = "m_Bar.r_Root.p_None"}))),
                    "T")});
           canonical = None; hidden = false};
        get = None})];
    compiled = false; removed = []; doc = []}
# let sg' = get_ok @@ Tools.expansion_of_module env (Component.Delayed.get m);;
val sg' : Tools.expansion =
  Odoc_xref2.Tools.Signature
   {Odoc_xref2.Component.Signature.items =
     [Odoc_xref2.Component.Signature.Module (`LModule (Foo, 23),
       Odoc_model.Lang.Signature.Ordinary,
       {Odoc_xref2.Component.Delayed.v =
         Some
          {Odoc_xref2.Component.Module.locs = None; doc = [];
           type_ =
            Odoc_xref2.Component.Module.ModuleType
             (Odoc_xref2.Component.ModuleType.Path
               {Odoc_xref2.Component.ModuleType.p_expansion = None;
                p_path =
                 `Dot
                   (`Resolved
                      (`Substituted
                         (`Gpath
                            (`Identifier
                               {Odoc_model__Paths_types.iv =
                                 `Module
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
                                    Bar);
                                ihash = 608577; ikey = "m_Bar.r_Root.p_None"}))),
                    "T")});
           canonical = None; hidden = false};
        get = None})];
    compiled = false; removed = []; doc = []}
```

```ocaml env=e1
let resolved = Common.compile_signature sg;;
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
                       Bar);
                   ihash = 608577; ikey = "m_Bar.r_Root.p_None"},
                T),
             `Module
               (`Apply
                  (`Apply
                     (`Apply
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
                                App);
                            ihash = 855073208; ikey = "m_App.r_Root.p_None"},
                         `Identifier
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
                                Bar);
                            ihash = 608577; ikey = "m_Bar.r_Root.p_None"}),
                      `Identifier
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
                             Foo);
                         ihash = 249248993; ikey = "m_Foo.r_Root.p_None"}),
                   `Identifier
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
                          FooBarInt);
                      ihash = 706684202; ikey = "m_FooBarInt.r_Root.p_None"}),
                Foo)),
          bar)),
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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get t_manifest resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`ModuleType
               (`Module
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
                   T),
                S),
             `Module
               (`Apply
                  (`Module
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
                      O),
                   `Identifier
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
                      ihash = 716453475; ikey = "m_M.r_Root.p_None"}),
                N)),
          t)),
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
let resolved = Common.compile_signature sg;;
```

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
                          Dep1);
                      ihash = 393430064; ikey = "m_Dep1.r_Root.p_None"},
                   S),
                `Module
                  (`Module
                     (`Apply
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
                                Dep2);
                            ihash = 739333691; ikey = "m_Dep2.r_Root.p_None"},
                         `Identifier
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
                                Dep1);
                            ihash = 393430064; ikey = "m_Dep1.r_Root.p_None"}),
                      A),
                   Y)),
             `Dot
               (`Apply
                  (`Identifier
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
                           Dep2);
                       ihash = 739333691; ikey = "m_Dep2.r_Root.p_None"},
                      false),
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
                           Dep1);
                       ihash = 393430064; ikey = "m_Dep1.r_Root.p_None"},
                      false)),
                "B")),
          c)),
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
let resolved = Common.compile_signature sg;;
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
                       Dep4);
                   ihash = 1019199703; ikey = "m_Dep4.r_Root.p_None"},
                T),
             `Module
               (`Module
                  (`Apply
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
                             Dep5);
                         ihash = 592809356; ikey = "m_Dep5.r_Root.p_None"},
                      `Identifier
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
                             Dep4);
                         ihash = 1019199703; ikey = "m_Dep4.r_Root.p_None"}),
                   Z),
                X)),
          b)),
   []))
# Common.LangUtils.Lens.get (type_manifest "dep3") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
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
                          {Odoc_model__Paths_types.iv = `Page (None, None);
                           ihash = 236059787; ikey = "p_None"},
                         Root);
                     ihash = 818126955; ikey = "r_Root.p_None"},
                    Dep3);
                ihash = 403763666; ikey = "m_Dep3.r_Root.p_None"},
             `Dot
               (`Dot
                  (`Apply
                     (`Identifier
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
                              Dep5);
                          ihash = 592809356; ikey = "m_Dep5.r_Root.p_None"},
                         false),
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
                              Dep4);
                          ihash = 1019199703; ikey = "m_Dep4.r_Root.p_None"},
                         false)),
                   "Z"),
                "Y")),
          a)),
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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep4") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Subst
            (`AliasModuleType
               (`ModuleType
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
                          Dep6);
                      ihash = 489035468; ikey = "m_Dep6.r_Root.p_None"},
                   S),
                `ModuleType
                  (`Subst
                     (`ModuleType
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
                                Dep6);
                            ihash = 489035468; ikey = "m_Dep6.r_Root.p_None"},
                         T),
                      `Module
                        (`Apply
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
                                    ihash = 818126955;
                                    ikey = "r_Root.p_None"},
                                   Dep7);
                               ihash = 108620130;
                               ikey = "m_Dep7.r_Root.p_None"},
                            `Identifier
                              {Odoc_model__Paths_types.iv =
                                `Module
                                  ({Odoc_model__Paths_types.iv =
                                     `Root
                                       (Some
                                         {Odoc_model__Paths_types.iv =
                                           `Page (None, None);
                                          ihash = 236059787; ikey = "p_None"},
                                        Root);
                                    ihash = 818126955;
                                    ikey = "r_Root.p_None"},
                                   Dep6);
                               ihash = 489035468;
                               ikey = "m_Dep6.r_Root.p_None"}),
                         M)),
                   R)),
             `Module
               (`Subst
                  (`ModuleType
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
                             Dep6);
                         ihash = 489035468; ikey = "m_Dep6.r_Root.p_None"},
                      T),
                   `Module
                     (`Apply
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
                                Dep7);
                            ihash = 108620130; ikey = "m_Dep7.r_Root.p_None"},
                         `Identifier
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
                                Dep6);
                            ihash = 489035468; ikey = "m_Dep6.r_Root.p_None"}),
                      M)),
                Y)),
          d)),
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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "dep5") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
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
                       {Odoc_model__Paths_types.iv = `Page (None, None);
                        ihash = 236059787; ikey = "p_None"},
                      Root);
                  ihash = 818126955; ikey = "r_Root.p_None"},
                 Dep13);
             ihash = 726816582; ikey = "m_Dep13.r_Root.p_None"},
          c)),
   []))
```

```ocaml env=e1
let test_data = {|
module With7 : functor
  (X : sig
     module type T
   end)
  -> sig
  module type T = X.T
end

module With9 : sig
  module type S = sig
    type t
  end
end

module With10 : sig
  (** {!With10.T} is a submodule type. *)
  module type T = sig
    module M : sig
      module type S
    end

    module N : M.S
  end
end

module type With11 = With7(With10).T with module M = With9 and type N.t = int
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Common.compile_signature sg;;
let with11 = Common.LangUtils.Lens.Signature.module_type "With11"

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
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (type_manifest "t") resolved;;
- : Odoc_model.Lang.TypeExpr.t option =
Some
 (Odoc_model.Lang.TypeExpr.Constr
   (`Resolved
      (`Type
         (`Hidden
            (`Hidden
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
                       {Hidden__}1);
                   ihash = 91343490; ikey = "m_{Hidden__}1.r_Root.p_None"})),
          t)),
   []))
```


How about references now

```ocaml env=e1
let test_data = {|
type t
(** [t] {!t} *)
|};;
let sg = Common.signature_of_mli_string test_data;;
let resolved = Common.compile_signature sg;;
```

```ocaml env=e1
# Common.LangUtils.Lens.get (Common.LangUtils.Lens.Signature.type_ "t") resolved;;
- : Odoc_model.Lang.TypeDecl.t =
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
 locs = None;
 doc =
  [{Odoc_model__.Location_.location =
     {Odoc_model__.Location_.file = "";
      start = {Odoc_model__.Location_.line = 3; column = 6};
      end_ = {Odoc_model__.Location_.line = 3; column = 14}};
    value =
     `Paragraph
       [{Odoc_model__.Location_.location =
          {Odoc_model__.Location_.file = "";
           start = {Odoc_model__.Location_.line = 3; column = 6};
           end_ = {Odoc_model__.Location_.line = 3; column = 9}};
         value = `Code_span "t"};
        {Odoc_model__.Location_.location =
          {Odoc_model__.Location_.file = "";
           start = {Odoc_model__.Location_.line = 3; column = 9};
           end_ = {Odoc_model__.Location_.line = 3; column = 10}};
         value = `Space};
        {Odoc_model__.Location_.location =
          {Odoc_model__.Location_.file = "";
           start = {Odoc_model__.Location_.line = 3; column = 10};
           end_ = {Odoc_model__.Location_.line = 3; column = 14}};
         value = `Reference (`Root ("t", `TUnknown), [])}]}];
 canonical = None;
 equation =
  {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
   manifest = None; constraints = []};
 representation = None}
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
# Link.signature Env.empty id sg ;;
- : Odoc_model.Lang.Signature.t =
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
          compiled = false; doc = []})};
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
       manifest = None; constraints = []};
     representation = None});
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
           M1);
       ihash = 756272831; ikey = "mt_M1.r_Root.p_None"};
     locs = None; doc = []; canonical = None;
     expr =
      Some
       (Odoc_model.Lang.ModuleType.With
         {Odoc_model.Lang.ModuleType.w_substitutions =
           [Odoc_model.Lang.ModuleType.TypeEq (`Dot (`Root, "t"),
             {Odoc_model.Lang.TypeDecl.Equation.params = [];
              private_ = false;
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
                             u);
                         ihash = 15973539; ikey = "t_u.r_Root.p_None"}),
                  []));
              constraints = []})];
          w_expansion = None;
          w_expr =
           Odoc_model.Lang.ModuleType.U.Path
            (`Resolved
               (`Identifier
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
                   ihash = 459143770; ikey = "mt_M.r_Root.p_None"}))})}];
 compiled = false; doc = []}
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
let resolved = Common.compile_signature sg;;
let expanded = Link.signature Env.empty id resolved;;
let module_M_expansion =
  let open Common.LangUtils.Lens in
  Signature.module_ "M" |-- Module.type_ |-~ Module.decl_moduletype
```

```ocaml env=e1
# Common.LangUtils.Lens.get module_M_expansion expanded ;;
- : Odoc_model.Lang.ModuleType.expr =
Odoc_model.Lang.ModuleType.Path
 {Odoc_model.Lang.ModuleType.p_expansion =
   Some
    (Odoc_model.Lang.ModuleType.Signature
      {Odoc_model.Lang.Signature.items =
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            {Odoc_model__Paths_types.iv =
              `Type
                ({Odoc_model__Paths_types.iv =
                   `Module
                     ({Odoc_model__Paths_types.iv =
                        `Root
                          (Some
                            {Odoc_model__Paths_types.iv = `Page (None, None);
                             ihash = 236059787; ikey = "p_None"},
                           Root);
                       ihash = 818126955; ikey = "r_Root.p_None"},
                      M);
                  ihash = 716453475; ikey = "m_M.r_Root.p_None"},
                 s);
             ihash = 395135148; ikey = "t_s.m_M.r_Root.p_None"};
           locs = None; doc = []; canonical = None;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation =
            Some
             (Odoc_model.Lang.TypeDecl.Representation.Variant
               [{Odoc_model.Lang.TypeDecl.Constructor.id =
                  {Odoc_model__Paths_types.iv =
                    `Constructor
                      ({Odoc_model__Paths_types.iv =
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
                            s);
                        ihash = 395135148; ikey = "t_s.m_M.r_Root.p_None"},
                       <abstr>);
                   ihash = 2570800; ikey = "ctor_C.t_s.m_M.r_Root.p_None"};
                 doc = [];
                 args =
                  Odoc_model.Lang.TypeDecl.Constructor.Tuple
                   [Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Type
                           (`Apply
                              (`Identifier
                                 {Odoc_model__Paths_types.iv =
                                   `Module
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
                                      Foo);
                                  ihash = 249248993;
                                  ikey = "m_Foo.r_Root.p_None"},
                               `Identifier
                                 {Odoc_model__Paths_types.iv =
                                   `Module
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
                                      Bar);
                                  ihash = 608577;
                                  ikey = "m_Bar.r_Root.p_None"}),
                            t)),
                     [])];
                 res = None}])})];
       compiled = true; doc = []});
  p_path =
   `Resolved
     (`ModuleType
        (`Apply
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
                   Foo);
               ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
            `Identifier
              {Odoc_model__Paths_types.iv =
                `Module
                  ({Odoc_model__Paths_types.iv =
                     `Root
                       (Some
                         {Odoc_model__Paths_types.iv = `Page (None, None);
                          ihash = 236059787; ikey = "p_None"},
                        Root);
                    ihash = 818126955; ikey = "r_Root.p_None"},
                   Bar);
               ihash = 608577; ikey = "m_Bar.r_Root.p_None"}),
         S))}
```

# Shadowing

Let's see what happens when we have nested shadowing:

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
let test_data = {|
module Foo : sig
  type t
  val id : t
end

module Foo2 : sig
  include module type of struct include Foo end
  type t
  val id2 : t
end

module Foo3 : sig
  include module type of struct include Foo2 end
  type t
  val id3 : t
end
|};;
let sg = Common.signature_of_mli_string test_data;;
let module_expansion_include_sig name n =
  let open Common.LangUtils.Lens in
  Signature.module_ name |-- Module.type_ |-~ Module.decl_moduletype |-~ ModuleType.expr_signature |-- Signature.includes |-~ nth n |-- Include.expansion_sig
let m_e_i_s_value mod_name n val_name =
  let open Common.LangUtils.Lens in
   module_expansion_include_sig mod_name n |-- Signature.value val_name
```

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
# Common.LangUtils.Lens.get (m_e_i_s_value "Foo3" 0 "id") sg;;
- : Odoc_model.Lang.Value.t =
{Odoc_model.Lang.Value.id =
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Module
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            Foo3);
        ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
       id);
   ihash = 424389437; ikey = "v_id.m_Foo3.r_Root.p_None"};
 locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
 type_ =
  Odoc_model.Lang.TypeExpr.Constr
   (`Dot
      (`Identifier
         ({Odoc_model__Paths_types.iv =
            `Module
              ({Odoc_model__Paths_types.iv =
                 `Root
                   (Some
                     {Odoc_model__Paths_types.iv = `Page (None, None);
                      ihash = 236059787; ikey = "p_None"},
                    Root);
                ihash = 818126955; ikey = "r_Root.p_None"},
               Foo);
           ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
          false),
       "t"),
   [])}
# Common.LangUtils.Lens.get (m_e_i_s_value "Foo3" 0 "id2") sg;;
- : Odoc_model.Lang.Value.t =
{Odoc_model.Lang.Value.id =
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Module
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            Foo3);
        ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
       id2);
   ihash = 412619918; ikey = "v_id2.m_Foo3.r_Root.p_None"};
 locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
 type_ =
  Odoc_model.Lang.TypeExpr.Constr
   (`Identifier
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Module
                ({Odoc_model__Paths_types.iv =
                   `Root
                     (Some
                       {Odoc_model__Paths_types.iv = `Page (None, None);
                        ihash = 236059787; ikey = "p_None"},
                      Root);
                  ihash = 818126955; ikey = "r_Root.p_None"},
                 Foo3);
             ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
            {t}3);
        ihash = 271372153; ikey = "t_{t}3.m_Foo3.r_Root.p_None"},
       false),
   [])}
```


And what happens when we include multiple things defining a `t`?

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
let test_data = {|
module Foo : sig
  type t
  val id : t
end

module Foo2 : sig
  type t
  val id2 : t
end

module Foo3 : sig
  include module type of struct include Foo end
  include module type of struct include Foo2 end
  type t
  val id3 : t
end
|};;
let sg = Common.signature_of_mli_string test_data;;
```

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
# Common.LangUtils.Lens.get (module_expansion_include_sig "Foo3" 0) sg;;
- : Odoc_model.Lang.Signature.t =
{Odoc_model.Lang.Signature.items =
  [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
    {Odoc_model.Lang.TypeDecl.id =
      {Odoc_model__Paths_types.iv =
        `Type
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           {t}4);
       ihash = 671044364; ikey = "t_{t}4.m_Foo3.r_Root.p_None"};
     locs = None; doc = []; canonical = None;
     equation =
      {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
       manifest =
        Some
         (Odoc_model.Lang.TypeExpr.Constr
           (`Dot
              (`Identifier
                 ({Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `Root
                           (Some
                             {Odoc_model__Paths_types.iv = `Page (None, None);
                              ihash = 236059787; ikey = "p_None"},
                            Root);
                        ihash = 818126955; ikey = "r_Root.p_None"},
                       Foo);
                   ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
                  false),
               "t"),
           []));
       constraints = []};
     representation = None});
   Odoc_model.Lang.Signature.Value
    {Odoc_model.Lang.Value.id =
      {Odoc_model__Paths_types.iv =
        `Value
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           id);
       ihash = 424389437; ikey = "v_id.m_Foo3.r_Root.p_None"};
     locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
     type_ =
      Odoc_model.Lang.TypeExpr.Constr
       (`Identifier
          ({Odoc_model__Paths_types.iv =
             `Type
               ({Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     Foo3);
                 ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
                {t}4);
            ihash = 671044364; ikey = "t_{t}4.m_Foo3.r_Root.p_None"},
           false),
       [])}];
 compiled = false; doc = []}
# Common.LangUtils.Lens.get (module_expansion_include_sig "Foo3" 1) sg;;
- : Odoc_model.Lang.Signature.t =
{Odoc_model.Lang.Signature.items =
  [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
    {Odoc_model.Lang.TypeDecl.id =
      {Odoc_model__Paths_types.iv =
        `Type
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           {t}5);
       ihash = 67089224; ikey = "t_{t}5.m_Foo3.r_Root.p_None"};
     locs = None; doc = []; canonical = None;
     equation =
      {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
       manifest =
        Some
         (Odoc_model.Lang.TypeExpr.Constr
           (`Dot
              (`Identifier
                 ({Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `Root
                           (Some
                             {Odoc_model__Paths_types.iv = `Page (None, None);
                              ihash = 236059787; ikey = "p_None"},
                            Root);
                        ihash = 818126955; ikey = "r_Root.p_None"},
                       Foo2);
                   ihash = 926621908; ikey = "m_Foo2.r_Root.p_None"},
                  false),
               "t"),
           []));
       constraints = []};
     representation = None});
   Odoc_model.Lang.Signature.Value
    {Odoc_model.Lang.Value.id =
      {Odoc_model__Paths_types.iv =
        `Value
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           id2);
       ihash = 412619918; ikey = "v_id2.m_Foo3.r_Root.p_None"};
     locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
     type_ =
      Odoc_model.Lang.TypeExpr.Constr
       (`Identifier
          ({Odoc_model__Paths_types.iv =
             `Type
               ({Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     Foo3);
                 ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
                {t}5);
            ihash = 67089224; ikey = "t_{t}5.m_Foo3.r_Root.p_None"},
           false),
       [])}];
 compiled = false; doc = []}
```


And what happens when we override values?

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
let test_data = {|
module Foo : sig
  type t
  val x : int
  val id : t
end

module Foo2 : sig
  type t
  val id2 : t
end

module Foo3 : sig
  include module type of struct include Foo end
  include module type of struct include Foo2 end
  type t
  val x : float
  val id3 : t
end
|};;
let sg = Common.signature_of_mli_string test_data;;
```

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
# Common.LangUtils.Lens.get (module_expansion_include_sig "Foo3" 0) sg;;
- : Odoc_model.Lang.Signature.t =
{Odoc_model.Lang.Signature.items =
  [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
    {Odoc_model.Lang.TypeDecl.id =
      {Odoc_model__Paths_types.iv =
        `Type
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           {t}6);
       ihash = 133032212; ikey = "t_{t}6.m_Foo3.r_Root.p_None"};
     locs = None; doc = []; canonical = None;
     equation =
      {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
       manifest =
        Some
         (Odoc_model.Lang.TypeExpr.Constr
           (`Dot
              (`Identifier
                 ({Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `Root
                           (Some
                             {Odoc_model__Paths_types.iv = `Page (None, None);
                              ihash = 236059787; ikey = "p_None"},
                            Root);
                        ihash = 818126955; ikey = "r_Root.p_None"},
                       Foo);
                   ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
                  false),
               "t"),
           []));
       constraints = []};
     representation = None});
   Odoc_model.Lang.Signature.Value
    {Odoc_model.Lang.Value.id =
      {Odoc_model__Paths_types.iv =
        `Value
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           {x}7);
       ihash = 314949087; ikey = "v_{x}7.m_Foo3.r_Root.p_None"};
     locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
     type_ =
      Odoc_model.Lang.TypeExpr.Constr
       (`Identifier
          ({Odoc_model__Paths_types.iv = `CoreType int; ihash = 432452609;
            ikey = "coret_int"},
           false),
       [])};
   Odoc_model.Lang.Signature.Value
    {Odoc_model.Lang.Value.id =
      {Odoc_model__Paths_types.iv =
        `Value
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           id);
       ihash = 424389437; ikey = "v_id.m_Foo3.r_Root.p_None"};
     locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
     type_ =
      Odoc_model.Lang.TypeExpr.Constr
       (`Identifier
          ({Odoc_model__Paths_types.iv =
             `Type
               ({Odoc_model__Paths_types.iv =
                  `Module
                    ({Odoc_model__Paths_types.iv =
                       `Root
                         (Some
                           {Odoc_model__Paths_types.iv = `Page (None, None);
                            ihash = 236059787; ikey = "p_None"},
                          Root);
                      ihash = 818126955; ikey = "r_Root.p_None"},
                     Foo3);
                 ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
                {t}6);
            ihash = 133032212; ikey = "t_{t}6.m_Foo3.r_Root.p_None"},
           false),
       [])}];
 compiled = false; doc = []}
```


And overriding modules?

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
let test_data = {|
module Foo : sig
   module Bar : sig
     type t
   end

   val id : Bar.t
end

module Foo3 : sig
  include module type of struct include Foo end

  module Bar : sig
    type u
  end

  val id3 : Bar.u
end
|};;
let sg = Common.signature_of_mli_string test_data;;
```

<!-- $MDX version>=4.08,env=e1 -->
```ocaml
# Common.LangUtils.Lens.get (module_expansion_include_sig "Foo3" 0) sg;;
- : Odoc_model.Lang.Signature.t =
{Odoc_model.Lang.Signature.items =
  [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
    {Odoc_model.Lang.Module.id =
      {Odoc_model__Paths_types.iv =
        `Module
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           {Bar}9);
       ihash = 658027043; ikey = "m_{Bar}9.m_Foo3.r_Root.p_None"};
     locs = None; doc = [];
     type_ =
      Odoc_model.Lang.Module.Alias
       (`Dot
          (`Identifier
             ({Odoc_model__Paths_types.iv =
                `Module
                  ({Odoc_model__Paths_types.iv =
                     `Root
                       (Some
                         {Odoc_model__Paths_types.iv = `Page (None, None);
                          ihash = 236059787; ikey = "p_None"},
                        Root);
                    ihash = 818126955; ikey = "r_Root.p_None"},
                   Foo);
               ihash = 249248993; ikey = "m_Foo.r_Root.p_None"},
              false),
           "Bar"),
        None);
     canonical = None; hidden = false});
   Odoc_model.Lang.Signature.Value
    {Odoc_model.Lang.Value.id =
      {Odoc_model__Paths_types.iv =
        `Value
          ({Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                Foo3);
            ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
           id);
       ihash = 424389437; ikey = "v_id.m_Foo3.r_Root.p_None"};
     locs = None; value = Odoc_model.Lang.Value.Abstract; doc = [];
     type_ =
      Odoc_model.Lang.TypeExpr.Constr
       (`Dot
          (`Identifier
             ({Odoc_model__Paths_types.iv =
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
                        Foo3);
                    ihash = 670280318; ikey = "m_Foo3.r_Root.p_None"},
                   {Bar}9);
               ihash = 658027043; ikey = "m_{Bar}9.m_Foo3.r_Root.p_None"},
              true),
           "t"),
       [])}];
 compiled = false; doc = []}
```
