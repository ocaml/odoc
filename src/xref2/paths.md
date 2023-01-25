Paths
=====

There are various type declarations for values that allow us to specify items within an OCaml signature.
These are Identifiers, Paths, Fragments and References.

Preamble for the following examples:

```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ModuleTypeName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.PageName.fmt;;
let id = Common.id;;
let mod_sig =
    let open Common.LangUtils.Lens in
    Module.type_ |-~ Module.decl_moduletype |-~ ModuleType.expr_signature
let functor_sig =
    let open Common.LangUtils.Lens in
    Module.type_ |-~ Module.decl_moduletype |-~ ModuleType.expr_functor |-- snd |-~ ModuleType.expr_signature
let functor_arg_sig =
    let open Common.LangUtils.Lens in
    Module.type_ |-~ Module.decl_moduletype |-~ ModuleType.expr_functor |-- fst |-~ FunctorParameter.named |-- FunctorParameter.expr |-~ ModuleType.expr_signature
```

Identifiers
-----------

Identifiers are the most basic way to describe a specific element in a signature. The type is split into specific
subtypes for specific types of elements, and there is a union type to which all identifiers can be cast. For
example, we have the type `Identifier.type_` for a type declaration. This is:

```ocaml skip
type type_ = [
    | `Type of signature * TypeName.t
    | `CoreType of TypeName.t
]
```

so we either have named type declarations that are associated with a particular signature, or core types that aren't
(e.g. `int`). The `signature` type is anything that can contain elements such as these type declarations:

```ocaml skip
type signature = [
    | `Root of Root.t * UnitName.t
    | `Module of signature * ModuleName.t
    | `Parameter of signature * ModuleName.t
    | `Result of signature
    | `ModuleType of signature * ModuleTypeName.t
]
```

A `Root` refers to a compilation unit (think 'mli' file) - these can clearly contain type declarations. The
`` `Module `` and `` `ModuleType `` constructors are fairly obvious - these also have a parent `signature`
and are named. The `` `Parameter `` constructor refers to the parameter of a functor, and the `` `Result ``
constructor is used to refer to the definitions of the result of the functor. As an example:

```ocaml env=e1
let test_data = {|
module type S = sig
    type t
end
module M : sig
    type m_t = int
end
module F(X : sig type foo end) : sig
    open X
    type f_t = foo
end
|};;
let sg = Common.signature_of_mli_string test_data;;
```

Here we have:

- S: `` `ModuleType (`Root root, "S") ``;

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_type "S" |-- ModuleType.id) sg);;
- : Odoc_model.Paths.Identifier.ModuleType.t =
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
 ihash = 527535255; ikey = "mt_S.r_Root.p_None"}
```

- M: `` `Module (`Root root, "M") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- Module.id) sg);;
- : Odoc_model.Paths.Identifier.Module.t =
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
 ihash = 716453475; ikey = "m_M.r_Root.p_None"}
```

- F: `` `Module (`Root root, "F") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- Module.id) sg);;
- : Odoc_model.Paths.Identifier.Module.t =
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
 ihash = 748202139; ikey = "m_F.r_Root.p_None"}
```

- m_t: `` `Type ( `Module ( `Root root, "M"), "m_t" ) ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- Signature.type_ "m_t" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths.Identifier.Type.t =
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
     m_t);
 ihash = 493774927; ikey = "t_m_t.m_M.r_Root.p_None"}
```

- f_t: `` `Type (`Result (`Module (`Root root, "F")), "f_t") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- functor_sig |-- Signature.type_ "f_t" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths.Identifier.Type.t =
{Odoc_model__Paths_types.iv =
  `Type
    ({Odoc_model__Paths_types.iv =
       `Result
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
      ihash = 709672416; ikey = "___result__.m_F.r_Root.p_None"},
     f_t);
 ihash = 344808614; ikey = "t_f_t.___result__.m_F.r_Root.p_None"}
```

- foo: `` `Type (`Parameter (`Module (`Root root, "F"), "X"), "foo") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- functor_arg_sig |-- Signature.type_ "foo" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths.Identifier.Type.t =
{Odoc_model__Paths_types.iv =
  `Type
    ({Odoc_model__Paths_types.iv =
       `Parameter
         ({Odoc_model__Paths_types.iv =
            `Module
              ({Odoc_model__Paths_types.iv =
                 `Root
                   (Some
                     {Odoc_model__Paths_types.iv = `Page (None, None);
                      ihash = 236059787; ikey = "p_None"},
                    Root);
                ihash = 818126955; ikey = "r_Root.p_None"},
               F);
           ihash = 748202139; ikey = "m_F.r_Root.p_None"},
          X);
      ihash = 930266402; ikey = "p_X.m_F.r_Root.p_None"},
     foo);
 ihash = 212207131; ikey = "t_foo.p_X.m_F.r_Root.p_None"}
```

There are many other types of Identifier: type, constructor, field, etc.

Paths
=====

A Path is a 'user visible' description of an identifier. Whenever a 'dot' is typed this
is usually a path. Initially the compiler will specify exactly only the first element
of the path, and the subsequent components of the path will just be strings. For example,

```ocaml env=e1
let example = {|
module M : sig
    module N : sig
        type t
        type x1 = t
    end
    type x2 = N.t
end
type x3 = M.N.t
|}
let sg = Common.signature_of_mli_string example;;
```

All three types `x1`, `x2` and `x3` are pointing at `M.N.t`. Here we define a lens
to extract the manifest of a type:

```ocaml env=e1
let type_constr_path name =
  let open Common.LangUtils.Lens in
  Signature.type_ name |-- TypeDecl.equation |-- TypeDecl.Equation.manifest |-~ option |-~ TypeExpr.constr |-- fst
```

and now we can get the paths for all three type declarations:

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- Signature.module_ "N" |-- mod_sig |-- type_constr_path "x1") sg);;
- : Odoc_model.Paths.Path.Type.t =
`Identifier
  ({Odoc_model__Paths_types.iv =
     `Type
       ({Odoc_model__Paths_types.iv =
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
                  M);
              ihash = 716453475; ikey = "m_M.r_Root.p_None"},
             N);
         ihash = 1041581453; ikey = "m_N.m_M.r_Root.p_None"},
        t);
    ihash = 311238448; ikey = "t_t.m_N.m_M.r_Root.p_None"},
   false)
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- type_constr_path "x2") sg);;
- : Odoc_model.Paths.Path.Type.t =
`Dot
  (`Identifier
     ({Odoc_model__Paths_types.iv =
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
                M);
            ihash = 716453475; ikey = "m_M.r_Root.p_None"},
           N);
       ihash = 1041581453; ikey = "m_N.m_M.r_Root.p_None"},
      false),
   "t")
# Common.LangUtils.Lens.(get (type_constr_path "x3") sg);;
- : Odoc_model.Paths.Path.Type.t =
`Dot
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
      "N"),
   "t")
```

We can resolve the paths:

```ocaml env=e1
let sg' = Common.compile_signature sg;;
```

and now the paths are:

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- Signature.module_ "N" |-- mod_sig |-- type_constr_path "x1") sg');;
- : Odoc_model.Paths.Path.Type.t =
`Resolved
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
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
                    M);
                ihash = 716453475; ikey = "m_M.r_Root.p_None"},
               N);
           ihash = 1041581453; ikey = "m_N.m_M.r_Root.p_None"},
          t);
      ihash = 311238448; ikey = "t_t.m_N.m_M.r_Root.p_None"})
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- type_constr_path "x2") sg');;
- : Odoc_model.Paths.Path.Type.t =
`Resolved
  (`Type
     (`Identifier
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
                  M);
              ihash = 716453475; ikey = "m_M.r_Root.p_None"},
             N);
         ihash = 1041581453; ikey = "m_N.m_M.r_Root.p_None"},
      t))
# Common.LangUtils.Lens.(get (type_constr_path "x3") sg');;
- : Odoc_model.Paths.Path.Type.t =
`Resolved
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
         N),
      t))
```

So the difference in the paths is essentially at what point we switch to the identifier.

We also encode other information into the path during the resolution process. This takes
the form of additional constructors embedded into the path. These are:

### Subst

The `` `Subst `` construct is declared as:

```ocaml skip
| `Subst of module_type * module_
```

and is used in a quite specific place - when a functor argument contains a module type
declaration, and the functor body contains a module that is contrained by that signature.
For example:

```ocaml env=e1
let example = {|
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
|};;
```

```ocaml env=e1
# let sg = Common.compile_signature (Common.signature_of_mli_string example) ;;
val sg : Odoc_model.Lang.Signature.t =
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
             ARG);
         ihash = 379411454; ikey = "mt_ARG.r_Root.p_None"};
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
                           ARG);
                       ihash = 379411454; ikey = "mt_ARG.r_Root.p_None"},
                      S);
                  ihash = 208722936; ikey = "mt_S.mt_ARG.r_Root.p_None"};
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
                                                {Odoc_model__Paths_types.iv =
                                                  `Page (None, None);
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
                                S);
                            ihash = 313393860;
                            ikey = "mt_S.p_X.m_F.r_Root.p_None"};
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
                            ARG);
                        ihash = 379411454; ikey = "mt_ARG.r_Root.p_None"})}},
           Odoc_model.Lang.ModuleType.Signature
            {Odoc_model.Lang.Signature.items =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  {Odoc_model__Paths_types.iv =
                    `Module
                      ({Odoc_model__Paths_types.iv =
                         `Result
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
                                F);
                            ihash = 748202139;
                            ikey =
                             "m_F.r_Root."... (* string length 17; truncated *)};
                        ihash = 709672416;
                        ikey =
                         "___result"... (* string length 29; truncated *)},
                       N);
                   ihash = 837385364;
                   ikey = "m_N.___r"... (* string length 33; truncated *)};
                 locs = None; doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     {Odoc_model.Lang.ModuleType.p_expansion = ...;
                      p_path = ...});
                 canonical = ...; hidden = ...});
               ...];
             compiled = ...; doc = ...}));
       canonical = ...; hidden = ...});
     ...];
   compiled = ...; doc = ...}
```

The problem here is that odoc will not generate a page for the module `F(M)`.
Normally links into the body of the functor would simply link to the declaration
of the functor itself, but in this case there is no `type N.t` there.
What we have to do instead is link to the signature that contains the
type - in this case `M.S`. Since we necessarily need to have located the definition
of `t` during the resolution process we embed it into the returned resolved
path as this `` `Subst `` constructor:

```ocaml env=e1
# Common.LangUtils.Lens.(get (type_constr_path "t") sg) ;;
- : Odoc_model.Paths.Path.Type.t =
`Resolved
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
                   M);
               ihash = 716453475; ikey = "m_M.r_Root.p_None"},
            S),
         `Module
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
                      F);
                  ihash = 748202139; ikey = "m_F.r_Root.p_None"},
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
                      M);
                  ihash = 716453475; ikey = "m_M.r_Root.p_None"}),
            N)),
      t))
```

This way we can render the path as `F(M).N.t` but actually link to `M.S.t`
in the html.

### SubstAlias


