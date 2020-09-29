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
#install_printer Odoc_model.Names.ParameterName.fmt;;
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
    | `Parameter of signature * ParameterName.t
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
- : Odoc_model.Paths_types.Identifier.reference_module_type =
`ModuleType (`Root (`RootPage None, Root), S)
```

- M: `` `Module (`Root root, "M") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- Module.id) sg);;
- : Odoc_model.Paths_types.Identifier.module_ =
`Module (`Root (`RootPage None, Root), M)
```

- F: `` `Module (`Root root, "F") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- Module.id) sg);;
- : Odoc_model.Paths_types.Identifier.module_ =
`Module (`Root (`RootPage None, Root), F)
```

- m_t: `` `Type ( `Module ( `Root root, "M"), "m_t" ) ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- Signature.type_ "m_t" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths_types.Identifier.type_ =
`Type (`Module (`Root (`RootPage None, Root), M), m_t)
```

- f_t: `` `Type (`Result (`Module (`Root root, "F")), "f_t") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- functor_sig |-- Signature.type_ "f_t" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths_types.Identifier.type_ =
`Type (`Result (`Module (`Root (`RootPage None, Root), F)), f_t)
```

- foo: `` `Type (`Parameter (`Module (`Root root, "F"), "X"), "foo") ``

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "F" |-- functor_arg_sig |-- Signature.type_ "foo" |-- TypeDecl.id) sg);;
- : Odoc_model.Paths_types.Identifier.type_ =
`Type (`Parameter (`Module (`Root (`RootPage None, Root), F), X), foo)
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
- : Odoc_model.Paths_types.Path.type_ =
`Identifier
  (`Type (`Module (`Module (`Root (`RootPage None, Root), M), N), t), false)
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- type_constr_path "x2") sg);;
- : Odoc_model.Paths_types.Path.type_ =
`Dot
  (`Identifier
     (`Module (`Module (`Root (`RootPage None, Root), M), N), false),
   "t")
# Common.LangUtils.Lens.(get (type_constr_path "x3") sg);;
- : Odoc_model.Paths_types.Path.type_ =
`Dot
  (`Dot (`Identifier (`Module (`Root (`RootPage None, Root), M), false), "N"),
   "t")
```

We can resolve the paths:

```ocaml env=e1
let sg' = Compile.signature Env.empty id sg;;
```

and now the paths are:

```ocaml env=e1
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- Signature.module_ "N" |-- mod_sig |-- type_constr_path "x1") sg');;
- : Odoc_model.Paths_types.Path.type_ =
`Resolved
  (`Identifier
     (`Type (`Module (`Module (`Root (`RootPage None, Root), M), N), t)))
# Common.LangUtils.Lens.(get (Signature.module_ "M" |-- mod_sig |-- type_constr_path "x2") sg');;
- : Odoc_model.Paths_types.Path.type_ =
`Resolved
  (`Type
     (`Identifier (`Module (`Module (`Root (`RootPage None, Root), M), N)),
      t))
# Common.LangUtils.Lens.(get (type_constr_path "x3") sg');;
- : Odoc_model.Paths_types.Path.type_ =
`Resolved
  (`Type
     (`Module (`Identifier (`Module (`Root (`RootPage None, Root), M)), N),
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
let sg = Compile.signature Env.empty id (Common.signature_of_mli_string example);;
```

The problem here is that odoc will not generate a page for the module `F(M)`.
Normally links into the body of the functor would simply link to the declaration
of the functor itself, but in this case there is no `type N.t` there.
What we have to do instead is link to the signature that contains the
type - in this case `M.S`. Since we necessarily need to have located the definition
of `t` during the resolution process we embed it into the returned resolved
path as this `` `Subst `` constructor:

```ocaml env=e1
# Common.LangUtils.Lens.(get (type_constr_path "t") sg)
- : Odoc_model.Paths_types.Path.type_ =
`Resolved
  (`Type
     (`Subst
        (`ModuleType
           (`Identifier (`Module (`Root (`RootPage None, Root), M)), S),
         `Module
           (`Apply
              (`Identifier (`Module (`Root (`RootPage None, Root), F)),
               `Identifier (`Module (`Root (`RootPage None, Root), M))),
            N)),
      t))
```

This way we can render the path as `F(M).N.t` but actually link to `M.S.t`
in the html.

### SubstAlias


