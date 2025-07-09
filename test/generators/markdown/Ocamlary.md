
# Module `Ocamlary`

This is an *interface* with **all** of the *module system* features. This documentation demonstrates:

- comment formatting
- unassociated comments
- documentation sections
- module system documentation including
  
  1. submodules
  2. module aliases
  3. module types
  4. module type aliases
  5. modules with signatures
  6. modules with aliased signatures
A numbered list:

1. 3
2. 2
3. 1
David Sheets is the author.

author David Sheets
You may find more information about this HTML documentation renderer at [github.com/dsheets/ocamlary](https://github.com/dsheets/ocamlary).

This is some verbatim text:

```
verbatim
```
This is some verbatim text:

```
[][df[]]}}
```
Here is some raw LaTeX: 

Here is an index table of `Empty` modules:

[`Empty`](./Ocamlary-Empty.md) A plain, empty module
[`EmptyAlias`](./Ocamlary-Empty.md) A plain module alias of Empty
Odoc doesn't support `{!indexlist}`.

Here is some superscript: x2

Here is some subscript: x0

Here are some escaped brackets: { \[ @ \] }

Here is some *emphasis* `followed by code`.

An unassociated comment


## Level 1


### Level 2


#### Level 3


##### Level 4


#### Basic module stuff

```
module Empty : sig ... end
```
A plain, empty module

```
module type Empty = sig ... end
```
An ambiguous, misnamed module type

```
module type MissingComment = sig ... end
```
An ambiguous, misnamed module type


## Section 9000

```
module EmptyAlias = Empty
```
A plain module alias of `Empty`


#### EmptySig

```
module type EmptySig = sig ... end
```
A plain, empty module signature

```
module type EmptySigAlias = EmptySig
```
A plain, empty module signature alias of

```
module ModuleWithSignature : EmptySig
```
A plain module of a signature of [`EmptySig`](./Ocamlary-module-type-EmptySig.md) (reference)

```
module ModuleWithSignatureAlias : EmptySigAlias
```
A plain module with an alias signature

```
module One : sig ... end
```
```
module type SigForMod = sig ... end
```
There's a signature in a module in this signature.

```
module type SuperSig = sig ... end
```
For a good time, see [A Labeled Section Header Inside of a Signature](./Ocamlary-module-type-SuperSig-module-type-SubSigA.md#subSig) or [Another Labeled Section Header Inside of a Signature](./Ocamlary-module-type-SuperSig-module-type-SubSigB.md#subSig) or [`SuperSig.EmptySig`](./Ocamlary-module-type-SuperSig-module-type-EmptySig.md). Section [Section 9000](./#s9000) is also interesting. [EmptySig](./#emptySig) is the section and [`EmptySig`](./Ocamlary-module-type-EmptySig.md) is the module signature.

```
module Buffer : sig ... end
```
References are resolved after everything, so `{!Buffer.t}` won't resolve.

Some text before exception title.


#### Basic exception stuff

After exception title.

```
exception Kaboom of unit
```
Unary exception constructor

```
exception Kablam of unit * unit
```
Binary exception constructor

```
exception Kapow of unit * unit
```
Unary exception constructor over binary tuple

```
exception EmptySig
```
[`EmptySig`](./Ocamlary-module-type-EmptySig.md) is a module and [`EmptySig`](./#exception-EmptySig) is this exception.

```
exception EmptySigAlias
```
[`EmptySigAlias`](./#exception-EmptySigAlias) is this exception.


#### Basic type and value stuff with advanced doc comments

```
type ('a, 'b) a_function = 'a -> 'b
```
[`a_function`](./#type-a_function) is this type and [`a_function`](./#val-a_function) is the value below.

```
val a_function : x:int -> int
```
This is `a_function` with param and return type.

parameter x the x coordinate
returns the y coordinate
```
val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function
```
```
val fun_maybe : ?yes:unit -> unit -> int
```
```
val not_found : unit -> unit
```
raises `Not_found` That's all it does
```
val kaboom : unit -> unit
```
raises [`Kaboom`](./#exception-Kaboom) That's all it does
```
val ocaml_org : string
```
see [http://ocaml.org/](http://ocaml.org/) The OCaml Web site
```
val some_file : string
```
see `some_file` The file called some\_file
```
val some_doc : string
```
see some\_doc The document called some\_doc
```
val since_mesozoic : unit
```
This value was introduced in the Mesozoic era.

since mesozoic
```
val changing : unit
```
This value has had changes in 1\.0.0, 1\.1.0, and 1\.2.0.

before 1\.0.0 before 1.0.0
before 1\.1.0 before 1.1.0
version 1\.2.0

#### Some Operators

```
val (~-) : unit
```
```
val (!) : unit
```
```
val (@) : unit
```
```
val ($) : unit
```
```
val (%) : unit
```
```
val (&) : unit
```
```
val (*) : unit
```
```
val (-) : unit
```
```
val (+) : unit
```
```
val (-?) : unit
```
```
val (/) : unit
```
```
val (:=) : unit
```
```
val (=) : unit
```
```
val (land) : unit
```

#### Advanced Module Stuff

```
module CollectionModule : sig ... end
```
This comment is for `CollectionModule`.

```
module type COLLECTION = module type of CollectionModule
```
module type of

```
module Recollection
  (C : COLLECTION) : 
  COLLECTION
    with type collection = C.element list
     and type element = C.collection
```
This comment is for `CollectionModule`.

```
module type MMM = sig ... end
```
```
module type RECOLLECTION = MMM with module C = Recollection(CollectionModule)
```
```
module type RecollectionModule = sig ... end
```
```
module type A = sig ... end
```
```
module type B = sig ... end
```
```
module type C = sig ... end
```
This module type includes two signatures.

```
module FunctorTypeOf
  (Collection : module type of CollectionModule) : 
  sig ... end
```
This comment is for `FunctorTypeOf`.

```
module type IncludeModuleType = sig ... end
```
This comment is for `IncludeModuleType`.

```
module type ToInclude = sig ... end
```
```
module IncludedA : sig ... end
```
```
module type IncludedB = sig ... end
```

#### Advanced Type Stuff

```
type record = {
```
`field1 : int;`
This comment is for `field1`.

`field2 : int;`
This comment is for `field2`.

```
}
```
This comment is for `record`.

This comment is also for `record`.

```
type mutable_record = {
```
`mutable a : int;`
`a` is first and mutable

`b : unit;`
`b` is second and immutable

`mutable c : int;`
`c` is third and mutable

```
}
```
```
type universe_record = {
```
`nihilate : 'a. 'a -> unit;`
```
}
```
```
type variant = 
```
```
| TagA
```
This comment is for `TagA`.

```
| ConstrB of int
```
This comment is for `ConstrB`.

```
| ConstrC of int * int
```
This comment is for binary `ConstrC`.

```
| ConstrD of int * int
```
This comment is for unary `ConstrD` of binary tuple.

```

```
This comment is for `variant`.

This comment is also for `variant`.

```
type poly_variant = [ 
```
```
| `TagA
```
```
| `ConstrB of int
```
```
 ]
```
This comment is for `poly_variant`.

Wow\! It was a polymorphic variant\!

```
type (_, _) full_gadt = 
```
```
| Tag : (unit, unit) full_gadt
```
```
| First : 'a -> ('a, unit) full_gadt
```
```
| Second : 'a -> (unit, 'a) full_gadt
```
```
| Exist : 'a * 'b -> ('b, unit) full_gadt
```
```

```
This comment is for `full_gadt`.

Wow\! It was a GADT\!

```
type 'a partial_gadt = 
```
```
| AscribeTag : 'a partial_gadt
```
```
| OfTag of 'a partial_gadt
```
```
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt
```
```

```
This comment is for `partial_gadt`.

Wow\! It was a mixed GADT\!

```
type alias = variant
```
This comment is for `alias`.

```
type tuple = (alias * alias) * alias * (alias * alias)
```
This comment is for `tuple`.

```
type variant_alias = variant = 
```
```
| TagA
```
```
| ConstrB of int
```
```
| ConstrC of int * int
```
```
| ConstrD of int * int
```
```

```
This comment is for `variant_alias`.

```
type record_alias = record = {
```
`field1 : int;`
`field2 : int;`
```
}
```
This comment is for `record_alias`.

```
type poly_variant_union = [ 
```
```
| poly_variant
```
```
| `TagC
```
```
 ]
```
This comment is for `poly_variant_union`.

```
type 'a poly_poly_variant = [ 
```
```
| `TagA of 'a
```
```
 ]
```
```
type ('a, 'b) bin_poly_poly_variant = [ 
```
```
| `TagA of 'a
```
```
| `ConstrB of 'b
```
```
 ]
```
```
type 'a open_poly_variant = [> `TagA ] as 'a
```
```
type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a
```
```
type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2
```
```
type 'a poly_fun = ([> `ConstrB of int ] as 'a) -> 'a
```
```
type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]
```
```
type 'a closed_poly_variant = [< `One | `Two ] as 'a
```
```
type 'a clopen_poly_variant = [< `One | `Two of int | `Three Two Three ] as 'a
```
```
type nested_poly_variant = [ 
```
```
| `A
```
```
| `B of [ `B1 | `B2 ]
```
```
| `C
```
```
| `D of [ `D1 of [ `D1a ] ]
```
```
 ]
```
```
type ('a, 'b) full_gadt_alias = ('a, 'b) full_gadt = 
```
```
| Tag : (unit, unit) full_gadt_alias
```
```
| First : 'a -> ('a, unit) full_gadt_alias
```
```
| Second : 'a -> (unit, 'a) full_gadt_alias
```
```
| Exist : 'a * 'b -> ('b, unit) full_gadt_alias
```
```

```
This comment is for `full_gadt_alias`.

```
type 'a partial_gadt_alias = 'a partial_gadt = 
```
```
| AscribeTag : 'a partial_gadt_alias
```
```
| OfTag of 'a partial_gadt_alias
```
```
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias
```
```

```
This comment is for `partial_gadt_alias`.

```
exception Exn_arrow : unit -> exn
```
This comment is for [`Exn_arrow`](./#exception-Exn_arrow).

```
type mutual_constr_a = 
```
```
| A
```
```
| B_ish of mutual_constr_b
```
This comment is between [`mutual_constr_a`](./#type-mutual_constr_a) and [`mutual_constr_b`](./#type-mutual_constr_b).

```

```
This comment is for [`mutual_constr_a`](./#type-mutual_constr_a) then [`mutual_constr_b`](./#type-mutual_constr_b).

```
and mutual_constr_b = 
```
```
| B
```
```
| A_ish of mutual_constr_a
```
This comment must be here for the next to associate correctly.

```

```
This comment is for [`mutual_constr_b`](./#type-mutual_constr_b) then [`mutual_constr_a`](./#type-mutual_constr_a).

```
type rec_obj = < f : int ; g : unit -> unit ; h : rec_obj >
```
```
type 'a open_obj = < f : int ; g : unit -> unit.. > as 'a
```
```
type 'a oof = (< a : unit.. > as 'a) -> 'a
```
```
type 'a any_obj = < .. > as 'a
```
```
type empty_obj = <  >
```
```
type one_meth = < meth : unit >
```
```
type ext = ..
```
A mystery wrapped in an ellipsis

```
type ext += 
```
```
| ExtA
```
```

```
```
type ext += 
```
```
| ExtB
```
```

```
```
type ext += 
```
```
| ExtC of unit
```
```
| ExtD of ext
```
```

```
```
type ext += 
```
```
| ExtE
```
```

```
```
type ext += private 
```
```
| ExtF
```
```

```
```
type 'a poly_ext = ..
```
'a poly\_ext

```
type poly_ext += 
```
```
| Foo of 'b
```
```
| Bar of 'b * 'b
```
'b poly\_ext

```

```
```
type poly_ext += 
```
```
| Quux of 'c
```
'c poly\_ext

```

```
```
module ExtMod : sig ... end
```
```
type ExtMod.t += 
```
```
| ZzzTop0
```
It's got the rock

```

```
```
type ExtMod.t += 
```
```
| ZzzTop of unit
```
and it packs a unit.

```

```
```
val launch_missiles : unit -> unit
```
Rotate keys on my mark...

```
type my_mod = (module COLLECTION)
```
A brown paper package tied up with string

```
class empty_class : object ... end
```
```
class one_method_class : object ... end
```
```
class two_method_class : object ... end
```
```
class 'a param_class : 'a -> object ... end
```
```
type my_unit_object = unit param_class
```
```
type 'a my_unit_class = unit param_class as 'a
```
```
module Dep1 : sig ... end
```
```
module Dep2 (Arg : sig ... end) : sig ... end
```
```
type dep1 = Dep2(Dep1).B.c
```
```
module Dep3 : sig ... end
```
```
module Dep4 : sig ... end
```
```
module Dep5 (Arg : sig ... end) : sig ... end
```
```
type dep2 = Dep5(Dep4).Z.X.b
```
```
type dep3 = Dep5(Dep4).Z.Y.a
```
```
module Dep6 : sig ... end
```
```
module Dep7 (Arg : sig ... end) : sig ... end
```
```
type dep4 = Dep7(Dep6).M.Y.d
```
```
module Dep8 : sig ... end
```
```
module Dep9 (X : sig ... end) : sig ... end
```
```
module type Dep10 = Dep9(Dep8).T with type t = int
```
```
module Dep11 : sig ... end
```
```
module Dep12 (Arg : sig ... end) : sig ... end
```
```
module Dep13 : Dep12(Dep11).T
```
```
type dep5 = Dep13.c
```
```
module type With1 = sig ... end
```
```
module With2 : sig ... end
```
```
module With3 : With1 with module M = With2
```
```
type with1 = With3.N.t
```
```
module With4 : With1 with module M := With2
```
```
type with2 = With4.N.t
```
```
module With5 : sig ... end
```
```
module With6 : sig ... end
```
```
module With7 (X : sig ... end) : sig ... end
```
```
module type With8 =
  With7(With6).T with module M = With5 and type M.N.t = With5.N.t
```
```
module With9 : sig ... end
```
```
module With10 : sig ... end
```
```
module type With11 = With7(With10).T with module M = With9 and type N.t = int
```
```
module type NestedInclude1 = sig ... end
```
```
module type NestedInclude2 = sig ... end
```
```
type nested_include = int
```
```
module DoubleInclude1 : sig ... end
```
```
module DoubleInclude3 : sig ... end
```
```
type double_include
```
```
module IncludeInclude1 : sig ... end
```
```
module type IncludeInclude2 = sig ... end
```
```
module IncludeInclude2_M : sig ... end
```
```
type include_include
```

## Trying the {!modules: ...} command.

With ocamldoc, toplevel units will be linked and documented, while submodules will behave as simple references.

With odoc, everything should be resolved (and linked) but only toplevel units will be documented.

[`Dep1.X`](./Ocamlary-Dep1-X.md) 
[`Ocamlary.IncludeInclude1`](./Ocamlary-IncludeInclude1.md) 
[`Ocamlary`](#) This is an interface with all of the module system features. This documentation demonstrates:

#### Weirder usages involving module types

[`IncludeInclude1.IncludeInclude2_M`](./Ocamlary-IncludeInclude1-IncludeInclude2_M.md) 
[`Dep4.X`](./Ocamlary-Dep4-X.md) 

## Playing with @canonical paths

```
module CanonicalTest : sig ... end
```
Some ref to [`CanonicalTest.Base_Tests.C.t`](./Ocamlary-CanonicalTest-Base_Tests-C.md#type-t) and [`CanonicalTest.Base_Tests.L.id`](./Ocamlary-CanonicalTest-Base-List.md#val-id). But also to [`CanonicalTest.Base.List`](./Ocamlary-CanonicalTest-Base-List.md) and [`CanonicalTest.Base.List.t`](./Ocamlary-CanonicalTest-Base-List.md#type-t)


## Aliases again

```
module Aliases : sig ... end
```
Let's imitate jst's layout.


## Section title splicing

I can refer to

- `{!section:indexmodules}` : [Trying the {!modules: ...} command.](./#indexmodules)
- `{!aliases}` : [Aliases again](./#aliases)
But also to things in submodules:

- `{!section:SuperSig.SubSigA.subSig}` : [A Labeled Section Header Inside of a Signature](./Ocamlary-module-type-SuperSig-module-type-SubSigA.md#subSig)
- `{!Aliases.incl}` : [include of Foo](./Ocamlary-Aliases.md#incl)
And just to make sure we do not mess up:

- `{{!section:indexmodules}A}` : [A](./#indexmodules)
- `{{!aliases}B}` : [B](./#aliases)
- `{{!section:SuperSig.SubSigA.subSig}C}` : [C](./Ocamlary-module-type-SuperSig-module-type-SubSigA.md#subSig)
- `{{!Aliases.incl}D}` : [D](./Ocamlary-Aliases.md#incl)

## New reference syntax

```
module type M = sig ... end
```
```
module M : sig ... end
```
Here goes:

- `{!module-M.t}` : [`M.t`](./Ocamlary-M.md#type-t)
- `{!module-type-M.t}` : [`M.t`](./Ocamlary-module-type-M.md#type-t)
```
module Only_a_module : sig ... end
```
- `{!Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!module-Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!module-Only_a_module.type-t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!type:Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
```
module type TypeExt = sig ... end
```
```
type new_t = ..
```
```
type new_t += 
```
```
| C
```
```

```
```
module type TypeExtPruned = TypeExt with type t := new_t
```
```
module Op : sig ... end
```