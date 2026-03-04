
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

```ocaml
module Empty : sig ... end
```
A plain, empty module

```ocaml
module type Empty = sig ... end
```
An ambiguous, misnamed module type

```ocaml
module type MissingComment = sig ... end
```
An ambiguous, misnamed module type


## Section 9000

```ocaml
module EmptyAlias = Empty
```
A plain module alias of `Empty`


#### EmptySig

```ocaml
module type EmptySig = sig ... end
```
A plain, empty module signature

```ocaml
module type EmptySigAlias = EmptySig
```
A plain, empty module signature alias of

```ocaml
module ModuleWithSignature : EmptySig
```
A plain module of a signature of [`EmptySig`](./Ocamlary-module-type-EmptySig.md) (reference)

```ocaml
module ModuleWithSignatureAlias : EmptySigAlias
```
A plain module with an alias signature

```ocaml
module One : sig ... end
```
```ocaml
module type SigForMod = sig ... end
```
There's a signature in a module in this signature.

```ocaml
module type SuperSig = sig ... end
```
For a good time, see [A Labeled Section Header Inside of a Signature](./Ocamlary-module-type-SuperSig-module-type-SubSigA.md#subSig) or [Another Labeled Section Header Inside of a Signature](./Ocamlary-module-type-SuperSig-module-type-SubSigB.md#subSig) or [`SuperSig.EmptySig`](./Ocamlary-module-type-SuperSig-module-type-EmptySig.md). Section [Section 9000](./#s9000) is also interesting. [EmptySig](./#emptySig) is the section and [`EmptySig`](./Ocamlary-module-type-EmptySig.md) is the module signature.

```ocaml
module Buffer : sig ... end
```
References are resolved after everything, so `{!Buffer.t}` won't resolve.

Some text before exception title.


#### Basic exception stuff

After exception title.

```ocaml
exception Kaboom of unit
```
Unary exception constructor

```ocaml
exception Kablam of unit * unit
```
Binary exception constructor

```ocaml
exception Kapow of unit * unit
```
Unary exception constructor over binary tuple

```ocaml
exception EmptySig
```
[`EmptySig`](./Ocamlary-module-type-EmptySig.md) is a module and [`EmptySig`](./#exception-EmptySig) is this exception.

```ocaml
exception EmptySigAlias
```
[`EmptySigAlias`](./#exception-EmptySigAlias) is this exception.


#### Basic type and value stuff with advanced doc comments

```ocaml
type ('a, 'b) a_function = 'a -> 'b
```
[`a_function`](./#type-a_function) is this type and [`a_function`](./#val-a_function) is the value below.

```ocaml
val a_function : x:int -> int
```
This is `a_function` with param and return type.

parameter x the x coordinate
returns the y coordinate
```ocaml
val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function
```
```ocaml
val fun_maybe : ?yes:unit -> unit -> int
```
```ocaml
val not_found : unit -> unit
```
raises `Not_found` That's all it does
```ocaml
val kaboom : unit -> unit
```
raises [`Kaboom`](./#exception-Kaboom) That's all it does
```ocaml
val ocaml_org : string
```
see [http://ocaml.org/](http://ocaml.org/) The OCaml Web site
```ocaml
val some_file : string
```
see `some_file` The file called some\_file
```ocaml
val some_doc : string
```
see some\_doc The document called some\_doc
```ocaml
val since_mesozoic : unit
```
This value was introduced in the Mesozoic era.

since mesozoic
```ocaml
val changing : unit
```
This value has had changes in 1\.0.0, 1\.1.0, and 1\.2.0.

before 1\.0.0 before 1.0.0
before 1\.1.0 before 1.1.0
version 1\.2.0

#### Some Operators

```ocaml
val (~-) : unit
```
```ocaml
val (!) : unit
```
```ocaml
val (@) : unit
```
```ocaml
val ($) : unit
```
```ocaml
val (%) : unit
```
```ocaml
val (&) : unit
```
```ocaml
val (*) : unit
```
```ocaml
val (-) : unit
```
```ocaml
val (+) : unit
```
```ocaml
val (-?) : unit
```
```ocaml
val (/) : unit
```
```ocaml
val (:=) : unit
```
```ocaml
val (=) : unit
```
```ocaml
val (land) : unit
```

#### Advanced Module Stuff

```ocaml
module CollectionModule : sig ... end
```
This comment is for `CollectionModule`.

```ocaml
module type COLLECTION = module type of CollectionModule
```
module type of

```ocaml
module Recollection
  (C : COLLECTION) : 
  COLLECTION
    with type collection = C.element list
     and type element = C.collection
```
This comment is for `CollectionModule`.

```ocaml
module type MMM = sig ... end
```
```ocaml
module type RECOLLECTION = MMM with module C = Recollection(CollectionModule)
```
```ocaml
module type RecollectionModule = sig ... end
```
```ocaml
module type A = sig ... end
```
```ocaml
module type B = sig ... end
```
```ocaml
module type C = sig ... end
```
This module type includes two signatures.

```ocaml
module FunctorTypeOf
  (Collection : module type of CollectionModule) : 
  sig ... end
```
This comment is for `FunctorTypeOf`.

```ocaml
module type IncludeModuleType = sig ... end
```
This comment is for `IncludeModuleType`.

```ocaml
module type ToInclude = sig ... end
```
```ocaml
module IncludedA : sig ... end
```
```ocaml
module type IncludedB = sig ... end
```

#### Advanced Type Stuff

```ocaml
type record = {
  field1 : int; (* This comment is for field1. *)
  field2 : int; (* This comment is for field2. *)
}
```
This comment is for `record`.

This comment is also for `record`.

```ocaml
type mutable_record = {
  mutable a : int; (* a is first and mutable *)
  b : unit; (* b is second and immutable *)
  mutable c : int; (* c is third and mutable *)
}
```
```ocaml
type universe_record = {
  nihilate : 'a. 'a -> unit;
}
```
```ocaml
type variant = 
  | TagA (* This comment is for TagA. *)
  | ConstrB of int (* This comment is for ConstrB. *)
  | ConstrC of int * int (* This comment is for binary ConstrC. *)
  | ConstrD of int * int (* This comment is for unary ConstrD of binary tuple. *)
```
This comment is for `variant`.

This comment is also for `variant`.

```ocaml
type poly_variant = [ 
  | `TagA
  | `ConstrB of int
 ]
```
This comment is for `poly_variant`.

Wow\! It was a polymorphic variant\!

```ocaml
type (_, _) full_gadt = 
  | Tag : (unit, unit) full_gadt
  | First : 'a -> ('a, unit) full_gadt
  | Second : 'a -> (unit, 'a) full_gadt
  | Exist : 'a * 'b -> ('b, unit) full_gadt
```
This comment is for `full_gadt`.

Wow\! It was a GADT\!

```ocaml
type 'a partial_gadt = 
  | AscribeTag : 'a partial_gadt
  | OfTag of 'a partial_gadt
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt
```
This comment is for `partial_gadt`.

Wow\! It was a mixed GADT\!

```ocaml
type alias = variant
```
This comment is for `alias`.

```ocaml
type tuple = (alias * alias) * alias * (alias * alias)
```
This comment is for `tuple`.

```ocaml
type variant_alias = variant = 
  | TagA
  | ConstrB of int
  | ConstrC of int * int
  | ConstrD of int * int
```
This comment is for `variant_alias`.

```ocaml
type record_alias = record = {
  field1 : int;
  field2 : int;
}
```
This comment is for `record_alias`.

```ocaml
type poly_variant_union = [ 
  | poly_variant
  | `TagC
 ]
```
This comment is for `poly_variant_union`.

```ocaml
type 'a poly_poly_variant = [ 
  | `TagA of 'a
 ]
```
```ocaml
type ('a, 'b) bin_poly_poly_variant = [ 
  | `TagA of 'a
  | `ConstrB of 'b
 ]
```
```ocaml
type 'a open_poly_variant = [> `TagA ] as 'a
```
```ocaml
type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a
```
```ocaml
type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2
```
```ocaml
type 'a poly_fun = ([> `ConstrB of int ] as 'a) -> 'a
```
```ocaml
type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]
```
```ocaml
type 'a closed_poly_variant = [< `One | `Two ] as 'a
```
```ocaml
type 'a clopen_poly_variant = [< `One | `Two of int | `Three Two Three ] as 'a
```
```ocaml
type nested_poly_variant = [ 
  | `A
  | `B of [ `B1 | `B2 ]
  | `C
  | `D of [ `D1 of [ `D1a ] ]
 ]
```
```ocaml
type ('a, 'b) full_gadt_alias = ('a, 'b) full_gadt = 
  | Tag : (unit, unit) full_gadt_alias
  | First : 'a -> ('a, unit) full_gadt_alias
  | Second : 'a -> (unit, 'a) full_gadt_alias
  | Exist : 'a * 'b -> ('b, unit) full_gadt_alias
```
This comment is for `full_gadt_alias`.

```ocaml
type 'a partial_gadt_alias = 'a partial_gadt = 
  | AscribeTag : 'a partial_gadt_alias
  | OfTag of 'a partial_gadt_alias
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias
```
This comment is for `partial_gadt_alias`.

```ocaml
exception Exn_arrow : unit -> exn
```
This comment is for [`Exn_arrow`](./#exception-Exn_arrow).

```ocaml
type mutual_constr_a = 
  | A
  | B_ish of mutual_constr_b (* This comment is between mutual_constr_a and mutual_constr_b. *)
```
This comment is for [`mutual_constr_a`](./#type-mutual_constr_a) then [`mutual_constr_b`](./#type-mutual_constr_b).

```ocaml
and mutual_constr_b = 
  | B
  | A_ish of mutual_constr_a (* This comment must be here for the next to associate correctly. *)
```
This comment is for [`mutual_constr_b`](./#type-mutual_constr_b) then [`mutual_constr_a`](./#type-mutual_constr_a).

```ocaml
type rec_obj = < f : int ; g : unit -> unit ; h : rec_obj >
```
```ocaml
type 'a open_obj = < f : int ; g : unit -> unit.. > as 'a
```
```ocaml
type 'a oof = (< a : unit.. > as 'a) -> 'a
```
```ocaml
type 'a any_obj = < .. > as 'a
```
```ocaml
type empty_obj = <  >
```
```ocaml
type one_meth = < meth : unit >
```
```ocaml
type ext = ..
```
A mystery wrapped in an ellipsis

```ocaml
type ext += 
  | ExtA
```
```ocaml
type ext += 
  | ExtB
```
```ocaml
type ext += 
  | ExtC of unit
  | ExtD of ext
```
```ocaml
type ext += 
  | ExtE
```
```ocaml
type ext += private 
  | ExtF
```
```ocaml
type 'a poly_ext = ..
```
'a poly\_ext

```ocaml
type poly_ext += 
  | Foo of 'b
  | Bar of 'b * 'b (* 'b poly_ext *)
```
```ocaml
type poly_ext += 
  | Quux of 'c (* 'c poly_ext *)
```
```ocaml
module ExtMod : sig ... end
```
```ocaml
type ExtMod.t += 
  | ZzzTop0 (* It's got the rock *)
```
```ocaml
type ExtMod.t += 
  | ZzzTop of unit (* and it packs a unit. *)
```
```ocaml
val launch_missiles : unit -> unit
```
Rotate keys on my mark...

```ocaml
type my_mod = (module COLLECTION)
```
A brown paper package tied up with string

```ocaml
class empty_class : object ... end
```
```ocaml
class one_method_class : object ... end
```
```ocaml
class two_method_class : object ... end
```
```ocaml
class 'a param_class : 'a -> object ... end
```
```ocaml
type my_unit_object = unit param_class
```
```ocaml
type 'a my_unit_class = unit param_class as 'a
```
```ocaml
module Dep1 : sig ... end
```
```ocaml
module Dep2 (Arg : sig ... end) : sig ... end
```
```ocaml
type dep1 = Dep2(Dep1).B.c
```
```ocaml
module Dep3 : sig ... end
```
```ocaml
module Dep4 : sig ... end
```
```ocaml
module Dep5 (Arg : sig ... end) : sig ... end
```
```ocaml
type dep2 = Dep5(Dep4).Z.X.b
```
```ocaml
type dep3 = Dep5(Dep4).Z.Y.a
```
```ocaml
module Dep6 : sig ... end
```
```ocaml
module Dep7 (Arg : sig ... end) : sig ... end
```
```ocaml
type dep4 = Dep7(Dep6).M.Y.d
```
```ocaml
module Dep8 : sig ... end
```
```ocaml
module Dep9 (X : sig ... end) : sig ... end
```
```ocaml
module type Dep10 = Dep9(Dep8).T with type t = int
```
```ocaml
module Dep11 : sig ... end
```
```ocaml
module Dep12 (Arg : sig ... end) : sig ... end
```
```ocaml
module Dep13 : Dep12(Dep11).T
```
```ocaml
type dep5 = Dep13.c
```
```ocaml
module type With1 = sig ... end
```
```ocaml
module With2 : sig ... end
```
```ocaml
module With3 : With1 with module M = With2
```
```ocaml
type with1 = With3.N.t
```
```ocaml
module With4 : With1 with module M := With2
```
```ocaml
type with2 = With4.N.t
```
```ocaml
module With5 : sig ... end
```
```ocaml
module With6 : sig ... end
```
```ocaml
module With7 (X : sig ... end) : sig ... end
```
```ocaml
module type With8 =
  With7(With6).T with module M = With5 and type M.N.t = With5.N.t
```
```ocaml
module With9 : sig ... end
```
```ocaml
module With10 : sig ... end
```
```ocaml
module type With11 = With7(With10).T with module M = With9 and type N.t = int
```
```ocaml
module type NestedInclude1 = sig ... end
```
```ocaml
module type NestedInclude2 = sig ... end
```
```ocaml
type nested_include = int
```
```ocaml
module DoubleInclude1 : sig ... end
```
```ocaml
module DoubleInclude3 : sig ... end
```
```ocaml
type double_include
```
```ocaml
module IncludeInclude1 : sig ... end
```
```ocaml
module type IncludeInclude2 = sig ... end
```
```ocaml
module IncludeInclude2_M : sig ... end
```
```ocaml
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

```ocaml
module CanonicalTest : sig ... end
```
Some ref to [`CanonicalTest.Base_Tests.C.t`](./Ocamlary-CanonicalTest-Base_Tests-C.md#type-t) and [`CanonicalTest.Base_Tests.L.id`](./Ocamlary-CanonicalTest-Base-List.md#val-id). But also to [`CanonicalTest.Base.List`](./Ocamlary-CanonicalTest-Base-List.md) and [`CanonicalTest.Base.List.t`](./Ocamlary-CanonicalTest-Base-List.md#type-t)


## Aliases again

```ocaml
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

```ocaml
module type M = sig ... end
```
```ocaml
module M : sig ... end
```
Here goes:

- `{!module-M.t}` : [`M.t`](./Ocamlary-M.md#type-t)
- `{!module-type-M.t}` : [`M.t`](./Ocamlary-module-type-M.md#type-t)
```ocaml
module Only_a_module : sig ... end
```
- `{!Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!module-Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!module-Only_a_module.type-t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
- `{!type:Only_a_module.t}` : [`Only_a_module.t`](./Ocamlary-Only_a_module.md#type-t)
```ocaml
module type TypeExt = sig ... end
```
```ocaml
type new_t = ..
```
```ocaml
type new_t += 
  | C
```
```ocaml
module type TypeExtPruned = TypeExt with type t := new_t
```
```ocaml
module Op : sig ... end
```