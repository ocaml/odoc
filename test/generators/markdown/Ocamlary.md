Ocamlary

Module `Ocamlary`

This is an _interface_ with **all** of the _module system_ features. This
documentation demonstrates:

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

@author David Sheets

You may find more information about this HTML documentation renderer at
[github.com/dsheets/ocamlary](https://github.com/dsheets/ocamlary).

This is some verbatim text:

```
verbatim
```
This is some verbatim text:

```
[][df[]]}}
```
Here is some raw LaTeX:  $e^{i\pi} = -1$ 

Here is an index table of `Empty` modules:

@[`Empty`](Ocamlary.Empty.md) A plain, empty module

@[`EmptyAlias`](Ocamlary.Empty.md) A plain module alias of `Empty`

Odoc doesn't support `{!indexlist}`.

Here is some superscript: x<sup>2</sup>

Here is some subscript: x<sub>0</sub>

Here are some escaped brackets: { [ @ ] }

Here is some _emphasis_ `followed by code`.

An unassociated comment

# Level 1

## Level 2

---

### Level 3

---

#### Level 4

---

### Basic module stuff

---

<a id="module-Empty"></a>

###### module [Empty](Ocamlary.Empty.md)

A plain, empty module

<a id="module-type-Empty"></a>

###### module type [Empty](Ocamlary.module-type-Empty.md)

An ambiguous, misnamed module type

<a id="module-type-MissingComment"></a>

###### module type [MissingComment](Ocamlary.module-type-MissingComment.md)

An ambiguous, misnamed module type

# Section 9000

<a id="module-EmptyAlias"></a>

###### module EmptyAlias =

> [Empty](Ocamlary.Empty.md)

A plain module alias of `Empty`

### EmptySig

---

<a id="module-type-EmptySig"></a>

###### module type [EmptySig](Ocamlary.module-type-EmptySig.md)

A plain, empty module signature

<a id="module-type-EmptySigAlias"></a>

###### module type EmptySigAlias =

> [EmptySig](Ocamlary.module-type-EmptySig.md)

A plain, empty module signature alias of

<a id="module-ModuleWithSignature"></a>

###### module [ModuleWithSignature](Ocamlary.ModuleWithSignature.md)

A plain module of a signature of
[`EmptySig`](Ocamlary.module-type-EmptySig.md) (reference)

<a id="module-ModuleWithSignatureAlias"></a>

###### module
[ModuleWithSignatureAlias](Ocamlary.ModuleWithSignatureAlias.md)

A plain module with an alias signature

<a id="module-One"></a>

###### module [One](Ocamlary.One.md)

<a id="module-type-SigForMod"></a>

###### module type [SigForMod](Ocamlary.module-type-SigForMod.md)

There's a signature in a module in this signature.

<a id="module-type-SuperSig"></a>

###### module type [SuperSig](Ocamlary.module-type-SuperSig.md)

For a good time, see
[`subSig`](Ocamlary.module-type-SuperSig.module-type-SubSigA.md#subSig) or
[`subSig`](Ocamlary.module-type-SuperSig.module-type-SubSigB.md#subSig) or
[`SuperSig.EmptySig`](Ocamlary.module-type-SuperSig.module-type-EmptySig.md).
Section [Section 9000](#s9000) is also interesting. [EmptySig](#emptySig) is
the section and [`EmptySig`](Ocamlary.module-type-EmptySig.md) is the module
signature.

<a id="module-Buffer"></a>

###### module [Buffer](Ocamlary.Buffer.md)

References are resolved after everything, so `{!Buffer.t}` won't resolve.

Some text before exception title.

### Basic exception stuff

---

After exception title.

<a id="exception-Kaboom"></a>

###### exception Kaboom of unit

Unary exception constructor

<a id="exception-Kablam"></a>

###### exception Kablam of unit * unit

Binary exception constructor

<a id="exception-Kapow"></a>

###### exception Kapow of unit * unit

Unary exception constructor over binary tuple

<a id="exception-EmptySig"></a>

###### exception EmptySig

[`EmptySig`](Ocamlary.module-type-EmptySig.md) is a module and
[`EmptySig`](#exception-EmptySig) is this exception.

<a id="exception-EmptySigAlias"></a>

###### exception EmptySigAlias

[`EmptySigAlias`](#exception-EmptySigAlias) is this exception.

### Basic type and value stuff with advanced doc comments

---

<a id="type-a_function"></a>

###### type ('a, 'b) a_function =

> 'a -> 'b

[`a_function`](#type-a_function) is this type and
[`a_function`](#val-a_function) is the value below.

<a id="val-a_function"></a>

###### val a_function :

> x:int -> int

This is `a_function` with param and return type.

@parameter x

@returns

<a id="val-fun_fun_fun"></a>

###### val fun_fun_fun :

> 
>   ( ( int, int ) [a_function](#type-a_function), ( unit, unit )
> [a_function](#type-a_function) ) [a_function](#type-a_function)

<a id="val-fun_maybe"></a>

###### val fun_maybe :

> ?yes:unit -> unit -> int

<a id="val-not_found"></a>

###### val not_found :

> unit -> unit

@raises Not_found

<a id="val-ocaml_org"></a>

###### val ocaml_org :

> string

@see [http://ocaml.org/](http://ocaml.org/)

<a id="val-some_file"></a>

###### val some_file :

> string

@see `some_file`

<a id="val-some_doc"></a>

###### val some_doc :

> string

@see some_doc

<a id="val-since_mesozoic"></a>

###### val since_mesozoic :

> unit

This value was introduced in the Mesozoic era.

@since mesozoic

<a id="val-changing"></a>

###### val changing :

> unit

This value has had changes in 1.0.0, 1.1.0, and 1.2.0.

@before 1.0.0

@before 1.1.0

@version 1.2.0

### Some Operators

---

<a id="val-(~-)"></a>

###### val (~-) :

> unit

<a id="val-(!)"></a>

###### val (!) :

> unit

<a id="val-(@)"></a>

###### val (@) :

> unit

<a id="val-($)"></a>

###### val ($) :

> unit

<a id="val-(%)"></a>

###### val (%) :

> unit

<a id="val-(&)"></a>

###### val (&) :

> unit

<a id="val-(*)"></a>

###### val (*) :

> unit

<a id="val-(-)"></a>

###### val (-) :

> unit

<a id="val-(+)"></a>

###### val (+) :

> unit

<a id="val-(-?)"></a>

###### val (-?) :

> unit

<a id="val-(/)"></a>

###### val (/) :

> unit

<a id="val-(:=)"></a>

###### val (:=) :

> unit

<a id="val-(=)"></a>

###### val (=) :

> unit

<a id="val-(land)"></a>

###### val (land) :

> unit

### Advanced Module Stuff

---

<a id="module-CollectionModule"></a>

###### module [CollectionModule](Ocamlary.CollectionModule.md)

This comment is for `CollectionModule`.

<a id="module-type-COLLECTION"></a>

###### module type [COLLECTION](Ocamlary.module-type-COLLECTION.md)

module type of

<a id="module-Recollection"></a>

###### module [Recollection](Ocamlary.Recollection.md)

<a id="module-type-MMM"></a>

###### module type [MMM](Ocamlary.module-type-MMM.md)

<a id="module-type-RECOLLECTION"></a>

###### module type [RECOLLECTION](Ocamlary.module-type-RECOLLECTION.md)

<a id="module-type-RecollectionModule"></a>

###### module type
[RecollectionModule](Ocamlary.module-type-RecollectionModule.md)

<a id="module-type-A"></a>

###### module type [A](Ocamlary.module-type-A.md)

<a id="module-type-B"></a>

###### module type [B](Ocamlary.module-type-B.md)

<a id="module-type-C"></a>

###### module type [C](Ocamlary.module-type-C.md)

This module type includes two signatures.

<a id="module-FunctorTypeOf"></a>

###### module [FunctorTypeOf](Ocamlary.FunctorTypeOf.md)

This comment is for `FunctorTypeOf`.

<a id="module-type-IncludeModuleType"></a>

###### module type
[IncludeModuleType](Ocamlary.module-type-IncludeModuleType.md)

This comment is for `IncludeModuleType`.

<a id="module-type-ToInclude"></a>

###### module type [ToInclude](Ocamlary.module-type-ToInclude.md)

<a id="module-IncludedA"></a>

###### module [IncludedA](Ocamlary.IncludedA.md)

<a id="module-type-IncludedB"></a>

###### module type [IncludedB](Ocamlary.module-type-IncludedB.md)

### Advanced Type Stuff

---

<a id="type-record"></a>

###### type record = {

<a id="type-record.field1"></a>

######    `;int : field1`

This comment is for `field1`.

<a id="type-record.field2"></a>

######    `;int : field2`

This comment is for `field2`.

}

This comment is for `record`.

This comment is also for `record`.

<a id="type-mutable_record"></a>

###### type mutable_record = {

<a id="type-mutable_record.a"></a>

######    `;int : a mutable`

`a` is first and mutable

<a id="type-mutable_record.b"></a>

######    `;unit : b`

`b` is second and immutable

<a id="type-mutable_record.c"></a>

######    `;int : c mutable`

`c` is third and mutable

}

<a id="type-universe_record"></a>

###### type universe_record = {

<a id="type-universe_record.nihilate"></a>

######    nihilate : 'a. 'a -> unit;

}

<a id="type-variant"></a>

###### type variant = 

<a id="type-variant.TagA"></a>

######    | TagA

This comment is for `TagA`.

<a id="type-variant.ConstrB"></a>

######    | ConstrB of int

This comment is for `ConstrB`.

<a id="type-variant.ConstrC"></a>

######    | ConstrC of int * int

This comment is for binary `ConstrC`.

<a id="type-variant.ConstrD"></a>

######    | ConstrD of int * int

This comment is for unary `ConstrD` of binary tuple.

This comment is for `variant`.

This comment is also for `variant`.

<a id="type-poly_variant"></a>

###### type poly_variant = [ 

<a id="type-poly_variant.TagA"></a>

######    `| ``` `TagA ``

<a id="type-poly_variant.ConstrB"></a>

######    `| ``` int of `ConstrB ``

 ]

This comment is for `poly_variant`.

Wow! It was a polymorphic variant!

<a id="type-full_gadt"></a>

###### type (_, _) full_gadt = 

<a id="type-full_gadt.Tag"></a>

######    | Tag : ( unit, unit ) [full_gadt](#type-full_gadt)

<a id="type-full_gadt.First"></a>

######    | First : 'a -> ( 'a, unit ) [full_gadt](#type-full_gadt)

<a id="type-full_gadt.Second"></a>

######    | Second : 'a -> ( unit, 'a ) [full_gadt](#type-full_gadt)

<a id="type-full_gadt.Exist"></a>

######    | Exist : 'a * 'b -> ( 'b, unit ) [full_gadt](#type-full_gadt)

This comment is for `full_gadt`.

Wow! It was a GADT!

<a id="type-partial_gadt"></a>

###### type 'a partial_gadt = 

<a id="type-partial_gadt.AscribeTag"></a>

######    | AscribeTag : 'a [partial_gadt](#type-partial_gadt)

<a id="type-partial_gadt.OfTag"></a>

######    | OfTag of 'a [partial_gadt](#type-partial_gadt)

<a id="type-partial_gadt.ExistGadtTag"></a>

######    | ExistGadtTag : ( 'a -> 'b ) -> 'a
[partial_gadt](#type-partial_gadt)

This comment is for `partial_gadt`.

Wow! It was a mixed GADT!

<a id="type-alias"></a>

###### type alias =

> [variant](#type-variant)

This comment is for `alias`.

<a id="type-tuple"></a>

###### type tuple =

> ([alias](#type-alias) * [alias](#type-alias)) * [alias](#type-alias)
> * ([alias](#type-alias) * [alias](#type-alias))

This comment is for `tuple`.

<a id="type-variant_alias"></a>

###### type variant_alias = [variant](#type-variant) = 

<a id="type-variant_alias.TagA"></a>

######    | TagA

<a id="type-variant_alias.ConstrB"></a>

######    | ConstrB of int

<a id="type-variant_alias.ConstrC"></a>

######    | ConstrC of int * int

<a id="type-variant_alias.ConstrD"></a>

######    | ConstrD of int * int

This comment is for `variant_alias`.

<a id="type-record_alias"></a>

###### type record_alias = [record](#type-record) = {

<a id="type-record_alias.field1"></a>

######    `;int : field1`

<a id="type-record_alias.field2"></a>

######    `;int : field2`

}

This comment is for `record_alias`.

<a id="type-poly_variant_union"></a>

###### type poly_variant_union = [ 

<a id="type-poly_variant_union.poly_variant"></a>

######    `| `[poly_variant](#type-poly_variant)

<a id="type-poly_variant_union.TagC"></a>

######    `| ``` `TagC ``

 ]

This comment is for `poly_variant_union`.

<a id="type-poly_poly_variant"></a>

###### type 'a poly_poly_variant = [ 

<a id="type-poly_poly_variant.TagA"></a>

######    `| ``` 'a of `TagA ``

 ]

<a id="type-bin_poly_poly_variant"></a>

###### type ('a, 'b) bin_poly_poly_variant = [ 

<a id="type-bin_poly_poly_variant.TagA"></a>

######    `| ``` 'a of `TagA ``

<a id="type-bin_poly_poly_variant.ConstrB"></a>

######    `| ``` 'b of `ConstrB ``

 ]

<a id="type-open_poly_variant"></a>

###### type 'a open_poly_variant =

> [> `TagA ] as 'a

<a id="type-open_poly_variant2"></a>

###### type 'a open_poly_variant2 =

> [> `ConstrB of int ] as 'a

<a id="type-open_poly_variant_alias"></a>

###### type 'a open_poly_variant_alias =

> 'a [open_poly_variant](#type-open_poly_variant)
> [open_poly_variant2](#type-open_poly_variant2)

<a id="type-poly_fun"></a>

###### type 'a poly_fun =

> [> `ConstrB of int ] as 'a -> 'a

<a id="type-poly_fun_constraint"></a>

###### type 'a poly_fun_constraint =

> 'a -> 'a constraint 'a = [> `TagA ]

<a id="type-closed_poly_variant"></a>

###### type 'a closed_poly_variant =

> [< `One | `Two ] as 'a

<a id="type-clopen_poly_variant"></a>

###### type 'a clopen_poly_variant =

> [< `One | `Two of int | `Three Two Three ] as 'a

<a id="type-nested_poly_variant"></a>

###### type nested_poly_variant = [ 

<a id="type-nested_poly_variant.A"></a>

######    `| ``` `A ``

<a id="type-nested_poly_variant.B"></a>

######    `| ``` [ `B1 | `B2 ] of `B ``

<a id="type-nested_poly_variant.C"></a>

######    `| ``` `C ``

<a id="type-nested_poly_variant.D"></a>

######    `| ``` [ [ `D1a ]  of`D1 ] of `D ``

 ]

<a id="type-full_gadt_alias"></a>

###### type ('a, 'b) full_gadt_alias = ( 'a, 'b )
[full_gadt](#type-full_gadt) = 

<a id="type-full_gadt_alias.Tag"></a>

######    | Tag : ( unit, unit ) [full_gadt_alias](#type-full_gadt_alias)

<a id="type-full_gadt_alias.First"></a>

######    | First : 'a -> ( 'a, unit )
[full_gadt_alias](#type-full_gadt_alias)

<a id="type-full_gadt_alias.Second"></a>

######    | Second : 'a -> ( unit, 'a )
[full_gadt_alias](#type-full_gadt_alias)

<a id="type-full_gadt_alias.Exist"></a>

######    | Exist : 'a * 'b -> ( 'b, unit )
[full_gadt_alias](#type-full_gadt_alias)

This comment is for `full_gadt_alias`.

<a id="type-partial_gadt_alias"></a>

###### type 'a partial_gadt_alias = 'a [partial_gadt](#type-partial_gadt) = 

<a id="type-partial_gadt_alias.AscribeTag"></a>

######    | AscribeTag : 'a [partial_gadt_alias](#type-partial_gadt_alias)

<a id="type-partial_gadt_alias.OfTag"></a>

######    | OfTag of 'a [partial_gadt_alias](#type-partial_gadt_alias)

<a id="type-partial_gadt_alias.ExistGadtTag"></a>

######    | ExistGadtTag : ( 'a -> 'b ) -> 'a
[partial_gadt_alias](#type-partial_gadt_alias)

This comment is for `partial_gadt_alias`.

<a id="exception-Exn_arrow"></a>

###### exception Exn_arrow :

> unit -> exn

This comment is for [`Exn_arrow`](#exception-Exn_arrow).

<a id="type-mutual_constr_a"></a>

###### type mutual_constr_a = 

<a id="type-mutual_constr_a.A"></a>

######    | A

<a id="type-mutual_constr_a.B_ish"></a>

######    | B_ish of [mutual_constr_b](#type-mutual_constr_b)

This comment is between [`mutual_constr_a`](#type-mutual_constr_a) and
[`mutual_constr_b`](#type-mutual_constr_b).

This comment is for [`mutual_constr_a`](#type-mutual_constr_a) then
[`mutual_constr_b`](#type-mutual_constr_b).

<a id="type-mutual_constr_b"></a>

###### and mutual_constr_b = 

<a id="type-mutual_constr_b.B"></a>

######    | B

<a id="type-mutual_constr_b.A_ish"></a>

######    | A_ish of [mutual_constr_a](#type-mutual_constr_a)

This comment must be here for the next to associate correctly.

This comment is for [`mutual_constr_b`](#type-mutual_constr_b) then
[`mutual_constr_a`](#type-mutual_constr_a).

<a id="type-rec_obj"></a>

###### type rec_obj =

> < f : int ; g : unit -> unit ; h : [rec_obj](#type-rec_obj) >

<a id="type-open_obj"></a>

###### type 'a open_obj =

> < f : int ; g : unit -> unit.. > as 'a

<a id="type-oof"></a>

###### type 'a oof =

> < a : unit.. > as 'a -> 'a

<a id="type-any_obj"></a>

###### type 'a any_obj =

> < .. > as 'a

<a id="type-empty_obj"></a>

###### type empty_obj =

> <  >

<a id="type-one_meth"></a>

###### type one_meth =

> < meth : unit >

<a id="type-ext"></a>

###### type ext =

> ..

A mystery wrapped in an ellipsis

<a id="extension-decl-ExtA"></a>

###### type [ext](#type-ext) += 

<a id="extension-ExtA"></a>

######    | ExtA

<a id="extension-decl-ExtB"></a>

###### type [ext](#type-ext) += 

<a id="extension-ExtB"></a>

######    | ExtB

<a id="extension-decl-ExtC"></a>

###### type [ext](#type-ext) += 

<a id="extension-ExtC"></a>

######    | ExtC of unit

<a id="extension-ExtD"></a>

######    | ExtD of [ext](#type-ext)

<a id="extension-decl-ExtE"></a>

###### type [ext](#type-ext) += 

<a id="extension-ExtE"></a>

######    | ExtE

<a id="extension-decl-ExtF"></a>

###### type [ext](#type-ext) += 

<a id="extension-ExtF"></a>

######    | ExtF

<a id="type-poly_ext"></a>

###### type 'a poly_ext =

> ..

'a poly_ext

<a id="extension-decl-Foo"></a>

###### type [poly_ext](#type-poly_ext) += 

<a id="extension-Foo"></a>

######    | Foo of 'b

<a id="extension-Bar"></a>

######    | Bar of 'b * 'b

'b poly_ext

<a id="extension-decl-Quux"></a>

###### type [poly_ext](#type-poly_ext) += 

<a id="extension-Quux"></a>

######    | Quux of 'c

'c poly_ext

<a id="module-ExtMod"></a>

###### module [ExtMod](Ocamlary.ExtMod.md)

<a id="extension-decl-ZzzTop0"></a>

###### type [ExtMod.t](Ocamlary.ExtMod.md#type-t) += 

<a id="extension-ZzzTop0"></a>

######    | ZzzTop0

It's got the rock

<a id="extension-decl-ZzzTop"></a>

###### type [ExtMod.t](Ocamlary.ExtMod.md#type-t) += 

<a id="extension-ZzzTop"></a>

######    | ZzzTop of unit

and it packs a unit.

<a id="val-launch_missiles"></a>

###### val launch_missiles :

> unit -> unit

Rotate keys on my mark...

<a id="type-my_mod"></a>

###### type my_mod =

> (module [COLLECTION](Ocamlary.module-type-COLLECTION.md))

A brown paper package tied up with string

<a id="class-empty_class"></a>

###### class [empty_class](Ocamlary.empty_class.md)

<a id="class-one_method_class"></a>

###### class [one_method_class](Ocamlary.one_method_class.md)

<a id="class-two_method_class"></a>

###### class [two_method_class](Ocamlary.two_method_class.md)

<a id="class-param_class"></a>

###### class 'a [param_class](Ocamlary.param_class.md)

<a id="type-my_unit_object"></a>

###### type my_unit_object =

> unit [param_class](Ocamlary.param_class.md)

<a id="type-my_unit_class"></a>

###### type 'a my_unit_class =

> unit param_class as 'a

<a id="module-Dep1"></a>

###### module [Dep1](Ocamlary.Dep1.md)

<a id="module-Dep2"></a>

###### module [Dep2](Ocamlary.Dep2.md)

<a id="type-dep1"></a>

###### type dep1 =

> [Dep2(Dep1).B.c](Ocamlary.Dep1.module-type-S.c.md)

<a id="module-Dep3"></a>

###### module [Dep3](Ocamlary.Dep3.md)

<a id="module-Dep4"></a>

###### module [Dep4](Ocamlary.Dep4.md)

<a id="module-Dep5"></a>

###### module [Dep5](Ocamlary.Dep5.md)

<a id="type-dep2"></a>

###### type dep2 =

> [Dep5(Dep4).Z.X.b](Ocamlary.Dep4.module-type-T.md#type-b)

<a id="type-dep3"></a>

###### type dep3 =

> [Dep5(Dep4).Z.Y.a](Ocamlary.Dep3.md#type-a)

<a id="module-Dep6"></a>

###### module [Dep6](Ocamlary.Dep6.md)

<a id="module-Dep7"></a>

###### module [Dep7](Ocamlary.Dep7.md)

<a id="type-dep4"></a>

###### type dep4 =

> [Dep7(Dep6).M.Y.d](Ocamlary.Dep6.module-type-T.Y.md#type-d)

<a id="module-Dep8"></a>

###### module [Dep8](Ocamlary.Dep8.md)

<a id="module-Dep9"></a>

###### module [Dep9](Ocamlary.Dep9.md)

<a id="module-type-Dep10"></a>

###### module type [Dep10](Ocamlary.module-type-Dep10.md)

<a id="module-Dep11"></a>

###### module [Dep11](Ocamlary.Dep11.md)

<a id="module-Dep12"></a>

###### module [Dep12](Ocamlary.Dep12.md)

<a id="module-Dep13"></a>

###### module [Dep13](Ocamlary.Dep13.md)

<a id="type-dep5"></a>

###### type dep5 =

> [Dep13.c](Ocamlary.Dep13.c.md)

<a id="module-type-With1"></a>

###### module type [With1](Ocamlary.module-type-With1.md)

<a id="module-With2"></a>

###### module [With2](Ocamlary.With2.md)

<a id="module-With3"></a>

###### module [With3](Ocamlary.With3.md)

<a id="type-with1"></a>

###### type with1 =

> [With3.N.t](Ocamlary.With3.N.md#type-t)

<a id="module-With4"></a>

###### module [With4](Ocamlary.With4.md)

<a id="type-with2"></a>

###### type with2 =

> [With4.N.t](Ocamlary.With4.N.md#type-t)

<a id="module-With5"></a>

###### module [With5](Ocamlary.With5.md)

<a id="module-With6"></a>

###### module [With6](Ocamlary.With6.md)

<a id="module-With7"></a>

###### module [With7](Ocamlary.With7.md)

<a id="module-type-With8"></a>

###### module type [With8](Ocamlary.module-type-With8.md)

<a id="module-With9"></a>

###### module [With9](Ocamlary.With9.md)

<a id="module-With10"></a>

###### module [With10](Ocamlary.With10.md)

<a id="module-type-With11"></a>

###### module type [With11](Ocamlary.module-type-With11.md)

<a id="module-type-NestedInclude1"></a>

###### module type [NestedInclude1](Ocamlary.module-type-NestedInclude1.md)

<a id="module-type-NestedInclude2"></a>

###### module type [NestedInclude2](Ocamlary.module-type-NestedInclude2.md)

<a id="type-nested_include"></a>

###### type nested_include =

> int

<a id="module-DoubleInclude1"></a>

###### module [DoubleInclude1](Ocamlary.DoubleInclude1.md)

<a id="module-DoubleInclude3"></a>

###### module [DoubleInclude3](Ocamlary.DoubleInclude3.md)

<a id="type-double_include"></a>

###### type double_include

<a id="module-IncludeInclude1"></a>

###### module [IncludeInclude1](Ocamlary.IncludeInclude1.md)

<a id="module-type-IncludeInclude2"></a>

###### module type [IncludeInclude2](Ocamlary.module-type-IncludeInclude2.md)

<a id="module-IncludeInclude2_M"></a>

###### module [IncludeInclude2_M](Ocamlary.IncludeInclude2_M.md)

<a id="type-include_include"></a>

###### type include_include

# Trying the {!modules: ...} command.

With ocamldoc, toplevel units will be linked and documented, while submodules
will behave as simple references.

With odoc, everything should be resolved (and linked) but only toplevel units
will be documented.

@[`Dep1.X`](Ocamlary.Dep1.X.md)

@[`Ocamlary.IncludeInclude1`](Ocamlary.IncludeInclude1.md)

@[`Ocamlary`]() This is an _interface_ with **all** of the _module system_
features. This documentation demonstrates:

### Weirder usages involving module types

---

@[`IncludeInclude1.IncludeInclude2_M`](Ocamlary.IncludeInclude1.IncludeInclude2_M.md)

@[`Dep4.X`](Ocamlary.Dep4.X.md)

# Playing with @canonical paths

<a id="module-CanonicalTest"></a>

###### module [CanonicalTest](Ocamlary.CanonicalTest.md)

Some ref to
[`CanonicalTest.Base_Tests.C.t`](Ocamlary.CanonicalTest.Base_Tests.C.md#type-t)
and
[`CanonicalTest.Base_Tests.L.id`](Ocamlary.CanonicalTest.Base.List.md#val-id).
But also to [`CanonicalTest.Base.List`](Ocamlary.CanonicalTest.Base.List.md)
and [`CanonicalTest.Base.List.t`](Ocamlary.CanonicalTest.Base.List.md#type-t)

# Aliases again

<a id="module-Aliases"></a>

###### module [Aliases](Ocamlary.Aliases.md)

Let's imitate jst's layout.

# Section title splicing

I can refer to

- `{!section:indexmodules}` : [Trying the {!modules: ...}
  command.](#indexmodules)
  

- `{!aliases}` : [Aliases again](#aliases)
  

But also to things in submodules:

- `{!section:SuperSig.SubSigA.subSig}` :
  [`subSig`](Ocamlary.module-type-SuperSig.module-type-SubSigA.md#subSig)
  

- `{!Aliases.incl}` : [`incl`](Ocamlary.Aliases.md#incl)
  

And just to make sure we do not mess up:

- `{{!section:indexmodules}A}` : [A](#indexmodules)
  

- `{{!aliases}B}` : [B](#aliases)
  

- `{{!section:SuperSig.SubSigA.subSig}C}` :
  [C](Ocamlary.module-type-SuperSig.module-type-SubSigA.md#subSig)
  

- `{{!Aliases.incl}D}` : [D](Ocamlary.Aliases.md#incl)
  

# New reference syntax

<a id="module-type-M"></a>

###### module type [M](Ocamlary.module-type-M.md)

<a id="module-M"></a>

###### module [M](Ocamlary.M.md)

Here goes:

- `{!module-M.t}` : [`M.t`](Ocamlary.M.md#type-t)
  

- `{!module-type-M.t}` : [`M.t`](Ocamlary.module-type-M.md#type-t)
  

<a id="module-Only_a_module"></a>

###### module [Only_a_module](Ocamlary.Only_a_module.md)

- `{!Only_a_module.t}` :
  [`Only_a_module.t`](Ocamlary.Only_a_module.md#type-t)
  

- `{!module-Only_a_module.t}` :
  [`Only_a_module.t`](Ocamlary.Only_a_module.md#type-t)
  

- `{!module-Only_a_module.type-t}` :
  [`Only_a_module.t`](Ocamlary.Only_a_module.md#type-t)
  

- `{!type:Only_a_module.t}` :
  [`Only_a_module.t`](Ocamlary.Only_a_module.md#type-t)
  

<a id="module-type-TypeExt"></a>

###### module type [TypeExt](Ocamlary.module-type-TypeExt.md)

<a id="type-new_t"></a>

###### type new_t =

> ..

<a id="extension-decl-C"></a>

###### type [new_t](#type-new_t) += 

<a id="extension-C"></a>

######    | C

<a id="module-type-TypeExtPruned"></a>

###### module type [TypeExtPruned](Ocamlary.module-type-TypeExtPruned.md)
