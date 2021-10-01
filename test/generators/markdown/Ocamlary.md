Ocamlary

Module  `` Ocamlary `` 

This is an _interface_ with **all** of the _module system_ features. This documentation demonstrates:
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

@author : David Sheets

You may find more information about this HTML documentation renderer at github.com/dsheets/ocamlary.
This is some verbatim text:
    verbatimThis is some verbatim text:
    [][df[]]}}Here is some raw LaTeX: 
Here is an index table of  `` Empty ``  modules:

@ `` Empty ``  : A plain, empty module



@ `` EmptyAlias ``  : A plain module alias of  `` Empty `` 

Odoc doesn't support  `` {!indexlist} `` .
Here is some superscript: x<sup>2
Here is some subscript: x<sub>0
Here are some escaped brackets: { [ @ ] }
Here is some _emphasis_  `` followed by code `` .
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

###### module Empty : sig
###### end

A plain, empty module
###### module type Empty = sig

######     type t


###### end

An ambiguous, misnamed module type
###### module type MissingComment = sig

######     type t


###### end

An ambiguous, misnamed module type

# Section 9000

###### module EmptyAlias = Empty

A plain module alias of  `` Empty `` 

### EmptySig
---

###### module type EmptySig = sig
###### end

A plain, empty module signature
###### module type EmptySigAlias = EmptySig

A plain, empty module signature alias of
###### module ModuleWithSignature : sig
###### end

A plain module of a signature of  `` EmptySig ``  (reference)
###### module ModuleWithSignatureAlias : sig
###### end

A plain module with an alias signature
###### module One : sig

######     type one


###### end

###### module type SigForMod = sig

######     module Inner : sig

######         module type Empty = sig
######         end


######     end


###### end

There's a signature in a module in this signature.
###### module type SuperSig = sig

######     module type SubSigA = sig


 A Labeled Section Header Inside of a Signature
---

######         type t

######         module SubSigAMod : sig

######             type sub_sig_a_mod


######         end


######     end

######     module type SubSigB = sig


 Another Labeled Section Header Inside of a Signature
---

######         type t


######     end

######     module type EmptySig = sig

######         type not_actually_empty


######     end

######     module type One = sig

######         type two


######     end

######     module type SuperSig = sig
######     end


###### end

For a good time, see  `` SuperSig `` .SubSigA.subSig or  `` SuperSig `` .SubSigB.subSig or  `` SuperSig.EmptySig `` . Section Section 9000 is also interesting. EmptySig is the section and  `` EmptySig ``  is the module signature.
###### module Buffer : sig

######     val f : int -> unit


###### end

References are resolved after everything, so  `` {!Buffer.t} ``  won't resolve.
Some text before exception title.

### Basic exception stuff
---

After exception title.
###### exception Kaboom of unit

Unary exception constructor
###### exception Kablam of unit * unit

Binary exception constructor
###### exception Kapow of unit * unit

Unary exception constructor over binary tuple
###### exception EmptySig

 `` EmptySig ``  is a module and  `` EmptySig ``  is this exception.
###### exception EmptySigAlias

 `` EmptySigAlias ``  is this exception.

### Basic type and value stuff with advanced doc comments
---

###### type ('a, 'b) a_function = 'a -> 'b

 `` a_function ``  is this type and  `` a_function ``  is the value below.
###### val a_function : x:int -> int

This is  `` a_function ``  with param and return type.

@parameter x : the  `` x ``  coordinate




@returns : the  `` y ``  coordinate


###### val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function

###### val fun_maybe : ?yes:unit -> unit -> int

###### val not_found : unit -> unit


@raises Not_found : That's all it does


###### val ocaml_org : string


@see http://ocaml.org/ : The OCaml Web site


###### val some_file : string


@see  `` some_file ``  : The file called  `` some_file `` 


###### val some_doc : string


@see some_doc : The document called  `` some_doc `` 


###### val since_mesozoic : unit

This value was introduced in the Mesozoic era.

@since : mesozoic

###### val changing : unit

This value has had changes in 1.0.0, 1.1.0, and 1.2.0.

@before 1.0.0 : before 1.0.0




@before 1.1.0 : before 1.1.0




@version : 1.2.0


### Some Operators
---

###### val (~-) : unit

###### val (!) : unit

###### val (@) : unit

###### val ($) : unit

###### val (%) : unit

###### val (&) : unit

###### val (*) : unit

###### val (-) : unit

###### val (+) : unit

###### val (-?) : unit

###### val (/) : unit

###### val (:=) : unit

###### val (=) : unit

###### val (land) : unit


### Advanced Module Stuff
---

###### module CollectionModule : sig

######     type collection

This comment is for  `` collection `` .
######     type element

######     module InnerModuleA : sig

######         type t = collection

This comment is for  `` t `` .
######         module InnerModuleA' : sig

######             type t = (unit, unit) a_function

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleA' `` .
######         module type InnerModuleTypeA' = sig

######             type t = InnerModuleA'.t

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleTypeA' `` .

######     end

This comment is for  `` InnerModuleA `` .
######     module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

###### end

This comment is for  `` CollectionModule `` .
###### module type COLLECTION = sig

This comment is for  `` CollectionModule `` .
######     type collection

This comment is for  `` collection `` .
######     type element

######     module InnerModuleA : sig

######         type t = collection

This comment is for  `` t `` .
######         module InnerModuleA' : sig

######             type t = (unit, unit) a_function

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleA' `` .
######         module type InnerModuleTypeA' = sig

######             type t = InnerModuleA'.t

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleTypeA' `` .

######     end

This comment is for  `` InnerModuleA `` .
######     module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

###### end

module type of
###### module Recollection : sig


# Parameters

######     module C : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


# Signature

This comment is for  `` CollectionModule `` .
######     type collection = C.element list

This comment is for  `` collection `` .
######     type element = C.collection

######     module InnerModuleA : sig

######         type t = collection

This comment is for  `` t `` .
######         module InnerModuleA' : sig

######             type t = (unit, unit) a_function

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleA' `` .
######         module type InnerModuleTypeA' = sig

######             type t = InnerModuleA'.t

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleTypeA' `` .

######     end

This comment is for  `` InnerModuleA `` .
######     module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

###### end

###### module type MMM = sig

######     module C : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


###### end

###### module type RECOLLECTION = sig

######     module C = Recollection(CollectionModule)


###### end

###### module type RecollectionModule = sig

######     type collection = CollectionModule.element list

######     type element = CollectionModule.collection

######     module InnerModuleA : sig

######         type t = collection

This comment is for  `` t `` .
######         module InnerModuleA' : sig

######             type t = (unit, unit) a_function

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleA' `` .
######         module type InnerModuleTypeA' = sig

######             type t = InnerModuleA'.t

This comment is for  `` t `` .

######         end

This comment is for  `` InnerModuleTypeA' `` .

######     end

This comment is for  `` InnerModuleA `` .
######     module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

###### end

###### module type A = sig

######     type t

######     module Q : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


###### end

###### module type B = sig

######     type t

######     module Q : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


###### end

###### module type C = sig

######     type t

######     module Q : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


###### end

This module type includes two signatures.
###### module FunctorTypeOf : sig


# Parameters

######     module Collection : sig

This comment is for  `` CollectionModule `` .
######         type collection

This comment is for  `` collection `` .
######         type element

######         module InnerModuleA : sig

######             type t = collection

This comment is for  `` t `` .
######             module InnerModuleA' : sig

######                 type t = (unit, unit) a_function

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleA' `` .
######             module type InnerModuleTypeA' = sig

######                 type t = InnerModuleA'.t

This comment is for  `` t `` .

######             end

This comment is for  `` InnerModuleTypeA' `` .

######         end

This comment is for  `` InnerModuleA `` .
######         module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .

######     end


# Signature

######     type t = Collection.collection

This comment is for  `` t `` .

###### end

This comment is for  `` FunctorTypeOf `` .
###### module type IncludeModuleType = sig


###### end

This comment is for  `` IncludeModuleType `` .
###### module type ToInclude = sig

######     module IncludedA : sig

######         type t


######     end

######     module type IncludedB = sig

######         type s


######     end


###### end

###### module IncludedA : sig

######     type t


###### end

###### module type IncludedB = sig

######     type s


###### end


### Advanced Type Stuff
---

###### type record = {
######      `` field1 : int; `` 

This comment is for  `` field1 `` .

######      `` field2 : int; `` 

This comment is for  `` field2 `` .
###### }

This comment is for  `` record `` .
This comment is also for  `` record `` .
###### type mutable_record = {
######      `` mutable a : int; `` 

 `` a ``  is first and mutable

######      `` b : unit; `` 

 `` b ``  is second and immutable

######      `` mutable c : int; `` 

 `` c ``  is third and mutable
###### }

###### type universe_record = {
######      `` nihilate : a. 'a -> unit; `` 

###### }

###### type variant = 
######     | TagA

This comment is for  `` TagA `` .

######     | ConstrB of int

This comment is for  `` ConstrB `` .

######     | ConstrC of int * int

This comment is for binary  `` ConstrC `` .

######     | ConstrD of int * int

This comment is for unary  `` ConstrD ``  of binary tuple.


This comment is for  `` variant `` .
This comment is also for  `` variant `` .
###### type poly_variant = [ 
######      `` |  ``  `` `TagA `` 


######      `` |  ``  `` `ConstrB of int `` 

 ]

This comment is for  `` poly_variant `` .
Wow! It was a polymorphic variant!
###### type (_, _) full_gadt = 
######     | Tag : (unit, unit) full_gadt


######     | First : 'a -> ('a, unit) full_gadt


######     | Second : 'a -> (unit, 'a) full_gadt


######     | Exist : 'a * 'b -> ('b, unit) full_gadt



This comment is for  `` full_gadt `` .
Wow! It was a GADT!
###### type 'a partial_gadt = 
######     | AscribeTag : 'a partial_gadt


######     | OfTag of 'a partial_gadt


######     | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt



This comment is for  `` partial_gadt `` .
Wow! It was a mixed GADT!
###### type alias = variant

This comment is for  `` alias `` .
###### type tuple = (alias * alias) * alias * (alias * alias)

This comment is for  `` tuple `` .
###### type variant_alias = variant = 
######     | TagA


######     | ConstrB of int


######     | ConstrC of int * int


######     | ConstrD of int * int



This comment is for  `` variant_alias `` .
###### type record_alias = record = {
######      `` field1 : int; `` 


######      `` field2 : int; `` 

###### }

This comment is for  `` record_alias `` .
###### type poly_variant_union = [ 
######      `` |  ``  `` poly_variant `` 


######      `` |  ``  `` `TagC `` 

 ]

This comment is for  `` poly_variant_union `` .
###### type 'a poly_poly_variant = [ 
######      `` |  ``  `` `TagA of 'a `` 

 ]

###### type ('a, 'b) bin_poly_poly_variant = [ 
######      `` |  ``  `` `TagA of 'a `` 


######      `` |  ``  `` `ConstrB of 'b `` 

 ]

###### type 'a open_poly_variant = [> `TagA ] as 'a

###### type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a

###### type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2

###### type 'a poly_fun = [> `ConstrB of int ] as 'a -> 'a

###### type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]

###### type 'a closed_poly_variant = [< `One | `Two ] as 'a

###### type 'a clopen_poly_variant = [< `One | `Two of int | `Three Two Three ] as 'a

###### type nested_poly_variant = [ 
######      `` |  ``  `` `A `` 


######      `` |  ``  `` `B of [ `B1 | `B2 ] `` 


######      `` |  ``  `` `C `` 


######      `` |  ``  `` `D of [ `D1 of [ `D1a ] ] `` 

 ]

###### type ('a, 'b) full_gadt_alias = ('a, 'b) full_gadt = 
######     | Tag : (unit, unit) full_gadt_alias


######     | First : 'a -> ('a, unit) full_gadt_alias


######     | Second : 'a -> (unit, 'a) full_gadt_alias


######     | Exist : 'a * 'b -> ('b, unit) full_gadt_alias



This comment is for  `` full_gadt_alias `` .
###### type 'a partial_gadt_alias = 'a partial_gadt = 
######     | AscribeTag : 'a partial_gadt_alias


######     | OfTag of 'a partial_gadt_alias


######     | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias



This comment is for  `` partial_gadt_alias `` .
###### exception Exn_arrow : unit -> exn

This comment is for  `` Exn_arrow `` .
###### type mutual_constr_a = 
######     | A


######     | B_ish of mutual_constr_b

This comment is between  `` mutual_constr_a ``  and  `` mutual_constr_b `` .


This comment is for  `` mutual_constr_a ``  then  `` mutual_constr_b `` .
###### and mutual_constr_b = 
######     | B


######     | A_ish of mutual_constr_a

This comment must be here for the next to associate correctly.


This comment is for  `` mutual_constr_b ``  then  `` mutual_constr_a `` .
###### type rec_obj = < f : int; g : unit -> unit; h : rec_obj; >

###### type 'a open_obj = < f : int; g : unit -> unit; .. > as 'a

###### type 'a oof = < a : unit; .. > as 'a -> 'a

###### type 'a any_obj = < .. > as 'a

###### type empty_obj = < >

###### type one_meth = < meth : unit; >

###### type ext = ..

A mystery wrapped in an ellipsis
###### type ext += 
######     | ExtA



###### type ext += 
######     | ExtB



###### type ext += 
######     | ExtC of unit


######     | ExtD of ext



###### type ext += 
######     | ExtE



###### type ext += 
######     | ExtF



###### type 'a poly_ext = ..

'a poly_ext
###### type poly_ext += 
######     | Foo of 'b


######     | Bar of 'b * 'b

'b poly_ext


###### type poly_ext += 
######     | Quux of 'c

'c poly_ext


###### module ExtMod : sig

######     type t = ..

######     type t += 
######         | Leisureforce




###### end

###### type ExtMod.t += 
######     | ZzzTop0

It's got the rock


###### type ExtMod.t += 
######     | ZzzTop of unit

and it packs a unit.


###### val launch_missiles : unit -> unit

Rotate keys on my mark...
###### type my_mod = (module COLLECTION)

A brown paper package tied up with string
###### class  empty_class : object
###### end

###### class  one_method_class : object

######     method go : unit


###### end

###### class  two_method_class : object

######     method one : one_method_class

######     method undo : unit


###### end

###### class 'a param_class : object

######     method v : 'a


###### end

###### type my_unit_object = unit param_class

###### type 'a my_unit_class = unit param_class as 'a

###### module Dep1 : sig

######     module type S = sig

######         class  c : object

######             method m : int


######         end


######     end

######     module X : sig

######         module Y : sig

######             class  c : object

######                 method m : int


######             end


######         end


######     end


###### end

###### module Dep2 : sig


# Parameters

######     module Arg : sig

######         module type S

######         module X : sig

######             module Y : S


######         end


######     end


# Signature

######     module A : sig

######         module Y : Arg.S


######     end

######     module B = A.Y


###### end

###### type dep1 = Dep2(Dep1).B.c

###### module Dep3 : sig

######     type a


###### end

###### module Dep4 : sig

######     module type T = sig

######         type b


######     end

######     module type S = sig

######         module X : sig

######             type b


######         end

######         module Y : sig
######         end


######     end

######     module X : sig

######         type b


######     end


###### end

###### module Dep5 : sig


# Parameters

######     module Arg : sig

######         module type T

######         module type S = sig

######             module X : T

######             module Y : sig
######             end


######         end

######         module X : T


######     end


# Signature

######     module Z : sig

######         module X : Arg.T

######         module Y = Dep3


######     end


###### end

###### type dep2 = Dep5(Dep4).Z.X.b

###### type dep3 = Dep5(Dep4).Z.Y.a

###### module Dep6 : sig

######     module type S = sig

######         type d


######     end

######     module type T = sig

######         module type R = S

######         module Y : sig

######             type d


######         end


######     end

######     module X : sig

######         module type R = S

######         module Y : sig

######             type d


######         end


######     end


###### end

###### module Dep7 : sig


# Parameters

######     module Arg : sig

######         module type S

######         module type T = sig

######             module type R = S

######             module Y : R


######         end

######         module X : sig

######             module type R = S

######             module Y : R


######         end


######     end


# Signature

######     module M : sig

######         module type R = Arg.S

######         module Y : R


######     end


###### end

###### type dep4 = Dep7(Dep6).M.Y.d

###### module Dep8 : sig

######     module type T = sig

######         type t


######     end


###### end

###### module Dep9 : sig


# Parameters

######     module X : sig

######         module type T


######     end


# Signature

######     module type T = X.T


###### end

###### module type Dep10 = sig

######     type t = int


###### end

###### module Dep11 : sig

######     module type S = sig

######         class  c : object

######             method m : int


######         end


######     end


###### end

###### module Dep12 : sig


# Parameters

######     module Arg : sig

######         module type S


######     end


# Signature

######     module type T = Arg.S


###### end

###### module Dep13 : sig

######     class  c : object

######         method m : int


######     end


###### end

###### type dep5 = Dep13.c

###### module type With1 = sig

######     module M : sig

######         module type S


######     end

######     module N : M.S


###### end

###### module With2 : sig

######     module type S = sig

######         type t


######     end


###### end

###### module With3 : sig

######     module M = With2

######     module N : sig

######         type t


######     end


###### end

###### type with1 = With3.N.t

###### module With4 : sig

######     module N : sig

######         type t


######     end


###### end

###### type with2 = With4.N.t

###### module With5 : sig

######     module type S = sig

######         type t


######     end

######     module N : sig

######         type t


######     end


###### end

###### module With6 : sig

######     module type T = sig

######         module M : sig

######             module type S

######             module N : S


######         end


######     end


###### end

###### module With7 : sig


# Parameters

######     module X : sig

######         module type T


######     end


# Signature

######     module type T = X.T


###### end

###### module type With8 = sig

######     module M : sig

######         module type S = With5.S

######         module N : sig

######             type t = With5.N.t


######         end


######     end


###### end

###### module With9 : sig

######     module type S = sig

######         type t


######     end


###### end

###### module With10 : sig

######     module type T = sig

######         module M : sig

######             module type S


######         end

######         module N : M.S


######     end

 `` With10.T ``  is a submodule type.

###### end

###### module type With11 = sig

######     module M = With9

######     module N : sig

######         type t = int


######     end


###### end

###### module type NestedInclude1 = sig

######     module type NestedInclude2 = sig

######         type nested_include


######     end


###### end

###### module type NestedInclude2 = sig

######     type nested_include


###### end

###### type nested_include = int

###### module DoubleInclude1 : sig

######     module DoubleInclude2 : sig

######         type double_include


######     end


###### end

###### module DoubleInclude3 : sig

######     module DoubleInclude2 : sig

######         type double_include


######     end


###### end

###### type double_include

###### module IncludeInclude1 : sig

######     module type IncludeInclude2 = sig

######         type include_include


######     end

######     module IncludeInclude2_M : sig
######     end


###### end

###### module type IncludeInclude2 = sig

######     type include_include


###### end

###### module IncludeInclude2_M : sig
###### end

###### type include_include


# Trying the {!modules: ...} command.

With ocamldoc, toplevel units will be linked and documented, while submodules will behave as simple references.
With odoc, everything should be resolved (and linked) but only toplevel units will be documented.

@ `` Dep1.X ``  : 



@ `` Ocamlary.IncludeInclude1 ``  : 



@ `` Ocamlary ``  : This is an _interface_ with **all** of the _module system_ features. This documentation demonstrates:


### Weirder usages involving module types
---


@ `` IncludeInclude1.IncludeInclude2_M ``  : 



@ `` Dep4.X ``  : 


# Playing with @canonical paths

###### module CanonicalTest : sig

######     module Base : sig

######         module List : sig

######             type 'a t

######             val id : 'a t -> 'a t


######         end


######     end

######     module Base_Tests : sig

######         module C : sig

######             type 'a t

######             val id : 'a t -> 'a t


######         end

######         module L = Base.List

######         val foo : int L.t -> float L.t

######         val bar : 'a Base.List.t -> 'a Base.List.t

This is just  `` List `` .id, or rather  `` L.id `` 
######         val baz : 'a Base.List.t -> unit

We can't reference  `` Base__ ``  because it's hidden.  `` List `` .t ( `` List.t `` ) should resolve.

######     end

######     module List_modif : sig

######         type 'c t = 'c Base.List.t

######         val id : 'a t -> 'a t


######     end


###### end

###### val test : 'a CanonicalTest.Base__.List.t -> unit

Some ref to  `` CanonicalTest.Base_Tests.C.t ``  and  `` CanonicalTest.Base_Tests.L.id `` . But also to  `` CanonicalTest.Base.List ``  and  `` CanonicalTest.Base.List.t `` 

# Aliases again

###### module Aliases : sig

######     module Foo : sig

######         module A : sig

######             type t

######             val id : t -> t


######         end

######         module B : sig

######             type t

######             val id : t -> t


######         end

######         module C : sig

######             type t

######             val id : t -> t


######         end

######         module D : sig

######             type t

######             val id : t -> t


######         end

######         module E : sig

######             type t

######             val id : t -> t


######         end


######     end

######     module A' = Foo.A

######     type tata = Foo.A.t

######     type tbtb = Foo.B.t

######     type tete

######     type tata' = A'.t

######     type tete2 = Foo.E.t

######     module Std : sig

######         module A = Foo.A

######         module B = Foo.B

######         module C = Foo.C

######         module D = Foo.D

######         module E = Foo.E


######     end

######     type stde = Std.E.t


### include of Foo
---

Just for giggle, let's see what happens when we include  `` Foo `` .
######     module A = Foo.A

######     module B = Foo.B

######     module C = Foo.C

######     module D = Foo.D

######     module E : sig

######         type t

######         val id : t -> t


######     end

######     type testa = A.t

And also, let's refer to  `` A.t ``  and  `` Foo.B.id `` 
######     module P1 : sig

######         module Y : sig

######             type t

######             val id : t -> t


######         end


######     end

######     module P2 : sig

######         module Z = Z


######     end

######     module X1 = P2.Z

######     module X2 = P2.Z

######     type p1 = X1.t

######     type p2 = X2.t


###### end

Let's imitate jst's layout.

# Section title splicing

I can refer to
-  `` {!section:indexmodules} ``  : Trying the {!modules: ...} command.

-  `` {!aliases} ``  : Aliases again
But also to things in submodules:
-  `` {!section:SuperSig.SubSigA.subSig} ``  :  `` SuperSig `` .SubSigA.subSig

-  `` {!Aliases.incl} ``  :  `` incl `` 
And just to make sure we do not mess up:
-  `` {{!section:indexmodules}A} ``  : A

-  `` {{!aliases}B} ``  : B

-  `` {{!section:SuperSig.SubSigA.subSig}C} ``  : C

-  `` {{!Aliases.incl}D} ``  : D

# New reference syntax

###### module type M = sig

######     type t


###### end

###### module M : sig

######     type t


###### end

Here goes:
-  `` {!module-M.t} ``  :  `` M.t `` 

-  `` {!module-type-M.t} ``  :  `` M.t `` 
###### module Only_a_module : sig

######     type t


###### end

-  `` {!Only_a_module.t} ``  :  `` Only_a_module.t `` 

-  `` {!module-Only_a_module.t} ``  :  `` Only_a_module.t `` 

-  `` {!module-Only_a_module.type-t} ``  :  `` Only_a_module.t `` 

-  `` {!type:Only_a_module.t} ``  :  `` Only_a_module.t `` 
###### module type TypeExt = sig

######     type t = ..

######     type t += 
######         | C



######     val f : t -> unit


###### end

###### type new_t = ..

###### type new_t += 
######     | C



###### module type TypeExtPruned = sig

######     type new_t += 
######         | C



######     val f : new_t -> unit


###### end

