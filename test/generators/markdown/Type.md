Type

Module `Type`

<a id="type-abstract"></a>

###### type abstract

Some _documentation_ .

<a id="type-alias"></a>

###### type alias =

> int

<a id="type-private_"></a>

###### type private_ =

> private int

<a id="type-constructor"></a>

###### type 'a constructor =

> 'a

<a id="type-arrow"></a>

###### type arrow =

> int -> int

<a id="type-higher_order"></a>

###### type higher_order =

> ( int -> int ) -> int

<a id="type-labeled"></a>

###### type labeled =

> l : int -> int

<a id="type-optional"></a>

###### type optional =

> ? l : int -> int

<a id="type-labeled_higher_order"></a>

###### type labeled_higher_order =

> ( l : int -> int ) -> ( ? l : int -> int ) -> int

<a id="type-pair"></a>

###### type pair =

> int * int

<a id="type-parens_dropped"></a>

###### type parens_dropped =

> int * int

<a id="type-triple"></a>

###### type triple =

> int * int * int

<a id="type-nested_pair"></a>

###### type nested_pair =

> ( int * int ) * int

<a id="type-instance"></a>

###### type instance =

> int [constructor](#type-constructor)

<a id="type-long"></a>

###### type long =

>   [labeled_higher_order](#type-labeled_higher_order) ->   [ `Bar | `Baz of [triple](#type-triple) ] ->   [pair](#type-pair) ->   [labeled](#type-labeled) ->   [higher_order](#type-higher_order) ->   ( string -> int ) ->   ( int * float * char * string * char * unit ) option ->   [nested_pair](#type-nested_pair) ->   [arrow](#type-arrow) ->   string ->   [nested_pair](#type-nested_pair) array

<a id="type-variant_e"></a>

###### type variant_e = {

<a id="type-variant_e.a"></a>

######    `;int : a`

}

<a id="type-variant"></a>

###### type variant =

<a id="type-variant.A"></a>

######    | A

<a id="type-variant.B"></a>

######    | B of int

<a id="type-variant.C"></a>

######    | C

foo

<a id="type-variant.D"></a>

######    | D

_bar_

<a id="type-variant.E"></a>

######    | E of [variant_e](#type-variant_e)

<a id="type-variant_c"></a>

###### type variant_c = {

<a id="type-variant_c.a"></a>

######    `;int : a`

}

<a id="type-gadt"></a>

###### type _ gadt =

<a id="type-gadt.A"></a>

######    | A : int [gadt](#type-gadt)

<a id="type-gadt.B"></a>

######    | B : int -> string [gadt](#type-gadt)

<a id="type-gadt.C"></a>

######    | C : [variant_c](#type-variant_c) -> unit [gadt](#type-gadt)

<a id="type-degenerate_gadt"></a>

###### type degenerate_gadt =

<a id="type-degenerate_gadt.A"></a>

######    | A : [degenerate_gadt](#type-degenerate_gadt)

<a id="type-private_variant"></a>

###### type private_variant = private

<a id="type-private_variant.A"></a>

######    | A

<a id="type-record"></a>

###### type record = {

<a id="type-record.a"></a>

######    `;int : a`

<a id="type-record.b"></a>

######    `;int : b mutable`

<a id="type-record.c"></a>

######    `;int : c`

foo

<a id="type-record.d"></a>

######    `;int : d`

_bar_

<a id="type-record.e"></a>

######    `;'a'a.  : e`

}

<a id="type-polymorphic_variant"></a>

###### type polymorphic_variant = [

<a id="type-polymorphic_variant.A"></a>

######    `| ` `` `A ``

<a id="type-polymorphic_variant.B"></a>

######    `| ` `` int of `B ``

<a id="type-polymorphic_variant.C"></a>

######    `| ` `` unit*  int of `C ``

<a id="type-polymorphic_variant.D"></a>

######    `| ` `` `D ``

]

<a id="type-polymorphic_variant_extension"></a>

###### type polymorphic_variant_extension = [

<a id="type-polymorphic_variant_extension.polymorphic_variant"></a>

######    `| ` [polymorphic_variant](#type-polymorphic_variant)

<a id="type-polymorphic_variant_extension.E"></a>

######    `| ` `` `E ``

]

<a id="type-nested_polymorphic_variant"></a>

###### type nested_polymorphic_variant = [

<a id="type-nested_polymorphic_variant.A"></a>

######    `| ` `` [ `B | `C ] of `A ``

]

<a id="type-private_extenion#row"></a>

###### type private_extenion#row

<a id="type-private_extenion"></a>

###### and private_extenion = private [>

<a id="type-private_extenion.polymorphic_variant"></a>

######    `| ` [polymorphic_variant](#type-polymorphic_variant)

]

<a id="type-object_"></a>

###### type object_ =

> < a : int ; b : int ; c : int >

<a id="module-type-X"></a>

###### module type [X](Type.module-type-X.md)

<a id="type-module_"></a>

###### type module_ =

> ( module [X](Type.module-type-X.md) )

<a id="type-module_substitution"></a>

###### type module_substitution =

> ( module [X](Type.module-type-X.md) with type [t](Type.module-type-X.md#type-t) = int and type [u](Type.module-type-X.md#type-u) = unit )

<a id="type-covariant"></a>

###### type +'a covariant

<a id="type-contravariant"></a>

###### type -'a contravariant

<a id="type-bivariant"></a>

###### type _ bivariant =

> int

<a id="type-binary"></a>

###### type ('a, 'b) binary

<a id="type-using_binary"></a>

###### type using_binary =

> ( int , int ) [binary](#type-binary)

<a id="type-name"></a>

###### type 'custom name

<a id="type-constrained"></a>

###### type 'a constrained =

> 'a constraint 'a = int

<a id="type-exact_variant"></a>

###### type 'a exact_variant =

> 'a constraint 'a = [ `A | `B of int ]

<a id="type-lower_variant"></a>

###### type 'a lower_variant =

> 'a constraint 'a = [> `A | `B of int ]

<a id="type-any_variant"></a>

###### type 'a any_variant =

> 'a constraint 'a = [> ]

<a id="type-upper_variant"></a>

###### type 'a upper_variant =

> 'a constraint 'a = [< `A | `B of int ]

<a id="type-named_variant"></a>

###### type 'a named_variant =

> 'a constraint 'a = [< [polymorphic_variant](#type-polymorphic_variant) ]

<a id="type-exact_object"></a>

###### type 'a exact_object =

> 'a constraint 'a = < a : int ; b : int >

<a id="type-lower_object"></a>

###### type 'a lower_object =

> 'a constraint 'a = < a : int ; b : int .. >

<a id="type-poly_object"></a>

###### type 'a poly_object =

> 'a constraint 'a = < a : 'a. 'a >

<a id="type-double_constrained"></a>

###### type ('a, 'b) double_constrained =

> 'a * 'b constraint 'a = int constraint 'b = unit

<a id="type-as_"></a>

###### type as_ =

> int as ' a * 'a

<a id="type-extensible"></a>

###### type extensible =

> ..

<a id="extension-decl-Extension"></a>

###### type [extensible](#type-extensible) +=

<a id="extension-Extension"></a>

######    | Extension

Documentation for [`Extension`](#extension-Extension) .

<a id="extension-Another_extension"></a>

######    | Another_extension

Documentation for [`Another_extension`](#extension-Another_extension) .

<a id="type-mutually"></a>

###### type mutually =

<a id="type-mutually.A"></a>

######    | A of [recursive](#type-recursive)

<a id="type-recursive"></a>

###### and recursive =

<a id="type-recursive.B"></a>

######    | B of [mutually](#type-mutually)

<a id="exception-Foo"></a>

###### exception Foo of int * int
