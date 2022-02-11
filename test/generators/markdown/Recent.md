Recent

Module `Recent`

<a id="module-type-S"></a>

###### module type [S](Recent.module-type-S.md)

<a id="module-type-S1"></a>

###### module type [S1](Recent.module-type-S1.md)

<a id="type-variant"></a>

###### type variant = 

<a id="type-variant.A"></a>

> | A

<a id="type-variant.B"></a>

> | B of int

<a id="type-variant.C"></a>

> | C

foo

<a id="type-variant.D"></a>

> | D

_bar_

<a id="type-variant.E"></a>

> | E of {

<a id="type-variant.a"></a>

> a : int;

}

<a id="type-gadt"></a>

###### type _ gadt = 

<a id="type-gadt.A"></a>

> | A : int [gadt](#type-gadt)

<a id="type-gadt.B"></a>

> | B : int -> string [gadt](#type-gadt)

foo

<a id="type-gadt.C"></a>

> | C : {

<a id="type-gadt.a"></a>

> a : int;

} -> unit [gadt](#type-gadt)

<a id="type-polymorphic_variant"></a>

###### type polymorphic_variant = [ 

<a id="type-polymorphic_variant.A"></a>

> | `A

<a id="type-polymorphic_variant.B"></a>

> | `B of int

<a id="type-polymorphic_variant.C"></a>

> | `C

foo

<a id="type-polymorphic_variant.D"></a>

> | `D

bar

 ]

<a id="type-empty_variant"></a>

###### type empty_variant =

> |

<a id="type-nonrec_"></a>

###### type nonrec nonrec_ =

> int

<a id="type-empty_conj"></a>

###### type empty_conj = 

<a id="type-empty_conj.X"></a>

######    | X : [< \`X of & 'a & int * float ] ->
[empty_conj](#type-empty_conj)

<a id="type-conj"></a>

###### type conj = 

<a id="type-conj.X"></a>

######    | X : [< \`X of int & [< \`B of int & float ] ] ->
[conj](#type-conj)

<a id="val-empty_conj"></a>

###### val empty_conj :

> [< \`X of & 'a & int * float ]

<a id="val-conj"></a>

###### val conj :

> [< \`X of int & [< \`B of int & float ] ]

<a id="module-Z"></a>

###### module [Z](Recent.Z.md)

<a id="module-X"></a>

###### module [X](Recent.X.md)

<a id="module-type-PolyS"></a>

###### module type [PolyS](Recent.module-type-PolyS.md)
