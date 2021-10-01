Recent

Module  `` Recent `` 

###### module type S = sig
###### end

###### module type S1 = sig


## Parameters
---

######     module _ : sig
######     end


## Signature
---


###### end

###### type variant = 
######     | A


######     | B of int


######     | C

foo

######     | D

_bar_

######     | E of {
######      `` a : int; `` 

###### }



###### type _ gadt = 
######     | A : int gadt


######     | B : int -> string gadt

foo

######     | C : {
######      `` a : int; `` 

###### } -> unit gadt



###### type polymorphic_variant = [ 
######      `` |  ``  `` `A `` 


######      `` |  ``  `` `B of int `` 


######      `` |  ``  `` `C `` 

foo

######      `` |  ``  `` `D `` 

bar
 ]

###### type empty_variant = |

###### type nonrec nonrec_ = int

###### type empty_conj = 
######     | X : [< `X of & 'a & int * float ] -> empty_conj



###### type conj = 
######     | X : [< `X of int & [< `B of int & float ] ] -> conj



###### val empty_conj : [< `X of & 'a & int * float ]

###### val conj : [< `X of int & [< `B of int & float ] ]

###### module Z : sig

######     module Y : sig

######         module X : sig

######             type 'a t


######         end


######     end


###### end

###### module X : sig

######     module L := Z.Y

######     type t = int Z.Y.X.t

######     type u := int

######     type v = u Z.Y.X.t


###### end

###### module type PolyS = sig

######     type t = [ 
######          `` |  ``  `` `A `` 


######          `` |  ``  `` `B `` 

 ]


###### end

