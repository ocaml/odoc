OcamlaryCanonicalTest

Module  `` Ocamlary.CanonicalTest `` 

###### module Base : sig

######     module List : sig

######         type 'a t

######         val id : 'a t -> 'a t


######     end


###### end

###### module Base_Tests : sig

######     module C : sig

######         type 'a t

######         val id : 'a t -> 'a t


######     end

######     module L = Base.List

######     val foo : int L.t -> float L.t

######     val bar : 'a Base.List.t -> 'a Base.List.t

This is just  `` List `` .id, or rather  `` L.id `` 
######     val baz : 'a Base.List.t -> unit

We can't reference  `` Base__ ``  because it's hidden.  `` List `` .t ( `` List.t `` ) should resolve.

###### end

###### module List_modif : sig

######     type 'c t = 'c Base.List.t

######     val id : 'a t -> 'a t


###### end

