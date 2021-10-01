Recent_impl

Module  `` Recent_impl `` 

###### module Foo : sig

######     module A : sig

######         type t = 
######             | A




######     end

######     module B : sig

######         type t = 
######             | B




######     end


###### end

###### module B : sig

######     type t = 
######         | B




###### end

###### type u

###### module type S = sig

######     module F : sig


## Parameters
---

######         module _ : sig
######         end


## Signature
---

######         type t


######     end

######     module X : sig
######     end

######     val f : F(X).t


###### end

###### module B' = Foo.B

