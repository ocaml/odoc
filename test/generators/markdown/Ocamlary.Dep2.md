OcamlaryDep2

Module  `` Ocamlary.Dep2 `` 


# Parameters

###### module Arg : sig

######     module type S

######     module X : sig

######         module Y : S


######     end


###### end


# Signature

###### module A : sig

######     module Y : Arg.S


###### end

###### module B = A.Y

