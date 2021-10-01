OcamlaryDep5

Module  `` Ocamlary.Dep5 `` 


# Parameters

###### module Arg : sig

######     module type T

######     module type S = sig

######         module X : T

######         module Y : sig
######         end


######     end

######     module X : T


###### end


# Signature

###### module Z : sig

######     module X : Arg.T

######     module Y = Dep3


###### end

