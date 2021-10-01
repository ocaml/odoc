OcamlaryDep7

Module  `` Ocamlary.Dep7 `` 


# Parameters

###### module Arg : sig

######     module type S

######     module type T = sig

######         module type R = S

######         module Y : R


######     end

######     module X : sig

######         module type R = S

######         module Y : R


######     end


###### end


# Signature

###### module M : sig

######     module type R = Arg.S

######     module Y : R


###### end

