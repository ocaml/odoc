OcamlaryRecollection

Module  `` Ocamlary.Recollection `` 


# Parameters

###### module C : sig

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


# Signature

This comment is for  `` CollectionModule `` .
###### type collection = C.element list

This comment is for  `` collection `` .
###### type element = C.collection

###### module InnerModuleA : sig

######     type t = collection

This comment is for  `` t `` .
######     module InnerModuleA' : sig

######         type t = (unit, unit) a_function

This comment is for  `` t `` .

######     end

This comment is for  `` InnerModuleA' `` .
######     module type InnerModuleTypeA' = sig

######         type t = InnerModuleA'.t

This comment is for  `` t `` .

######     end

This comment is for  `` InnerModuleTypeA' `` .

###### end

This comment is for  `` InnerModuleA `` .
###### module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'

This comment is for  `` InnerModuleTypeA `` .
