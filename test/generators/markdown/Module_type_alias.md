Module_type_alias

Module  `` Module_type_alias `` 

Module Type Aliases
###### module type A = sig

######     type a


###### end

###### module type B = sig


## Parameters
---

######     module C : sig

######         type c


######     end


## Signature
---

######     type b


###### end

###### module type D = A

###### module type E = sig


## Parameters
---

######     module F : sig

######         type f


######     end

######     module C : sig

######         type c


######     end


## Signature
---

######     type b


###### end

###### module type G = sig


## Parameters
---

######     module H : sig

######         type h


######     end


## Signature
---

######     type a


###### end

###### module type I = B

