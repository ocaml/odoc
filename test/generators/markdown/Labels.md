Labels

Module  `` Labels `` 


# Attached to unit


# Attached to nothing

###### module A : sig


# Attached to module


###### end

###### type t

Attached to type
###### val f : t

Attached to value
###### val e : unit -> t

Attached to external
###### module type S = sig


### Attached to module type
---


###### end

###### class  c : object


# Attached to class


###### end

###### class type  cs = object


### Attached to class type
---


###### end

###### exception E

Attached to exception
###### type x = ..

###### type x += 
######     | X



Attached to extension
###### module S := A

Attached to module subst
###### type s := t

Attached to type subst
###### type u = 
######     | A'

Attached to constructor


###### type v = {
######      `` f : t; `` 

Attached to field
###### }

Testing that labels can be referenced
- Attached to unit

- Attached to nothing

- Attached to module

- Attached to type

- Attached to value

- Attached to class

- Attached to class type

- Attached to exception

- Attached to extension

- Attached to module subst

- Attached to type subst

- Attached to constructor

- Attached to field
