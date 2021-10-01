Functor2

Module  `` Functor2 `` 

###### module type S = sig

######     type t


###### end

###### module X : sig


# Parameters

######     module Y : sig

######         type t


######     end

######     module Z : sig

######         type t


######     end


# Signature

######     type y_t = Y.t

######     type z_t = Z.t

######     type x_t = y_t


###### end

###### module type XF = sig


## Parameters
---

######     module Y : sig

######         type t


######     end

######     module Z : sig

######         type t


######     end


## Signature
---

######     type y_t = Y.t

######     type z_t = Z.t

######     type x_t = y_t


###### end

