Functor

Module  `` Functor `` 

###### module type S = sig

######     type t


###### end

###### module type S1 = sig


## Parameters
---

######     module _ : sig

######         type t


######     end


## Signature
---

######     type t


###### end

###### module F1 : sig


# Parameters

######     module Arg : sig

######         type t


######     end


# Signature

######     type t


###### end

###### module F2 : sig


# Parameters

######     module Arg : sig

######         type t


######     end


# Signature

######     type t = Arg.t


###### end

###### module F3 : sig


# Parameters

######     module Arg : sig

######         type t


######     end


# Signature

######     type t = Arg.t


###### end

###### module F4 : sig


# Parameters

######     module Arg : sig

######         type t


######     end


# Signature

######     type t


###### end

###### module F5 : sig


# Parameters


# Signature

######     type t


###### end

