Include

Module  `` Include `` 

###### module type Not_inlined = sig

######     type t


###### end

###### type t

###### module type Inlined = sig

######     type u


###### end

###### type u

###### module type Not_inlined_and_closed = sig

######     type v


###### end

include Not_inlined_and_closed###### module type Not_inlined_and_opened = sig

######     type w


###### end

###### type w

###### module type Inherent_Module = sig

######     val a : t


###### end

###### module type Dorminant_Module = sig

######     val a : u


###### end

###### val a : u

