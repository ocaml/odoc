Include2

Module  `` Include2 `` 

###### module X : sig

######     type t = int


###### end

Comment about X that should not appear when including X below.
Comment about X that should not appear when including X below.
###### type t = int

###### module Y : sig

######     type t


###### end

Top-comment of Y.
###### module Y_include_synopsis : sig

######     type t = Y.t


###### end

The  `` include Y ``  below should have the synopsis from  `` Y `` 's top-comment attached to it.
###### module Y_include_doc : sig

######     type t = Y.t


###### end

