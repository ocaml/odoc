Toplevel_comments

Module  `` Toplevel_comments `` 

A doc comment at the beginning of a module is considered to be that module's doc.
###### module type T = sig

######     type t


###### end

Doc of  `` T `` , part 1.
###### module Include_inline : sig

######     type t


###### end

Doc of  `` T `` , part 2.
###### module Include_inline' : sig

######     type t


###### end

Doc of  `` Include_inline `` , part 1.
###### module type Include_inline_T = sig

######     type t


###### end

Doc of  `` T `` , part 2.
###### module type Include_inline_T' = sig

######     type t


###### end

Doc of  `` Include_inline_T' `` , part 1.
###### module M : sig
###### end

Doc of  `` M `` 
###### module M' : sig
###### end

Doc of  `` M' ``  from outside
###### module M'' : sig
###### end

Doc of  `` M'' `` , part 1.
###### module Alias : sig

######     type t


###### end

Doc of  `` Alias `` .
###### class  c1 : object
###### end

Doc of  `` c1 `` , part 1.
###### class type  ct = object
###### end

Doc of  `` ct `` , part 1.
###### class  c2 : object
###### end

Doc of  `` c2 `` .
###### module Ref_in_synopsis : sig

######     type t


###### end

 `` t `` .
