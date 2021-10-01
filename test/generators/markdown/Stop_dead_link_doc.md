Stop_dead_link_doc

Module  `` Stop_dead_link_doc `` 

###### module Foo : sig

######     type t


###### end

###### type foo = 
######     | Bar of Foo.t



###### type bar = 
######     | Bar of {
######      `` field : Foo.t; `` 

###### }



###### type foo_ = 
######     | Bar_ of int * Foo.t * int



###### type bar_ = 
######     | Bar__ of Foo.t option



###### type another_foo

###### type another_bar

###### type another_foo_

###### type another_bar_

