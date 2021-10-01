OcamlaryDep6

Module  `` Ocamlary.Dep6 `` 

###### module type S = sig

######     type d


###### end

###### module type T = sig

######     module type R = S

######     module Y : sig

######         type d


######     end


###### end

###### module X : sig

######     module type R = S

######     module Y : sig

######         type d


######     end


###### end

