OcamlaryDep1

Module  `` Ocamlary.Dep1 `` 

###### module type S = sig

######     class  c : object

######         method m : int


######     end


###### end

###### module X : sig

######     module Y : sig

######         class  c : object

######             method m : int


######         end


######     end


###### end

