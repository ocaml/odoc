Short output prints the canonical path of a module in place of the path it is
attached to. It does so only when the canonical path resolves. The canonical
path of [Hidden__other] names a module that does not exist.

  $ cat m.mli
  (** @canonical M.Pub *)
  module Hidden__pub : sig type t end
  
  module Pub = Hidden__pub
  
  (** @canonical Nonexistent.Other *)
  module Hidden__other : sig type t end
  
  module Other = Hidden__other
  $ ocamlc -bin-annot -c m.mli
  $ odoc compile m.cmti
  $ odoc link m.odoc

[Pub] prints as its canonical path. [Other] prints as the hidden module, since
its canonical path does not resolve.

  $ odoc_print --short -r Pub m.odocl
  module M.Pub =
    M.Pub
    
  $ odoc_print --short -r Other m.odocl
  module M.Other =
    M.Hidden__other
    

Long paths show both paths.

  $ odoc_print --short --long-paths -r Pub m.odocl
  module root(M).Pub =
    resolved(canonical(root(M).Hidden__pub,resolved(root(M).Pub)))
    
  $ odoc_print --short --long-paths -r Other m.odocl
  module root(M).Other =
    resolved(canonical(root(M).Hidden__other,root(Nonexistent).Other))
    
