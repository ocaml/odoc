This is an excerpt from ocamlary, showing an issue resolving hidden modules that
aren't roots.
  $ cat test.mli
  module CanonicalTest : sig
    module Base__List : sig
      type 'a t
  
      val id : 'a t -> 'a t
    end
  
    module Base__ : sig
      module List = Base__List
      (** @canonical Test.CanonicalTest.Base.List *)
    end
  
    module Base : sig
      module List = Base__.List
    end
  
    module Base_Tests : sig
      module C : module type of Base__.List
  
      open Base__
      module L = List
  
      val baz : 'a Base__.List.t -> unit
      (** We can't reference [Base__] because it's hidden.
          {!List.t} ([List.t]) should resolve. *)
    end
  end
  
  val test : 'a CanonicalTest.Base__.List.t -> unit
  


  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti

This shouldn't cause any warnings:

  $ odoc link test.odoc -I .
  File "test.odoc":
  Warning: Failed to lookup type identifier((root Test).CanonicalTest, false).Base__.List.t Parent_module: Parent_module: Find failure
  File "test.mli", line 25, characters 8-17:
  Warning: Failed to resolve reference unresolvedroot(List).t Couldn't find "List"




