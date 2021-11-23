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

