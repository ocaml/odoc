module type T = sig
  type t
end

module Int : sig
  type t = int

  val plus : t -> t -> t
end

(* Since [module type of] is interpreted "eagerly," subsequent substitutions
   should not change them, even when odoc preserves the [module type of]
   expression. Thus even though substitution can add information, we need to
   remove that information from any [module type of] for the substituted
   module. *)

module Basic : sig
  module type S = sig
    module M : T

    module N : module type of M
  end

  (* [P.N] should just have type [t] because [module type of M] has already
     evaluated to [T] *)
  module P : S with module M = Int
end

(* This should apply even when the module type that changes is nested. *)

module Wrapped_int : sig
  module M = Int
end

module Nested : sig
  module type S = sig
    module O : sig
      module M : T
    end

    module N1 : module type of O

    module N2 : module type of O.M
  end

  module P1 : S with module O = Wrapped_int

  module P2 : S with module O.M = Int
end

(* Same when the [module type of] goes through an alias, though oddly [P.t] ends
   up strengthened to [type t = M.t] since that's the module type of [M']. *)

module Via_alias : sig
  module type S = sig
    module M : T

    module M' = M

    module N : module type of M'
  end

  module P : S with module M = Int
end

(* Now we take the module type of something that _itself_ took the module type
   of [M] *)

module Cascade : sig
  module type S = sig
    module M : T

    module O : sig
      module I : module type of M
    end

    module N1 : module type of O

    (* Interestingly, this [module type of] expression is never invalidated
       and retains the link to [O.I]. *)
    module N2 : module type of O.I
  end

  module P : S with module M = Int
end

module List_of (T : T) : sig
  type t = T.t list

  val iter : t -> f:(T.t -> unit) -> unit
end

module In_functor : sig
  module type S = sig
    module M : functor (T : T) -> T

    module N1 : module type of M
    module N2 : module type of M (Int)
    module N2' : sig
      include module type of M (Int)
    end
  end

  (* [P.N] should just have type [t] because [module type of M] has already
     evaluated to [T] *)
  module P : S with module M = List_of
end

module Wrapped_list_of : sig
  module M = List_of
end

module In_nested_functor : sig
  module type S = sig
    module O : sig
      module M (_ : T) : T
    end

    (* {v
         module N1 : module type of O

         module N1' : module type of struct
           include O
         end

         module N1'' : sig
           include module type of O
         end

         module N2 : module type of O.M

         module N3 : module type of O.M (Int)

         module N3' : module type of struct
           include O.M (Int)
         end
       v} *)
    module N3'' : sig
      include module type of struct
        include O.M (Int)
      end
    end
  end

  module P1 : S with module O = Wrapped_list_of

  module P2 : S with module O.M = List_of
end

(* The same logic should apply to functor parameters, but _in reverse_ since
   functor types are contravariant in their parameters. In other words, a
   substitution can _remove_ information from a parameter type and we need
   to _put that information back_ into the [module type of] result. *)

module Identity (T : T) : T

module In_functor_parameter : sig
  module type S = sig
    module F (X : sig
      type t

      val plus : t -> t -> t
    end) : T

    module G : module type of F
    module H : sig
      include module type of struct
        include F (Int)
      end
    end
  end

  (* [P.G]'s argument type should have _both_ [plus] and [t] even though
     [Identity]'s argument type does not *)
  module P : S with module F = Identity
end
