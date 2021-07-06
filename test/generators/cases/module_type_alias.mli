(** Module Type Aliases *)


module type A = sig
  type a
end


module type B = functor ( C : sig type c end ) -> sig
  type b
end

module type D = A

module type E = functor ( F : sig type f end ) -> B

module type G = functor ( H : sig type h end) -> D

module type I = B

