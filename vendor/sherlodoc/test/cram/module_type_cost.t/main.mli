module M : sig
  val my_function : int -> int
end

module type S = sig
  val my_function : int -> int
end

module type Module_type = sig end

module Module_nype : sig end

module Make (M : S) : S