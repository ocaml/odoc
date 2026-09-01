module type S = sig
  type t
  type u

  val v : u

  module type H = sig val h : int end
  module Make (X : H) : sig val mk : t end
end

module X0 : sig
  include S
end

module X = X0

module User : sig
  type t = X.t
  include module type of X with type t := X.t
end

module User2 : sig
  include module type of X
end
