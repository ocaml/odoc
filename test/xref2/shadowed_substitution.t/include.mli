module type S = sig
  type t := int
  val x : t
  type t = string
  val y : t
end

module type I = sig
  include S
end

module type J = sig
  include sig
    include S
    type u := float
    val z : u
  end
end
