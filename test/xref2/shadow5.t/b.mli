module type X = sig
  type t
  val z : t
end

module type Y = sig
  type t = int
  val y : t
  include X with type t := t
end

module type Z = sig
  include Y

  type nonrec t = t
end

