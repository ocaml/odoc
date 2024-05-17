module type Y = sig
  type t = int
  val y : t
  include sig
    type nonrec t = t
    val z : t
  end with type t := t
end

module type Z = sig
  include Y

  type nonrec t = t
end

