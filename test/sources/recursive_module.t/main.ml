module rec A : sig
  type t = B.t
end =
  A

and B : sig
  type t = Cons of A.t
end =
  B
