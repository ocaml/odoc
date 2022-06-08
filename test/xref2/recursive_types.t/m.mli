type t

module M : sig
  type nonrec t = Foo of t
end

type x = Foo of y
and y = Bar of x

