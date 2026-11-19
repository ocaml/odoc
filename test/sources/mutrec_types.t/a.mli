type t = int

type a = A of b | Nil
and b = B of a

type c = { x : d }
and d = { y : c option }
