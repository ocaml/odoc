type t = string

type truc = A | B

let x = 2
let y = x + 1
let z a = if x = 1 || true then x + y else 0

module A = struct end
module B = A

module type T = sig end
module type U = T

type ext = ..
type ext += Foo | Bar

exception Exn

class cls = object end
class cls' = cls
class type ct = object end

let x _ = raise Exn

module X : sig
  type t
end = struct
  type t = int
end

type a1 = int
and a2 = a1

module F (M : sig
  type t
end) =
struct end

module FM = F (struct
  type t = int
end)
