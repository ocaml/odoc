type t = string

type truc = A | B

let xazaz = A

module Yoyo = struct
  type bli = Aa | Bb
end

let segr = Yoyo.Aa

let x = 2
let y = x + 1
let z a = if x = 1 || true then x + y else a
let z' a = if x = 1 || true then x + y else a

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
  module A : sig end
end) =
struct
  module B = M.A
end

module FM = F (struct
  module A = struct end
end)

module FF (A : sig end) (B : sig end) = struct end
module FF2 (A : sig
  module E : sig end
end) (A : sig
  module F : sig end
end) =
struct end
