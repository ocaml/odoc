module M = struct
  type t = A
end

module type S = sig
  type t = A
end

open (M : S)

