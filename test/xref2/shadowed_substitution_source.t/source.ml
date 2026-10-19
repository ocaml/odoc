module type S = sig
  type t = int
  type t := t list
  module type T = sig end
  module type T := sig type q end
  module M : sig end
  module M := Stdlib.List
end
