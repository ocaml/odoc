module M : sig
  module type N = sig
    type t
  end
end

class type t = object val v : int end

class foo : object val v : int end

