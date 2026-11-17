module type S = sig
  module M : sig
    module E : sig
      type t
    end
  end
end
