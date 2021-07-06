

module Local: sig
  type local := int * int
  module type local := sig type t = local end
  module type w = local
  module type s = sig end
end


module type s = sig end

module Basic: sig

  module type u = sig
    module type T = sig end
  end

  module type with_ = u with module type T = s

  module type u2 = sig
    module type T = sig end
    module M:T
  end

  module type with_2 = u2 with module type T = sig end

  module type a = sig
    module type b = s
    module M: b
  end

  module type c = a with module type b := s
end

module Nested : sig

  module type nested = sig
    module N: sig
      module type t = sig end
    end
  end

  module type with_ = nested with module type N.t = s
  module type with_subst = nested with module type N.t := s
end

module Structural: sig
  module type u = sig
    module type a = sig
      module type b = sig
        module type c = sig
          type t = A of t
        end
      end
    end
  end

  module type w = u
    with module type a =
         sig
           module type b = sig
             module type c = sig
               type t = A of t
             end
           end
         end
end
