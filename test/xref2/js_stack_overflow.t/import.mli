module type S0 = sig
  module Thing : sig
    module Config : sig end
  end
end

module type S = sig
  include S0

  module Thing : sig
    module Config = Thing.Config
  end
end
