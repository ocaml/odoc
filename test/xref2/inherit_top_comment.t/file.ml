module X__Hidden = struct
  (** Top comment of [X__Hidden] *)

  let x = 0
end

module X = struct
  include X__Hidden
end
