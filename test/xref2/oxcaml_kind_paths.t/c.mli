(** @canonical C.Canon *)
module Canon__ : sig
  kind_ ck = value mod portable
end

module Canon = Canon__

type c : Canon__.ck
