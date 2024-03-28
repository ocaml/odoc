module Hidden__ : sig
  type t
end

type t =
  | Variant of int
  | Hidden of Hidden__.t

type u =
  { not_hidden : int
  ; hidden : Hidden__.t }

