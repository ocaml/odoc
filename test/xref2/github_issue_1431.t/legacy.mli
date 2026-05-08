(** A signature with a sibling module declaration and a destructive module
    substitution whose manifest references that sibling.

    When this signature is loaded as the expansion of [include module type of
    Legacy] elsewhere, [Of_Lang] rewrites internal cross-references to local
    idents — so the manifest of [Original_components] becomes a
    [`Resolved (`Local _)] path. *)

module Components : sig
  type t
end

module Original_components := Components
