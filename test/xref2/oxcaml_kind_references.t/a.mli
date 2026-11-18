kind_ k = value mod portable
(** A kind abbreviation. *)

type k
(** A type with the same name as the kind. *)

module M : sig
  kind_ mk = value mod contended
end

module type S = sig
  kind_ sk = value mod portable
end

(** References:
    - {!kind-k} at the toplevel, distinct from {!type-k};
    - {!M.kind-mk} and {!module-M.kind-mk} in a submodule;
    - {!S.kind-sk} in a module type;
    - {{!kind-k}with text};
    - {!kind-mk} is not in scope;
    - {!kind-value} is built in, it has no declaration;
    - {!kind-k.foo} is not a valid reference. *)
type t
