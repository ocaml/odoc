module A_nonhidden : sig
  (** @canonical Test.A *)

  type t
end

module B__hidden : sig
  type t
end

module C__hidden : sig
  (** @canonical Test.C *)

  type t
end

(**/**)

module D_hidden : sig
  (** @canonical Test.D *)

  type t
end

(**/**)

(** This should not have an expansion *)
module A = A_nonhidden

(** This should have an expansion *)
module B = B__hidden

(** This should have an expansion *)
module C = C__hidden

(** This also should have an expansion *)
module D = D_hidden


(** This should render as A.t but link to A_nonhidden/index.html - since A has no expansion *)
type a = A_nonhidden.t

(** This should have no RHS as it's hidden and there is no canonical alternative *)
type b = B__hidden.t

(** This should render as C.t and link to C/index.html *)
type c = C__hidden.t

(** This should render as D.t and link to D/index.html *)
type d = D_hidden.t

