type refcounted = {

  (** This field has an atomic annotation which should appear in the docs.  *)
  mutable readers : int [@atomic];
}

(** This exception has a record with an atomic annotation that should appear *)
exception Atomic_exn of {mutable value : int [@atomic]}

(** The annotation should appear on anonymous records in constructors as well *)
type with_annot = Atomic_constr of {mutable value : int [@atomic]}
