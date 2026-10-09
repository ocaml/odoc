(** This type has an atomic annotation, but it is evaluated on an older
    compiler which does not support it, so it shouldn't be rendered. *)
type refcounted = {
  mutable readers : int [@atomic];
}
