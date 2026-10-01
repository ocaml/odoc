type refcounted = {

  (** This field has an atomic annotation which should appear in the docs.  *)
  mutable readers : int [@atomic];
}
