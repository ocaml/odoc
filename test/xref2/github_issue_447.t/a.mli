type u = Bar

(** {!constructor-Bar} {!u.constructor-Bar} *)

module M : sig
  type t = Foo
end

(** {!M.constructor-Foo} and {!M.Foo}

    {!M.t.constructor-Foo} and {!M.t.Foo} *)

class t : object end

(** {!t.constructor-A} *)
