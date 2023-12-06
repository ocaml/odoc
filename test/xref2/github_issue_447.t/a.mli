type u = Foo

(** {!constructor-Foo} {!u.constructor-Foo} {!Foo} *)

module M : sig
  type t = Foo
end

(** {!M.constructor-Foo} and {!M.Foo}

    {!M.t.constructor-Foo} and {!M.t.Foo} *)

class t : object end

(** {!t.constructor-A} *)
