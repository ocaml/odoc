class type empty =
object
end

class type mutually =
object
end

and recursive =
object
end

class mutually' : mutually
and recursive' : recursive

class type virtual empty_virtual =
object
end

class virtual empty_virtual' : empty

class type ['a] polymorphic =
object
end

class ['a] polymorphic' : ['a] polymorphic

(* Comments *)

class x : object end

class ['a] c :
  object
    inherit x
    (** Inherit. *)

    constraint 'a = int
    (** Constraint. *)

    (**/**)

    method foo : int

    (**/**)

    (** Floating comment. *)

    method bar : int
  end
