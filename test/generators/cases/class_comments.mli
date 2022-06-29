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
