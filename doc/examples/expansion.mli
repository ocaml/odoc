(** Examples of different features of Expansion *)

(** For details on what each of the following examples is showing,
see the explanations in the {{!page-features}Features page} *)

[@@@warning "-67"]
module Simple : sig
  (** Demonstrates simple expansion with a type equality *)

  module StringSet : Stdlib.Set.S with type t = string
end

module Aliases : sig
  (** Demonstrates expansion when a module is an alias to a hidden module. *)

  module Hidden__module : sig
    type t

    val f : t -> t
  end

  module Alias = Hidden__module
end

module ModuleTypeAliases : sig
  (** Demonstrates that module types are not expanded if they're a simple path to another. *)

  module type A = sig
    type t
  end

  module type B = A
end

module ModuleTypeAliases2 : sig
  (** Demonstrates that module types 'aliases' are produced by strengthening *)

  module A : sig
    module type A = sig
      type t
    end

    module X : A
  end

  module B : module type of struct
    include A
  end
end

module Functors : sig
  (** Demonstrates the expansion of functors *)

  module type Argument = sig
    type a
    (** This type [a] is declared in the Argument module type *)
  end

  module type Result = sig
    type r
    (** This type [r] is declared in the Result module type *)
  end

  module Functor : functor (X : Argument) (Y : Argument) -> Result
end

module Include : sig
  (** Demonstrates handling of include statements *)

  module type ToBeIncluded = sig
    type t

    val f : t -> t
    (** The description of [f] *)
  end

  module A : sig
    include ToBeIncluded

    val g : t -> t
  end

  module B : sig
    include ToBeIncluded
    (** @inline *)

    val g : t -> t
  end
end

module Shadowing : sig
  (** Demonstrates shadowing of identifiers in includes *)

  module type A = sig
    type t = int

    val f : t
  end

  module type B = sig
    include A

    type t = string

    val g : t
  end
end

module DeepEquality : sig
  (** Demonstrates expansion involving an equation on a type in a submodule *)

  module type SIG = sig
    type t
  end

  module type MODTYPE = sig
    module X : SIG

    module Y : SIG
  end

  type foo

  module M : MODTYPE with type X.t = foo
end

module DeepEquality2 : sig
  (** Demonstrates expansion involving an equation on a type in a submodule, but the submodule is already a simple signature *)

  module type MODTYPE = sig
    module X : sig
      type t
    end

    module Y : sig
      type t
    end
  end

  type foo

  module M : MODTYPE with type X.t = foo
end

module TypeSubstitution : sig
  (** Demonstrates expansion involving deep destructive type substitution. *)

  module type S = sig
    module M : sig
      type t
    end

    type t = M.t
  end

  module type T = S with type M.t := int
end

module ModuleTypeOf : sig
  (** Demonstrates expanding after recovering the type of a module *)

  module A : sig
    type t
    (** This comment for [type t] is written in module [A] *)
  end

  module M : module type of A

  module M' : module type of struct
    include A
  end
end

module ModuleTypeOfComplications : sig
  (** Demonstrates the interaction of [module type of] and destructive module substitution *)

  module type S = sig
    module X : sig
      type t
    end

    module type Y = module type of X

    module type Z = module type of struct
      include X
    end
  end

  module X1 : sig
    type t

    type u
  end

  module type T = S with module X := X1
end
