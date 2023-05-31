(** Examples of Path, Fragment and Reference Resolution *)

(** This module contains examples of some of the features of Resolution
as described in the page {!page-features}. See the explanations there for
details on what each of these demonstrates. *)

[@@@warning "-67"]

module Alias : sig
  (** Demonstrates a reference to an item in a module that's an alias *)

  module A : sig
    type t
  end

  module B = A

  type t = B.t

  module type A = sig
    type t
  end

  module type B = A

  (** {!module-type-B.t} *)
end

module HiddenAlias : sig
  (** Demonstrates a reference to an item in a module that's an alias of a
  hidden module. *)

  (**/**)

  module A : sig
    type t
  end

  (**/**)

  module B = A

  type t = B.t
end

module Canonical : sig
  (** Demonstrates the use of canonical tags *)

  module A : sig
    (** @canonical Odoc_examples.Resolution.Canonical.B *)

    type t
  end

  module B = A

  type t = A.t
end

module Fragments : sig
  (** Demonstrates the resolution of fragments *)

  module type A = sig
    module B : sig
      type t

      val f : t -> t
    end
  end

  module C : A with type B.t = int

  module D : module type of C.B with type t := int
end

module Hidden : sig
  (** Demonstrates paths to hidden items *)

  (**/**)

  type t = int

  type u

  (**/**)

  type v = T of t

  type w = U of u
end

module References : sig
  (** Examples of resolution of references *)

  module type A = sig
    type t
    (** type [t] in module type [A] *)
  end

  module A : sig
    type t
    (** type [t] in module [A] *)

    module B : sig
      type t
    end

    module type B = sig
      type t
    end
  end

  (** We can refer unambiguously to {!module-type-A.t} in module type [A] or {!module-A.t} in module [A],
and also where there are name clashes within the path: {!module-A.module-B.t} or {!module-A.module-type-B.t} *)
end

module Complicated_1 : sig
  (** Some more complicated examples of resolution *)

  module type A = sig
    module M : sig
      module type S
    end

    module N : M.S
  end

  module B : sig
    module type S = sig
      type t
    end
  end

  module C : A with module M = B with type N.t = int

  type t = C.N.t
end

module Complicated_2 : sig
  (** A very complicated example of resolution *)

  module type Type = sig
    module type T
  end

  module App : functor
    (T : Type)
    (F : functor (_ : Type) -> Type)
    (M : F(T).T)
    -> F(T).T

  module Bar : sig
    module type T = sig
      type bar
    end
  end

  module Foo : functor (T : Type) -> sig
    module type T = sig
      module Foo : T.T
    end
  end

  module FooBarInt : sig
    module Foo : sig
      type bar = int
    end
  end

  type t = App(Bar)(Foo)(FooBarInt).Foo.bar
end
