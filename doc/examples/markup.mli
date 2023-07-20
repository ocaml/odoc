(** Markup examples. *)

(** The OCaml manual gives a
{{:https://ocaml.org/manual/ocamldoc.html#ss:ocamldoc-placement}comprehensive example}
of comment placement. This has been replicated in the module Foo below to
show how this is rendered by [odoc]. *)

module type Foo = sig
  (** The first special comment of the file is the comment associated
    with the whole module.*)

  (** Special comments can be placed between elements and are kept
    by the OCamldoc tool, but are not associated to any element.
    [@]-tags in these comments are ignored.*)

  (*******************************************************************)
  (** Comments like the one above, with more than two asterisks,
    are ignored. *)

  (** The comment for function f. *)
  val f : int -> int -> int
  (** The continuation of the comment for function f. *)

  (* Hello, I'm a simple comment :-) *)
  exception My_exception of (int -> int) * int
  (** Comment for exception My_exception, even with a simple comment
    between the special comment and the exception.*)

  (** Comment for type weather  *)
  type weather =
    | Rain of int  (** The comment for constructor Rain *)
    | Sun  (** The comment for constructor Sun *)

  (** Comment for type weather2  *)
  type weather2 =
    | Rain of int  (** The comment for constructor Rain *)
    | Sun  (** The comment for constructor Sun *)
  (** I can continue the comment for type weather2 here
  because there is already a comment associated to the last constructor.*)

  (** The comment for type my_record *)
  type my_record = {
    foo : int;  (** Comment for field foo *)
    bar : string;  (** Comment for field bar *)
  }
  (** Continuation of comment for type my_record *)

  (** Comment for foo *)
  val foo : string
  (** This comment is associated to foo and not to bar. *)

  val bar : string
  (** This comment is associated to bar. *)

  class cl : object
    (** Interesting information about cl *)
  end

  (** The comment for class my_class *)
  class my_class : object
    inherit cl
    (** A comment to describe inheritance from cl *)

    val mutable tutu : string
    (** The comment for attribute tutu *)

    val toto : int
    (** The comment for attribute toto. *)

    (** This comment is not attached to titi since
        there is a blank line before titi, but is kept
        as a comment in the class. *)

    val titi : string

    method toto : string
    (** Comment for method toto *)

    method m : float -> int
    (** Comment for method m *)
  end

  (** The comment for the class type my_class_type *)
  class type my_class_type = object
    val mutable x : int
    (** The comment for variable x. *)

    method m : int -> int
    (** The comment for method m. *)
  end

  (** The comment for module Foo *)
  module Foo : sig
    val x : int
    (** The comment for x *)

    (** A special comment that is kept but not associated to any element *)
  end

  (** The comment for module type my_module_type. *)
  module type my_module_type = sig
    val x : int
    (** The comment for value x. *)

    (** The comment for module M. *)
    module M : sig
      val y : int
      (** The comment for value y. *)

      (* ... *)
    end
  end
end

module Stop : sig
  (** This module demonstrates the use of stop comments ([(**/**)]) *)

  class type foo = object
    method m : string
    (** comment for method m *)

    (**/**)

    method bar : int
    (** This method won't appear in the documentation *)
  end

  val foo : string
  (** This value appears in the documentation, since the Stop special comment
      in the class does not affect the parent module of the class.*)

  (**/**)

  val bar : string
  (** The value bar does not appear in the documentation.*)

  (**/**)

  type t = string
  (** The type t appears since in the documentation since the previous stop comment
  toggled off the "no documentation mode". *)
end

(** {2 Scoping rules} *)
module Scope : sig
  (** In this floating comment I can refer to type {!t} and value {!v}
    declared later in the signature *)

  type t

  val v : t

  val x : int

  val y : int

  module A : sig
    (** In this module I can refer to val {!x} declared above as well as
      type {!u} declared later in the parent module. Elements declared
      in this signature take priority, so {!y} refers to {!A.y} as
      opposed to the [y] declared in the parent signature.
      
      @see 'markup.mli' for a good time *)

    val y : string
  end

  type u
end

module Preamble_examples : sig
  (** This module demonstrates the various ways that preambles are calculated *)

  (** This is the comment attached to the declaration of Hidden__Module *)
  module Hidden__Module : sig
    (** This is the top comment declared in the module Hidden__module.
     
     This is the second paragraph in the module Hidden__module.
     
     @canonical Odoc_examples.Markup.Module *)

    type t
    (** This is a comment on type t *)
  end

  module Module = Hidden__Module
  (** This comment is on the declaration of Module as an alias of Hidden__Module *)

  (** This is the comment attached to the declaration of module Hidden__Module2 *)
  module Hidden__Module2 : sig
    (** This is the top comment declared in the module Hidden__module2.
       
       This is the second paragraph in the module Hidden__module2.
       
       @canonical Odoc_examples.Markup.Module2 *)

    type t
    (** This is a comment on type t *)
  end

  module Module2 = Hidden__Module2

  module Nonhidden_module : sig
    (** This is the top comment declared in the module Hidden__module2.
       
       This is the second paragraph in the module Hidden__module2.
    *)
  end

  module Module3 = Nonhidden_module
  (** This comment is on the declaration of Module3 as an alias of Nonhidden_module *)

  module Nonhidden_module2 : sig
    (** This is the top comment declared in the module Hidden__module2.
       
       This is the second paragraph in the module Hidden__module2.
    *)
  end

  module Module4 = Nonhidden_module2

  (** The [modules] special reference can be used to refer to a list of modules.
It uses the synopsis from the modules  *)
end
