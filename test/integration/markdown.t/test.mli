(** {1 This is a heading }*)

(** {2:label This has a label}*)

(** arrow (->) in a doc comment *)

val concat : string -> string -> string

(** {%html:foo:bar%} : a raw markup *)

type t
(** Doc for [type t]. *)

type a = t

type y'

module type Foo' = sig
  type foo
end

module Bar : sig
  type bar
end

module type Bar' = sig
  type bar'
end

module type Foo = sig
  type foo

  include Bar'

  module type Foo' = sig
    type foo'

    type days = Mon  (** Docs for [days]. *)

    type num = [ `One (** Docs for [`One]*)]
  end
end

type other_names = { given : string; nickname : string }

type name = {
  fname : string;  (** Docs for [fname] *)
  lname : string;
  others : other_names;
}

(** {2:foo Label} *)

(** {{:href} test_two } *)

(** {{:href} {b test}} *)

(** {{:href} test two foo } *)

(** {{:href} **barz** } *)

(** {v
verbatim
text
v} *)

(** See if listness is preserved. *)

(** This is an {i interface} with {b all} of the {e module system} features.
    This documentation demonstrates:
- comment formatting
- unassociated comments
- documentation sections
- module system documentation including
 {ol
 {- submodules}
 {- module aliases}
 {- module types}
 {- module type aliases}
 {- modules with signatures}
 {- modules with aliased signatures}
}

A numbered list:
+ 3
+ 2
+ 1

    David Sheets is the author.
    @author David Sheets
*)

(** The end foo end keyword in doc comment. *)
module Foo : sig
  type foo

  type poly = [ `Mon | `Tue ]

  type name = { fname : string; lname : string }
end

(** p1 *)

(** p2

   p3

   {ul
     {- a}
     {- b}
    } *)

(** This is where I begin my thing from. *)

(** {ol
      {- one}
      {- two}
    } *)

(** {ul
      {- Mon}
      {- Tue}
    } *)
