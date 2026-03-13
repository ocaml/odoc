(** Typed names for paths, identifiers, references and fragments.

    This module contains a module per type of named object in our internal
    representation of the langage, each containing an opaque type [t]. This
    allows us to ensure that, for example, we never mistake a module name for a
    module type name. *)

val parenthesise : string -> string
val contains_double_underscore : string -> bool
(* not the best place for this but. *)

val set_unique_ident : string -> unit

(** Name is the signature for names that could possibly be hidden. Hidden names
    occur when we generate items that don't have a path that will be exposed in
    the generated HTML. This can occur for a few reasons:

    1. explicitly hidden by the user with stop comments 2. generalised opens 3.
    shadowed identifiers

    In cases 1 and 2 the identifiers are available for use later in the
    signature (or more generally) whereas for case 3 they aren't, and it's
    helpful to keep this distinction. *)

module type Name = sig
  type t

  val to_string : t -> string

  val to_string_unsafe : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val hidden_of_string : string -> t

  val hidden_of_ident : Ident.t -> t

  val shadowed_of_string : string -> t

  val shadowed_of_ident : Ident.t -> t

  val equal_modulo_shadowing : t -> t -> bool

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  (* Returns true for any sort of hidden identifier - explicitly hidden or shadowed *)
  val is_hidden : t -> bool
end

(** Some named objects can't have internal names, so they have this simpler
    module. *)
module type SimpleName = sig
  type t

  val to_string : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

module ModuleName : Name

module ModuleTypeName : Name

module TypeName : Name

module ConstructorName : SimpleName

module FieldName : SimpleName

module UnboxedFieldName : SimpleName

module ExtensionName : SimpleName

module ExceptionName : SimpleName

module ValueName : Name

module MethodName : SimpleName

module InstanceVariableName : SimpleName

module LabelName : SimpleName

module PageName : SimpleName

module DefName : SimpleName

module LocalName : SimpleName

module AssetName : SimpleName
