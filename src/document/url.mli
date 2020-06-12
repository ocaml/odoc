open Result

open Odoc_model.Paths

val functor_arg_pos : Identifier.FunctorParameter.t -> int


module Error : sig
  type nonrec t =
    | Not_linkable of string
    | Uncaught_exn of string
    (* These should basicaly never happen *)
    | Unexpected_anchor of string

  val to_string : t -> string
end


module Path : sig

  type t = {
    kind : string;
    parent : t option;
    name : string;
  }

  type source = [
    | Identifier.Page.t
    | Identifier.Signature.t
    | Identifier.ClassSignature.t
  ]

  val from_identifier : [< source] -> t

  val last : t -> string
end

module Anchor : sig

  type t = {
    page : Path.t ;
    anchor : string;
    (** Anchor in {!page} where the element is attached *)

    kind : string;
    (** What kind of element the path points to.
        e.g. "module", "module-type", "exception", ... *)
  }

  val from_identifier : Identifier.t -> (t, Error.t) result

  val polymorphic_variant
    : type_ident:Identifier.t
    -> Odoc_model.Lang.TypeExpr.Polymorphic_variant.element
    -> t
end

type t = Anchor.t

val from_path : Path.t -> t

val from_identifier : stop_before:bool -> Identifier.t -> (t, Error.t) result
val from_identifier_exn : stop_before:bool -> Identifier.t -> t

val kind : Identifier.t -> string

val render_path : Odoc_model.Paths.Path.t -> string

val page : t -> Path.t
