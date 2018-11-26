open Result

open Model.Paths

type t = {
  page : string list;
  (** [Foo.Bar.lol] becomes [["lol"; "Bar"; "Foo"]]. *)

  anchor : string;
  (** Anchor in {!page} where the element is attached *)

  kind : string;
  (** What kind of element the path points to.
      e.g. "module", "module-type", "exception", ... *)
}
(** A low level representation of ocaml paths. *)

val to_string : t -> string

module Error : sig
  type nonrec t =
    | Not_linkable of string
    | Uncaught_exn of string
    (* These should basicaly never happen *)
    | Unexpected_anchor of t * string
    | Missing_anchor of t * string

  val to_string : t -> string
end

val from_identifier
  : stop_before:bool
  -> _ Identifier.t
  -> (t, Error.t) result

val anchor_of_id_exn
  : _ Identifier.t
  -> string

val kind_of_id_exn
  : _ Identifier.t
  -> string

val render_path : _ Path.t -> string

module Anchor : sig
  type t = {
    kind : string;
    name : string;
  }

  module Polymorphic_variant_decl : sig
    val from_element
      : type_ident:_ Identifier.t
      -> Model.Lang.TypeExpr.Polymorphic_variant.element
      -> t
  end

  module Module_listing : sig
    val from_reference : Reference.module_ -> t
  end
end
