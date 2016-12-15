open DocOck.Paths

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
    | Unexpected_anchor of t
    | Missing_anchor of t * string

  val to_string : t -> string
end

val from_identifier
  : get_package:('pkg -> string)
  -> stop_before:bool
  -> ('pkg, _) Identifier.t
  -> (t, Error.t) result

val anchor_of_id_exn
  : get_package:('pkg -> string)
  -> ('pkg, _) Identifier.t
  -> string

val kind_of_id_exn
  : get_package:('pkg -> string)
  -> ('pkg, _) Identifier.t
  -> string
