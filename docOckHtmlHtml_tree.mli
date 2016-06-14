open Html5.M
open DocOck.Paths

type t = private {
  name : string;
  content : [ `Html ] elt;
  children : t list
}

val traverse
    : f:(parents:string list -> string -> [ `Html ] elt -> unit)
  -> t
  -> unit

(** These two functions are used to track the depth while building the tree,
    which is needed to produce correct links. *)

val enter : ?kind:[ `Arg | `Mod | `Mty ] -> string -> unit

val leave : unit -> unit

(** {1 Page creator} *)

type 'a page_creator =
  ([< Html5_types.div_content_fun ] as 'a) elt ->
  path:string list ->
  [ `Html ] elt

val set_page_creator : _ page_creator -> unit

val make : [< Html5_types.div_content_fun ] elt * t list -> t
(** [make (body, children)] calls "the page creator" to turn [body] into an
    [[ `Html ] elt].
    If [set_page_creator] was not called, a default creator is used. *)

module Relative_link : sig
  module Id : sig
    val href : get_package:('a -> string) -> stop_before:bool ->
      ('a, _) Identifier.t -> string
  end

  val of_path : get_package:('a -> string) -> ('a, _) Path.t
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val of_fragment : get_package:('a -> string) -> base:'a Identifier.signature
    -> ('a, _, Fragment.sort) Fragment.raw
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val of_reference : ('a, _) Reference.t
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list
end
