open Odoc_model.Lang
open Odoc_utils

(** Skeletons represent a hierarchy of entries. It contains the least
    information to create an index, represented in a uniform way (compared to
    the [Lang] types) *)

type t = Entry.t Tree.t

val from_unit : Compilation_unit.t -> t

val from_page : Page.t -> t
