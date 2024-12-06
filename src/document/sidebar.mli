open Odoc_utils
open Types

type entry = Url.t option * Inline.one

type pages = { name : string; pages : entry Tree.t }
type library = { name : string; units : entry Tree.t list }

type t = { pages : pages list; libraries : library list }

val of_lang : Odoc_index.t -> t

val to_block : t -> Url.Path.t -> Types.Block.t
(** Generates the sidebar document given a global sidebar and the path at which
    it will be displayed *)
