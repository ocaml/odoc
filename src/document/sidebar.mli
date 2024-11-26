open Odoc_utils
open Types

type entry = {
  url : Url.t option;
  content : Inline.t;
  toc_status : [ `Open ] option;
}

type t = entry Tree.t list

val of_index : Odoc_index.t -> t

val to_block : t -> Url.Path.t -> Types.Block.t
(** Generates the sidebar document given a global sidebar and the path at which
    it will be displayed *)
