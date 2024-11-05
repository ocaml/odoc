type t

val of_lang : Odoc_index.t -> t

val to_block : t -> Url.Path.t -> Types.Block.t
(** Generates the sidebar document given a global sidebar and the path at which
    it will be displayed *)
