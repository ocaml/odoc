type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val pretty : t -> string

module type IO = Io.S

module Make (Io : IO) : sig
  val search : shards:Db.t list -> ?dynamic_sort:bool -> t -> Db.Entry.t list Io.t
  (** [search ~shard ~dynamic_sort {query; packages; limit}] returns [(pretty_query,
      results)] where [pretty_query] is a re-printed version of [query] and
      [results] is the list of results corresponding to the query and the
      various parameters.

      - [shards] is a list of databases. [results] is the union of the results of
        each database of the list [shards]. If [shards] is a very long list, [api]
        might be slow to return, but in some cases you do not have a choice.
        Currently, [index] generates only one shard, but it used to generate many
        to be able to handle the sheer size of the opam repository.

      - [~dynamic_sort] changes the order of [results]. It is [true] by default,
        and is only set to [false] for debugging purposes.

      - [query] is the query string whose shape is a list of space-separated
        words, followed by an optionnal [: ...] type annotation that filters the
        results by type. The type annotation accepts [_] as a wildcard : [: string
      -> _] will return entries that take a [string] as argument, but returns
        anything.

      - [limit] is the maximum length of [results]. Having a very large number
        might be an issue.

      - [packages] is not function, use [[]] for this argument. *)
end

module Blocking : sig
  val search : shards:Db.t list -> ?dynamic_sort:bool -> t -> Db.Entry.t list
end

(* val search_lwt : shards:Db.t list -> ?dynamic_sort:bool -> t -> Db.Entry.t list Lwt.t *)

(** For testing *)
module Private : sig
  module Succ = Succ

  module Type_parser : sig
    val of_string : string -> (Db.Typexpr.t, string) result
  end
end
