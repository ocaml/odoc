open Odoc_utils

(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Utilities to manipulate files and paths. *)

type file = Fpath.t

type directory

module Directory : sig
  type t = directory

  val dirname : t -> t

  val basename : t -> t

  val append : t -> t -> t

  val contains : parentdir:t -> file -> bool

  val compare : t -> t -> int

  val mkdir_p : t -> unit

  val of_file : file -> t

  val of_string : string -> t

  val to_string : t -> string

  val to_fpath : t -> Fpath.t

  val fold_files_rec : ?ext:string -> ('a -> file -> 'a) -> 'a -> t -> 'a
  (** [fold_files_rec_result ~ext f acc d] recursively folds [f] over the files
      with extension matching [ext] (defaults to [""]) contained in [d] and its
      sub directories. *)

  val fold_files_rec_result :
    ?ext:string ->
    ('a -> file -> ('a, msg) result) ->
    'a ->
    t ->
    ('a, [> msg ]) result
  (** [fold_files_rec_result ~ext f acc d] recursively folds [f] over the files
      with extension matching [ext] (defaults to [""]) contained in [d] and its
      sub directories. Stop as soon as [f] returns [Error _]. *)
end

module File : sig
  type t = file

  val create : directory:Directory.t -> name:string -> t

  val dirname : t -> Directory.t

  val basename : t -> t

  val append : Directory.t -> t -> t

  val set_ext : string -> t -> t

  val has_ext : string -> t -> bool

  val get_ext : t -> string

  val of_string : string -> t

  val to_string : t -> string

  val read : t -> (string, [> msg ]) result

  val copy : src:t -> dst:t -> (unit, [> msg ]) result

  val exists : t -> bool

  val of_segs : string list -> t
  (** [of_segs segs] Returns an absolute path if [segs] starts with an empty
      segment. Raises [Invalid_argument] if [segs] is empty. *)
end
