(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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

(** Management of the documentation environment.

    This is the module which does the link between packages, directories and
    {!Odoc_xref2}'s needs. *)

open Odoc_model

type t

type named_root = string * Fs.Directory.t

type roots = {
  page_roots : named_root list;
  lib_roots : named_root list;
  current_lib : named_root option;  (** The current [-L]. *)
  current_package : named_root option;  (** The current [-P]. *)
  current_dir : Fs.Directory.t;
      (** Directory containing the output for the current unit. *)
}

val create :
  important_digests:bool ->
  directories:Fs.Directory.t list ->
  open_modules:string list ->
  roots:roots option ->
  t
(** Prepare the environment for a given list of
    {{!Fs.Directory.t} include directories}, page roots and library roots.

    @param important_digests
      indicate whether digests should be compared when odoc_xref2 tries to
      lookup or fetch a unit. It defaults to [true]. *)

val lookup_page : t -> string -> Lang.Page.t option

(** Helpers for creating xref2 env. *)

val build_compile_env_for_unit :
  t -> Lang.Compilation_unit.t -> Odoc_xref2.Env.t
(** Initialize the environment for compiling the given module. *)

val build_link_env_for_unit : t -> Lang.Compilation_unit.t -> Odoc_xref2.Env.t
(** Initialize the environment for linking the given module. *)

val build_env_for_page : t -> Lang.Page.t -> Odoc_xref2.Env.t
(** Initialize the environment for the given page. *)

val build_compile_env_for_impl : t -> Lang.Implementation.t -> Odoc_xref2.Env.t
(** Initialize the environment for the given implementation. *)

val build_link_env_for_impl : t -> Lang.Implementation.t -> Odoc_xref2.Env.t
(** Initialize the environment for the given implementation. *)

val build_env_for_reference : t -> Odoc_xref2.Env.t
(** Initialize the environment for a reference. *)

val resolve_import : t -> string -> Root.t option
(** Similar to {!Odoc_xref2.Env.lookup_root_module} but save work by loading
    only the root. Only used when resolving imports, which are needed for the
    [link-deps] command. *)
