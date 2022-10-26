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

type t

val create :
  important_digests:bool ->
  directories:Fs.Directory.t list ->
  open_modules:string list ->
  t
(** Prepare the environment for a given list of
    {{!Fs.Directory.t} include directories}

    @param important_digests indicate whether digests should be compared when
    odoc_xref2 tries to lookup or fetch a unit. It defaults to [true]. *)

val lookup_page : t -> string -> Odoc_model.Lang.Page.t option

(** Helpers for creating xref2 env. *)

val build_compile_env_for_unit :
  t -> Odoc_model.Lang.Compilation_unit.t -> Odoc_xref2.Env.t
(** Initialize the environment for compiling the given module. *)

val build_link_env_for_unit :
  t -> Odoc_model.Lang.Compilation_unit.t -> Odoc_xref2.Env.t
(** Initialize the environment for linking the given module. *)

val build_env_for_page : t -> Odoc_model.Lang.Page.t -> Odoc_xref2.Env.t
(** Initialize the environment for the given page. *)

val build_env_for_reference : t -> Odoc_xref2.Env.t
(** Initialize the environment for a reference. *)

val resolve_import : t -> string -> Odoc_model.Root.t option
(** Similar to {!Odoc_xref2.Env.lookup_root_module} but save work by loading
    only the root. Only used when resolving imports, which are needed for the
    [link-deps] command. *)
