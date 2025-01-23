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

open Odoc_model
open Odoc_model.Paths
open Or_error

type package_spec = { package : string; output : Fpath.t }
type parent_spec = {
  parent : string option;
  children : string list;
  output : Fpath.t;
}

type parent_id_spec = { parent_id : string; output_dir : string }

type cli_spec =
  | CliNoParent of Fpath.t
  | CliPackage of package_spec
  | CliParent of parent_spec
  | CliParentId of parent_id_spec

val name_of_output : prefix:string -> Fs.File.t -> string
(** Compute the name of the page from the output file. Prefix is the prefix to
    remove from the filename. *)

val resolve_imports :
  Resolver.t ->
  Lang.Compilation_unit.Import.t list ->
  Lang.Compilation_unit.Import.t list

val resolve_parent_page :
  Resolver.t ->
  string ->
  (Identifier.ContainerPage.t * Lang.Page.child list, [> msg ]) result
(** Parse and resolve a parent reference. Returns the identifier of the parent
    and its children as a list of reference. *)

val mk_id : string -> Identifier.ContainerPage.t option
val path_of_id :
  string -> Comment.Identifier.Id.container_page option -> Fpath.t

val compile :
  resolver:Resolver.t ->
  hidden:bool ->
  cli_spec:cli_spec ->
  warnings_options:Odoc_model.Error.warnings_options ->
  short_title:string option ->
  Fpath.t ->
  (unit, [> msg ]) result
(** Produces .odoc files out of [.cm{i,t,ti}] or .mld files. *)
