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

type parent_cli_spec =
  | CliParent of string
  | CliPackage of string
  | CliNoparent

val name_of_output : prefix:string -> Fs.File.t -> string
(** Compute the name of the page from the output file. Prefix is the prefix to
    remove from the filename. *)

val resolve_parent_page :
  Resolver.t ->
  string ->
  (Identifier.ContainerPage.t * Lang.Page.child list, [> msg ]) result
(** Parse and resolve a parent reference. Returns the identifier of the parent
    and its children as a list of reference. *)

val compile :
  resolver:Resolver.t ->
  parent_cli_spec:parent_cli_spec ->
  hidden:bool ->
  children:string list ->
  output:Fs.File.t ->
  warnings_options:Odoc_model.Error.warnings_options ->
  source:(Fpath.t * string list) option ->
  cmt_filename_opt:string option ->
  Fs.File.t ->
  (unit, [> msg ]) result
(** Produces .odoc files out of [.cm{i,t,ti}] or .mld files. *)
