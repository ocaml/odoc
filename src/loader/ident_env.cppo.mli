(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

type t

val empty : unit -> t

val add_parameter :
  Paths.Identifier.Signature.t -> Ident.t -> Names.ModuleName.t -> t -> t

val handle_signature_type_items :
  Paths.Identifier.Signature.t -> Compat.signature -> t -> t

val add_signature_tree_items :
  Paths.Identifier.Signature.t -> Typedtree.signature -> t -> t

val add_structure_tree_items :
  Paths.Identifier.Signature.t -> Typedtree.structure -> t -> t

module Path : sig
  val read_module : t -> Path.t -> Paths.Path.Module.t

  val read_module_type : t -> Path.t -> Paths.Path.ModuleType.t

  val read_type : t -> Path.t -> Paths.Path.Type.t

  val read_class_type : t -> Path.t -> Paths.Path.ClassType.t
end

val find_module : t -> Ident.t -> Paths.Path.Module.t

val find_module_identifier : t -> Ident.t -> Paths.Identifier.Module.t

val find_parameter_identifier :
  t -> Ident.t -> Paths.Identifier.FunctorParameter.t

val find_module_type : t -> Ident.t -> Paths.Identifier.ModuleType.t

val find_value_identifier : t -> Ident.t -> Paths.Identifier.Value.t

val find_type : t -> Ident.t -> Paths.Identifier.Path.Type.t

val find_constructor_identifier : t -> Ident.t -> Paths.Identifier.Constructor.t

val find_extension_identifier : t -> Ident.t -> Paths.Identifier.Extension.t

val find_exception_identifier : t -> Ident.t -> Paths.Identifier.Exception.t

val find_type_identifier : t -> Ident.t -> Paths.Identifier.Type.t

val find_class_identifier : t -> Ident.t -> Paths.Identifier.Class.t

val is_shadowed : t -> Ident.t -> bool

val find_class_type_identifier : t -> Ident.t -> Paths.Identifier.ClassType.t

module Fragment : sig
  val read_module : Longident.t -> Paths.Fragment.Module.t

  val read_module_type : Longident.t -> Paths.Fragment.ModuleType.t

  val read_type : Longident.t -> Paths.Fragment.Type.t
end

val identifier_of_loc : t -> Location.t -> Paths.Identifier.t option
(** Each generated id has its location stored. This allows to get back the id
    knowing only the location. This is used to generate links to source from the
    resolution of a shape. *)

val iter_located_identifier :
  t -> (Location.t -> Paths.Identifier.t -> unit) -> unit
(** Iter on all stored pair [location]-[identifier]. *)
