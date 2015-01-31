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

open DocOckPaths
open DocOckTypes
open DocOckComponents

(** {3 Tables} *)

(** The type of tables of components *)
type 'a t

(** Create a table of the components of units. Optionally provide
    equality and hash functons. *)
val create: ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
 ('a Unit.t -> string -> 'a option) -> ('a -> 'a Unit.t) -> 'a t

(** {3 Identifier Lookup} *)

(** Lookup the components of a signature identifier *)
val signature_identifier : 'a t -> 'a Identifier.signature -> 'a Sig.t

(** Lookup the components of a class signature identifier *)
val class_signature_identifier : 'a t -> 'a Identifier.class_signature ->
      'a ClassSig.t

(** Lookup the components of a datatype identifier *)
val datatype_identifier : 'a t -> 'a Identifier.type_ -> 'a Datatype.t

(** {3 Path Lookup} *)

(** TODO: One day we will be able to remove the unit argument. *)
(** Lookup the components of a resolved module path *)
val resolved_module_path : 'a t -> 'a Unit.t ->
      'a Path.Resolved.module_ -> 'a Sig.t

(** TODO: One day we will be able to remove the unit argument. *)
(** Lookup the components of a resolved module type path *)
val resolved_module_type_path : 'a t -> 'a Unit.t ->
      'a Path.Resolved.module_type -> 'a Sig.t

(** TODO: One day we will be able to remove the unit argument. *)
(** Lookup the components of a resolved class type path *)
val resolved_class_type_path : 'a t -> 'a Unit.t ->
      'a Path.Resolved.class_type -> 'a ClassSig.t

(** Lookup the components of a module path, needed for module
    applications. *)
val module_path : 'a t -> 'a Unit.t -> 'a Path.module_ -> 'a Sig.t

(** {3 Fragment Lookup} *)

(** Table specialised to lookup fragments based on a module expression
    or path. *)
type 'a with_

(** Create specialised fragment table for a module type expression *)
val module_type_expr_with : 'a t -> 'a Unit.t ->
      'a ModuleType.expr -> 'a with_

(** Create specialised fragment table for a module path *)
val module_type_path_with : 'a t -> 'a Unit.t ->
      'a Path.module_type -> 'a with_

(** Lookup the components of a resolved module fragment *)
val resolved_signature_fragment : 'a with_ ->
      'a Fragment.Resolved.signature -> 'a Sig.t

(** {3 Reference Lookup} *)

(** Lookup the components of a resolved signature reference *)
val resolved_signature_reference : 'a t ->
      'a Reference.Resolved.signature -> 'a Sig.t

(** Lookup the components of a resolved class signature reference *)
val resolved_class_signature_reference : 'a t ->
      'a Reference.Resolved.class_signature -> 'a ClassSig.t

(** Lookup the components of a resolved datatype reference *)
val resolved_datatype_reference : 'a t -> 'a Reference.Resolved.datatype ->
      'a Datatype.t

(** {3 Root lookup} *)

(** Lookup the base of a unit name *)
val base : 'a t -> 'a Unit.t -> string -> 'a option
