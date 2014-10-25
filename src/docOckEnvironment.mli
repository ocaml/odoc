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

open DocOckPaths.Identifier

type 'a t

val empty : 'a t

val add_module : 'a signature -> 'a t -> Ident.t -> 'a t

val add_argument : 'a signature -> int -> 'a t -> Ident.t -> 'a t

val add_module_type : 'a signature -> 'a t -> Ident.t -> 'a t

val add_type : 'a signature -> 'a t -> Ident.t -> 'a t

val add_constructor : 'a type_ -> 'a t -> Ident.t -> 'a t

val add_field : 'a type_ -> 'a t -> Ident.t -> 'a t

val add_extension : 'a signature -> 'a t -> Ident.t -> 'a t

val add_exception : 'a signature -> 'a t -> Ident.t -> 'a t

val add_value : 'a signature -> 'a t -> Ident.t -> 'a t

val add_class : 'a signature -> 'a t -> Ident.t -> 'a t

val add_class_type : 'a signature -> 'a t -> Ident.t -> 'a t

val add_method : 'a class_signature -> 'a t -> string -> 'a t

val add_instance_variable : 'a class_signature -> 'a t -> string -> 'a t

val add_label : 'a container -> 'a t -> string -> 'a t


module Path : sig

  val read_module : 'a t -> Path.t -> 'a DocOckPaths.Path.module_

  val read_module_type : 'a t -> Path.t -> 'a DocOckPaths.Path.module_type

  val read_type : 'a t -> Path.t -> 'a DocOckPaths.Path.type_

  val read_class : 'a t -> Path.t -> 'a DocOckPaths.Path.class_

  val read_class_type : 'a t -> Path.t -> 'a DocOckPaths.Path.class_type

end


module Fragment : sig

  val read_module : Longident.t -> DocOckPaths.Fragment.module_

  val read_type : Longident.t -> DocOckPaths.Fragment.type_

end


module Reference : sig

  open DocOckPaths

  val read_module : 'a t -> string -> 'a Reference.module_

  val read_module_type : 'a t -> string -> 'a Reference.module_type

  val read_type : 'a t -> string -> 'a Reference.type_

  val read_constructor : 'a t -> string -> 'a Reference.constructor

  val read_field : 'a t -> string -> 'a Reference.field

  val read_extension : 'a t -> string -> 'a Reference.extension

  val read_exception : 'a t -> string -> 'a Reference.exception_

  val read_value : 'a t -> string -> 'a Reference.value

  val read_class : 'a t -> string -> 'a Reference.class_

  val read_class_type : 'a t -> string -> 'a Reference.class_type

  val read_method : 'a t -> string -> 'a Reference.method_

  val read_instance_variable : 'a t -> string -> 'a Reference.instance_variable

  val read_label : 'a t -> string -> 'a Reference.label

  val read_element : 'a t -> string -> 'a Reference.any

end
