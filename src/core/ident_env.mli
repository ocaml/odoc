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

open Paths.Identifier

type t

val empty : t

val add_module : signature -> Ident.t -> t -> t

val add_argument : signature -> int -> Ident.t -> t -> t

val add_module_type : signature -> Ident.t -> t -> t

val add_type : signature -> Ident.t -> t -> t

val add_class : signature -> Ident.t -> Ident.t -> Ident.t -> Ident.t -> t -> t

val add_class_type : signature -> Ident.t -> Ident.t -> Ident.t -> t -> t

val add_signature_type_items : signature -> Types.signature -> t -> t

val add_signature_tree_items : signature -> Typedtree.signature -> t -> t

val add_structure_tree_items : signature -> Typedtree.structure -> t -> t

module Path : sig

  val read_module : t -> Path.t -> Paths.Path.module_

  val read_module_type : t -> Path.t -> Paths.Path.module_type

  val read_type : t -> Path.t -> Paths.Path.type_

  val read_class_type : t -> Path.t -> Paths.Path.class_type

end


module Fragment : sig

  val read_module : Longident.t -> Paths.Fragment.module_

  val read_type : Longident.t -> Paths.Fragment.type_

end
