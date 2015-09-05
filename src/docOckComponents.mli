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

module rec Sig : sig

  type 'a t

  (** {3 Parents} *)

  val find_parent_module : string -> 'a t -> 'a Parent.module_

  val find_parent_apply : ('a Path.module_ -> 'a t) -> 'a Path.module_ ->
        'a t -> 'a Parent.module_

  val find_parent_module_type : string -> 'a t -> 'a Parent.module_type

  val find_parent_signature : string -> 'a t -> 'a Parent.signature

  val find_parent_class_signature : string -> 'a t -> 'a Parent.class_signature

  val find_parent_datatype : string -> 'a t -> 'a Parent.datatype

  val find_parent_sig_or_type : string -> 'a t -> 'a Parent.sig_or_type

  val find_parent_subst : 'a t -> 'a Parent.subst

  val find_parent : string -> 'a t -> 'a Parent.any

  (** {3 Elements} *)

  val find_module_element : string -> 'a t -> Element.signature_module

  val find_apply_element : 'a t -> Element.signature_module

  val find_module_type_element : string -> 'a t -> Element.signature_module_type

  val find_type_element : string -> 'a t -> Element.signature_type

  val find_constructor_element : string -> 'a t -> Element.signature_constructor

  val find_field_element : string -> 'a t -> Element.signature_field

  val find_extension_element : string -> 'a t -> Element.signature_extension

  val find_exception_element : string -> 'a t -> Element.signature_exception

  val find_value_element : string -> 'a t -> Element.signature_value

  val find_class_element : string -> 'a t -> Element.signature_class

  val find_class_type_element : string -> 'a t -> Element.signature_class_type

  val find_label_element : string -> 'a t -> Element.signature_label

  val find_element : string -> 'a t -> Element.signature

  (** {3 Lookup} *)

  val lookup_module : string -> 'a t -> 'a t

  val lookup_argument : int -> 'a t -> 'a t

  val lookup_apply : ('a Path.module_ -> 'a t) -> 'a Path.module_ ->
        'a t -> 'a t

  val lookup_module_type  : string -> 'a t -> 'a t

  val lookup_class_type : string -> 'a t -> 'a ClassSig.t

  val lookup_datatype : string -> 'a t -> 'a Datatype.t

  (** {3 Constructors} *)

  type 'a signature

  val empty : 'a signature

  val add_module : string -> 'a t -> 'a signature -> 'a signature

  val add_module_type : string -> 'a t -> 'a signature -> 'a signature

  val add_datatype : string -> 'a Datatype.t -> 'a signature -> 'a signature

  val add_class : string -> 'a ClassSig.t -> 'a signature -> 'a signature

  val add_class_type : string -> 'a ClassSig.t -> 'a signature -> 'a signature

  val add_element : string -> Element.signature -> 'a signature -> 'a signature

  val add_documentation : 'a Documentation.t -> 'a signature -> 'a signature

  val add_comment : 'a Documentation.comment -> 'a signature -> 'a signature

  val include_ : 'a t -> 'a signature -> 'a signature

  val modules : 'a t -> (string * 'a t) list

  val module_types : 'a t -> (string * 'a t) list

  val path : ('a Path.module_type -> 'a t) -> 'a Path.module_type -> 'a t

  val alias : ('a Path.module_ -> 'a t) -> 'a Path.module_ -> 'a t

  val signature : ('b -> 'a signature) -> 'b -> 'a t

  val functor_ : ('a -> 'a -> bool) option -> ('a -> int) option ->
                 'a Identifier.module_ -> 'a t -> 'a t -> 'a t

  val generative : 'a t -> 'a t

  val abstract : 'a t

  val unresolved : 'a t

  val with_module : 'a Fragment.module_ -> 'a t -> 'a t -> 'a t

  val with_module_subst : 'a Fragment.module_ -> 'a t -> 'a t

  val with_type_subst : 'a Fragment.type_ -> 'a t -> 'a t

end

and Datatype : sig

  type +'a t

  (** {3 Elements} *)

  val find_constructor_element : string -> 'a t -> Element.datatype_constructor

  val find_field_element : string -> 'a t -> Element.datatype_field

  val find_label_element : string -> 'a t -> Element.datatype_label

  val find_element : string -> 'a t -> Element.datatype

  (** {3 Constructors} *)

  val add_documentation : 'a Documentation.t -> 'a t -> 'a t

  val abstract : 'a t

  val variant : string -> string list -> 'a t

  val record : string -> string list -> 'a t

  val extensible : 'a t

  val unresolved : 'a t

end

and ClassSig : sig

  type 'a t

  (** {3 Elements} *)

  val find_method_element : string -> 'a t -> Element.class_signature_method

  val find_instance_variable_element : string -> 'a t ->
        Element.class_signature_instance_variable

  val find_label_element : string -> 'a t -> Element.class_signature_label

  val find_element : string -> 'a t -> Element.class_signature

  (** {3 Constructors} *)

  type 'a signature

  val empty : 'a signature

  val add_element : string -> Element.class_signature ->
    'a signature -> 'a signature

  val add_documentation : 'a Documentation.t -> 'a signature -> 'a signature

  val add_comment : 'a Documentation.comment -> 'a signature -> 'a signature

  val inherit_ : 'a t -> 'a signature -> 'a signature

  val constr : ('a Path.class_type -> 'a t) -> 'a Path.class_type ->
        'a t

  val signature : ('b -> 'a signature) -> 'b -> 'a t

  val unresolved : 'a t

end

and Parent : sig

  type kind = Kind.parent

  type ('a, 'b) t =
    | Module : 'a Sig.t -> ('a, [< kind > `Module]) t
    | ModuleType : 'a Sig.t -> ('a, [< kind > `ModuleType]) t
    | Datatype : 'a Datatype.t -> ('a, [< kind > `Type]) t
    | Class : 'a ClassSig.t -> ('a, [< kind > `Class]) t
    | ClassType : 'a ClassSig.t -> ('a, [< kind > `ClassType]) t

  type 'a signature = ('a, [`Module | `ModuleType]) t

  type 'a class_signature = ('a, [`Class |` ClassType]) t

  type 'a datatype = ('a, [`Type]) t

  type 'a module_ = ('a, [`Module]) t

  type 'a module_type = ('a, [`ModuleType]) t

  type 'a sig_or_type = ('a, [`Module | `ModuleType | `Type]) t

  type 'a any = ('a, kind) t

  type 'a subst =
    | Subst of 'a Path.module_type
    | SubstAlias of 'a Path.module_

end

and Element : sig

  type kind =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label ]

  type 'a t =
    | Module : [< kind > `Module] t
    | ModuleType : [< kind > `ModuleType] t
    | Type : [< kind > `Type] t
    | Constructor : string -> [< kind > `Constructor] t
    | Field : string -> [< kind > `Field] t
    | Extension : [< kind > `Extension] t
    | Exception : [< kind > `Exception] t
    | Value : [< kind > `Value] t
    | Class : [< kind > `Class] t
    | ClassType : [< kind > `ClassType] t
    | Method : [< kind > `Method] t
    | InstanceVariable : [< kind > `InstanceVariable] t
    | Label : string option -> [< kind > `Label] t

  type signature_module = [`Module] t

  type signature_module_type = [`ModuleType] t

  type signature_type = [`Type | `Class | `ClassType] t

  type signature_constructor = [`Constructor | `Extension | `Exception] t

  type signature_field = [`Field] t

  type signature_extension = [`Extension | `Exception] t

  type signature_exception = [`Exception] t

  type signature_value = [`Value] t

  type signature_class = [`Class] t

  type signature_class_type = [`Class | `ClassType] t

  type signature_label = [`Label] t

  type signature =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType | `Label ] t

  type datatype_constructor = [`Constructor] t

  type datatype_field = [`Field] t

  type datatype_label = [`Label] t

  type datatype = [ `Constructor | `Field | `Label] t

  type class_signature_method = [`Method] t

  type class_signature_instance_variable = [`InstanceVariable] t

  type class_signature_label = [`Label] t

  type class_signature = [ `Method | `InstanceVariable | `Label ] t

end
