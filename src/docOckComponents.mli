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

  val find_module_element : string -> 'a t -> 'a Element.signature_module

  val find_apply_element : 'a t -> 'a Element.signature_module

  val find_module_type_element : string -> 'a t -> 'a Element.signature_module_type

  val find_type_element : string -> 'a t -> 'a Element.signature_type

  val find_constructor_element : string -> 'a t -> 'a Element.signature_constructor

  val find_field_element : string -> 'a t -> 'a Element.signature_field

  val find_extension_element : string -> 'a t -> 'a Element.signature_extension

  val find_exception_element : string -> 'a t -> 'a Element.signature_exception

  val find_value_element : string -> 'a t -> 'a Element.signature_value

  val find_class_element : string -> 'a t -> 'a Element.signature_class

  val find_class_type_element : string -> 'a t -> 'a Element.signature_class_type

  val find_label_element : string -> 'a t -> 'a Element.signature_label

  val find_element : string -> 'a t -> 'a Element.signature

  val find_section_title : string -> 'a t -> 'a Documentation.text

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

  val add_element : string -> 'a Element.signature -> 'a signature -> 'a signature

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

  (** {3 Aliases handling} *)

  val set_canonical :
    'a t -> ('a Path.module_ * 'a Reference.module_) option -> 'a t

  val get_canonical :
    'a t -> ('a Path.module_ * 'a Reference.module_) option

  (** {3 Hidding} *)

  val set_hidden : 'a t -> bool -> 'a t

  val get_hidden : 'a t -> bool

end

and Datatype : sig

  type +'a t

  (** {3 Elements} *)

  val find_constructor_element : string -> 'a t -> 'a Element.datatype_constructor

  val find_field_element : string -> 'a t -> 'a Element.datatype_field

  val find_label_element : string -> 'a t -> 'a Element.datatype_label

  val find_element : string -> 'a t -> 'a Element.datatype

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

  val find_method_element : string -> 'a t -> 'a Element.class_signature_method

  val find_instance_variable_element : string -> 'a t ->
        'a Element.class_signature_instance_variable

  val find_label_element : string -> 'a t -> 'a Element.class_signature_label

  val find_element : string -> 'a t -> 'a Element.class_signature

  (** {3 Constructors} *)

  type 'a signature

  val empty : 'a signature

  val add_element : string -> 'a Element.class_signature ->
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

  type ('a, 'b) t =
    | Module :
        { canonical : ('a Path.module_ * 'a Reference.module_) option
        ; hidden : bool } -> ('a, [< kind > `Module]) t
    | ModuleType : ('a, [< kind > `ModuleType]) t
    | Type : ('a, [< kind > `Type]) t
    | Constructor : string -> ('a, [< kind > `Constructor]) t
    | Field : string -> ('a, [< kind > `Field]) t
    | Extension : ('a, [< kind > `Extension]) t
    | Exception : ('a, [< kind > `Exception]) t
    | Value : ('a, [< kind > `Value]) t
    | Class : ('a, [< kind > `Class]) t
    | ClassType : ('a, [< kind > `ClassType]) t
    | Method : ('a, [< kind > `Method]) t
    | InstanceVariable : ('a, [< kind > `InstanceVariable]) t
    | Label : string option -> ('a, [< kind > `Label]) t

  type 'a signature_module = ('a, [`Module]) t

  type 'a signature_module_type = ('a, [`ModuleType]) t

  type 'a signature_type = ('a, [`Type | `Class | `ClassType]) t

  type 'a signature_constructor = ('a, [`Constructor | `Extension | `Exception]) t

  type 'a signature_field = ('a, [`Field]) t

  type 'a signature_extension = ('a, [`Extension | `Exception]) t

  type 'a signature_exception = ('a, [`Exception]) t

  type 'a signature_value = ('a, [`Value]) t

  type 'a signature_class = ('a, [`Class]) t

  type 'a signature_class_type = ('a, [`Class | `ClassType]) t

  type 'a signature_label = ('a, [`Label]) t

  type 'a signature =
    ('a, [ `Module | `ModuleType | `Type
         | `Constructor | `Field | `Extension
         | `Exception | `Value | `Class | `ClassType | `Label ]) t

  type 'a datatype_constructor = ('a, [`Constructor]) t

  type 'a datatype_field = ('a, [`Field]) t

  type 'a datatype_label = ('a, [`Label]) t

  type 'a datatype = ('a, [ `Constructor | `Field | `Label]) t

  type 'a class_signature_method = ('a, [`Method]) t

  type 'a class_signature_instance_variable = ('a, [`InstanceVariable]) t

  type 'a class_signature_label = ('a, [`Label]) t

  type 'a class_signature = ('a, [ `Method | `InstanceVariable | `Label ]) t

end
