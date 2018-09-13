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

open Model
open Paths

module rec Sig : sig

  type t

  (** {3 Parents} *)

  val find_parent_module : string -> t -> Parent.module_

  val find_parent_apply : (Path.module_ -> t) -> Path.module_ ->
        t -> Parent.module_

  val find_parent_module_type : string -> t -> Parent.module_type

  val find_parent_signature : string -> t -> Parent.signature

  val find_parent_class_signature : string -> t -> Parent.class_signature

  val find_parent_datatype : string -> t -> Parent.datatype

  val find_parent_sig_or_type : string -> t -> Parent.sig_or_type

  val find_parent_subst : t -> Parent.subst

  val find_parent : string -> t -> Parent.any

  (** {3 Elements} *)

  val find_module_element : string -> t -> Element.signature_module

  val find_apply_element : t -> Element.signature_module

  val find_module_type_element : string -> t -> Element.signature_module_type

  val find_type_element : string -> t -> Element.signature_type

  val find_constructor_element : string -> t -> Element.signature_constructor

  val find_field_element : string -> t -> Element.signature_field

  val find_extension_element : string -> t -> Element.signature_extension

  val find_exception_element : string -> t -> Element.signature_exception

  val find_value_element : string -> t -> Element.signature_value

  val find_class_element : string -> t -> Element.signature_class

  val find_class_type_element : string -> t -> Element.signature_class_type

  val find_label_element : string -> t -> Element.signature_label

  val find_element : string -> t -> Element.signature

  val find_section_title : string -> t -> Model.Comment.link_content

  (** {3 Lookup} *)

  val lookup_module : string -> t -> t

  val lookup_argument : int -> t -> t

  val lookup_apply : (Path.module_ -> t) -> Path.module_ -> t -> t

  val lookup_module_type  : string -> t -> t

  val lookup_class_type : string -> t -> ClassSig.t

  val lookup_datatype : string -> t -> Datatype.t

  (** {3 Constructors} *)

  type signature

  val empty : signature

  val add_module : string -> t -> signature -> signature

  val add_module_type : string -> t -> signature -> signature

  val add_datatype : string -> Datatype.t -> signature -> signature

  val add_class : string -> ClassSig.t -> signature -> signature

  val add_class_type : string -> ClassSig.t -> signature -> signature

  val add_element : string -> Element.signature -> signature -> signature

  val add_documentation : Model.Comment.docs -> signature -> signature

  val add_comment : Model.Comment.docs_or_stop -> signature -> signature

  val include_ : t -> signature -> signature

  val modules : t -> (string * t) list

  val module_types : t -> (string * t) list

  val path : (Path.module_type -> t) -> Path.module_type -> t

  val alias : (Path.module_ -> t) -> Path.module_ -> t

  val signature : ('b -> signature) -> 'b -> t

  val functor_ : (Root.t -> Root.t -> bool) option -> (Root.t -> int) option ->
                 Identifier.module_ -> t -> t -> t

  val generative : t -> t

  val abstract : t

  val unresolved : t

  val with_module : Fragment.module_ -> t -> t -> t

  val with_module_subst : Fragment.module_ -> t -> t

  val with_type_subst : Fragment.type_ -> t -> t

  (** {3 Aliases handling} *)

  val set_canonical :
    t -> (Path.module_ * Reference.module_) option -> t

  val get_canonical :
    t -> (Path.module_ * Reference.module_) option

  (** {3 Hidding} *)

  val set_hidden : t -> bool -> t

  val get_hidden : t -> bool

end

and Datatype : sig

  type t

  (** {3 Elements} *)

  val find_constructor_element : string -> t -> Element.datatype_constructor

  val find_field_element : string -> t -> Element.datatype_field

  val find_label_element : string -> t -> Element.datatype_label

  val find_element : string -> t -> Element.datatype

  (** {3 Constructors} *)

  val add_documentation : Model.Comment.docs -> t -> t

  val abstract : t

  val variant : string -> string list -> t

  val record : string -> string list -> t

  val extensible : t

  val unresolved : t

end

and ClassSig : sig

  type t

  (** {3 Elements} *)

  val find_method_element : string -> t -> Element.class_signature_method

  val find_instance_variable_element : string -> t ->
        Element.class_signature_instance_variable

  val find_label_element : string -> t -> Element.class_signature_label

  val find_element : string -> t -> Element.class_signature

  (** {3 Constructors} *)

  type signature

  val empty : signature

  val add_element : string -> Element.class_signature -> signature -> signature

  val add_documentation : Model.Comment.docs -> signature -> signature

  val add_comment : Model.Comment.docs_or_stop -> signature -> signature

  val inherit_ : t -> signature -> signature

  val constr : (Path.class_type -> t) -> Path.class_type -> t

  val signature : ('b -> signature) -> 'b -> t

  val unresolved : t

end

and Parent : sig

  type kind = Kind.parent

  type 'kind t =
    | Module : Sig.t -> [< kind > `Module] t
    | ModuleType : Sig.t -> [< kind > `ModuleType] t
    | Datatype : Datatype.t -> [< kind > `Type] t
    | Class : ClassSig.t -> [< kind > `Class] t
    | ClassType : ClassSig.t -> [< kind > `ClassType] t

  type signature = [`Module | `ModuleType] t

  type class_signature = [`Class |` ClassType] t

  type datatype = [`Type] t

  type module_ = [`Module] t

  type module_type = [`ModuleType] t

  type sig_or_type = [`Module | `ModuleType | `Type] t

  type any = kind t

  type subst =
    | Subst of Path.module_type
    | SubstAlias of Path.module_

end

and Page : sig

  type t

  (** {3 Elements} *)

  val find_label_element : string -> t -> Element.page_label

  val find_section_title : string -> t -> Model.Comment.link_content

  (** {3 Constructor} *)

  val of_doc : Model.Comment.docs -> t
end

and Element : sig

  type kind =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label ]

  type mod_t = { canonical : (Path.module_ * Reference.module_) option
        ; hidden : bool }

  type 'kind t =
    | Module : mod_t -> [< kind > `Module] t
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

  type page_label = [`Label] t
end
