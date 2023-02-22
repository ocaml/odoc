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

module Ocaml_ident = Ident
module Ocaml_env = Env

(** Identifiers for definitions *)

module Identifier : sig
  (** {2 Generic operations} *)

  type 'a id = 'a Paths_types.id = { iv : 'a; ihash : int; ikey : string }

  module Any : sig
    type t = Paths_types.Identifier.any

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module RootModule : sig
    type t = Paths_types.Identifier.root_module

    type t_pv = Paths_types.Identifier.root_module_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int

    val name : t -> string
  end

  module Signature : sig
    type t = Paths_types.Identifier.signature

    type t_pv = Paths_types.Identifier.signature_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int

    val root : [< t_pv ] id -> RootModule.t
  end

  module ClassSignature : sig
    type t = Paths_types.Identifier.class_signature

    type t_pv = Paths_types.Identifier.class_signature_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module DataType : sig
    type t = Paths_types.Identifier.datatype

    type t_pv = Paths_types.Identifier.datatype_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Parent : sig
    type t = Paths_types.Identifier.parent

    type t_pv = Paths_types.Identifier.parent_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module LabelParent : sig
    type t = Paths_types.Identifier.label_parent

    type t_pv = Paths_types.Identifier.label_parent_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Module : sig
    type t = Paths_types.Identifier.module_

    type t_pv = Paths_types.Identifier.module_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int

    val root : t -> RootModule.t
  end

  module FunctorParameter : sig
    type t = Paths_types.Identifier.functor_parameter

    type t_pv = Paths_types.Identifier.functor_parameter_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module FunctorResult : sig
    type t = Paths_types.Identifier.functor_result

    type t_pv = Paths_types.Identifier.functor_result_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ModuleType : sig
    type t = Paths_types.Identifier.module_type

    type t_pv = Paths_types.Identifier.module_type_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Type : sig
    type t = Paths_types.Identifier.type_

    type t_pv = Paths_types.Identifier.type_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Constructor : sig
    type t = Paths_types.Identifier.constructor

    type t_pv = Paths_types.Identifier.constructor_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Field : sig
    type t = Paths_types.Identifier.field

    type t_pv = Paths_types.Identifier.field_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Extension : sig
    type t = Paths_types.Identifier.extension

    type t_pv = Paths_types.Identifier.extension_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Exception : sig
    type t = Paths_types.Identifier.exception_

    type t_pv = Paths_types.Identifier.exception_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Value : sig
    type t = Paths_types.Identifier.value

    type t_pv = Paths_types.Identifier.value_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Class : sig
    type t = Paths_types.Identifier.class_

    type t_pv = Paths_types.Identifier.class_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ClassType : sig
    type t = Paths_types.Identifier.class_type

    type t_pv = Paths_types.Identifier.class_type_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Method : sig
    type t = Paths_types.Identifier.method_

    type t_pv = Paths_types.Identifier.method_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module InstanceVariable : sig
    type t = Paths_types.Identifier.instance_variable

    type t_pv = Paths_types.Identifier.instance_variable_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Label : sig
    type t = Paths_types.Identifier.label

    type t_pv = Paths_types.Identifier.label_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Page : sig
    type t = Paths_types.Identifier.page

    type t_pv = Paths_types.Identifier.page_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ContainerPage : sig
    type t = Paths_types.Identifier.container_page

    type t_pv = Paths_types.Identifier.container_page_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module SourceDir : sig
    type t = Paths_types.Identifier.source_dir
    type t_pv = Paths_types.Identifier.source_dir_pv
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
    val name : t -> string
  end

  module SourcePage : sig
    type t = Paths_types.Identifier.source_page
    type t_pv = Paths_types.Identifier.source_page_pv
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
    val name : t -> string
  end

  module OdocId : sig
    type t = Paths_types.Identifier.odoc_id

    type t_pv = Paths_types.Identifier.odoc_id_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Path : sig
    module Module : sig
      type t = Paths_types.Identifier.path_module

      type t_pv = Paths_types.Identifier.path_module_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int

      val root : t -> RootModule.t
    end

    module ModuleType : sig
      type t = Paths_types.Identifier.path_module_type

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    module Type : sig
      type t = Paths_types.Identifier.path_type

      type t_pv = Paths_types.Identifier.path_type_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    module ClassType : sig
      type t = Paths_types.Identifier.path_class_type

      type t_pv = Paths_types.Identifier.path_class_type_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    type t = Paths_types.Identifier.path_any
  end

  type t_pv = Paths_types.Identifier.any_pv

  type t = Paths_types.Identifier.any

  val hash : t -> int

  val name : [< t_pv ] id -> string

  val root : [< t_pv ] id -> RootModule.t_pv id option

  val compare : t -> t -> int

  val equal : ([< t_pv ] id as 'a) -> 'a -> bool

  val label_parent : [< t_pv ] id -> LabelParent.t

  module Maps : sig
    module Any : Map.S with type key = Any.t

    module FunctorParameter : Map.S with type key = FunctorParameter.t

    module Module : Map.S with type key = Module.t

    module ModuleType : Map.S with type key = ModuleType.t

    module Type : Map.S with type key = Type.t

    module Class : Map.S with type key = Class.t

    module ClassType : Map.S with type key = ClassType.t

    module Label : Map.S with type key = Label.t

    module Path : sig
      module Type : Map.S with type key = Path.Type.t

      module ClassType : Map.S with type key = Path.ClassType.t
    end
  end

  module Mk : sig
    open Names

    val page :
      ContainerPage.t option * PageName.t ->
      [> `Page of ContainerPage.t option * PageName.t ] id

    val leaf_page :
      ContainerPage.t option * PageName.t ->
      [> `LeafPage of ContainerPage.t option * PageName.t ] id

    val source_page : ContainerPage.t * string list -> SourcePage.t

    val root :
      ContainerPage.t option * ModuleName.t ->
      [> `Root of ContainerPage.t option * ModuleName.t ] id

    val module_ :
      Signature.t * ModuleName.t ->
      [> `Module of Signature.t * ModuleName.t ] id

    val parameter :
      Signature.t * ModuleName.t ->
      [> `Parameter of Signature.t * ModuleName.t ] id

    val result : Signature.t -> [> `Result of Signature.t ] id

    val module_type :
      Signature.t * ModuleTypeName.t ->
      [> `ModuleType of Signature.t * ModuleTypeName.t ] id

    val class_ :
      Signature.t * ClassName.t -> [> `Class of Signature.t * ClassName.t ] id

    val class_type :
      Signature.t * ClassTypeName.t ->
      [> `ClassType of Signature.t * ClassTypeName.t ] id

    val type_ :
      Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ] id

    val core_type : string -> [> `CoreType of TypeName.t ] id

    val constructor :
      Type.t * ConstructorName.t ->
      [> `Constructor of Type.t * ConstructorName.t ] id

    val field :
      Parent.t * FieldName.t -> [> `Field of Parent.t * FieldName.t ] id

    val extension :
      Signature.t * ExtensionName.t ->
      [> `Extension of Signature.t * ExtensionName.t ] id

    val exception_ :
      Signature.t * ExceptionName.t ->
      [> `Exception of Signature.t * ExceptionName.t ] id

    val core_exception : string -> [> `CoreException of ExceptionName.t ] id

    val value :
      Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ] id

    val method_ :
      ClassSignature.t * MethodName.t ->
      [> `Method of ClassSignature.t * MethodName.t ] id

    val instance_variable :
      ClassSignature.t * InstanceVariableName.t ->
      [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ] id

    val label :
      LabelParent.t * LabelName.t ->
      [> `Label of LabelParent.t * LabelName.t ] id
  end
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig
  module Resolved : sig
    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val is_hidden : t -> weak_canonical_test:bool -> bool

      val identifier : t -> Identifier.Path.Module.t

      val root : t -> string option
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_path.module_type

      val is_hidden : t -> weak_canonical_test:bool -> bool

      (* val identifier : t -> Identifier.Path.ModuleType.t *)
    end

    module Type : sig
      type t = Paths_types.Resolved_path.type_

      val of_ident : Identifier.Path.Type.t -> t

      val is_hidden : t -> bool

      (* val identifier : t -> Identifier.Path.Type.t *)
    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      val of_ident : Identifier.Path.ClassType.t -> t

      val is_hidden : t -> bool
    end

    type t = Paths_types.Resolved_path.any

    val identifier : t -> Identifier.t

    val is_hidden : t -> bool
  end

  module Module : sig
    type t = Paths_types.Path.module_

    val root : t -> string option
  end

  module ModuleType : sig
    type t = Paths_types.Path.module_type
  end

  module Type : sig
    type t = Paths_types.Path.type_
  end

  module ClassType : sig
    type t = Paths_types.Path.class_type
  end

  type t = Paths_types.Path.any

  val is_hidden : t -> bool
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig
  module Resolved : sig
    module Signature : sig
      type t = Paths_types.Resolved_fragment.signature

      val split : t -> string * t option
    end

    module Module : sig
      type t = Paths_types.Resolved_fragment.module_

      val split : t -> string * t option
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_fragment.module_type

      val split : t -> string * t option
    end

    module Type : sig
      type t = Paths_types.Resolved_fragment.type_

      val split : t -> string * t option
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    type root = Paths_types.Resolved_fragment.root

    type t = Paths_types.Resolved_fragment.any

    val identifier : t -> Identifier.t

    val is_hidden : t -> bool
  end

  module Signature : sig
    type t = Paths_types.Fragment.signature

    val split : t -> string * t option
  end

  module Module : sig
    type t = Paths_types.Fragment.module_

    val split : t -> string * t option
  end

  module ModuleType : sig
    type t = Paths_types.Fragment.module_type

    val split : t -> string * t option
  end

  module Type : sig
    type t = Paths_types.Fragment.type_

    val split : t -> string * t option
  end

  type leaf = Paths_types.Fragment.leaf

  type t = Paths_types.Fragment.any
end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig
  module Resolved : sig
    module Signature : sig
      type t = Paths_types.Resolved_reference.signature
    end

    module ClassSignature : sig
      type t = Paths_types.Resolved_reference.class_signature
    end

    module DataType : sig
      type t = Paths_types.Resolved_reference.datatype
    end

    module Parent : sig
      type t = Paths_types.Resolved_reference.parent
    end

    module LabelParent : sig
      type t = Paths_types.Resolved_reference.label_parent
    end

    module Module : sig
      type t = Paths_types.Resolved_reference.module_
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_reference.module_type
    end

    module Type : sig
      type t = Paths_types.Resolved_reference.type_
    end

    module Constructor : sig
      type t = Paths_types.Resolved_reference.constructor
    end

    module Field : sig
      type t = Paths_types.Resolved_reference.field
    end

    module Extension : sig
      type t = Paths_types.Resolved_reference.extension
    end

    module Exception : sig
      type t = Paths_types.Resolved_reference.exception_
    end

    module Value : sig
      type t = Paths_types.Resolved_reference.value
    end

    module Class : sig
      type t = Paths_types.Resolved_reference.class_
    end

    module ClassType : sig
      type t = Paths_types.Resolved_reference.class_type
    end

    module Method : sig
      type t = Paths_types.Resolved_reference.method_
    end

    module InstanceVariable : sig
      type t = Paths_types.Resolved_reference.instance_variable
    end

    module Label : sig
      type t = Paths_types.Resolved_reference.label
    end

    module Page : sig
      type t = Paths_types.Resolved_reference.page
    end

    type t = Paths_types.Resolved_reference.any

    val identifier : t -> Identifier.t
  end

  module Signature : sig
    type t = Paths_types.Reference.signature
  end

  module ClassSignature : sig
    type t = Paths_types.Reference.class_signature
  end

  module DataType : sig
    type t = Paths_types.Reference.datatype
  end

  module Parent : sig
    type t = Paths_types.Reference.parent
  end

  module LabelParent : sig
    type t = Paths_types.Reference.label_parent
  end

  module Module : sig
    type t = Paths_types.Reference.module_
  end

  module ModuleType : sig
    type t = Paths_types.Reference.module_type
  end

  module Type : sig
    type t = Paths_types.Reference.type_
  end

  module Constructor : sig
    type t = Paths_types.Reference.constructor
  end

  module Field : sig
    type t = Paths_types.Reference.field
  end

  module Extension : sig
    type t = Paths_types.Reference.extension
  end

  module Exception : sig
    type t = Paths_types.Reference.exception_
  end

  module Value : sig
    type t = Paths_types.Reference.value
  end

  module Class : sig
    type t = Paths_types.Reference.class_
  end

  module ClassType : sig
    type t = Paths_types.Reference.class_type
  end

  module Method : sig
    type t = Paths_types.Reference.method_
  end

  module InstanceVariable : sig
    type t = Paths_types.Reference.instance_variable
  end

  module Label : sig
    type t = Paths_types.Reference.label
  end

  module Page : sig
    type t = Paths_types.Reference.page
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any
end
