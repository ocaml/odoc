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

  module type IdSig = sig
    type t
    type t_pv
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Id = Paths_types.Identifier

  module Any : IdSig with type t = Id.any and type t_pv = Id.any_pv

  module RootModule :
    IdSig with type t = Id.root_module and type t_pv = Id.root_module_pv

  module Signature :
    IdSig with type t = Id.signature and type t_pv = Id.signature_pv

  module ClassSignature :
    IdSig with type t = Id.class_signature and type t_pv = Id.class_signature_pv

  module LabelParent :
    IdSig with type t = Id.label_parent and type t_pv = Id.label_parent_pv

  module Module : IdSig with type t = Id.module_ and type t_pv = Id.module_pv

  module FunctorParameter : sig
    include
      IdSig
        with type t = Id.functor_parameter
         and type t_pv = Id.functor_parameter_pv

    val functor_arg_pos : t -> int
    (** Gets the index in which the functor argument is, in the argument list.
      Useful to turn identifiers into unique anchors, since multiple arguments
      can have the same name. *)
  end

  module ModuleType :
    IdSig with type t = Id.module_type and type t_pv = Id.module_type_pv

  module Type : IdSig with type t = Id.type_ and type t_pv = Id.type_pv

  module SourceDir :
    IdSig with type t = Id.source_dir and type t_pv = Id.source_dir_pv

  module Class : IdSig with type t = Id.class_ and type t_pv = Id.class_pv

  module ClassType :
    IdSig with type t = Id.class_type and type t_pv = Id.class_type_pv

  module DataType : sig
    type t = Id.datatype
    type t_pv = Id.datatype_pv
  end
  module FieldParent : sig
    type t = Id.field_parent
    type t_pv = Id.field_parent_pv
  end

  module FunctorResult : sig
    type t = Id.functor_result
    type t_pv = Id.functor_result_pv
  end

  module Constructor : sig
    type t = Id.constructor
    type t_pv = Id.constructor_pv
  end

  module Field : sig
    type t = Id.field
    type t_pv = Id.field_pv
  end

  module Extension : sig
    type t = Id.extension
    type t_pv = Id.extension_pv
  end

  module ExtensionDecl : sig
    type t = Paths_types.Identifier.extension_decl

    type t_pv = Paths_types.Identifier.extension_decl_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Exception : sig
    type t = Id.exception_
    type t_pv = Id.exception_pv
  end

  module Value : sig
    type t = Id.value
    type t_pv = Id.value_pv
  end

  module Method : sig
    type t = Id.method_
    type t_pv = Id.method_pv
  end

  module InstanceVariable : sig
    type t = Id.instance_variable
    type t_pv = Id.instance_variable_pv
  end
  module Label : IdSig with type t = Id.label and type t_pv = Id.label_pv

  module Page : sig
    type t = Id.page
    type t_pv = Id.page_pv
  end

  module ContainerPage : sig
    type t = Id.container_page
    type t_pv = Id.container_page_pv
  end

  module NonSrc : sig
    type t = Id.non_src
    type t_pv = Id.non_src_pv
  end

  module SourcePage : sig
    type t = Id.source_page
    type t_pv = Id.source_page_pv
  end

  module SourceLocation : sig
    type t = Id.source_location
    type t_pv = Id.source_location_pv
  end

  module AssetFile : sig
    type t = Id.asset_file
    type t_pv = Id.asset_file_pv
  end

  module OdocId : sig
    type t = Id.odoc_id
    type t_pv = Id.odoc_id_pv
  end

  module Path : sig
    module Module :
      IdSig with type t = Id.path_module and type t_pv = Id.path_module_pv

    module ModuleType :
      IdSig with type t = Id.path_module_type and type t_pv = Id.module_type_pv

    module Type :
      IdSig with type t = Id.path_type and type t_pv = Id.path_type_pv

    module DataType :
      IdSig with type t = Id.path_datatype and type t_pv = Id.path_datatype_pv

    module Constructor :
      IdSig with type t = Id.path_constructor and type t_pv = Id.constructor_pv

    module Value : IdSig with type t = Id.path_value and type t_pv = Id.value_pv

    module ClassType :
      IdSig
        with type t = Id.path_class_type
         and type t_pv = Id.path_class_type_pv

    type t = Id.path_any
  end

  type t = Id.any

  type t_pv = Id.any_pv

  val hash : t -> int

  val name : [< t_pv ] id -> string

  (* val root : [< t_pv ] id -> RootModule.t_pv id option *)

  val fullname : [< t_pv ] id -> string list
  (** The fullname of value [x] in module [M] is [M.x], whereas the regular name
      is [x]. *)

  val is_internal : [< t_pv ] id -> bool

  val compare : t -> t -> int

  val equal : ([< t_pv ] id as 'a) -> 'a -> bool

  val label_parent : [< NonSrc.t_pv ] id -> LabelParent.t

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

    val asset_file : Page.t * string -> AssetFile.t

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
      DataType.t * ConstructorName.t ->
      [> `Constructor of DataType.t * ConstructorName.t ] id

    val field :
      FieldParent.t * FieldName.t ->
      [> `Field of FieldParent.t * FieldName.t ] id

    val extension :
      Signature.t * ExtensionName.t ->
      [> `Extension of Signature.t * ExtensionName.t ] id

    val extension_decl :
      Signature.t * (ExtensionName.t * ExtensionName.t) ->
      [> `ExtensionDecl of Signature.t * ExtensionName.t * ExtensionName.t ] id
    (** [extension_decl (sg, e1, eN)] defines an extension declaration where [sg] is the parent,
        [e1] is the first constructor of the extension, and [eN] is the constructor the Id is created for.
        [e1] will be used for the url, and [eN] will be the one displayed.
        The first constructor of the extension will always be used to reference the extension point. *)

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

    val source_location :
      SourcePage.t * DefName.t ->
      [> `SourceLocation of SourcePage.t * DefName.t ] id

    val source_location_mod :
      SourcePage.t -> [> `SourceLocationMod of SourcePage.t ] id

    val source_location_int :
      SourcePage.t * LocalName.t ->
      [> `SourceLocationInternal of SourcePage.t * LocalName.t ] id
  end
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig
  module Resolved : sig
    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val is_hidden : t -> weak_canonical_test:bool -> bool

      (* val identifier : t -> Identifier.Path.Module.t *)

      (* val root : t -> string option *)
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_path.module_type

      (* val is_hidden : t -> weak_canonical_test:bool -> bool *)

      (* val identifier : t -> Identifier.Path.ModuleType.t *)
    end

    module Type : sig
      type t = Paths_types.Resolved_path.type_

      (* val of_ident : Identifier.Path.Type.t -> t *)

      (* val is_hidden : t -> bool *)

      (* val identifier : t -> Identifier.Path.Type.t *)
    end

    module DataType : sig
      type t = Paths_types.Resolved_path.datatype
    end

    module Constructor : sig
      type t = Paths_types.Resolved_path.constructor
    end

    module Value : sig
      type t = Paths_types.Resolved_path.value

      (* val of_ident : Identifier.Path.Value.t -> t *)

      (* val is_hidden : t -> bool *)

      (* val identifier : t -> Identifier.Path.Type.t *)
    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      (* val of_ident : Identifier.Path.ClassType.t -> t *)

      (* val is_hidden : t -> bool *)
    end

    type t = Paths_types.Resolved_path.any

    val identifier : t -> Identifier.t

    val is_hidden : t -> bool
  end

  module Module : sig
    type t = Paths_types.Path.module_

    (* val root : t -> string option *)
  end

  module ModuleType : sig
    type t = Paths_types.Path.module_type
  end

  module Type : sig
    type t = Paths_types.Path.type_
  end

  module DataType : sig
    type t = Paths_types.Path.datatype
  end

  module Constructor : sig
    type t = Paths_types.Path.constructor
  end

  module Value : sig
    type t = Paths_types.Path.value
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
    end

    module Module : sig
      type t = Paths_types.Resolved_fragment.module_
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_fragment.module_type
    end

    module Type : sig
      type t = Paths_types.Resolved_fragment.type_
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    type root = Paths_types.Resolved_fragment.root

    type t = Paths_types.Resolved_fragment.any

    val identifier : t -> Identifier.t

    val is_hidden : t -> bool
  end

  module Signature : sig
    type t = Paths_types.Fragment.signature
  end

  module Module : sig
    type t = Paths_types.Fragment.module_
  end

  module ModuleType : sig
    type t = Paths_types.Fragment.module_type
  end

  module Type : sig
    type t = Paths_types.Fragment.type_
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

    module FieldParent : sig
      type t = Paths_types.Resolved_reference.field_parent
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

    module ExtensionDecl : sig
      type t = Paths_types.Resolved_reference.extension_decl
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

  module FragmentTypeParent : sig
    type t = Paths_types.Reference.fragment_type_parent
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

  module ExtensionDecl : sig
    type t = Paths_types.Reference.extension_decl
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
