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

  module type IdSig = sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Id = Paths_types.Identifier

  module Any : IdSig with type t = Id.any

  module RootModule : IdSig with type t = Id.root_module

  module Signature : IdSig with type t = Id.signature

  module ClassSignature : IdSig with type t = Id.class_signature

  module LabelParent : IdSig with type t = Id.label_parent

  module Module : IdSig with type t = Id.module_

  module FunctorParameter : sig
    include IdSig with type t = Id.functor_parameter

    val functor_arg_pos : t -> int
    (** Gets the index in which the functor argument is, in the argument list.
        Useful to turn identifiers into unique anchors, since multiple arguments
        can have the same name. *)
  end

  module ModuleType : IdSig with type t = Id.module_type

  module Type : IdSig with type t = Id.type_

  module Class : IdSig with type t = Id.class_

  module ClassType : IdSig with type t = Id.class_type

  module DataType : sig
    type t = Id.datatype
  end
  module FieldParent : sig
    type t = Id.field_parent
  end
  module UnboxedFieldParent : sig
    type t = Id.unboxed_field_parent
  end

  module FunctorResult : sig
    type t = Id.functor_result
  end

  module Constructor : sig
    type t = Id.constructor
  end

  module Field : sig
    type t = Id.field
  end

  module UnboxedField : sig
    type t = Id.unboxed_field
  end

  module Extension : sig
    type t = Id.extension
  end

  module ExtensionDecl : sig
    type t = Paths_types.Identifier.extension_decl

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Exception : sig
    type t = Id.exception_
  end

  module Value : sig
    type t = Id.value
  end

  module Method : sig
    type t = Id.method_
  end

  module InstanceVariable : sig
    type t = Id.instance_variable
  end
  module Label : IdSig with type t = Id.label

  module Page : sig
    type t = Id.page
  end

  module LeafPage : sig
    type t = Id.leaf_page
  end

  module ContainerPage : sig
    type t = Id.container_page
  end

  module NonSrc : sig
    type t = Id.non_src
    val hash : t -> int
    val equal : ([< t ] as 'a) -> 'a -> bool
  end

  module SourcePage : sig
    type t = Id.source_page
  end

  module SourceLocation : sig
    type t = Id.source_location
  end

  module AssetFile : sig
    type t = Id.asset_file
  end

  module OdocId : sig
    type t = Id.odoc_id
  end

  module Path : sig
    module Module : IdSig with type t = Id.path_module

    module ModuleType : IdSig with type t = Id.path_module_type

    module Type : IdSig with type t = Id.path_type

    module Value : IdSig with type t = Id.path_value

    module ClassType : IdSig with type t = Id.path_class_type

    type t = Id.path_any
  end

  type t = Id.any

  val hash : t -> int

  val name : [< t ] -> string

  val fullname : [< t ] -> string list
  (** The fullname of value [x] in module [M] is [M.x], whereas the regular name
      is [x]. *)

  val is_hidden : [< t ] -> bool

  val compare : t -> t -> int

  val equal : ([< t ] as 'a) -> 'a -> bool

  val label_parent : [< NonSrc.t ] -> LabelParent.t

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

  module Hashtbl : sig
    module Any : Hashtbl.S with type key = Any.t
    module ContainerPage : Hashtbl.S with type key = ContainerPage.t
    module LeafPage : Hashtbl.S with type key = LeafPage.t
    module RootModule : Hashtbl.S with type key = RootModule.t
    module SourcePage : Hashtbl.S with type key = SourcePage.t
  end

  module Mk : sig
    open Names

    val page :
      ContainerPage.t option * PageName.t ->
      [> `Page of ContainerPage.t option * PageName.t ]

    val leaf_page :
      ContainerPage.t option * PageName.t ->
      [> `LeafPage of ContainerPage.t option * PageName.t ]

    val source_page : ContainerPage.t * string -> SourcePage.t

    val asset_file : Page.t * AssetName.t -> AssetFile.t

    val root :
      ContainerPage.t option * ModuleName.t ->
      [> `Root of ContainerPage.t option * ModuleName.t ]

    val implementation : string -> [> `Implementation of ModuleName.t ]

    val module_ :
      Signature.t * ModuleName.t -> [> `Module of Signature.t * ModuleName.t ]

    val parameter :
      Signature.t * ModuleName.t -> [> `Parameter of Signature.t * ModuleName.t ]

    val result : Signature.t -> [> `Result of Signature.t ]

    val module_type :
      Signature.t * ModuleTypeName.t ->
      [> `ModuleType of Signature.t * ModuleTypeName.t ]

    val class_ :
      Signature.t * TypeName.t -> [> `Class of Signature.t * TypeName.t ]

    val class_type :
      Signature.t * TypeName.t -> [> `ClassType of Signature.t * TypeName.t ]

    val type_ :
      Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ]

    val core_type : string -> [> `CoreType of TypeName.t ]

    val constructor :
      DataType.t * ConstructorName.t ->
      [> `Constructor of DataType.t * ConstructorName.t ]

    val field :
      FieldParent.t * FieldName.t -> [> `Field of FieldParent.t * FieldName.t ]

    val unboxed_field :
      UnboxedFieldParent.t * UnboxedFieldName.t ->
      [> `UnboxedField of UnboxedFieldParent.t * UnboxedFieldName.t ]

    val extension :
      Signature.t * ExtensionName.t ->
      [> `Extension of Signature.t * ExtensionName.t ]

    val extension_decl :
      Signature.t * (ExtensionName.t * ExtensionName.t) ->
      [> `ExtensionDecl of Signature.t * ExtensionName.t * ExtensionName.t ]
    (** [extension_decl (sg, e1, eN)] defines an extension declaration where
        [sg] is the parent, [e1] is the first constructor of the extension, and
        [eN] is the constructor the Id is created for. [e1] will be used for the
        url, and [eN] will be the one displayed. The first constructor of the
        extension will always be used to reference the extension point. *)

    val exception_ :
      Signature.t * ExceptionName.t ->
      [> `Exception of Signature.t * ExceptionName.t ]

    val value :
      Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ]

    val method_ :
      ClassSignature.t * MethodName.t ->
      [> `Method of ClassSignature.t * MethodName.t ]

    val instance_variable :
      ClassSignature.t * InstanceVariableName.t ->
      [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ]

    val label :
      LabelParent.t * LabelName.t -> [> `Label of LabelParent.t * LabelName.t ]

    val source_location :
      SourcePage.t * DefName.t ->
      [> `SourceLocation of SourcePage.t * DefName.t ]

    val source_location_mod :
      SourcePage.t -> [> `SourceLocationMod of SourcePage.t ]

    val source_location_int :
      SourcePage.t * LocalName.t ->
      [> `SourceLocationInternal of SourcePage.t * LocalName.t ]
  end

  val fresh_include_parent : Signature.t -> Signature.t
  (** Create a synthetic parent identifier for items inside an include's module
      type expression. Uses a lowercase module name (illegal in normal OCaml) to
      ensure no clashes with real identifiers. Each call returns a fresh
      identifier. *)

  val fresh_module_arg_parent : unit -> Signature.t
  (** Create a synthetic parent identifier for module arguments, which can't
      have unique identifier, as they can be introduced multiple times with the
      same name in a single type expression . *)
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig
  module Resolved : sig
    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val is_hidden : t -> weak_canonical_test:bool -> bool

      val equal : t -> t -> bool

      val hash : t -> int

      module Hashtbl : Hashtbl.S with type key = t

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

    module Value : sig
      type t = Paths_types.Resolved_path.value
    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      (* val of_ident : Identifier.Path.ClassType.t -> t *)

      (* val is_hidden : t -> bool *)
    end

    type t = Paths_types.Resolved_path.any

    val identifier : t -> Identifier.t option
    (** If the path points to a core type, no identifier can be generated *)

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

    val identifier : t -> Identifier.t option

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

    module UnboxedFieldParent : sig
      type t = Paths_types.Resolved_reference.unboxed_field_parent
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

    module UnboxedField : sig
      type t = Paths_types.Resolved_reference.unboxed_field
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

    module Asset : sig
      type t = Paths_types.Resolved_reference.asset

      val identifier : t -> Identifier.AssetFile.t
    end

    type t = Paths_types.Resolved_reference.any

    val identifier : t -> Identifier.t option
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

  module UnboxedField : sig
    type t = Paths_types.Reference.unboxed_field
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

  module Asset : sig
    type t = Paths_types.Reference.asset
  end

  module Hierarchy : sig
    type t = Paths_types.Reference.hierarchy
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any
  type tag_hierarchy = Paths_types.Reference.tag_hierarchy
end
