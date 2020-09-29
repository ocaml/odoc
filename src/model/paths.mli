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

(** Identifiers for definitions *)

module Identifier : sig
  (** {2 Generic operations} *)

  module Signature : sig
    type t = Paths_types.Identifier.signature

    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module ClassSignature : sig
    type t = Paths_types.Identifier.class_signature
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module DataType : sig
    type t = Paths_types.Identifier.datatype
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Parent : sig
    type t = Paths_types.Identifier.parent
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module LabelParent : sig
    type t = Paths_types.Identifier.label_parent
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module RootModule : sig
    type t = Paths_types.Identifier.root_module
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Module : sig
    type t = Paths_types.Identifier.module_
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module FunctorParameter : sig
    type t = Paths_types.Identifier.functor_parameter
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module ModuleType : sig
    type t = Paths_types.Identifier.module_type
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Type : sig
    type t = Paths_types.Identifier.type_
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Constructor : sig
    type t = Paths_types.Identifier.constructor
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Field : sig
    type t = Paths_types.Identifier.field
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Extension : sig
    type t = Paths_types.Identifier.extension
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Exception : sig
    type t = Paths_types.Identifier.exception_
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Value : sig
    type t = Paths_types.Identifier.value
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Class : sig
    type t = Paths_types.Identifier.class_
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module ClassType : sig
    type t = Paths_types.Identifier.class_type
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Method : sig
    type t = Paths_types.Identifier.method_
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module InstanceVariable : sig
    type t = Paths_types.Identifier.instance_variable
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Label : sig
    type t = Paths_types.Identifier.label
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Page : sig
    type t = Paths_types.Identifier.page
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module ContainerPage : sig
    type t = Paths_types.Identifier.container_page
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module OdocId : sig
    type t = Paths_types.Identifier.odoc_id
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
    end

  module Path : sig

    module Module : sig
      type t = Paths_types.Identifier.path_module
      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
      end

    module ModuleType : sig
      type t = Paths_types.Identifier.path_module_type
      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
      end

    module Type : sig
      type t = Paths_types.Identifier.path_type
      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
      end

    module ClassType : sig
      type t = Paths_types.Identifier.path_class_type
      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
      end

    type t = Paths_types.Identifier.path_any
    
  end

  type t = Paths_types.Identifier.any

  val hash : t -> int

  val name : [< t] -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val label_parent : [< t] -> LabelParent.t

  module Sets : sig
    module Signature : Set.S with type elt = Signature.t
    module ClassSignature : Set.S with type elt = ClassSignature.t
    module DataType: Set.S with type elt = DataType.t
    module Parent: Set.S with type elt = Parent.t
    module LabelParent: Set.S with type elt = LabelParent.t
    module Module: Set.S with type elt = Module.t
    module ModuleType: Set.S with type elt = ModuleType.t
    module Type: Set.S with type elt = Type.t
    module Constructor: Set.S with type elt = Constructor.t
    module Field: Set.S with type elt = Field.t
    module Extension: Set.S with type elt = Extension.t
    module Exception: Set.S with type elt = Exception.t
    module Value: Set.S with type elt = Value.t
    module Class: Set.S with type elt = Class.t
    module ClassType: Set.S with type elt = ClassType.t
    module Method: Set.S with type elt = Method.t
    module InstanceVariable: Set.S with type elt = InstanceVariable.t
    module Label: Set.S with type elt = Label.t
    module Page: Set.S with type elt = Page.t
    module ContainerPage: Set.S with type elt = ContainerPage.t
  end

  module Maps : sig
    module Signature : Map.S with type key = Signature.t
    module ClassSignature : Map.S with type key = ClassSignature.t
    module DataType: Map.S with type key = DataType.t
    module Parent: Map.S with type key = Parent.t
    module LabelParent: Map.S with type key = LabelParent.t
    module FunctorParameter : Map.S with type key = FunctorParameter.t
    module Module: Map.S with type key = Module.t
    module ModuleType: Map.S with type key = ModuleType.t
    module Type: Map.S with type key = Type.t
    module Constructor: Map.S with type key = Constructor.t
    module Field: Map.S with type key = Field.t
    module Extension: Map.S with type key = Extension.t
    module Exception: Map.S with type key = Exception.t
    module Value: Map.S with type key = Value.t
    module Class: Map.S with type key = Class.t
    module ClassType: Map.S with type key = ClassType.t
    module Method: Map.S with type key = Method.t
    module InstanceVariable: Map.S with type key = InstanceVariable.t
    module Label: Map.S with type key = Label.t
    module Page: Map.S with type key = Page.t
    module ContainerPage : Map.S with type key = ContainerPage.t
    module Path : sig
      module Module: Map.S with type key = Path.Module.t
      module ModuleType: Map.S with type key = Path.ModuleType.t
      module Type: Map.S with type key = Path.Type.t
      module ClassType : Map.S with type key = Path.ClassType.t
    end
  end
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val of_ident : Identifier.Path.Module.t -> t

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.Module.t

      val canonical_ident : t -> Identifier.Path.Module.t option
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_path.module_type

      val of_ident : Identifier.Path.ModuleType.t -> t

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.ModuleType.t

      val canonical_ident : t -> Identifier.Path.ModuleType.t option

    end

    module Type : sig
      type t = Paths_types.Resolved_path.type_

      val of_ident : Identifier.Path.Type.t -> t

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.Type.t

    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      val of_ident : Identifier.Path.ClassType.t -> t

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.ClassType.t

    end

    type t = Paths_types.Resolved_path.any

    val identifier : t -> Identifier.t
  end

  module Module : sig
    type t = Paths_types.Path.module_
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

    module Type : sig
      type t = Paths_types.Resolved_fragment.type_

      val split : t -> string * t option

    end

    type t = Paths_types.Resolved_fragment.any

    val identifier : t -> Identifier.t

  end

  module Signature : sig
    type t = Paths_types.Fragment.signature

    val split : t -> string * t option
  end

  module Module : sig
    type t = Paths_types.Fragment.module_

    val split : t -> string * t option
  end

  module Type : sig
    type t = Paths_types.Fragment.type_

    val split : t -> string * t option
  end

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
end

