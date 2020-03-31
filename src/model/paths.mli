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

  end

  module ClassSignature : sig
    type t = Paths_types.Identifier.class_signature
  end

  module DataType : sig
    type t = Paths_types.Identifier.datatype
  end

  module Parent : sig
    type t = Paths_types.Identifier.parent
  end

  module LabelParent : sig
    type t = Paths_types.Identifier.label_parent
  end

  module Module : sig
    type t = Paths_types.Identifier.module_
  end

  module ModuleType : sig
    type t = Paths_types.Identifier.module_type
  end

  module Type : sig
    type t = Paths_types.Identifier.type_
  end

  module Constructor : sig
    type t = Paths_types.Identifier.constructor
  end

  module Field : sig
    type t = Paths_types.Identifier.field
  end

  module Extension : sig
    type t = Paths_types.Identifier.extension
  end

  module Exception : sig
    type t = Paths_types.Identifier.exception_
  end

  module Value : sig
    type t = Paths_types.Identifier.value
  end

  module Class : sig
    type t = Paths_types.Identifier.class_
  end

  module ClassType : sig
    type t = Paths_types.Identifier.class_type
  end

  module Method : sig
    type t = Paths_types.Identifier.method_
  end

  module InstanceVariable : sig
    type t = Paths_types.Identifier.instance_variable
  end

  module Label : sig
    type t = Paths_types.Identifier.label
  end

  module Page : sig
    type t = Paths_types.Identifier.page
  end

  module Path : sig

    module Module : sig
      type t = Paths_types.Identifier.path_module
    end

    module ModuleType : sig
      type t = Paths_types.Identifier.path_module_type
    end

    module Type : sig
      type t = Paths_types.Identifier.path_type
    end

    module ClassType : sig
      type t = Paths_types.Identifier.path_class_type
    end

    type t = Paths_types.Identifier.path_any
  end

  type t = Paths_types.Identifier.any

  val hash : t -> int

  val name : [< t] -> string
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

