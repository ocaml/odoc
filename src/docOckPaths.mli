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

(** Paths of documentation *)

(** {3 Kinds } *)

module Kind : sig

  (** {4 General purpose kinds} *)

  (** Any possible referent *)
  type any =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label ]

  (** A referent that can contain signature items *)
  type signature = [ `Module | `ModuleType ]

  (** A referent that can contain class signature items *)
  type class_signature = [ `Class | `ClassType ]

  (** A referent that can contain datatype items *)
  type datatype = [ `Type ]

  (** A referent that can contain other items *)
  type parent = [ signature | class_signature | datatype ]

  (** {4 Identifier kinds}
      The kind of an identifier directly corresponds to the kind of its
      referent. *)

  type identifier = any

  type identifier_module = [ `Module ]
  type identifier_module_type = [ `ModuleType ]
  type identifier_type =  [ `Type ]
  type identifier_constructor = [ `Constructor ]
  type identifier_field = [ `Field ]
  type identifier_extension = [ `Extension ]
  type identifier_exception = [ `Exception ]
  type identifier_value = [ `Value ]
  type identifier_class = [ `Class ]
  type identifier_class_type = [ `ClassType ]
  type identifier_method = [ `Method ]
  type identifier_instance_variable = [ `InstanceVariable ]
  type identifier_label = [ `Label ]

  (** {4 Path kinds}
      There are four kinds of OCaml path:

        - module
        - module type
        - type
        - class type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path may refer to a class definition). *)

  type path = [ `Module | `ModuleType | `Type | `Class | `ClassType ]

  type path_module = [ `Module ]
  type path_module_type = [ `ModuleType ]
  type path_type = [ `Type | `Class | `ClassType ]
  type path_class_type = [ `Class | `ClassType ]

  (** {4 Fragment kinds}
      There are two kinds of OCaml path fragment:

        - module
        - type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path fragment may refer to a class
      definition). *)

  type fragment = [ `Module | `Type | `Class | `ClassType ]

  type fragment_module = [ `Module ]
  type fragment_type = [ `Type | `Class | `ClassType ]

  (** {4 Reference kinds}
      There is one reference kind for each kind of referent. However,
      the kind of a reference does not refer to the kind of its
      referent, but to the kind with which the reference was annotated.

      This means that reference kinds do not correspond directly to the
      kind of their referent because we used more relaxed rules when
      resolving a reference. For example, a reference annotated as being
      to a constructor can be resolved to the definition of an exception
      (which can be thought of a sort of constructor). *)

  type reference = any

  type reference_module = [ `Module ]
  type reference_module_type = [ `ModuleType ]
  type reference_type = [ `Type | `Class | `ClassType ]
  type reference_constructor = [ `Constructor | `Extension | `Exception ]
  type reference_field = [ `Field ]
  type reference_extension = [ `Extension | `Exception ]
  type reference_exception = [ `Exception ]
  type reference_value = [ `Value ]
  type reference_class = [ `Class ]
  type reference_class_type = [ `Class | `ClassType ]
  type reference_method = [ `Method ]
  type reference_instance_variable = [ `InstanceVariable ]
  type reference_label = [ `Label ]

end

open Kind

(** {3 Identifiers} **)

(** Identifiers for definitions *)
module Identifier : sig

  type kind = Kind.identifier

  type ('a, 'b) t =
    | Root : 'a -> ('a, [< kind > `Module]) t
    | Module : 'a signature * string -> ('a, [< kind > `Module]) t
    | Argument : 'a signature * int * string -> ('a, [< kind > `Module]) t
    | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
    | Type : 'a signature * string -> ('a, [< kind > `Type]) t
    | CoreType : string -> ('a, [< kind > `Type]) t
    | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
    | Field : 'a datatype * string -> ('a, [< kind > `Field]) t
    | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
    | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
    | CoreException : string -> ('a, [< kind > `Exception]) t
    | Value : 'a signature * string -> ('a, [< kind > `Value]) t
    | Class : 'a signature * string -> ('a, [< kind > `Class]) t
    | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
    | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
    | InstanceVariable : 'a class_signature * string ->
                           ('a, [< kind > `InstanceVariable]) t
    | Label : 'a parent * string -> ('a, [< kind > `Label]) t

  and 'a any = ('a, kind) t
  and 'a signature = ('a, Kind.signature) t
  and 'a class_signature = ('a, Kind.class_signature) t
  and 'a datatype = ('a, Kind.datatype) t
  and 'a parent = ('a, Kind.parent) t

  type 'a module_ = ('a, identifier_module) t
  type 'a module_type = ('a, identifier_module_type) t
  type 'a type_ =  ('a, identifier_type) t
  type 'a constructor = ('a, identifier_constructor) t
  type 'a field = ('a, identifier_field) t
  type 'a extension = ('a, identifier_extension) t
  type 'a exception_ = ('a, identifier_exception) t
  type 'a value = ('a, identifier_value) t
  type 'a class_ = ('a, identifier_class) t
  type 'a class_type = ('a, identifier_class_type) t
  type 'a method_ = ('a, identifier_method) t
  type 'a instance_variable = ('a, identifier_instance_variable) t
  type 'a label = ('a, identifier_label) t

  type 'a path_module = ('a, Kind.path_module) t
  type 'a path_module_type = ('a, Kind.path_module_type) t
  type 'a path_type =  ('a, Kind.path_type) t
  type 'a path_class_type = ('a, Kind.path_class_type) t

  type 'a fragment_module = ('a, Kind.fragment_module) t
  type 'a fragment_type =  ('a, Kind.fragment_type) t

  type 'a reference_module = ('a, Kind.reference_module) t
  type 'a reference_module_type = ('a, Kind.reference_module_type) t
  type 'a reference_type =  ('a, Kind.reference_type) t
  type 'a reference_constructor = ('a, Kind.reference_constructor) t
  type 'a reference_field = ('a, Kind.reference_field) t
  type 'a reference_extension = ('a, Kind.reference_extension) t
  type 'a reference_exception = ('a, Kind.reference_exception) t
  type 'a reference_value = ('a, Kind.reference_value) t
  type 'a reference_class = ('a, Kind.reference_class) t
  type 'a reference_class_type = ('a, Kind.reference_class_type) t
  type 'a reference_method = ('a, Kind.reference_method) t
  type 'a reference_instance_variable = ('a, Kind.reference_instance_variable) t
  type 'a reference_label = ('a, Kind.reference_label) t

  val signature_of_module : 'a module_ -> 'a signature

  val signature_of_module_type : 'a module_type -> 'a signature

  val class_signature_of_class : 'a class_ -> 'a class_signature

  val class_signature_of_class_type : 'a class_type -> 'a class_signature

  val datatype_of_type : 'a type_ -> 'a datatype

  val parent_of_signature : 'a signature -> 'a parent

  val parent_of_class_signature : 'a class_signature -> 'a parent

  val parent_of_datatype : 'a datatype -> 'a parent

  val any : ('a, 'b) t -> 'a any

  val name : ('a, 'b) t -> string option

end

(** {3 Paths} *)

(** OCaml paths *)
module rec Path : sig

  module Resolved : sig

    type kind = Kind.path

    type ('a, 'b) t =
      | Identifier : ('a, 'b) Identifier.t -> ('a, [< kind] as 'b) t
      | Module : 'a module_ * string -> ('a, [< kind > `Module]) t
      | Apply : 'a module_ * 'a Path.module_ -> ('a, [< kind > `Module]) t
      | ModuleType : 'a module_ * string -> ('a, [< kind > `ModuleType]) t
      | Type : 'a module_ * string -> ('a, [< kind > `Type]) t
      | Class : 'a module_ * string -> ('a, [< kind > `Class]) t
      | ClassType : 'a module_ * string -> ('a, [< kind > `ClassType]) t

    and 'a any = ('a, kind) t

    and 'a module_ = ('a, path_module) t
    and 'a module_type = ('a, path_module_type) t
    and 'a type_ = ('a, path_type) t
    and 'a class_type = ('a, path_class_type) t

    val ident_module : 'a Identifier.module_ -> 'a module_

    val ident_module_type : 'a Identifier.module_type -> 'a module_type

    val ident_type : 'a Identifier.type_ -> 'a type_

    val ident_class : 'a Identifier.class_ -> 'a class_type

    val ident_class_type : 'a Identifier.class_type -> 'a class_type

    val any : ('a, 'b) t -> 'a any

    val identifier: ('a, 'b) t -> ('a, 'b) Identifier.t

  end

  type kind = Kind.path

  type ('a, 'b) t =
    | Resolved : ('a, 'b) Resolved.t -> ('a, 'b) t
    | Root : string -> ('a, [< kind > `Module]) t
    | Dot : 'a module_ * string -> ('a, [< kind]) t
    | Apply : 'a module_ * 'a module_ -> ('a, [< kind > `Module]) t

  and 'a any = ('a, kind) t

  and 'a module_ = ('a, path_module) t
  and 'a module_type = ('a, path_module_type) t
  and 'a type_ = ('a, path_type) t
  and 'a class_type = ('a, path_class_type) t

  val ident_module : 'a Identifier.module_ -> 'a module_

  val ident_module_type : 'a Identifier.module_type -> 'a module_type

  val ident_type : 'a Identifier.type_ -> 'a type_

  val ident_class : 'a Identifier.class_ -> 'a class_type

  val ident_class_type : 'a Identifier.class_type -> 'a class_type

  val any : ('a, 'b) t -> 'a any

  val module_ : 'a module_ -> string -> 'a module_

  val apply : 'a module_ -> 'a module_ -> 'a module_

  val module_type : 'a module_ -> string -> 'a module_type

  val type_ : 'a module_ -> string -> 'a type_

  val class_ : 'a module_ -> string -> 'a class_type

  val class_type_ : 'a module_ -> string -> 'a class_type

end

(** {3 Fragments} *)

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    type kind = Kind.fragment

    type sort = [ `Root | `Branch ]

    type ('a, 'b) raw =
      | Root : ('a, [< sort > `Root]) raw
      | Module : signature * string -> ([< kind > `Module], [< sort > `Branch]) raw
      | Type : signature * string -> ([< kind > `Type], [< sort > `Branch]) raw
      | Class : signature * string -> ([< kind > `Class], [< sort > `Branch]) raw
      | ClassType : signature * string -> ([< kind > `ClassType], [< sort > `Branch]) raw

    and 'a t = ('a, [`Branch]) raw

    and any = kind t
    and signature = (fragment_module, [`Root | `Branch]) raw

    and module_ = fragment_module t
    and type_ = fragment_type t

    val signature_of_module : module_ -> signature

    val any : 'a t -> any

    val path: 'a Path.module_ -> 'b t -> ('a, 'b) Path.t

    val identifier: 'a Identifier.signature -> 'b t ->
                    ('a, 'b) Identifier.t

    val split : 'a t -> string * 'a t option

  end

  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('a, 'b) raw =
    | Resolved : ('a, 'b) Resolved.raw -> ('a, 'b) raw
    | Dot : signature * string -> ([< kind], [< sort > `Branch]) raw

  and 'a t = ('a, [`Branch]) raw

  and any = kind t
  and signature = (fragment_module, [`Root | `Branch]) raw

  and module_ = fragment_module t
  and type_ = fragment_type t

  val signature_of_module : module_ -> signature

  val any : 'a t -> any

  val path: 'a Path.module_ -> 'b t -> ('a, 'b) Path.t

  val split: 'a t -> string * 'a t option

end


(** {3 References} *)

(** References to definitions *)
module Reference : sig

  module Resolved : sig

    type kind = Kind.reference

    type ('a, 'b) t =
      | Identifier : ('a, 'b) Identifier.t -> ('a, 'b) t
      | Module : 'a signature * string -> ('a, [< kind > `Module]) t
      | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
      | Type : 'a signature * string -> ('a, [< kind > `Type]) t
      | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
      | Field : 'a datatype * string -> ('a, [< kind > `Field]) t
      | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
      | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
      | Value : 'a signature * string -> ('a, [< kind > `Value]) t
      | Class : 'a signature * string -> ('a, [< kind > `Class]) t
      | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
      | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
      | InstanceVariable : 'a class_signature * string ->
                             ('a, [< kind > `InstanceVariable]) t
      | Label : 'a parent * string -> ('a, [< kind > `Label]) t

    and 'a any = ('a, kind) t
    and 'a signature = ('a, Kind.signature) t
    and 'a class_signature = ('a, Kind.class_signature) t
    and 'a datatype = ('a, Kind.datatype) t
    and 'a parent = ('a, Kind.parent) t

    type 'a module_ = ('a, reference_module) t
    type 'a module_type = ('a, reference_module_type) t
    type 'a type_ = ('a, reference_type) t
    type 'a constructor = ('a, reference_constructor) t
    type 'a field = ('a, reference_field) t
    type 'a extension = ('a, reference_extension) t
    type 'a exception_ = ('a, reference_exception) t
    type 'a value = ('a, reference_value) t
    type 'a class_ = ('a, reference_class) t
    type 'a class_type = ('a, reference_class_type) t
    type 'a method_ = ('a, reference_method) t
    type 'a instance_variable = ('a, reference_instance_variable) t
    type 'a label = ('a, reference_label) t

    val ident_module : 'a Identifier.module_ -> 'a module_

    val ident_module_type : 'a Identifier.module_type -> 'a module_type

    val ident_type : 'a Identifier.type_ -> 'a type_

    val ident_datatype : 'a Identifier.type_ -> 'a datatype

    val ident_constructor : 'a Identifier.constructor -> 'a constructor

    val ident_field : 'a Identifier.field -> 'a field

    val ident_extension : 'a Identifier.extension -> 'a extension

    val ident_exception : 'a Identifier.exception_ -> 'a exception_

    val ident_value : 'a Identifier.value -> 'a value

    val ident_class : 'a Identifier.class_ -> 'a class_

    val ident_class_type : 'a Identifier.class_type -> 'a class_type

    val ident_method : 'a Identifier.method_ -> 'a method_

    val ident_instance_variable : 'a Identifier.instance_variable ->
                                    'a instance_variable

    val ident_label : 'a Identifier.label -> 'a label


    val signature_of_module : 'a module_ -> 'a signature

    val signature_of_module_type : 'a module_type -> 'a signature

    val class_signature_of_class : 'a class_ -> 'a class_signature

    val class_signature_of_class_type : 'a class_type -> 'a class_signature

    val parent_of_signature : 'a signature -> 'a parent

    val parent_of_class_signature : 'a class_signature -> 'a parent

    val parent_of_datatype : 'a datatype -> 'a parent

    val any : ('a, 'b) t -> 'a any

    val identifier: ('a, 'b) t -> ('a, 'b) Identifier.t

  end

  type kind = Kind.reference

  type ('a, 'b) t =
    | Resolved : ('a, 'b) Resolved.t -> ('a, 'b) t
    | Root : string -> ('a, [< kind]) t
    | Dot : 'a parent * string -> ('a, [< kind]) t

  and 'a any = ('a, kind) t
  and 'a signature = ('a, Kind.signature) t
  and 'a class_signature = ('a, Kind.class_signature) t
  and 'a datatype = ('a, Kind.datatype) t
  and 'a parent = ('a, Kind.parent) t

  type 'a module_ = ('a, reference_module) t
  type 'a module_type = ('a, reference_module_type) t
  type 'a type_ = ('a, reference_type) t
  type 'a constructor = ('a, reference_constructor) t
  type 'a field = ('a, reference_field) t
  type 'a extension = ('a, reference_extension) t
  type 'a exception_ = ('a, reference_exception) t
  type 'a value = ('a, reference_value) t
  type 'a class_ = ('a, reference_class) t
  type 'a class_type = ('a, reference_class_type) t
  type 'a method_ = ('a, reference_method) t
  type 'a instance_variable = ('a, reference_instance_variable) t
  type 'a label = ('a, reference_label) t

  val ident_module : 'a Identifier.module_ -> 'a module_

  val ident_module_type : 'a Identifier.module_type -> 'a module_type

  val ident_type : 'a Identifier.type_ -> 'a type_

  val ident_datatype : 'a Identifier.type_ -> 'a datatype

  val ident_constructor : 'a Identifier.constructor -> 'a constructor

  val ident_field : 'a Identifier.field -> 'a field

  val ident_extension : 'a Identifier.extension -> 'a extension

  val ident_exception : 'a Identifier.exception_ -> 'a exception_

  val ident_value : 'a Identifier.value -> 'a value

  val ident_class : 'a Identifier.class_ -> 'a class_

  val ident_class_type : 'a Identifier.class_type -> 'a class_type

  val ident_method : 'a Identifier.method_ -> 'a method_

  val ident_instance_variable : 'a Identifier.instance_variable ->
                                  'a instance_variable

  val ident_label : 'a Identifier.label -> 'a label

  val signature_of_module : 'a module_ -> 'a signature

  val signature_of_module_type : 'a module_type -> 'a signature

  val class_signature_of_class : 'a class_ -> 'a class_signature

  val class_signature_of_class_type : 'a class_type -> 'a class_signature

  val parent_of_signature : 'a signature -> 'a parent

  val parent_of_class_signature : 'a class_signature -> 'a parent

  val parent_of_datatype : 'a datatype -> 'a parent

  val any : ('a, 'b) t -> 'a any

end
