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

(** {3 Sexp serialization} *)

type sexp =
  | List of sexp list
  | Atom of string

val string_of_sexp : sexp -> string

(** {1 Paths} *)

(** Every path is annotated with its kind. *)
module Kind : sig

  (** {4 General purpose kinds} *)

  (** Any possible referent *)
  type any =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label | `Page ]

  (** A referent that can contain signature items *)
  type signature = [ `Module | `ModuleType ]

  (** A referent that can contain class signature items *)
  type class_signature = [ `Class | `ClassType ]

  (** A referent that can contain datatype items *)
  type datatype = [ `Type ]

  (** A referent that can contain page items *)
  type page = [ `Page ]

  (** A referent that can contain other items *)
  type parent = [ signature | class_signature | datatype ]

  type label_parent = [ parent | page ]

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
  type identifier_page = [ `Page ]

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
      (which is a sort of constructor). *)

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
  type reference_page = [ `Page ]

end

open Kind

(** Identifiers for definitions *)
module Identifier : sig

  type kind = Kind.identifier

  type 'kind t =
    | Root : Root.t * string -> [< kind > `Module] t
    | Page : Root.t * string -> [< kind > `Page] t
    | Module : signature * string -> [< kind > `Module] t
    | Argument : signature * int * string -> [< kind > `Module] t
    | ModuleType : signature * string -> [< kind > `ModuleType] t
    | Type : signature * string -> [< kind > `Type] t
    | CoreType : string -> [< kind > `Type] t
    | Constructor : datatype * string -> [< kind > `Constructor] t
    | Field : parent * string -> [< kind > `Field] t
    | Extension : signature * string -> [< kind > `Extension] t
    | Exception : signature * string -> [< kind > `Exception] t
    | CoreException : string -> [< kind > `Exception] t
    | Value : signature * string -> [< kind > `Value] t
    | Class : signature * string -> [< kind > `Class] t
    | ClassType : signature * string -> [< kind > `ClassType] t
    | Method : class_signature * string -> [< kind > `Method] t
    | InstanceVariable : class_signature * string ->
        [< kind > `InstanceVariable] t
    | Label : label_parent * string -> [< kind > `Label] t

  and any = kind t
  and signature = Kind.signature t
  and class_signature = Kind.class_signature t
  and datatype = Kind.datatype t
  and parent = Kind.parent t
  and label_parent = Kind.label_parent t

  type module_ = identifier_module t
  type module_type = identifier_module_type t
  type type_ = identifier_type t
  type constructor = identifier_constructor t
  type field = identifier_field t
  type extension = identifier_extension t
  type exception_ = identifier_exception t
  type value = identifier_value t
  type class_ = identifier_class t
  type class_type = identifier_class_type t
  type method_ = identifier_method t
  type instance_variable = identifier_instance_variable t
  type label = identifier_label t
  type page = identifier_page t

  type path_module = Kind.path_module t
  type path_module_type = Kind.path_module_type t
  type path_type = Kind.path_type t
  type path_class_type = Kind.path_class_type t

  type fragment_module = Kind.fragment_module t
  type fragment_type = Kind.fragment_type t

  type reference_module = Kind.reference_module t
  type reference_module_type = Kind.reference_module_type t
  type reference_type =  Kind.reference_type t
  type reference_constructor = Kind.reference_constructor t
  type reference_field = Kind.reference_field t
  type reference_extension = Kind.reference_extension t
  type reference_exception = Kind.reference_exception t
  type reference_value = Kind.reference_value t
  type reference_class = Kind.reference_class t
  type reference_class_type = Kind.reference_class_type t
  type reference_method = Kind.reference_method t
  type reference_instance_variable = Kind.reference_instance_variable t
  type reference_label = Kind.reference_label t
  type reference_page = Kind.reference_page t

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val signature_of_module_type : module_type -> signature

  val class_signature_of_class : class_ -> class_signature

  val class_signature_of_class_type : class_type -> class_signature

  val datatype_of_type : type_ -> datatype

  val parent_of_signature : signature -> parent

  val parent_of_class_signature : class_signature -> parent

  val parent_of_datatype : datatype -> parent

  val label_parent_of_parent : parent -> label_parent

  val label_parent_of_page : page -> label_parent

  val any : 'kind t -> any

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int

  (** {3 Printing} *)

  val name : 'kind t -> string

  val sexp_of_t : 'kind t -> sexp

  (** {2 Root retrieval} *)

  val signature_root : signature -> Root.t

  val module_root : module_ -> Root.t

  val module_type_root : module_type -> Root.t

  val class_signature_root : class_signature -> Root.t
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    type kind = Kind.path

    type 'kind t =
      | Identifier : 'kind Identifier.t -> ([< kind] as 'kind) t
      | Subst : module_type * module_ -> [< kind > `Module] t
      | SubstAlias : module_ * module_ -> [< kind > `Module] t
      | Hidden : module_ -> [< kind > `Module ] t
      | Module : module_ * string -> [< kind > `Module] t
        (* TODO: The canonical path should be a reference not a path *)
      | Canonical : module_ * Path.module_ -> [< kind > `Module] t
      | Apply : module_ * Path.module_ -> [< kind > `Module] t
      | ModuleType : module_ * string -> [< kind > `ModuleType] t
      | Type : module_ * string -> [< kind > `Type] t
      | Class : module_ * string -> [< kind > `Class] t
      | ClassType : module_ * string -> [< kind > `ClassType] t

    and any = kind t
    and module_ = path_module t
    and module_type = path_module_type t
    and type_ = path_type t
    and class_type = path_class_type t

    (** {2 Creators} *)

    val ident_module : Identifier.module_ -> [< kind > `Module] t

    val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

    val ident_type : Identifier.type_ -> [< kind > `Type] t

    val ident_class : Identifier.class_ -> [< kind > `Class] t

    val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

    (** {2 Explicit coercion} *)

    val any : 'kind t -> any

    (** {2 Generic operations} *)

    val equal : 'kind t -> 'kind t -> bool

    val hash : 'kind t -> int

    val identifier: 'kind t -> 'kind Identifier.t
    (** [identifier rp] extracts the identifier present at the "root" of [rp]. *)

    val is_hidden : 'kind t -> bool
    (** [is_hidden rp] is [true] when some prefix of [rp] (which is not under a
        [Canonical]) is the [Hidden] constructor.

        [Canonical] are treated specialy because we expect them to rewrite a
        hidden path to a non-hidden one. *)

    val sexp_of_t : 'kind t -> sexp

    val rebase : Identifier.signature -> 'kind t -> 'kind t

    val equal_identifier : 'kind Identifier.t -> 'kind t -> bool
  end

  type kind = Kind.path

  type 'kind t =
    | Resolved : 'kind Resolved.t -> 'kind t
    | Root : string -> [< kind > `Module] t
    | Forward : string -> [< kind > `Module] t
    | Dot : module_ * string -> [< kind] t
    | Apply : module_ * module_ -> [< kind > `Module] t

  and any = kind t
  and module_ = path_module t
  and module_type = path_module_type t
  and type_ = path_type t
  and class_type = path_class_type t

  (** {2 Creators} *)

  val ident_module : Identifier.module_ -> [< kind > `Module] t

  val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

  val ident_type : Identifier.type_ -> [< kind > `Type] t

  val ident_class : Identifier.class_ -> [< kind > `Class] t

  val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

  val module_ : module_ -> string -> [< kind > `Module] t

  val apply : module_ -> module_ -> [< kind > `Module] t

  val module_type : module_ -> string -> [< kind > `ModuleType] t

  val type_ : module_ -> string -> [< kind > `Type] t

  val class_ : module_ -> string -> [< kind > `Class] t

  val class_type_ : module_ -> string -> [< kind > `ClassType] t

  (** {2 Explicit coercions} *)

  val any : 'kind t -> any

  val type_of_class_type : class_type -> type_

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int

  val sexp_of_t : 'kind t -> sexp

  val is_hidden : 'kind t -> bool
  (** cf. {!Resolved.is_hidden} *)
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    type kind = Kind.fragment

    type sort = [ `Root | `Branch ]

    type ('b, 'c) raw =
      | Root : ('b, [< sort > `Root]) raw
      | Subst : Path.Resolved.module_type * module_ ->
                ([< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | SubstAlias : Path.Resolved.module_ * module_ ->
                ([< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | Module : signature * string -> ([< kind > `Module], [< sort > `Branch]) raw
      | Type : signature * string -> ([< kind > `Type], [< sort > `Branch]) raw
      | Class : signature * string -> ([< kind > `Class], [< sort > `Branch]) raw
      | ClassType : signature * string -> ([< kind > `ClassType], [< sort > `Branch]) raw

    and 'b t = ('b, [`Branch]) raw
    and any = kind t
    and signature = (fragment_module, [`Root | `Branch]) raw
    and module_ = fragment_module t

    type type_ = fragment_type t

    (** {2 Explicit coercions} *)

    val signature_of_module : module_ -> signature

    val any : 'b t -> any

    val any_sort : ('b, 'c) raw -> ('b, sort) raw

    (** {2 Attaching fragments to valid paths} *)

    val path: Path.module_ -> 'b t -> 'b Path.t

    val identifier: Identifier.signature -> 'b t -> 'b Identifier.t

    (** {2 Generic operations} *)

    val equal : 'b t -> 'b t -> bool

    val hash : 'b t -> int

    val sexp_of_t : (_, _) raw -> sexp

    val split : 'b t -> string * 'b t option

  end

  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('b, 'c) raw =
    | Resolved : ('b, 'c) Resolved.raw -> ('b, 'c) raw
    | Dot : signature * string -> ([< kind], [< sort > `Branch]) raw

  and 'b t = ('b, [`Branch]) raw
  and any = kind t
  and signature = (fragment_module, [`Root | `Branch]) raw

  type module_ = fragment_module t
  type type_ = fragment_type t

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val any_sort : ('b, 'c) raw -> ('b, sort) raw

  val any : 'b t -> any

  (** {2 Attaching fragments to valid paths} *)

  val path: Path.module_ -> 'b t -> 'b Path.t

  (** {2 Generic operations} *)

  val equal : 'b t -> 'b t -> bool

  val hash : 'b t -> int

  val sexp_of_t : (_, _) raw -> sexp

  val split: 'b t -> string * 'b t option

end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig

  module Resolved : sig

    type kind = Kind.reference

    type 'kind t =
      | Identifier : 'kind Identifier.t -> 'kind t
      | SubstAlias : Path.Resolved.module_ * module_ -> [< kind > `Module ] t
      | Module : signature * string -> [< kind > `Module] t
      | Canonical : module_ * Reference.module_ -> [< kind > `Module] t
      | ModuleType : signature * string -> [< kind > `ModuleType] t
      | Type : signature * string -> [< kind > `Type] t
      | Constructor : datatype * string -> [< kind > `Constructor] t
      | Field : parent * string -> [< kind > `Field] t
      | Extension : signature * string -> [< kind > `Extension] t
      | Exception : signature * string -> [< kind > `Exception] t
      | Value : signature * string -> [< kind > `Value] t
      | Class : signature * string -> [< kind > `Class] t
      | ClassType : signature * string -> [< kind > `ClassType] t
      | Method : class_signature * string -> [< kind > `Method] t
      | InstanceVariable : class_signature * string ->
          [< kind > `InstanceVariable] t
      | Label : label_parent * string -> [< kind > `Label] t

    and any = kind t
    and signature = Kind.signature t
    and class_signature = Kind.class_signature t
    and datatype = Kind.datatype t
    and parent = Kind.parent t
    and module_ = reference_module t
    and label_parent = Kind.label_parent t

    type module_type = reference_module_type t
    type type_ = reference_type t
    type constructor = reference_constructor t
    type field = reference_field t
    type extension = reference_extension t
    type exception_ = reference_exception t
    type value = reference_value t
    type class_ = reference_class t
    type class_type = reference_class_type t
    type method_ = reference_method t
    type instance_variable = reference_instance_variable t
    type label = reference_label t
    type page = reference_page t

    (** {2 Creators} *)

    val ident_module : Identifier.module_ -> [< kind > `Module] t

    val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

    val ident_type : Identifier.type_ -> [< kind > `Type] t

    val ident_constructor : Identifier.constructor -> [< kind > `Constructor] t

    val ident_field : Identifier.field -> [< kind > `Field] t

    val ident_extension : Identifier.extension -> [< kind > `Extension] t

    val ident_exception : Identifier.exception_ -> [< kind > `Exception] t

    val ident_value : Identifier.value -> [< kind > `Value] t

    val ident_class : Identifier.class_ -> [< kind > `Class] t

    val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

    val ident_method : Identifier.method_ -> [< kind > `Method] t

    val ident_instance_variable : Identifier.instance_variable ->
          [< kind > `InstanceVariable] t

    val ident_label : Identifier.label -> [< kind > `Label] t

    val ident_page : Identifier.page -> [< kind > `Page] t

    (** {2 Explicit coercions} *)

    val signature_of_module : module_ -> signature

    val signature_of_module_type : module_type -> signature

    val class_signature_of_class : class_ -> class_signature

    val class_signature_of_class_type : class_type -> class_signature

    val parent_of_signature : signature -> parent

    val parent_of_class_signature : class_signature -> parent

    val parent_of_datatype : datatype -> parent

    val label_parent_of_parent : parent -> label_parent

    val label_parent_of_page : page -> label_parent

    val any : 'kind t -> any

    (** {2 Generic operations} *)

    val equal : 'kind t -> 'kind t -> bool

    val hash : 'kind t -> int

    val identifier: 'kind t -> 'kind Identifier.t
    (** [identifier rr] extracts the identifier present at the "root" of [rr]. *)

    val rebase : Identifier.signature -> 'kind t -> 'kind t

    val sexp_of_t : 'kind t -> sexp

  end

  type kind = Kind.reference

  type _ tag =
    | TUnknown : [< kind ] tag
    | TModule : [< kind > `Module ] tag
    | TModuleType : [< kind > `ModuleType ] tag
    | TType : [< kind > `Type ] tag
    | TConstructor : [< kind > `Constructor ] tag
    | TField : [< kind > `Field ] tag
    | TExtension : [< kind > `Extension ] tag
    | TException : [< kind > `Exception ] tag
    | TValue : [< kind > `Value ] tag
    | TClass : [< kind > `Class ] tag
    | TClassType : [< kind > `ClassType ] tag
    | TMethod : [< kind > `Method ] tag
    | TInstanceVariable : [< kind > `InstanceVariable ] tag
    | TLabel : [< kind > `Label ] tag
    | TPage : [< kind > `Page ] tag

  type 'kind t =
    | Resolved : 'kind Resolved.t -> 'kind t
    | Root : string * 'kind tag -> 'kind t
    | Dot : label_parent * string -> [< kind ] t
    | Module : signature * string -> [< kind > `Module] t
    | ModuleType : signature * string -> [< kind > `ModuleType] t
    | Type : signature * string -> [< kind > `Type] t
    | Constructor : datatype * string -> [< kind > `Constructor] t
    | Field : parent * string -> [< kind > `Field] t
    | Extension : signature * string -> [< kind > `Extension] t
    | Exception : signature * string -> [< kind > `Exception] t
    | Value : signature * string -> [< kind > `Value] t
    | Class : signature * string -> [< kind > `Class] t
    | ClassType : signature * string -> [< kind > `ClassType] t
    | Method : class_signature * string -> [< kind > `Method] t
    | InstanceVariable : class_signature * string ->
        [< kind > `InstanceVariable] t
    | Label : label_parent * string -> [< kind > `Label] t

  and any = kind t
  and signature = Kind.signature t
  and class_signature = Kind.class_signature t
  and datatype = Kind.datatype t
  and parent = Kind.parent t
  and label_parent = [ Kind.parent | Kind.page ] t

  type module_ = reference_module t
  type module_type = reference_module_type t
  type type_ = reference_type t
  type constructor = reference_constructor t
  type field = reference_field t
  type extension = reference_extension t
  type exception_ = reference_exception t
  type value = reference_value t
  type class_ = reference_class t
  type class_type = reference_class_type t
  type method_ = reference_method t
  type instance_variable = reference_instance_variable t
  type label = reference_label t
  type page = reference_page t

  (** {2 Creators} *)

  val ident_module : Identifier.module_ -> [< kind > `Module] t

  val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

  val ident_type : Identifier.type_ -> [< kind > `Type] t

  val ident_constructor : Identifier.constructor -> [< kind > `Constructor] t

  val ident_field : Identifier.field -> [< kind > `Field] t

  val ident_extension : Identifier.extension -> [< kind > `Extension] t

  val ident_exception : Identifier.exception_ -> [< kind > `Exception] t

  val ident_value : Identifier.value -> [< kind > `Value] t

  val ident_class : Identifier.class_ -> [< kind > `Class] t

  val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

  val ident_method : Identifier.method_ -> [< kind > `Method] t

  val ident_instance_variable : Identifier.instance_variable ->
        [< kind > `InstanceVariable] t

  val ident_label : Identifier.label -> [< kind > `Label] t

  val module_ : signature -> string -> [< kind > `Module] t

  val module_type : signature -> string ->
        [< kind > `ModuleType] t

  val type_ : signature -> string -> [< kind > `Type] t

  val constructor : datatype -> string -> [< kind > `Constructor] t

  val field : parent -> string -> [< kind > `Field] t

  val extension : signature -> string -> [< kind > `Extension] t

  val exception_ : signature -> string -> [< kind > `Exception] t

  val value : signature -> string -> [< kind > `Value] t

  val class_ : signature -> string -> [< kind > `Class] t

  val class_type : signature -> string -> [< kind > `ClassType] t

  val method_ : class_signature -> string -> [< kind > `Method] t

  val instance_variable : class_signature -> string ->
        [< kind > `InstanceVariable] t

  val label : label_parent -> string -> [< kind > `Label] t

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val signature_of_module_type : module_type -> signature

  val class_signature_of_class : class_ -> class_signature

  val class_signature_of_class_type : class_type -> class_signature

  val parent_of_signature : signature -> parent

  val parent_of_class_signature : class_signature -> parent

  val parent_of_datatype : datatype -> parent

  val label_parent_of_parent : parent -> label_parent

  val any : 'kind t -> any

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int

  val sexp_of_t : 'kind t -> sexp
end
