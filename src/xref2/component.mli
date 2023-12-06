(** Component module *)

module ModuleMap : Map.S with type key = Ident.module_

module TypeMap : Map.S with type key = Ident.type_

module PathModuleMap : Map.S with type key = Ident.path_module
(** Useful maps *)

module ModuleTypeMap : Map.S with type key = Ident.module_type

module PathTypeMap : Map.S with type key = Ident.path_type

module PathValueMap : Map.S with type key = Ident.path_value

module PathClassTypeMap : Map.S with type key = Ident.path_class_type

module IdentMap : Map.S with type key = Ident.any

(** Delayed is a bit like Lazy.t but may in the future offer the chance to peek inside
    to be able to optimize the calculation *)
module Delayed : sig
  val eager : bool ref
  (** If [eager] is true then no delaying is done. Most useful for testing and
        documentation *)

  type 'a t = { mutable v : 'a option; mutable get : (unit -> 'a) option }

  val get : 'a t -> 'a

  val put : (unit -> 'a) -> 'a t

  val put_val : 'a -> 'a t
end

module Opt : sig
  val map : ('a -> 'b) -> 'a option -> 'b option
end

(** {2 Components}
  
    The modules/types here are very similar to those in {!module:Odoc_model.Lang},
    in most cases the only difference being that we have {{!module:Ident}Idents} 
    (which are local) rather than {{!module:Odoc_model.Paths.Identifier}Identifiers}
    (which are global), {{!module:Cpath}Cpaths} instead of {{!module:Odoc_model.Paths.Path}Paths},
    and {{!module:Cfrag}Cfrags} rather than {{!module:Odoc_model.Paths.Fragment}Fragments}.
    All of these are to support the use of local idents.
    
    Note that we still use global {{!module:Odoc_model.Paths.Reference}References}
    rather than local ones - this is because at the point these components are being
    used all the references are unresolved, and hence do not contain any resolved
    global identifiers. When resolving references, we construct paths in parallel,
    which also helps avoid the need for local references.

    These idents of items are kept outside of the types themselves in
    order to help with laziness.

    There are a few other minor differences:
    
    - Signatures keep track of removed items. These items are removed during
      destructive substitution.

*)

module rec Module : sig
  type decl =
    | Alias of Cpath.module_ * ModuleType.simple_expansion option
    | ModuleType of ModuleType.expr

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    type_ : decl;
    canonical : Odoc_model.Paths.Path.Module.t option;
    hidden : bool;
  }
end

and ModuleSubstitution : sig
  type t = { doc : CComment.docs; manifest : Cpath.module_ }
end

and ModuleTypeSubstitution : sig
  type t = { doc : CComment.docs; manifest : ModuleType.expr }
end

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : CComment.docs;
      }
    end

    type element = Type of TypeExpr.t | Constructor of Constructor.t

    type t = { kind : kind; elements : element list }
  end

  module Object : sig
    type method_ = { name : string; type_ : TypeExpr.t }

    type field = Method of method_ | Inherit of TypeExpr.t

    type t = { fields : field list; open_ : bool }
  end

  module Package : sig
    type substitution = Cfrag.type_ * TypeExpr.t

    type t = { path : Cpath.module_type; substitutions : substitution list }
  end

  type label = Odoc_model.Lang.TypeExpr.label

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Cpath.type_ * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Cpath.class_type * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t
end

and Extension : sig
  module Constructor : sig
    type t = {
      name : string;
      locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
      doc : CComment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    type_path : Cpath.type_;
    doc : CComment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end

and Exception : sig
  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end

and FunctorParameter : sig
  type parameter = { id : Ident.functor_parameter; expr : ModuleType.expr }

  type t = Named of parameter | Unit
end

and ModuleType : sig
  type substitution =
    | ModuleEq of Cfrag.module_ * Module.decl
    | ModuleSubst of Cfrag.module_ * Cpath.module_
    | ModuleTypeEq of Cfrag.module_type * ModuleType.expr
    | ModuleTypeSubst of Cfrag.module_type * ModuleType.expr
    | TypeEq of Cfrag.type_ * TypeDecl.Equation.t
    | TypeSubst of Cfrag.type_ * TypeDecl.Equation.t

  type type_of_desc =
    | ModPath of Cpath.module_
    | StructInclude of Cpath.module_

  type simple_expansion =
    | Signature of Signature.t
    | Functor of FunctorParameter.t * simple_expansion

  type typeof_t = {
    t_desc : type_of_desc;
    t_expansion : simple_expansion option;
  }

  module U : sig
    type expr =
      | Path of Cpath.module_type
      | Signature of Signature.t
      | With of substitution list * expr
      | TypeOf of typeof_t
  end

  type path_t = {
    p_expansion : simple_expansion option;
    p_path : Cpath.module_type;
  }

  type with_t = {
    w_substitutions : substitution list;
    w_expansion : simple_expansion option;
    w_expr : U.expr;
  }

  type expr =
    | Path of path_t
    | Signature of Signature.t
    | With of with_t
    | Functor of FunctorParameter.t * expr
    | TypeOf of typeof_t

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    canonical : Odoc_model.Paths.Path.ModuleType.t option;
    expr : expr option;
  }
end

and TypeDecl : sig
  module Field : sig
    type t = {
      name : string;
      doc : CComment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      name : string;
      doc : CComment.docs;
      args : argument;
      res : TypeExpr.t option;
    }
  end

  module Representation : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Extensible
  end

  type param = Odoc_model.Lang.TypeDecl.param

  module Equation : sig
    type t = {
      params : param list;
      private_ : bool;
      manifest : TypeExpr.t option;
      constraints : (TypeExpr.t * TypeExpr.t) list;
    }
  end

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    canonical : Odoc_model.Paths.Path.Type.t option;
    equation : Equation.t;
    representation : Representation.t option;
  }
end

and Signature : sig
  type recursive = Odoc_model.Lang.Signature.recursive

  type item =
    | Module of Ident.module_ * recursive * Module.t Delayed.t
    | ModuleSubstitution of Ident.module_ * ModuleSubstitution.t
    | ModuleType of Ident.module_type * ModuleType.t Delayed.t
    | ModuleTypeSubstitution of Ident.module_type * ModuleTypeSubstitution.t
    | Type of Ident.type_ * recursive * TypeDecl.t Delayed.t
    | TypeSubstitution of Ident.type_ * TypeDecl.t
    | Exception of Ident.exception_ * Exception.t
    | TypExt of Extension.t
    | Value of Ident.value * Value.t Delayed.t
    | Class of Ident.class_ * recursive * Class.t
    | ClassType of Ident.class_type * recursive * ClassType.t
    | Include of Include.t
    | Open of Open.t
    | Comment of CComment.docs_or_stop

  (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
  type removed_item =
    | RModule of Ident.module_ * Cpath.Resolved.module_
    | RType of Ident.type_ * TypeExpr.t * TypeDecl.Equation.t
    | RModuleType of Ident.module_type * ModuleType.expr

  type t = {
    items : item list;
    compiled : bool;
    removed : removed_item list;
    doc : CComment.docs;
  }
end

and Open : sig
  type t = { expansion : Signature.t; doc : CComment.docs }
end

and Include : sig
  type decl = Alias of Cpath.module_ | ModuleType of ModuleType.U.expr

  type t = {
    parent : Odoc_model.Paths.Identifier.Signature.t;
    strengthened : Cpath.module_ option;
    doc : CComment.docs;
    status : [ `Default | `Inline | `Closed | `Open ];
    shadowed : Odoc_model.Lang.Include.shadowed;
    expansion_ : Signature.t;
    decl : decl;
    loc : Odoc_model.Location_.span;
  }
end

and Value : sig
  type value = Odoc_model.Lang.Value.value

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    type_ : TypeExpr.t;
    value : value;
  }
end

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    type_ : decl;
    expansion : ClassSignature.t option;
  }
end

and ClassType : sig
  type expr =
    | Constr of Cpath.class_type * TypeExpr.t list
    | Signature of ClassSignature.t

  type t = {
    locs : Odoc_model.Paths.Identifier.SourceLocation.t option;
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end

and ClassSignature : sig
  module Constraint : sig
    type t = { left : TypeExpr.t; right : TypeExpr.t; doc : CComment.docs }
  end

  module Inherit : sig
    type t = { expr : ClassType.expr; doc : CComment.docs }
  end

  type item =
    | Method of Ident.method_ * Method.t
    | InstanceVariable of Ident.instance_variable * InstanceVariable.t
    | Constraint of Constraint.t
    | Inherit of Inherit.t
    | Comment of CComment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list; doc : CComment.docs }
end

and Method : sig
  type t = {
    doc : CComment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end

and InstanceVariable : sig
  type t = {
    doc : CComment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end

and Substitution : sig
  type subst_module =
    [ `Prefixed of Cpath.module_ * Cpath.Resolved.module_
    | `Substituted
    | `Renamed of Ident.path_module ]

  type subst_module_type =
    [ `Prefixed of Cpath.module_type * Cpath.Resolved.module_type
    | `Renamed of Ident.module_type ]

  type subst_type =
    [ `Prefixed of Cpath.type_ * Cpath.Resolved.type_
    | `Renamed of Ident.path_type ]

  type subst_class_type =
    [ `Prefixed of Cpath.class_type * Cpath.Resolved.class_type
    | `Renamed of Ident.path_class_type ]

  type t = {
    module_ : subst_module PathModuleMap.t;
    module_type : subst_module_type ModuleTypeMap.t;
    type_ : subst_type PathTypeMap.t;
    class_type : subst_class_type PathClassTypeMap.t;
    type_replacement : (TypeExpr.t * TypeDecl.Equation.t) PathTypeMap.t;
    module_type_replacement : ModuleType.expr ModuleTypeMap.t;
    path_invalidating_modules : Ident.path_module list;
    module_type_of_invalidating_modules : Ident.path_module list;
    unresolve_opaque_paths : bool;
  }
end

and CComment : sig
  type block_element =
    [ Odoc_model.Comment.nestable_block_element
    | `Heading of Label.t
    | `Tag of Odoc_model.Comment.tag ]

  type docs = block_element Odoc_model.Comment.with_location list

  type docs_or_stop = [ `Docs of docs | `Stop ]
end

and Label : sig
  type t = {
    attrs : Odoc_model.Comment.heading_attrs;
    label : Ident.label;
    text : Odoc_model.Comment.paragraph;
    location : Odoc_model.Location_.span;
  }
end

module Element : sig
  open Odoc_model.Paths

  type module_ = [ `Module of Identifier.Path.Module.t * Module.t Delayed.t ]

  type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]

  type datatype = [ `Type of Identifier.Type.t * TypeDecl.t ]

  type value = [ `Value of Identifier.Value.t * Value.t ]

  type label = [ `Label of Identifier.Label.t * Label.t ]

  type class_ = [ `Class of Identifier.Class.t * Class.t ]

  type class_type = [ `ClassType of Identifier.ClassType.t * ClassType.t ]

  type type_ = [ datatype | class_ | class_type ]

  type signature = [ module_ | module_type ]

  type constructor =
    [ `Constructor of Identifier.Constructor.t * TypeDecl.Constructor.t ]

  type exception_ = [ `Exception of Identifier.Exception.t * Exception.t ]

  type extension =
    [ `Extension of
      Identifier.Extension.t * Extension.Constructor.t * Extension.t ]

  type extension_decl =
    [ `ExtensionDecl of Identifier.Extension.t * Extension.Constructor.t ]

  type field = [ `Field of Identifier.Field.t * TypeDecl.Field.t ]

  (* No component for pages yet *)
  type page = [ `Page of Identifier.Page.t * Odoc_model.Lang.Page.t ]

  type label_parent = [ signature | type_ | page ]

  type fragment_type_parent = [ signature | datatype ]

  type any =
    [ signature
    | value
    | datatype
    | label
    | class_
    | class_type
    | constructor
    | exception_
    | extension
    | extension_decl
    | field
    | page ]

  val identifier : [< any ] -> Identifier.t
end

(** Formatting functions for components *)
module Fmt : sig
  val signature : Format.formatter -> Signature.t -> unit

  val removed_item : Format.formatter -> Signature.removed_item -> unit

  val removed_item_list :
    Format.formatter -> Signature.removed_item list -> unit

  val class_ : Format.formatter -> Class.t -> unit

  val class_type : Format.formatter -> ClassType.t -> unit

  val include_ : Format.formatter -> Include.t -> unit

  val value : Format.formatter -> Value.t -> unit

  val module_decl : Format.formatter -> Module.decl -> unit

  val include_decl : Format.formatter -> Include.decl -> unit

  val module_ : Format.formatter -> Module.t -> unit

  val module_type : Format.formatter -> ModuleType.t -> unit

  val simple_expansion : Format.formatter -> ModuleType.simple_expansion -> unit

  val module_type_type_of_desc :
    Format.formatter -> ModuleType.type_of_desc -> unit

  val u_module_type_expr : Format.formatter -> ModuleType.U.expr -> unit

  val module_type_expr : Format.formatter -> ModuleType.expr -> unit

  val functor_parameter : Format.formatter -> FunctorParameter.t -> unit

  val functor_parameter_parameter :
    Format.formatter -> FunctorParameter.parameter -> unit

  val type_decl : Format.formatter -> TypeDecl.t -> unit

  val type_equation : Format.formatter -> TypeDecl.Equation.t -> unit

  val exception_ : Format.formatter -> Exception.t -> unit

  val extension : Format.formatter -> Extension.t -> unit

  val substitution : Format.formatter -> ModuleType.substitution -> unit

  val substitution_list :
    Format.formatter -> ModuleType.substitution list -> unit

  val type_expr_list : Format.formatter -> TypeExpr.t list -> unit

  val type_object : Format.formatter -> TypeExpr.Object.t -> unit

  val type_class :
    Format.formatter -> Cpath.class_type * TypeExpr.t list -> unit

  val type_package : Format.formatter -> TypeExpr.Package.t -> unit

  val type_expr_polymorphic_variant :
    Format.formatter -> TypeExpr.Polymorphic_variant.t -> unit

  val type_expr : Format.formatter -> TypeExpr.t -> unit

  val resolved_module_path : Format.formatter -> Cpath.Resolved.module_ -> unit

  val module_path : Format.formatter -> Cpath.module_ -> unit

  val resolved_module_type_path :
    Format.formatter -> Cpath.Resolved.module_type -> unit

  val module_type_path : Format.formatter -> Cpath.module_type -> unit

  val resolved_type_path : Format.formatter -> Cpath.Resolved.type_ -> unit

  val resolved_value_path : Format.formatter -> Cpath.Resolved.value -> unit

  val resolved_parent_path : Format.formatter -> Cpath.Resolved.parent -> unit

  val type_path : Format.formatter -> Cpath.type_ -> unit

  val value_path : Format.formatter -> Cpath.value -> unit

  val constructor_path : Format.formatter -> Cpath.constructor -> unit

  val resolved_class_type_path :
    Format.formatter -> Cpath.Resolved.class_type -> unit

  val class_type_path : Format.formatter -> Cpath.class_type -> unit

  val model_path : Format.formatter -> Odoc_model.Paths.Path.t -> unit

  val model_resolved_path :
    Format.formatter -> Odoc_model.Paths.Path.Resolved.t -> unit

  val model_identifier :
    Format.formatter -> Odoc_model.Paths.Identifier.t -> unit

  val model_fragment : Format.formatter -> Odoc_model.Paths.Fragment.t -> unit

  val model_resolved_fragment :
    Format.formatter -> Odoc_model.Paths.Fragment.Resolved.t -> unit

  val resolved_root_fragment : Format.formatter -> Cfrag.root -> unit

  val resolved_signature_fragment :
    Format.formatter -> Cfrag.resolved_signature -> unit

  val resolved_module_fragment :
    Format.formatter -> Cfrag.resolved_module -> unit

  val resolved_type_fragment : Format.formatter -> Cfrag.resolved_type -> unit

  val signature_fragment : Format.formatter -> Cfrag.signature -> unit

  val module_fragment : Format.formatter -> Cfrag.module_ -> unit

  val module_type_fragment : Format.formatter -> Cfrag.module_type -> unit

  val type_fragment : Format.formatter -> Cfrag.type_ -> unit

  val model_resolved_reference :
    Format.formatter -> Odoc_model.Paths.Reference.Resolved.t -> unit

  val model_reference : Format.formatter -> Odoc_model.Paths.Reference.t -> unit
end

module Of_Lang : sig
  type map

  val empty : unit -> map

  val identifier :
    ('a -> 'b -> 'c) -> 'b -> 'a -> [> `Identifier of 'a | `Local of 'c ]

  val resolved_module_path :
    map -> Odoc_model.Paths.Path.Resolved.Module.t -> Cpath.Resolved.module_

  val resolved_module_type_path :
    map ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t ->
    Cpath.Resolved.module_type

  val resolved_type_path :
    map -> Odoc_model.Paths.Path.Resolved.Type.t -> Cpath.Resolved.type_

  val resolved_value_path :
    map -> Odoc_model.Paths.Path.Resolved.Value.t -> Cpath.Resolved.value

  val resolved_constructor_path :
    map ->
    Odoc_model.Paths.Path.Resolved.Constructor.t ->
    Cpath.Resolved.constructor

  val resolved_class_type_path :
    map ->
    Odoc_model.Paths.Path.Resolved.ClassType.t ->
    Cpath.Resolved.class_type

  val module_path : map -> Odoc_model.Paths.Path.Module.t -> Cpath.module_

  val module_type_path :
    map -> Odoc_model.Paths.Path.ModuleType.t -> Cpath.module_type

  val type_path : map -> Odoc_model.Paths.Path.Type.t -> Cpath.type_

  val datatype : map -> Odoc_model.Paths.Path.DataType.t -> Cpath.datatype

  val value_path : map -> Odoc_model.Paths.Path.Value.t -> Cpath.value

  val constructor_path :
    map -> Odoc_model.Paths.Path.Constructor.t -> Cpath.constructor

  val class_type_path :
    map -> Odoc_model.Paths.Path.ClassType.t -> Cpath.class_type

  val resolved_signature_fragment :
    map ->
    Odoc_model.Paths.Fragment.Resolved.Signature.t ->
    Cfrag.resolved_signature

  val resolved_module_fragment :
    map -> Odoc_model.Paths.Fragment.Resolved.Module.t -> Cfrag.resolved_module

  val resolved_module_type_fragment :
    map ->
    Odoc_model.Paths.Fragment.Resolved.ModuleType.t ->
    Cfrag.resolved_module_type

  val resolved_type_fragment :
    map -> Odoc_model.Paths.Fragment.Resolved.Type.t -> Cfrag.resolved_type

  val signature_fragment :
    map -> Odoc_model.Paths.Fragment.Signature.t -> Cfrag.signature

  val module_fragment :
    map -> Odoc_model.Paths.Fragment.Module.t -> Cfrag.module_

  val type_fragment : map -> Odoc_model.Paths.Fragment.Type.t -> Cfrag.type_

  val module_type_fragment :
    map -> Odoc_model.Paths.Fragment.ModuleType.t -> Cfrag.module_type

  val type_decl : map -> Odoc_model.Lang.TypeDecl.t -> TypeDecl.t

  val type_decl_representation :
    map ->
    Odoc_model.Lang.TypeDecl.Representation.t ->
    TypeDecl.Representation.t

  val type_decl_constructor :
    map -> Odoc_model.Lang.TypeDecl.Constructor.t -> TypeDecl.Constructor.t

  val type_decl_constructor_argument :
    map ->
    Odoc_model.Lang.TypeDecl.Constructor.argument ->
    TypeDecl.Constructor.argument

  val type_decl_field :
    map -> Odoc_model.Lang.TypeDecl.Field.t -> TypeDecl.Field.t

  val type_equation :
    map -> Odoc_model.Lang.TypeDecl.Equation.t -> TypeDecl.Equation.t

  val type_expr_polyvar :
    map ->
    Odoc_model.Lang.TypeExpr.Polymorphic_variant.t ->
    TypeExpr.Polymorphic_variant.t

  val type_object :
    map -> Odoc_model.Lang.TypeExpr.Object.t -> TypeExpr.Object.t

  val type_package :
    map -> Odoc_model.Lang.TypeExpr.Package.t -> TypeExpr.Package.t

  val type_expression : map -> Odoc_model.Lang.TypeExpr.t -> TypeExpr.t

  val module_decl : map -> Odoc_model.Lang.Module.decl -> Module.decl

  val include_decl : map -> Odoc_model.Lang.Include.decl -> Include.decl

  val module_ : map -> Odoc_model.Lang.Module.t -> Module.t

  val with_module_type_substitution :
    map -> Odoc_model.Lang.ModuleType.substitution -> ModuleType.substitution

  val functor_parameter :
    map ->
    Ident.functor_parameter ->
    Odoc_model.Lang.FunctorParameter.parameter ->
    FunctorParameter.parameter

  val simple_expansion :
    map ->
    Odoc_model.Lang.ModuleType.simple_expansion ->
    ModuleType.simple_expansion

  val extension : map -> Odoc_model.Lang.Extension.t -> Extension.t

  val extension_constructor :
    map -> Odoc_model.Lang.Extension.Constructor.t -> Extension.Constructor.t

  val exception_ : map -> Odoc_model.Lang.Exception.t -> Exception.t

  val u_module_type_expr :
    map -> Odoc_model.Lang.ModuleType.U.expr -> ModuleType.U.expr

  val module_type_expr :
    map -> Odoc_model.Lang.ModuleType.expr -> ModuleType.expr

  val module_type : map -> Odoc_model.Lang.ModuleType.t -> ModuleType.t

  val value : map -> Odoc_model.Lang.Value.t -> Value.t

  val include_ : map -> Odoc_model.Lang.Include.t -> Include.t

  val class_ : map -> Odoc_model.Lang.Class.t -> Class.t

  val class_decl : map -> Odoc_model.Lang.Class.decl -> Class.decl

  val class_type_expr : map -> Odoc_model.Lang.ClassType.expr -> ClassType.expr

  val class_type : map -> Odoc_model.Lang.ClassType.t -> ClassType.t

  val class_signature :
    map -> Odoc_model.Lang.ClassSignature.t -> ClassSignature.t

  val method_ : map -> Odoc_model.Lang.Method.t -> Method.t

  val instance_variable :
    map -> Odoc_model.Lang.InstanceVariable.t -> InstanceVariable.t

  val module_substitution :
    map -> Odoc_model.Lang.ModuleSubstitution.t -> ModuleSubstitution.t

  val module_of_module_substitution :
    map -> Odoc_model.Lang.ModuleSubstitution.t -> Module.t

  val signature : map -> Odoc_model.Lang.Signature.t -> Signature.t

  val open_ : map -> Odoc_model.Lang.Open.t -> Open.t

  val apply_sig_map : map -> Odoc_model.Lang.Signature.t -> Signature.t

  val docs : map -> Odoc_model.Comment.docs -> CComment.docs

  val docs_or_stop :
    map -> Odoc_model.Comment.docs_or_stop -> CComment.docs_or_stop
end

val module_of_functor_argument : FunctorParameter.parameter -> Module.t

val extract_signature_doc : Signature.t -> CComment.docs
