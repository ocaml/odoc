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

open Paths

(** {3 Modules} *)

module rec Module : sig
  type decl =
    | Alias of (Path.Module.t * ModuleType.simple_expansion option)
    | ModuleType of ModuleType.expr

  type t = {
    id : Identifier.Module.t;
    doc : Comment.docs;
    type_ : decl;
    canonical : Path.Module.t option;
    hidden : bool;
  }

  module Equation : sig
    type t = decl
  end
end =
  Module

and FunctorParameter : sig
  type parameter = {
    id : Identifier.FunctorParameter.t;
    expr : ModuleType.expr;
  }

  type t = Unit | Named of parameter
end =
  FunctorParameter

(** {3 Modules Types} *)

and ModuleType : sig
  type substitution =
    | ModuleEq of Fragment.Module.t * Module.Equation.t
    | ModuleTypeEq of Fragment.ModuleType.t * ModuleType.expr
    | TypeEq of Fragment.Type.t * TypeDecl.Equation.t
    | ModuleSubst of Fragment.Module.t * Path.Module.t
    | ModuleTypeSubst of Fragment.ModuleType.t * ModuleType.expr
    | TypeSubst of Fragment.Type.t * TypeDecl.Equation.t

  type type_of_desc =
    | ModPath of Path.Module.t
    | StructInclude of Path.Module.t

  type simple_expansion =
    | Signature of Signature.t
    | Functor of FunctorParameter.t * simple_expansion

  type typeof_t = {
    t_desc : type_of_desc;
    t_expansion : simple_expansion option;
  }

  module U : sig
    type expr =
      | Path of Path.ModuleType.t
      | Signature of Signature.t
      | With of substitution list * expr
      | TypeOf of typeof_t

    (* Nb. this may have an expansion! *)
  end

  type path_t = {
    p_expansion : simple_expansion option;
    p_path : Path.ModuleType.t;
  }

  type with_t = {
    w_substitutions : substitution list;
    w_expansion : simple_expansion option;
    w_expr : U.expr;
  }

  type expr =
    | Path of path_t
    | Signature of Signature.t
    | Functor of FunctorParameter.t * expr
    | With of with_t
    | TypeOf of typeof_t

  type t = {
    id : Identifier.ModuleType.t;
    doc : Comment.docs;
    canonical : Path.ModuleType.t option;
    expr : expr option;
  }
end =
  ModuleType

and ModuleSubstitution : sig
  type t = {
    id : Identifier.Module.t;
    doc : Comment.docs;
    manifest : Path.Module.t;
  }
end =
  ModuleSubstitution

and ModuleTypeSubstitution : sig
  type t = {
    id : Identifier.ModuleType.t;
    doc : Comment.docs;
    manifest : ModuleType.expr;
  }
end =
  ModuleTypeSubstitution

(** {3 Signatures} *)

and Signature : sig
  type recursive = Ordinary | And | Nonrec | Rec

  type item =
    | Module of recursive * Module.t
    | ModuleType of ModuleType.t
    | ModuleSubstitution of ModuleSubstitution.t
    | ModuleTypeSubstitution of ModuleTypeSubstitution.t
    | Open of Open.t
    | Type of recursive * TypeDecl.t
    | TypeSubstitution of TypeDecl.t
    | TypExt of Extension.t
    | Exception of Exception.t
    | Value of Value.t
    | Class of recursive * Class.t
    | ClassType of recursive * ClassType.t
    | Include of Include.t
    | Comment of Comment.docs_or_stop

  type t = {
    items : item list;
    compiled : bool;
    doc : Comment.docs;  (** The top comment. *)
  }
end =
  Signature

and Open : sig
  type t = { expansion : Signature.t }
end =
  Open

(** {3 Includes} *)

and Include : sig
  type shadowed = {
    s_modules : string list;
    s_module_types : string list;
    s_values : string list;
    s_types : string list;
    s_classes : string list;
    s_class_types : string list;
  }

  type expansion = { shadowed : shadowed; content : Signature.t }

  (* Explicitly unexpanded decl *)
  type decl = Alias of Path.Module.t | ModuleType of ModuleType.U.expr

  type t = {
    loc : Location_.span;
    parent : Identifier.Signature.t;
    strengthened : Path.Module.t option;
    doc : Comment.docs;
    status : [ `Inline | `Closed | `Open | `Default ];
    decl : decl;
    expansion : expansion;
  }
end =
  Include

(** {3 Type Declarations} *)

and TypeDecl : sig
  module Field : sig
    type t = {
      id : Identifier.Field.t;
      doc : Comment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      id : Identifier.Constructor.t;
      doc : Comment.docs;
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

  type variance = Pos | Neg

  type param_desc = Any | Var of string

  type param = {
    desc : param_desc;
    variance : variance option;
    injectivity : bool;
  }

  module Equation : sig
    type t = {
      params : param list;
      private_ : bool;
      manifest : TypeExpr.t option;
      constraints : (TypeExpr.t * TypeExpr.t) list;
    }
  end

  type t = {
    id : Identifier.Type.t;
    doc : Comment.docs;
    canonical : Path.Type.t option;
    equation : Equation.t;
    representation : Representation.t option;
  }
end =
  TypeDecl

(** {3 Type extensions} *)

and Extension : sig
  module Constructor : sig
    type t = {
      id : Identifier.Extension.t;
      doc : Comment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    parent : Identifier.Signature.t;
    type_path : Path.Type.t;
    doc : Comment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end =
  Extension

(** {3 Exception} *)
and Exception : sig
  type t = {
    id : Identifier.Exception.t;
    doc : Comment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end =
  Exception

(** {3 Values} *)

and Value : sig
  type value = Abstract | External of string list

  type t = {
    id : Identifier.Value.t;
    doc : Comment.docs;
    type_ : TypeExpr.t;
    value : value;
  }
end =
  Value

(** {3 Classes} *)

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    id : Identifier.Class.t;
    doc : Comment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    type_ : decl;
    expansion : ClassSignature.t option;
  }
end =
  Class

(** {3 Class Types} *)

and ClassType : sig
  type expr =
    | Constr of Path.ClassType.t * TypeExpr.t list
    | Signature of ClassSignature.t

  type t = {
    id : Identifier.ClassType.t;
    doc : Comment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end =
  ClassType

(** {3 Class Signatures} *)

and ClassSignature : sig
  type item =
    | Method of Method.t
    | InstanceVariable of InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of Comment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list; doc : Comment.docs }
end =
  ClassSignature

(** {3 Methods} *)

and Method : sig
  type t = {
    id : Identifier.Method.t;
    doc : Comment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  Method

(** {3 Instance variables} *)

and InstanceVariable : sig
  type t = {
    id : Identifier.InstanceVariable.t;
    doc : Comment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  InstanceVariable

(** {3 Type expressions} *)

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Fixed | Closed of string list | Open

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : Comment.docs;
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
    type substitution = Fragment.Type.t * TypeExpr.t

    type t = { path : Path.ModuleType.t; substitutions : substitution list }
  end

  type label = Label of string | Optional of string

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Path.Type.t * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Path.ClassType.t * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t
end =
  TypeExpr

(** {3 Compilation units} *)

module rec Compilation_unit : sig
  module Import : sig
    type t =
      | Unresolved of string * Digest.t option
      | Resolved of Root.t * Names.ModuleName.t
  end

  module Source : sig
    type t = { file : string; build_dir : string; digest : Digest.t }
  end

  module Packed : sig
    type item = { id : Identifier.Module.t; path : Path.Module.t }

    type t = item list
  end

  type content = Module of Signature.t | Pack of Packed.t

  type t = {
    id : Identifier.RootModule.t;
    root : Root.t;
    digest : Digest.t;
    imports : Import.t list;
    source : Source.t option;
    interface : bool;
    hidden : bool;
    content : content;
    expansion : Signature.t option;
    linked : bool;  (** Whether this unit has been linked. *)
    canonical : Path.Module.t option;
  }
end =
  Compilation_unit

module rec Page : sig
  type t = {
    name : Identifier.Page.t;
    root : Root.t;
    content : Comment.docs;
    children : Reference.t list;
    digest : Digest.t;
    linked : bool;
  }
end =
  Page

let umty_of_mty : ModuleType.expr -> ModuleType.U.expr option = function
  | Signature sg -> Some (Signature sg)
  | Path { p_path; _ } -> Some (Path p_path)
  | Functor _ -> None
  | TypeOf t -> Some (TypeOf t)
  | With { w_substitutions; w_expr; _ } -> Some (With (w_substitutions, w_expr))

(** Query the top-comment of a signature. This is [s.doc] most of the time with
    an exception for signature starting with an inline includes. *)
let extract_signature_doc (s : Signature.t) =
  match (s.doc, s.items) with
  | [], Include { expansion; status = `Inline; _ } :: _ ->
      (* A signature that starts with an [@inline] include inherits the
         top-comment from the expansion. This comment is not rendered for
         [include] items. *)
      expansion.content.doc
  | doc, _ -> doc
