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

(** Type of documentation *)

module Root = Root
module Paths_types = Paths_types

open Paths_types

(** {3 Documentation} *)

module rec Documentation : sig

  type style =
    | Bold
    | Italic
    | Emphasize
    | Center
    | Left
    | Right
    | Superscript
    | Subscript
    | Custom of string

  type reference =
    | Element of Reference.any
    | Link of string
    | Custom of string * string

  type see =
    | Url of string
    | File of string
    | Doc of string

  type text = text_element list

  and text_element =
    | Raw of string
    | Code of string
    | PreCode of string
    | Verbatim of string
    | Style of style * text
    | List of text list
    | Enum of text list
    | Newline
    | Title of int * Identifier.label option * text
    | Reference of reference * text option
    | Target of string option * string
    | Special of special

  and tag =
    | Author of string
    | Version of string
    | See of see * text
    | Since of string
    | Before of string * text
    | Deprecated of text
    | Param of string * text
    | Raise of string * text
    | Return of text
    | Inline
    | Tag of string * text
    | Canonical of Path.module_ * Reference.module_

  and special =
    | Modules of (Reference.module_ * text) list
    | Index


  module Error : sig

    module Position : sig

      type t =
        { line: int;
          column: int; }

    end

    module Offset : sig

      type t =
        { start: Position.t;
          finish: Position.t; }

    end

    module Location : sig

      type t =
        { filename: string;
          start: Position.t;
          finish: Position.t; }

    end

    type t =
      { origin: Identifier.any; (** TODO remove this *)
        offset: Offset.t;
        location: Location.t option;
        message: string; }

  end

  type body =
    { text: text;
      tags: tag list; }

  type t =
    | Ok of body
    | Error of Error.t

  type comment =
    | Documentation of t
    | Stop

end = Documentation

(** {3 Modules} *)

module rec Module : sig

  type expansion =
    | AlreadyASig
    | Signature of Signature.t
    | Functor of FunctorArgument.t option list * Signature.t

  type decl =
    | Alias of Path.module_
    | ModuleType of ModuleType.expr

  type t =
    { id: Identifier.module_;
      doc: Documentation.t;
      type_: decl;
      canonical : (Path.module_ * Reference.module_) option;
      hidden : bool;
      display_type : decl option;
      expansion: expansion option;
    }

  module Equation : sig

    type t = decl

  end

end = Module

and FunctorArgument : sig
  type t = {
    id : Identifier.module_;
    expr : ModuleType.expr;
    expansion: Module.expansion option;
  }
end = FunctorArgument

(** {3 Modules Types} *)

and ModuleType : sig

  type substitution =
    | ModuleEq of Fragment.module_ * Module.Equation.t
    | TypeEq of Fragment.type_ * TypeDecl.Equation.t
    | ModuleSubst of Fragment.module_ * Path.module_
    | TypeSubst of Fragment.type_ * string list * Path.type_

  type expr =
    | Path of Path.module_type
    | Signature of Signature.t
    | Functor of FunctorArgument.t option * expr
    | With of expr * substitution list
    | TypeOf of Module.decl

  type t =
    { id: Identifier.module_type;
      doc: Documentation.t;
      expr: expr option;
      expansion: Module.expansion option;
    }

end = ModuleType

(** {3 Signatures} *)

and Signature : sig

  type item =
    | Module of Module.t
    | ModuleType of ModuleType.t
    | Type of TypeDecl.t
    | TypExt of Extension.t
    | Exception of Exception.t
    | Value of Value.t
    | External of External.t
    | Class of Class.t
    | ClassType of ClassType.t
    | Include of Include.t
    | Comment of Documentation.comment

  type t = item list

end = Signature

(** {3 Includes} *)

and Include : sig
  type expansion = {
    resolved: bool;
    content: Signature.t;
  }

  type t =
    { parent: Identifier.signature;
      doc: Documentation.t;
      decl: Module.decl;
      expansion: expansion; }

end = Include

(** {3 Type Declarations} *)

and TypeDecl : sig

  module Field : sig

    type t =
      { id: Identifier.field;
        doc: Documentation.t;
        mutable_ : bool;
        type_: TypeExpr.t; }

  end

  module Constructor : sig
    type argument =
      | Tuple of TypeExpr.t list
      | Record of Field.t list

    type t =
      { id: Identifier.constructor;
        doc: Documentation.t;
        args: argument;
        res: TypeExpr.t option; }

  end


  module Representation : sig

    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Extensible

  end

  type variance =
    | Pos
    | Neg

  type param_desc =
    | Any
    | Var of string

  type param = param_desc * variance option

  module Equation : sig

    type t =
      { params: param list;
        private_: bool;
        manifest: TypeExpr.t option;
        constraints: (TypeExpr.t * TypeExpr.t) list; }

  end

  type t =
    { id: Identifier.type_;
      doc: Documentation.t;
      equation: Equation.t;
      representation: Representation.t option; }

end = TypeDecl

(** {3 Type extensions} *)

and Extension : sig

  module Constructor : sig

    type t =
      { id: Identifier.extension;
        doc: Documentation.t;
        args: TypeDecl.Constructor.argument;
        res: TypeExpr.t option; }

  end

  type t =
    { type_path: Path.type_;
      doc: Documentation.t;
      type_params: TypeDecl.param list;
      private_: bool;
      constructors: Constructor.t list; }

end = Extension

(** {3 Exception} *)
and Exception : sig

  type t =
    { id: Identifier.exception_;
      doc: Documentation.t;
      args: TypeDecl.Constructor.argument;
      res: TypeExpr.t option; }

end = Exception


(** {3 Values} *)

and Value : sig

  type t =
    { id: Identifier.value;
      doc: Documentation.t;
      type_: TypeExpr.t; }

end = Value

(** {3 External values} *)

and External : sig

  type t =
    { id: Identifier.value;
      doc: Documentation.t;
      type_: TypeExpr.t;
      primitives: string list; }

end = External

(** {3 Classes} *)

and Class : sig

  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t =
    { id: Identifier.class_;
      doc: Documentation.t;
      virtual_: bool;
      params: TypeDecl.param list;
      type_: decl;
      expansion: ClassSignature.t option; }

end = Class

(** {3 Class Types} *)

and ClassType : sig

  type expr =
    | Constr of Path.class_type * TypeExpr.t list
    | Signature of ClassSignature.t

  type t =
    { id: Identifier.class_type;
      doc: Documentation.t;
      virtual_: bool;
      params: TypeDecl.param list;
      expr: expr;
      expansion: ClassSignature.t option; }

end = ClassType

(** {3 Class Signatures} *)

and ClassSignature : sig

  type item =
    | Method of Method.t
    | InstanceVariable of InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of Documentation.comment

  type t =
    { self: TypeExpr.t option;
      items: item list; }

end = ClassSignature

(** {3 Methods} *)

and Method : sig

  type t =
    { id: Identifier.method_;
      doc: Documentation.t;
      private_: bool;
      virtual_: bool;
      type_: TypeExpr.t; }

end = Method

(** {3 Instance variables} *)

and InstanceVariable : sig

  type t =
    { id: Identifier.instance_variable;
      doc: Documentation.t;
      mutable_: bool;
      virtual_: bool;
      type_: TypeExpr.t; }

end = InstanceVariable

(** {3 Type expressions} *)

and TypeExpr : sig

  module Variant : sig

    type kind =
      | Fixed
      | Closed of string list
      | Open

    type element =
      | Type of TypeExpr.t
      | Constructor of string * bool * TypeExpr.t list

    type t =
      { kind: kind;
        elements: element list;}

  end

  module Object : sig

    type method_ =
      { name: string;
        type_: TypeExpr.t; }

    type t =
      { methods: method_ list;
        open_ : bool; }

  end

  module Package : sig

    type substitution = Fragment.type_ * TypeExpr.t

    type t =
      { path: Path.module_type;
        substitutions: substitution list; }

  end

  type label =
    | Label of string
    | Optional of string

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Path.type_ * t list
    | Variant of TypeExpr.Variant.t
    | Object of TypeExpr.Object.t
    | Class of Path.class_type * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t

end = TypeExpr

(** {3 Compilation units} *)

module rec Compilation_unit : sig

  module Import : sig

    type t =
      | Unresolved of string * Digest.t option
      | Resolved of Root.t

  end

  module Source : sig

    type t =
      { file: string;
        build_dir: string;
        digest: Digest.t; }

  end

  module Packed : sig

    type item =
      { id: Identifier.module_;
        path: Path.module_; }

    type t = item list

  end

  type content =
    | Module of Signature.t
    | Pack of Packed.t

  type t =
    { id: Identifier.module_;
      doc: Documentation.t;
      digest: Digest.t;
      imports: Import.t list;
      source: Source.t option;
      interface: bool;
      hidden: bool;
      content: content;
      expansion: Signature.t option; }

end = Compilation_unit

module rec Page : sig
  type t =
    { name: Identifier.page;
      content: Documentation.t;
      digest: Digest.t; }
end = Page
