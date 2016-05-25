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

open DocOckPaths

(** {3 Documentation} *)

module Documentation : sig

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

  type 'a reference =
    | Module of 'a Reference.module_
    | ModuleType of 'a Reference.module_type
    | Type of 'a Reference.type_
    | Constructor of 'a Reference.constructor
    | Field of 'a Reference.field
    | Extension of 'a Reference.extension
    | Exception of 'a Reference.exception_
    | Value of 'a Reference.value
    | Class of 'a Reference.class_
    | ClassType of 'a Reference.class_type
    | Method of 'a Reference.method_
    | InstanceVariable of 'a Reference.instance_variable
    | Element of 'a Reference.any
    | Section of 'a Reference.label
    | Link of string
    | Custom of string * string

  type 'a special =
    Modules of 'a Reference.module_ list
  | Index

  type see =
    | Url of string
    | File of string
    | Doc of string

  type 'a text = 'a text_element list

  and 'a text_element =
    | Raw of string
    | Code of string
    | PreCode of string
    | Verbatim of string
    | Style of style * 'a text
    | List of 'a text list
    | Enum of 'a text list
    | Newline
    | Title of int * 'a Identifier.label option * 'a text
    | Reference of 'a reference * 'a text option
    | Target of string option * string
    | Special of 'a special

  and 'a tag =
    | Author of string
    | Version of string
    | See of see * 'a text
    | Since of string
    | Before of string * 'a text
    | Deprecated of 'a text
    | Param of string * 'a text
    | Raise of string * 'a text
    | Return of 'a text
    | Inline
    | Tag of string * 'a text

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

    type 'a t =
      { origin: 'a Identifier.any; (** TODO remove this *)
        offset: Offset.t;
        location: Location.t option;
        message: string; }

  end

  type 'a body =
    { text: 'a text;
      tags: 'a tag list; }

  type 'a t =
    | Ok of 'a body
    | Error of 'a Error.t

  type 'a comment =
    | Documentation of 'a t
    | Stop

end

(** {3 Modules} *)

module rec Module : sig

  type 'a expansion =
    | Signature of 'a Signature.t
    | Functor of 'a FunctorArgument.t option list * 'a Signature.t

  type 'a decl =
    | Alias of 'a Path.module_
    | ModuleType of 'a ModuleType.expr

  type 'a t =
    { id: 'a Identifier.module_;
      doc: 'a Documentation.t;
      type_: 'a decl;
      expansion: 'a expansion option;
    }

  module Equation : sig

    type 'a t = 'a decl

  end

end

and FunctorArgument : sig
  type 'a t = {
    id : 'a Identifier.module_;
    expr : 'a ModuleType.expr;
    expansion: 'a Module.expansion option;
  }
end

(** {3 Modules Types} *)

and ModuleType : sig

  type 'a substitution =
    | ModuleEq of 'a Fragment.module_ * 'a Module.Equation.t
    | TypeEq of 'a Fragment.type_ * 'a TypeDecl.Equation.t
    | ModuleSubst of 'a Fragment.module_ * 'a Path.module_
    | TypeSubst of 'a Fragment.type_ * string list * 'a Path.type_

  type 'a expr =
    | Path of 'a Path.module_type
    | Signature of 'a Signature.t
    | Functor of 'a FunctorArgument.t option * 'a expr
    | With of 'a expr * 'a substitution list
    | TypeOf of 'a Module.decl

  type 'a t =
    { id: 'a Identifier.module_type;
      doc: 'a Documentation.t;
      expr: 'a expr option;
      expansion: 'a Module.expansion option;
    }

end

(** {3 Signatures} *)

and Signature : sig

  type 'a item =
    | Module of 'a Module.t
    | ModuleType of 'a ModuleType.t
    | Type of 'a TypeDecl.t
    | TypExt of 'a Extension.t
    | Exception of 'a Exception.t
    | Value of 'a Value.t
    | External of 'a External.t
    | Class of 'a Class.t
    | ClassType of 'a ClassType.t
    | Include of 'a Include.t
    | Comment of 'a Documentation.comment

  type 'a t = 'a item list

end

(** {3 Includes} *)
and Include : sig

  type 'a t =
    { parent: 'a Identifier.signature;
      doc: 'a Documentation.t;
      decl: 'a Module.decl;
      expansion: 'a Signature.t option; }

end

(** {3 Type Declarations} *)

and TypeDecl : sig

  module Constructor : sig

    type 'a t =
      { id: 'a Identifier.constructor;
        doc: 'a Documentation.t;
        args: 'a TypeExpr.t list;
        res: 'a TypeExpr.t option; }

  end

  module Field : sig

    type 'a t =
      { id: 'a Identifier.field;
        doc: 'a Documentation.t;
        mutable_ : bool;
        type_: 'a TypeExpr.t; }

  end

  module Representation : sig

    type 'a t =
      | Variant of 'a Constructor.t list
      | Record of 'a Field.t list
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

    type 'a t =
      { params: param list;
        private_: bool;
        manifest: 'a TypeExpr.t option;
        constraints: ('a TypeExpr.t * 'a TypeExpr.t) list; }

  end

  type 'a t =
    { id: 'a Identifier.type_;
      doc: 'a Documentation.t;
      equation: 'a Equation.t;
      representation: 'a Representation.t option; }

end

(** {3 Type extensions} *)

and Extension : sig

  module Constructor : sig

    type 'a t =
      { id: 'a Identifier.extension;
        doc: 'a Documentation.t;
        args: 'a TypeExpr.t list;
        res: 'a TypeExpr.t option; }

  end

  type 'a t =
    { type_path: 'a Path.type_;
      doc: 'a Documentation.t;
      type_params: TypeDecl.param list;
      private_: bool;
      constructors: 'a Constructor.t list; }

end

(** {3 Exception} *)
and Exception : sig

  type 'a t =
    { id: 'a Identifier.exception_;
      doc: 'a Documentation.t;
      args: 'a TypeExpr.t list;
      res: 'a TypeExpr.t option; }

end


(** {3 Values} *)

and Value : sig

  type 'a t =
    { id: 'a Identifier.value;
      doc: 'a Documentation.t;
      type_: 'a TypeExpr.t; }

end

(** {3 External values} *)

and External : sig

  type 'a t =
    { id: 'a Identifier.value;
      doc: 'a Documentation.t;
      type_: 'a TypeExpr.t;
      primitives: string list; }

end

(** {3 Classes} *)

and Class : sig

  type 'a decl =
    | ClassType of 'a ClassType.expr
    | Arrow of TypeExpr.label option * 'a TypeExpr.t * 'a decl

  type 'a t =
    { id: 'a Identifier.class_;
      doc: 'a Documentation.t;
      virtual_: bool;
      params: TypeDecl.param list;
      type_: 'a decl; }

end

(** {3 Class Types} *)

and ClassType : sig

  type 'a expr =
    | Constr of 'a Path.class_type * 'a TypeExpr.t list
    | Signature of 'a ClassSignature.t

  type 'a t =
    { id: 'a Identifier.class_type;
      doc: 'a Documentation.t;
      virtual_: bool;
      params: TypeDecl.param list;
      expr: 'a expr; }

end

(** {3 Class Signatures} *)

and ClassSignature : sig

  type 'a item =
    | Method of 'a Method.t
    | InstanceVariable of 'a InstanceVariable.t
    | Constraint of 'a TypeExpr.t * 'a TypeExpr.t
    | Inherit of 'a ClassType.expr
    | Comment of 'a Documentation.comment

  type 'a t =
    { self: 'a TypeExpr.t option;
      items: 'a item list; }

end

(** {3 Methods} *)

and Method : sig

  type 'a t =
    { id: 'a Identifier.method_;
      doc: 'a Documentation.t;
      private_: bool;
      virtual_: bool;
      type_: 'a TypeExpr.t; }

end

(** {3 Instance variables} *)

and InstanceVariable : sig

  type 'a t =
    { id: 'a Identifier.instance_variable;
      doc: 'a Documentation.t;
      mutable_: bool;
      virtual_: bool;
      type_: 'a TypeExpr.t; }

end

(** {3 Type expressions} *)

and TypeExpr : sig

  module Variant : sig

    type kind =
      | Fixed
      | Closed of string list
      | Open

    type 'a element =
      | Type of 'a TypeExpr.t
      | Constructor of string * bool * 'a TypeExpr.t list

    type 'a t =
      { kind: kind;
        elements: 'a element list;}

  end

  module Object : sig

    type 'a method_ =
      { name: string;
        type_: 'a TypeExpr.t; }

    type 'a t =
      { methods: 'a method_ list;
        open_ : bool; }

  end

  module Package : sig

    type 'a substitution = 'a Fragment.type_ * 'a TypeExpr.t

    type 'a t =
      { path: 'a Path.module_type;
        substitutions: 'a substitution list; }

  end

  type label =
    | Label of string
    | Optional of string

  type 'a t =
    | Var of string
    | Any
    | Alias of 'a t * string
    | Arrow of label option * 'a t * 'a t
    | Tuple of 'a t list
    | Constr of 'a Path.type_ * 'a t list
    | Variant of 'a TypeExpr.Variant.t
    | Object of 'a TypeExpr.Object.t
    | Class of 'a Path.class_type * 'a t list
    | Poly of string list * 'a t
    | Package of 'a TypeExpr.Package.t

end

(** {3 Compilation units} *)

module Unit : sig

  module Import : sig

    type 'a t =
      | Unresolved of string * Digest.t option
      | Resolved of 'a

  end

  module Source : sig

    type 'a t =
      { file: string;
        build_dir: string;
        digest: Digest.t; }

  end

  module Packed : sig

    type 'a item =
      { id: 'a Identifier.module_;
        path: 'a Path.module_; }

    type 'a t = 'a item list

  end

  type 'a content =
    | Module of 'a Signature.t
    | Pack of 'a Packed.t

  type 'a t =
    { id: 'a Identifier.module_;
      doc: 'a Documentation.t;
      digest: Digest.t;
      imports: 'a Import.t list;
      source: 'a Source.t option;
      interface: bool;
      hidden: bool;
      content: 'a content;
      expansion: 'a Signature.t option; }

end
