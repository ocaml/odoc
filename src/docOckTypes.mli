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
    | Title of int * string option * 'a text
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
    | Tag of string * 'a text

  type 'a t =
    { text: 'a text;
      tags: 'a tag list; }

  type 'a comment =
    | Documentation of 'a t
    | Stop

end

(** {3 Modules} *)

module rec Module : sig

  type 'a decl =
    | Alias of 'a Path.module_
    | ModuleType of 'a ModuleType.expr

  type 'a t =
    { id: 'a Identifier.module_;
      doc: 'a Documentation.t;
      type_: 'a decl; }

  module Equation : sig

    type 'a t = 'a decl

  end

end

(** {3 Modules Types} *)

and ModuleType : sig

  type 'a substitution =
    | ModuleDecl of Fragment.module_ * 'a Module.Equation.t
    | TypeDecl of Fragment.type_ * 'a TypeDecl.Equation.t
    | ModuleSubst of Fragment.module_ * 'a Path.module_
    | TypeSubst of Fragment.type_ * 'a TypeExpr.t

  type 'a expr =
    | Ident of 'a Path.module_type
    | Signature of 'a Signature.t
    | Functor of (string * 'a expr) option * 'a expr
    | With of 'a expr * 'a substitution list
    | TypeOf of 'a Module.decl

  type 'a t =
    { id: 'a Identifier.module_type;
      doc: 'a Documentation.t;
      expr: 'a expr option; }

end

(** {3 Signatures} *)

and Signature : sig

  type 'a item =
    | Value of 'a Value.t
    | External of 'a External.t
    | Type of 'a TypeDecl.t
    | TypExt of 'a Extension.t
    | Exception of 'a Exception.t
    | Class of 'a Class.t
    | ClassType of 'a ClassType.t
    | Module of 'a Module.t
    | ModuleType of 'a ModuleType.t
    | Include of 'a ModuleType.expr
    | Comment of 'a Documentation.comment

  type 'a t = 'a item list

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
        type_: 'a TypeExpr.t; }

  end

  type 'a kind =
    | Variant of 'a Constructor.t list
    | Record of 'a Field.t list
    | Extensible

  type variance =
    | Pos
    | Neg

  type param_desc =
    | Any
    | Var of string

  type param = param_desc * variance option

  type 'a t =
    { id: 'a Identifier.type_;
      doc: 'a Documentation.t;
      params: param list;
      private_: bool;
      manifest: 'a TypeExpr.t option;
      constraints: ('a TypeExpr.t * 'a TypeExpr.t) list;
      kind: 'a kind option; }

  module Equation : sig

    type 'a t =
      { params: param list;
        private_: bool;
        manifest: 'a TypeExpr.t; }

  end

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
      primatives: string list; }

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

(** {3 Class Signatures *)

and ClassSignature : sig

  type 'a item =
    | InstanceVariable of 'a InstanceVariable.t
    | Method of 'a Method.t
    | Constraint of 'a TypeExpr.t * 'a TypeExpr.t
    | Inherit of 'a ClassType.expr
    | Comment of 'a Documentation.comment

  type 'a t =
    { self: 'a TypeExpr.t;
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

    type 'a substitution = Fragment.type_ * 'a TypeExpr.t

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

  type 'a import =
    | Unresolved of string * Digest.t option
    | Resolved of 'a

  type 'a t =
    { module_: 'a Module.t;
      imports: 'a import list; }

end
