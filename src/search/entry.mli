open Odoc_model.Lang
open Odoc_model.Paths

type type_decl_entry = {
  canonical : Path.Type.t option;
  equation : TypeDecl.Equation.t;
  representation : TypeDecl.Representation.t option;
}

type class_type_entry = { virtual_ : bool; params : TypeDecl.param list }

type method_entry = { private_ : bool; virtual_ : bool; type_ : TypeExpr.t }

type class_entry = { virtual_ : bool; params : TypeDecl.param list }

type type_extension_entry = {
  type_path : Path.Type.t;
  type_params : TypeDecl.param list;
  private_ : bool;
}

type constructor_entry = {
  args : TypeDecl.Constructor.argument;
  res : TypeExpr.t;
}

type field_entry = {
  mutable_ : bool;
  type_ : TypeExpr.t;
  parent_type : TypeExpr.t;
}

type instance_variable_entry = {
  mutable_ : bool;
  virtual_ : bool;
  type_ : TypeExpr.t;
}

type doc_entry = Paragraph | Heading | CodeBlock | MathBlock | Verbatim

type value_entry = { value : Value.value; type_ : TypeExpr.t }

type kind =
  | TypeDecl of type_decl_entry
  | Module
  | Value of value_entry
  | Doc of doc_entry
  | Exception of constructor_entry
  | Class_type of class_type_entry
  | Method of method_entry
  | Class of class_entry
  | TypeExtension of type_extension_entry
  | ExtensionConstructor of constructor_entry
  | ModuleType
  | Constructor of constructor_entry
  | Field of field_entry

type t = {
  id : Odoc_model.Paths.Identifier.Any.t;
  doc : Odoc_model.Comment.docs;
  kind : kind;
}

val entries_of_item :
  Odoc_model.Paths.Identifier.Any.t -> Odoc_model.Fold.item -> t list
