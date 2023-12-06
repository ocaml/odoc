(* Lang_of *)
open Odoc_model.Paths

type maps

val empty : unit -> maps

val with_fragment_root : Cfrag.root -> maps

val with_shadowed : Odoc_model.Lang.Include.shadowed -> maps

module Opt = Component.Opt

module Path : sig
  val module_ : maps -> Cpath.module_ -> Path.Module.t

  val module_type : maps -> Cpath.module_type -> Path.ModuleType.t

  val type_ : maps -> Cpath.type_ -> Path.Type.t

  val class_type : maps -> Cpath.class_type -> Path.ClassType.t

  val resolved_module : maps -> Cpath.Resolved.module_ -> Path.Resolved.Module.t

  val resolved_parent : maps -> Cpath.Resolved.parent -> Path.Resolved.Module.t

  val resolved_module_type :
    maps -> Cpath.Resolved.module_type -> Path.Resolved.ModuleType.t

  val resolved_type : maps -> Cpath.Resolved.type_ -> Path.Resolved.Type.t

  val resolved_datatype :
    maps -> Cpath.Resolved.datatype -> Path.Resolved.DataType.t

  val resolved_value : maps -> Cpath.Resolved.value -> Path.Resolved.Value.t

  val resolved_constructor :
    maps -> Cpath.Resolved.constructor -> Path.Resolved.Constructor.t

  val resolved_class_type :
    maps -> Cpath.Resolved.class_type -> Path.Resolved.ClassType.t

  val module_fragment : maps -> Cfrag.module_ -> Fragment.Module.t

  val signature_fragment : maps -> Cfrag.signature -> Fragment.Signature.t

  val type_fragment : maps -> Cfrag.type_ -> Fragment.Type.t

  val resolved_module_fragment :
    maps -> Cfrag.resolved_module -> Fragment.Resolved.Module.t

  val resolved_module_type_fragment :
    maps -> Cfrag.resolved_module_type -> Fragment.Resolved.ModuleType.t

  val resolved_signature_fragment :
    maps -> Cfrag.resolved_signature -> Fragment.Resolved.Signature.t

  val resolved_type_fragment :
    maps -> Cfrag.resolved_type -> Fragment.Resolved.Type.t
end

val signature_items :
  Identifier.Signature.t ->
  maps ->
  Component.Signature.item list ->
  Odoc_model.Lang.Signature.item list

val signature :
  Identifier.Signature.t ->
  maps ->
  Component.Signature.t ->
  Odoc_model.Lang.Signature.t

val class_ :
  maps ->
  Identifier.Signature.t ->
  Ident.class_ ->
  Component.Class.t ->
  Odoc_model.Lang.Class.t

val class_decl :
  maps ->
  Identifier.Path.ClassType.t ->
  Component.Class.decl ->
  Odoc_model.Lang.Class.decl

val class_type_expr :
  maps ->
  Identifier.Path.ClassType.t ->
  Component.ClassType.expr ->
  Odoc_model.Lang.ClassType.expr

val class_type :
  maps ->
  Identifier.Signature.t ->
  Ident.class_type ->
  Component.ClassType.t ->
  Odoc_model.Lang.ClassType.t

val class_signature :
  maps ->
  Identifier.ClassSignature.t ->
  Component.ClassSignature.t ->
  Odoc_model.Lang.ClassSignature.t

val method_ :
  maps ->
  Identifier.ClassSignature.t ->
  Ident.method_ ->
  Component.Method.t ->
  Odoc_model.Lang.Method.t

val instance_variable :
  maps ->
  Identifier.ClassSignature.t ->
  Ident.instance_variable ->
  Component.InstanceVariable.t ->
  Odoc_model.Lang.InstanceVariable.t

val include_ :
  Identifier.Signature.t ->
  maps ->
  Component.Include.t ->
  Odoc_model.Lang.Include.t

val open_ :
  Identifier.Signature.t -> maps -> Component.Open.t -> Odoc_model.Lang.Open.t

val value_ :
  maps ->
  Identifier.Signature.t ->
  Ident.value ->
  Component.Value.t ->
  Odoc_model.Lang.Value.t

val typ_ext :
  maps ->
  Identifier.Signature.t ->
  Component.Extension.t ->
  Odoc_model.Lang.Extension.t

val extension_constructor :
  maps ->
  Identifier.Signature.t ->
  Component.Extension.Constructor.t ->
  Odoc_model.Lang.Extension.Constructor.t

val module_ :
  maps ->
  Identifier.Signature.t ->
  Ident.module_ ->
  Component.Module.t ->
  Odoc_model.Lang.Module.t

val module_substitution :
  maps ->
  Identifier.Signature.t ->
  Ident.module_ ->
  Component.ModuleSubstitution.t ->
  Odoc_model.Lang.ModuleSubstitution.t

val module_decl :
  maps ->
  Identifier.Signature.t ->
  Component.Module.decl ->
  Odoc_model.Lang.Module.decl

val module_type_expr :
  maps ->
  Identifier.Signature.t ->
  Component.ModuleType.expr ->
  Odoc_model.Lang.ModuleType.expr

val module_type :
  maps ->
  Identifier.Signature.t ->
  Ident.module_type ->
  Component.ModuleType.t Component.Delayed.t ->
  Odoc_model.Lang.ModuleType.t

val simple_expansion :
  maps ->
  Identifier.Signature.t ->
  Component.ModuleType.simple_expansion ->
  Odoc_model.Lang.ModuleType.simple_expansion

val type_decl_constructor_argument :
  maps ->
  Identifier.FieldParent.t ->
  Component.TypeDecl.Constructor.argument ->
  Odoc_model.Lang.TypeDecl.Constructor.argument

val type_decl_field :
  maps ->
  Identifier.FieldParent.t ->
  Component.TypeDecl.Field.t ->
  Odoc_model.Lang.TypeDecl.Field.t

val type_decl_equation :
  maps ->
  Identifier.FieldParent.t ->
  Component.TypeDecl.Equation.t ->
  Odoc_model.Lang.TypeDecl.Equation.t

val type_decl :
  maps ->
  Identifier.Signature.t ->
  Ident.type_ ->
  Component.TypeDecl.t ->
  Odoc_model.Lang.TypeDecl.t

val type_decl_representation :
  maps ->
  Identifier.Type.t ->
  Component.TypeDecl.Representation.t ->
  Odoc_model.Lang.TypeDecl.Representation.t

val type_decl_constructor :
  maps ->
  Identifier.DataType.t ->
  Component.TypeDecl.Constructor.t ->
  Odoc_model.Lang.TypeDecl.Constructor.t

val type_expr_package :
  maps ->
  Identifier.LabelParent.t ->
  Component.TypeExpr.Package.t ->
  Odoc_model.Lang.TypeExpr.Package.t

val type_expr :
  maps ->
  Identifier.LabelParent.t ->
  Component.TypeExpr.t ->
  Odoc_model.Lang.TypeExpr.t

val type_expr_polyvar :
  maps ->
  Identifier.LabelParent.t ->
  Component.TypeExpr.Polymorphic_variant.t ->
  Odoc_model.Lang.TypeExpr.Polymorphic_variant.t

val type_expr_object :
  maps ->
  Identifier.LabelParent.t ->
  Component.TypeExpr.Object.t ->
  Odoc_model.Lang.TypeExpr.Object.t

val functor_parameter :
  maps ->
  Component.FunctorParameter.parameter ->
  Odoc_model.Lang.FunctorParameter.parameter

val exception_ :
  maps ->
  Identifier.Signature.t ->
  Ident.exception_ ->
  Component.Exception.t ->
  Odoc_model.Lang.Exception.t

val block_element :
  Identifier.LabelParent.t ->
  Component.CComment.block_element Odoc_model.Location_.with_location ->
  Odoc_model.Comment.block_element Odoc_model.Location_.with_location

val docs :
  Identifier.LabelParent.t -> Component.CComment.docs -> Odoc_model.Comment.docs

val docs_or_stop :
  Identifier.LabelParent.t ->
  Component.CComment.docs_or_stop ->
  Odoc_model.Comment.docs_or_stop
