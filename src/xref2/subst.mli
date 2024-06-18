(* Subst *)
open Component

type t = Component.Substitution.t

val identity : t

val unresolve_opaque_paths : t -> t

val path_invalidate_module : Ident.module_ -> t -> t

val add_module :
  Ident.module_ -> Cpath.module_ -> Cpath.Resolved.module_ -> t -> t

val add_module_type :
  Ident.module_type -> Cpath.module_type -> Cpath.Resolved.module_type -> t -> t

val add_type : Ident.type_ -> Cpath.type_ -> Cpath.Resolved.type_ -> t -> t

val add_class :
  Ident.type_ -> Cpath.class_type -> Cpath.Resolved.class_type -> t -> t

val add_class_type :
  Ident.type_ -> Cpath.class_type -> Cpath.Resolved.class_type -> t -> t

val add_type_replacement :
  Ident.type_ -> TypeExpr.t -> TypeDecl.Equation.t -> t -> t

val add_module_type_replacement : Ident.module_type -> ModuleType.expr -> t -> t

val add_module_substitution : Ident.module_ -> t -> t

val type_ : t -> Component.TypeDecl.t -> Component.TypeDecl.t

val type_expr : t -> Component.TypeExpr.t -> Component.TypeExpr.t

val module_ : t -> Component.Module.t -> Component.Module.t

val module_type : t -> Component.ModuleType.t -> Component.ModuleType.t

val module_type_substitution :
  t -> Component.ModuleTypeSubstitution.t -> Component.ModuleTypeSubstitution.t

val module_substitution :
  t -> Component.ModuleSubstitution.t -> Component.ModuleSubstitution.t

val module_type_expr :
  t -> Component.ModuleType.expr -> Component.ModuleType.expr

val exception_ : t -> Component.Exception.t -> Component.Exception.t

val extension : t -> Component.Extension.t -> Component.Extension.t

val include_ : t -> Component.Include.t -> Component.Include.t

val open_ : t -> Component.Open.t -> Component.Open.t

val value : t -> Component.Value.t -> Component.Value.t

val class_ : t -> Component.Class.t -> Component.Class.t

val class_decl : t -> Component.Class.decl -> Component.Class.decl

val class_type : t -> Component.ClassType.t -> Component.ClassType.t

val signature : t -> Component.Signature.t -> Component.Signature.t

val apply_sig_map_items : t -> Signature.item list -> Signature.item list
(** Apply substitutions. *)
