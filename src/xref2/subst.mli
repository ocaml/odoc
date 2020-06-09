(* Subst *)

val identity : Component.Substitution.t

val add_module :
  Ident.module_ ->
  Cpath.Resolved.module_ ->
  Component.Substitution.t ->
  Component.Substitution.t

val add_module_type :
  Ident.module_type ->
  Cpath.Resolved.module_type ->
  Component.Substitution.t ->
  Component.Substitution.t

val add_type :
  Ident.type_ ->
  Cpath.Resolved.type_ ->
  Component.Substitution.t ->
  Component.Substitution.t

val add_class :
  Ident.class_ ->
  Cpath.Resolved.class_type ->
  Component.Substitution.t ->
  Component.Substitution.t

val add_class_type :
  Ident.class_type ->
  Cpath.Resolved.class_type ->
  Component.Substitution.t ->
  Component.Substitution.t

val add_type_replacement :
  Ident.path_type ->
  Component.TypeExpr.t ->
  Component.Substitution.t ->
  Component.Substitution.t

val type_ :
  Component.Substitution.t -> Component.TypeDecl.t -> Component.TypeDecl.t

val type_expr :
  Component.Substitution.t -> Component.TypeExpr.t -> Component.TypeExpr.t

val module_ :
  Component.Substitution.t -> Component.Module.t -> Component.Module.t

val module_type :
  Component.Substitution.t -> Component.ModuleType.t -> Component.ModuleType.t

val module_substitution :
  Component.Substitution.t ->
  Component.ModuleSubstitution.t ->
  Component.ModuleSubstitution.t

val module_type_expr :
  Component.Substitution.t ->
  Component.ModuleType.expr ->
  Component.ModuleType.expr

val exception_ :
  Component.Substitution.t -> Component.Exception.t -> Component.Exception.t

val extension :
  Component.Substitution.t -> Component.Extension.t -> Component.Extension.t

val external_ :
  Component.Substitution.t -> Component.External.t -> Component.External.t

val include_ :
  Component.Substitution.t -> Component.Include.t -> Component.Include.t

val open_ : Component.Substitution.t -> Component.Open.t -> Component.Open.t

val value : Component.Substitution.t -> Component.Value.t -> Component.Value.t

val class_ : Component.Substitution.t -> Component.Class.t -> Component.Class.t

val class_decl :
  Component.Substitution.t -> Component.Class.decl -> Component.Class.decl

val class_type :
  Component.Substitution.t -> Component.ClassType.t -> Component.ClassType.t

val signature :
  Component.Substitution.t -> Component.Signature.t -> Component.Signature.t

val apply_sig_map :
  Component.Substitution.t ->
  Component.Signature.item list ->
  Component.Signature.removed_item list ->
  Component.Signature.t
