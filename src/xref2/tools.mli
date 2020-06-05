(** Tools for manipulating the component data structures *)

open Odoc_model.Paths

module ResolvedMonad : sig
    type ('a, 'b) t = Resolved of 'a | Unresolved of 'b
end

type resolve_module_result =
  (Cpath.Resolved.module_ * Component.Module.t, Cpath.module_) ResolvedMonad.t

type resolve_module_type_result =
  (Cpath.Resolved.module_type * Component.ModuleType.t, Cpath.module_type) ResolvedMonad.t

type resolve_type_result =
  (Cpath.Resolved.type_ * (Find.type_, Component.TypeExpr.t) Find.found, Cpath.type_) ResolvedMonad.t

type resolve_class_type_result =
  (Cpath.Resolved.class_type * Find.class_type, Cpath.class_type) ResolvedMonad.t

type process_error = [ `OpaqueModule | `UnresolvedForwardPath ]

type handle_subs_error = [ `UnresolvedPath of [ `Module of Cpath.module_ ] ]

type signature_of_module_error =
  [ `OpaqueModule
  | `UnresolvedForwardPath
  | `UnresolvedPath of
    [ `Module of Cpath.module_ | `ModuleType of Cpath.module_type ] ]

type module_lookup_error =
  [ `Local of Env.t * Ident.module_ (* Found local path *)
  | `Unresolved_apply (* [`Apply] argument is not [`Resolved] *)
  | `Find_failure
  | `Lookup_failure of Identifier.Module.t
  | `Fragment_root
  | `Parent_sig of signature_of_module_error
  | `Parent_module_type of module_type_lookup_error
  | `Parent_module of module_lookup_error
  | `Parent of module_lookup_error
  | `Parent_expr of module_type_expr_of_module_error ]

and parent_lookup_error =
  [ `Parent_sig of signature_of_module_error
  | `Parent_module_type of module_type_lookup_error
  | `Parent of module_lookup_error
  | `Parent_expr of module_type_expr_of_module_error
  | `Parent_module of module_lookup_error
  | `Fragment_root
  ]

and module_type_expr_of_module_error =
  [ `ApplyNotFunctor
  | `OpaqueModule
  | `UnresolvedForwardPath
  | handle_subs_error
  | `Parent_module of module_lookup_error ]

and module_type_lookup_error =
  [ `LocalMT of Env.t * Cpath.Resolved.module_type
  | `Find_failure
  | `Parent_sig of signature_of_module_error
  | `Parent_module_type of module_type_lookup_error
  | `Parent_module of module_lookup_error
  | `Parent of module_lookup_error
  | `Parent_expr of module_type_expr_of_module_error
  | `Lookup_failureMT of Identifier.ModuleType.t
  | `Fragment_root
  | `Unresolved_apply ]

and type_lookup_error =
  [ `Local of Env.t * Cpath.Resolved.type_
  | `Unhandled of Cpath.Resolved.type_
  | `Parent_sig of signature_of_module_error
  | `Parent_module_type of module_type_lookup_error
  | `Parent_module of module_lookup_error

  | `Parent of module_lookup_error
  | `Parent_expr of module_type_expr_of_module_error
  | `Fragment_root

  | `Find_failure
  | `Lookup_failure of Odoc_model.Paths_types.Identifier.path_type ]

and class_type_lookup_error =
  [ `Local of Env.t * Cpath.Resolved.class_type
  | `Unhandled of Cpath.Resolved.class_type
  | `Parent_module of module_lookup_error
  | `Parent_module_type of module_type_lookup_error
  | `Parent_sig of signature_of_module_error
  | `Parent of module_lookup_error
  | `Parent_expr of module_type_expr_of_module_error
  | `Fragment_root

  | `Find_failure
  | `Lookup_failure of Odoc_model.Paths_types.Identifier.path_class_type ]

val reset_caches : unit -> unit


val lookup_module :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_ ->
    (Component.Module.t, module_lookup_error) Result.result

val lookup_module_type :
    mark_substituted:bool -> Env.t ->
    Cpath.Resolved.module_type ->
    (Component.ModuleType.t, module_type_lookup_error) Result.result

val lookup_type : Env.t -> Cpath.Resolved.type_ ->
    ((Find.type_, Component.TypeExpr.t) Find.found, type_lookup_error) Result.result


val resolve_module : mark_substituted:bool ->
    add_canonical:bool ->
    Env.t ->
    Cpath.module_ ->
    resolve_module_result

val resolve_module_type :
    mark_substituted:bool ->
    Env.t ->
    Cpath.module_type ->
    resolve_module_type_result

val resolve_type : Env.t -> Cpath.type_ ->
    resolve_type_result




val resolve_module_path : Env.t -> Cpath.module_ -> (Cpath.Resolved.module_, Cpath.module_) ResolvedMonad.t
val resolve_module_type_path : Env.t -> Cpath.module_type -> (Cpath.Resolved.module_type, Cpath.module_type) ResolvedMonad.t
val resolve_type_path : Env.t -> Cpath.type_ -> (Cpath.Resolved.type_, Cpath.type_) ResolvedMonad.t

val reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
val reresolve_module_type : Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type
val reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_
val reresolve_parent : Env.t -> Cpath.Resolved.parent -> Cpath.Resolved.parent

val handle_module_type_lookup : Env.t -> string -> Cpath.Resolved.parent -> Component.Signature.t -> (Cpath.Resolved.module_type * Component.ModuleType.t) option


val signature_of_module : Env.t ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result

val signature_of_module_type :     Env.t ->
    Component.ModuleType.t ->
    (Component.Signature.t, signature_of_module_error) Result.result

val signature_of_module_type_expr :     Env.t ->
    Component.ModuleType.expr ->
    (Component.Signature.t, signature_of_module_error) Result.result
    
val class_signature_of_class_type :
    Env.t -> Component.ClassType.t -> Component.ClassSignature.t option

val class_signature_of_class :
    Env.t -> Component.Class.t -> Component.ClassSignature.t option


type module_modifiers =
    [ `Aliased of Cpath.Resolved.module_
    | `SubstAliased of Cpath.Resolved.module_
    | `SubstMT of Cpath.Resolved.module_type
    ]
val get_module_path_modifiers : Env.t -> add_canonical:bool -> Component.Module.t -> module_modifiers option

val prefix_signature : Cpath.Resolved.parent * Component.Signature.t -> Component.Signature.t

val resolve_module_fragment : Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_ ->
    Cfrag.resolved_module option 

val resolve_type_fragment : 
Env.t ->
Cfrag.root * Component.Signature.t ->
Cfrag.type_ ->
Cfrag.resolved_type option

val reresolve_module_fragment : Env.t -> Cfrag.resolved_module -> Cfrag.resolved_module

val reresolve_type_fragment : Env.t -> Cfrag.resolved_type -> Cfrag.resolved_type

val fragmap_module :
Env.t ->
Cfrag.module_ ->
Component.ModuleType.substitution ->
Component.Signature.t ->
(Component.Signature.t, handle_subs_error) Result.result

val fragmap_type :
Env.t ->
Cfrag.type_ ->
Component.ModuleType.substitution ->
Component.Signature.t ->
Component.Signature.t

val handle_signature_with_subs :
Env.t ->
Component.Signature.t ->
Component.ModuleType.substitution list ->
(Component.Signature.t, handle_subs_error) Result.result
