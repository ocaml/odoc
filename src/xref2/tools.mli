(** Tools for manipulating the component data structures *)

open Odoc_model.Paths

module ResolvedMonad : sig
    type ('a, 'b) t = Resolved of 'a | Unresolved of 'b
end

(** {2 Errors} *)

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




(** {2 Lookup and resolve functions} *)

(** The following lookup and resolve functions take {{!module:Cpath.Resolved}resolved paths}
    (for lookup) or {{!module:Cpath.Unresolved}unresolved paths} (for resolve)
    and an {{!type:Env.t}environment} and return the representation of the
    component. The resolve functions additionally return the resolved path.
    There are some common arguments:

    - {!type:Env.t} is the environment that maps from {{!type:Odoc_model.Paths.Identifier.t}Identifiers}
      to {{!module:Component}Components}.
    - [mark_substituted] indicates that all paths in the resulting module
      that are the result of a substitution, either via a functor
      application or via a `with module..` construct, will be marked with a
      [`Substituted] constructor
    - [add_canonical] asks for [`Canonical] constructors to be added to modules
      for which there is a defined canonical path. If the 
    
    If the path is a 'Forward' path, that is, a path to a module that has not yet been
    compiled, then it may not be possible to resolve the path if this is being called
    during the 'compile' phase, in which case the function will return an unresolved path with no
    component. Resolution should be attempted again during the link phase.

    On entry the assumption is that all {{!type:Odoc_model.Paths.Identifier.t}Identifiers}
    in the paths are available in [env], except where there are forward paths. If the
    environment does not contain all the required modules (for example, if odoc has not
    been called on all of the dependent modules), an unresolved path will be returned
    with no component.
*)

(** [lookup_module ~mark_substituted env p] takes a resolved module cpath [p] and
    an environment and returns a representation of the module. 
*)
val lookup_module :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_ ->
    (Component.Module.t, module_lookup_error) Result.result

(** [lookup_module_type ~mark_substituted env p] takes a resolved module type
    cpath and an environment and returns a representation of the module type.
*)
val lookup_module_type :
    mark_substituted:bool -> Env.t ->
    Cpath.Resolved.module_type ->
    (Component.ModuleType.t, module_type_lookup_error) Result.result

(** [lookup_type env p] takes a resolved type path and an environment and returns
    a representation of the type. The type can be an ordinary type, a class type
    or a class. If the type has been destructively substituted, the path to the
    replacement type will be returned instead. *)
val lookup_type : Env.t -> Cpath.Resolved.type_ ->
    ((Find.type_, Component.TypeExpr.t) Find.found, type_lookup_error) Result.result



(** [resolve_module ~mark_substituted ~add_canonical env p] takes an unresolved
    module path and an environment and returns a tuple of the resolved module
    path alongside a representation of the module itself. *)
val resolve_module : mark_substituted:bool ->
    add_canonical:bool ->
    Env.t ->
    Cpath.module_ ->
    (Cpath.Resolved.module_ * Component.Module.t, Cpath.module_) ResolvedMonad.t


    
(** [resolve_module_type ~mark_substituted env p] takes an unresolved module
    type path and an environment and returns a tuple of the resolved module type
    path alongside a representation of the module type itself. *)
val resolve_module_type :
    mark_substituted:bool ->
    Env.t ->
    Cpath.module_type ->
    (Cpath.Resolved.module_type * Component.ModuleType.t, Cpath.module_type) ResolvedMonad.t



(** [resolve_type env p] takes an unresolved
    type path and an environment and returns a tuple of the resolved type
    path alongside a representation of the type itself. As with {!val:lookup_type}
    the returned type is either the type, class or class type, or if has been
    destructively substituted the return value is the path
    to the replaced type, class or class type. *)
val resolve_type : Env.t -> Cpath.type_ ->
    (Cpath.Resolved.type_ * (Find.type_, Component.TypeExpr.t) Find.found, Cpath.type_) ResolvedMonad.t



(** {3 Convenience functions } *)

(** The following functions are convenience functions called from {!module:Compile}
    or {!module:Link}, and simply call the [resolve_*] functions above and ignore
    the component. For the cases of modules and module types, these functions will
    additionally prepend an [OpaqueModule] or [OpaqueModuleType] constructor to the
    path if the module or module type cannot be expanded *)

val resolve_module_path : Env.t -> Cpath.module_ -> (Cpath.Resolved.module_, Cpath.module_) ResolvedMonad.t
val resolve_module_type_path : Env.t -> Cpath.module_type -> (Cpath.Resolved.module_type, Cpath.module_type) ResolvedMonad.t
val resolve_type_path : Env.t -> Cpath.type_ -> (Cpath.Resolved.type_, Cpath.type_) ResolvedMonad.t

(** {2 Re-resolve functions} *)

(** The re-resolve functions are called during the link phase to resolve canonical
    paths. They take as input only resolved paths. If any path remains unresolved
    as link phase, the path should be resolved via a call to {!resolve_module} or
    similar, and then passed to {!reresolve_module} *)

val reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
val reresolve_module_type : Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type
val reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_

(** {2 Ref_tools helpers} *)

(** The following functions are exposed for use in the {!module:Ref_tools} module
    only, allowing that module to reuse the machinery in this module for the
    resolution of {{!module:Odoc_model.Paths.Reference}References} *)

val reresolve_parent : Env.t -> Cpath.Resolved.parent -> Cpath.Resolved.parent

val handle_module_type_lookup : Env.t -> string -> Cpath.Resolved.parent -> Component.Signature.t -> Component.Substitution.t -> (Cpath.Resolved.module_type * Component.ModuleType.t) option

type module_modifiers =
    [ `Aliased of Cpath.Resolved.module_
    | `SubstAliased of Cpath.Resolved.module_
    | `SubstMT of Cpath.Resolved.module_type
    ]
val get_module_path_modifiers : Env.t -> add_canonical:bool -> Component.Module.t -> module_modifiers option

val prefix_signature : Cpath.Resolved.parent * Component.Signature.t -> Component.Signature.t

val signature_of_module :
    Env.t ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result

val signature_of_module_type :
    Env.t ->
    Component.ModuleType.t ->
    (Component.Signature.t, signature_of_module_error) Result.result



    
val class_signature_of_class_type :
    Env.t -> Component.ClassType.t -> Component.ClassSignature.t option

val class_signature_of_class :
    Env.t -> Component.Class.t -> Component.ClassSignature.t option


(** {2 Fragment resolution} *)
 
(** The following functions are use for the resolution of {{!type:Odoc_model.Paths.Fragment.t}Fragments}
    Whilst resolving fragments it is necessary to process them in order, applying
    the 'with' expression of module or type equality or substitution, before resolving
    the next fragment. The function [signature_of_module_type_expr] is used to supply
    the signature for the first fragment. For this purpose, [mark_substituted] should
    be [true]. As for the path resolution functions above, the resolve functions may
    be called during compile or link, whereas the reresolve functions should only be called
    during the link phase. *)
val signature_of_module_type_expr :  mark_substituted:bool ->    Env.t ->
    Component.ModuleType.expr ->
    (Component.Signature.t, signature_of_module_error) Result.result
    
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

(** {2 Fragmap functions} *)

(** The following functions take a signature and apply a transformation to
    it corresponding to one item in a [with type|module] module type. The
    functions construct a representation that is self-contained, meaning
    correct even without having to remember that it is the result of a
    fragment modification. For example:

    {[
        module type S = sig
            type t
        end
        module type T = sig
          module M : S
        end
        module type Z = T with type M.t = int
    ]}

    these functions should return a representation of [Z] as:

    {[
        module type T = sig
            module M : S with type t = int
        end
    ]}

    In particular, it will also handle [include] statements such that if
    a type or module introduced by an [include] is the subject of a fragment modification,
    the [include] statement will be marked as having a fragment modifier
    applied.
*)

(** [fragmap_module ~mark_substituted env frag sub sg] takes an environment [env]
    and signature [sg], a type fragment [frag] and a fragment substitution (e.g.
    [ModuleSubst] to destructively substitute a module), and returns the substituted
    signature. *)
val fragmap_module :
mark_substituted:bool ->
Env.t ->
Cfrag.module_ ->
Component.ModuleType.substitution ->
Component.Signature.t ->
(Component.Signature.t, handle_subs_error) Result.result

(** [fragmap_type ~mark_substituted env frag sub sg] takes an environment [env]
    and signature [sg], a module fragment [frag] and a fragment substitution (e.g.
    [TypeSubst] to destructively substitute a type), and returns the substituted
    signature. *)
val fragmap_type :
Env.t ->
Cfrag.type_ ->
Component.ModuleType.substitution ->
Component.Signature.t ->
Component.Signature.t

(** [handle_signature_with_subs ~mark_substituted env sg subs] applies the
    fragment modifiers [subs], in order, to the supplied signature [sg]. *)
val handle_signature_with_subs :
mark_substituted : bool ->
Env.t ->
Component.Signature.t ->
Component.ModuleType.substitution list ->
(Component.Signature.t, handle_subs_error) Result.result



(** {2 Cache handling} *)

(** In order to resolve paths quickly, several imperative caches are used.
    The following functions are used to manipulate the caches *)

val reset_caches : unit -> unit (** Empty the caches completely *)

val disable_all_caches : unit -> unit (** Disable the caches completely *)

