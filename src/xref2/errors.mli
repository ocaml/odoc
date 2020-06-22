(** Errors raised by Tools *)

open Odoc_model.Paths

type handle_subs_error =
  [ `UnresolvedPath of [ `Module of Cpath.module_ ]
    (** Failed to resolve a module path when applying a fragment item *) ]

type signature_of_module_error =
  [ `OpaqueModule  (** The module does not have an expansion *)
  | `UnresolvedForwardPath
    (** The module signature depends upon a forward path *)
  | `UnresolvedPath of
    [ `Module of Cpath.module_ | `ModuleType of Cpath.module_type ]
    (** The path to the module or module type could not be resolved *) ]

type simple_module_lookup_error =
  [ `Local of Env.t * Ident.path_module
    (** Internal error: Found local path during lookup *)
  | `Unresolved_apply  (** [`Apply] argument is not [`Resolved] *)
  | `Find_failure
    (*** Internal error: the module was not found in the parent signature *)
  | `Lookup_failure of Identifier.Path.Module.t
    (** Could not find the module in the environment *) ]

and simple_module_type_expr_of_module_error =
  [ `ApplyNotFunctor
    (** Internal error: attempt made to apply a module that's not a functor *)
  | `OpaqueModule  (** The module does not have an expansion *)
  | `UnresolvedForwardPath
    (** The module signature depends upon a forward path *)
  | `UnresolvedPath of
    [ `Module of Cpath.module_ | `ModuleType of Cpath.module_type ] ]

and simple_module_type_lookup_error =
  [ `LocalMT of Env.t * Cpath.Resolved.module_type
    (** Internal error: Found local path during lookup *)
  | `Find_failure
    (** Internal error: the module was not found in the parent signature *)
  | `Lookup_failureMT of Identifier.ModuleType.t
    (** Could not find the module in the environment *) ]

and simple_type_lookup_error =
  [ `LocalType of Env.t * Cpath.Resolved.type_
    (** Internal error: Found local path during lookup *)
  | `Find_failure
    (** Internal error: the type was not found in the parent signature *)
  | `Lookup_failureT of Identifier.Path.Type.t
    (** Could not find the module in the environment *) ]

and parent_lookup_error =
  [ `Parent_sig of signature_of_module_error
    (** Error found while calculating the parent signature *)
  | `Parent_module_type of simple_module_type_lookup_error
    (** Error found while looking up parent module type *)
  | `Parent_expr of simple_module_type_expr_of_module_error
    (** Error found while evaluating parent module expression *)
  | `Parent_module of simple_module_lookup_error
    (** Error found while looking up parent module *)
  | `Fragment_root (* Encountered unexpected fragment root *) ]

type any =
  [ parent_lookup_error
  | simple_type_lookup_error
  | simple_module_type_lookup_error
  | simple_module_type_expr_of_module_error
  | simple_module_lookup_error
  | signature_of_module_error ]
