open Odoc_model

module Tools_error = struct
  open Paths
  (** Errors raised by Tools *)

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
    | (*** Internal error: the module was not found in the parent signature *)
      `Lookup_failure of
      Identifier.Path.Module.t
      (** Could not find the module in the environment *)
    | `Lookup_failure_root of string  (** Could not find the root module *)
    | `Parent of parent_lookup_error ]

  and simple_module_type_expr_of_module_error =
    [ `ApplyNotFunctor
      (** Internal error: attempt made to apply a module that's not a functor *)
    | `OpaqueModule  (** The module does not have an expansion *)
    | `UnresolvedForwardPath
      (** The module signature depends upon a forward path *)
    | `UnresolvedPath of
      [ `Module of Cpath.module_ | `ModuleType of Cpath.module_type ]
    | `Parent of parent_lookup_error ]

  and simple_module_type_lookup_error =
    [ `LocalMT of Env.t * Ident.module_type
      (** Internal error: Found local path during lookup *)
    | `Find_failure
      (** Internal error: the module was not found in the parent signature *)
    | `Lookup_failureMT of Identifier.ModuleType.t
      (** Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and simple_type_lookup_error =
    [ `LocalType of Env.t * Ident.path_type
      (** Internal error: Found local path during lookup *)
    | `Class_replaced
      (** Class was replaced with a destructive substitution and we're not sure what to do now *)
    | `Find_failure
      (** Internal error: the type was not found in the parent signature *)
    | `Lookup_failureT of Identifier.Path.Type.t
      (** Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and parent_lookup_error =
    [ `Parent_sig of signature_of_module_error
      (** Error found while calculating the parent signature *)
    | `Parent_module_type of simple_module_type_lookup_error
      (** Error found while looking up parent module type *)
    | `Parent_expr of simple_module_type_expr_of_module_error
      (** Error found while evaluating parent module expression *)
    | `Parent_module of simple_module_lookup_error
      (** Error found while looking up parent module *)
    | `Fragment_root (* Encountered unexpected fragment root *)
    | `Parent of parent_lookup_error ]

  type any =
    [ simple_type_lookup_error
    | simple_module_type_lookup_error
    | simple_module_type_expr_of_module_error
    | simple_module_lookup_error
    | signature_of_module_error ]

  let rec pp : Format.formatter -> any -> unit =
   fun fmt err ->
    match err with
    | `OpaqueModule -> Format.fprintf fmt "OpaqueModule"
    | `UnresolvedForwardPath -> Format.fprintf fmt "Unresolved forward path"
    | `UnresolvedPath (`Module p) ->
        Format.fprintf fmt "Unresolved module path %a" Component.Fmt.module_path
          p
    | `UnresolvedPath (`ModuleType p) ->
        Format.fprintf fmt "Unresolved module type path %a"
          Component.Fmt.module_type_path p
    | `LocalMT (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Local (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `LocalType (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Unresolved_apply -> Format.fprintf fmt "Unresolved apply"
    | `Find_failure -> Format.fprintf fmt "Find failure"
    | `Lookup_failure m ->
        Format.fprintf fmt "Lookup failure (module): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failure_root r ->
        Format.fprintf fmt "Lookup failure (root module): %s" r
    | `Lookup_failureMT m ->
        Format.fprintf fmt "Lookup failure (module type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureT m ->
        Format.fprintf fmt "Lookup failure (type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `ApplyNotFunctor -> Format.fprintf fmt "Apply module is not a functor"
    | `Class_replaced -> Format.fprintf fmt "Class replaced"
    | `Parent p -> pp_parent fmt p

  and pp_parent : Format.formatter -> parent_lookup_error -> unit =
   fun fmt err ->
    match err with
    | `Parent p -> pp_parent fmt p
    | `Parent_sig e -> Format.fprintf fmt "Parent_sig: %a" pp (e :> any)
    | `Parent_module_type e ->
        Format.fprintf fmt "Parent_module_type: %a" pp (e :> any)
    | `Parent_expr e -> Format.fprintf fmt "Parent_expr: %a" pp (e :> any)
    | `Parent_module e -> Format.fprintf fmt "Parent_module: %a" pp (e :> any)
    | `Fragment_root -> Format.fprintf fmt "Fragment root"
end

(** To use as [Lookup_failures.kind]. *)
let rec kind_of_module_cpath = function
  | `Root _ -> Some `Root
  | `Substituted p' | `Dot (p', _) -> kind_of_module_cpath p'
  | `Apply (a, b) -> (
      match kind_of_module_cpath a with
      | Some _ as a -> a
      | None -> kind_of_module_cpath b )
  | _ -> None

let rec kind_of_module_type_cpath = function
  | `Substituted p' -> kind_of_module_type_cpath p'
  | `Dot (p', _) -> kind_of_module_cpath p'
  | _ -> None

let rec kind_of_error = function
  | `UnresolvedPath (`Module cp) -> kind_of_module_cpath cp
  | `UnresolvedPath (`ModuleType cp) -> kind_of_module_type_cpath cp
  | `Lookup_failure (`Root _) | `Lookup_failure_root _ -> Some `Root
  | `Parent (`Parent_sig e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module_type e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_expr e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent _ as e) -> kind_of_error (e :> Tools_error.any)
  | _ -> None

let report ~what ?tools_error action =
  let kind =
    match tools_error with
    | Some e -> kind_of_error (e :> Tools_error.any)
    | None -> (
        match what with
        | `Include (Component.Include.Alias cp) -> kind_of_module_cpath cp
        | `Module (`Root _) -> Some `Root
        | _ -> None )
  in
  let action =
    match action with
    | `Lookup -> "lookup"
    | `Expand -> "compile expansion for"
    | `Resolve_module_type -> "resolve type of"
    | `Resolve -> "resolve"
    | `Compile -> "compile"
  in
  let pp_tools_error fmt = function
    | Some e -> Format.fprintf fmt " %a" Tools_error.pp (e :> Tools_error.any)
    | None -> ()
  in
  let r ?(kind = kind) subject pp_a a =
    Lookup_failures.report ?kind "Failed to %s %s %a%a" action subject pp_a a
      pp_tools_error tools_error
  in
  let open Component.Fmt in
  let fmt_id fmt id = model_identifier fmt (id :> Paths.Identifier.t) in
  match what with
  | `Functor_parameter id -> r "functor parameter" fmt_id id
  | `Value id -> r "value" fmt_id id
  | `Class id -> r "class" fmt_id id
  | `Class_type id -> r "class type" fmt_id id
  | `Module id -> r "module" fmt_id id
  | `Module_type id -> r "module type" fmt_id id
  | `Module_path path -> r "module path" module_path path
  | `Module_type_path path -> r "module type path" module_type_path path
  | `Include decl -> r "include" include_decl decl
  | `Package path ->
      r "module package" module_type_path (path :> Cpath.module_type)
  | `Type cfrag -> r "type" type_fragment cfrag
  | `Type_path path -> r "type" type_path path
  | `With_module frag -> r "module substitution" module_fragment frag
  | `With_type frag -> r "type substitution" type_fragment frag
  | `Module_type_expr cexpr -> r "module type expression" module_type_expr cexpr
  | `Module_type_u_expr cexpr -> r "module type u expression" u_module_type_expr cexpr
