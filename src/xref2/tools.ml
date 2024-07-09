open Odoc_model.Names

(* Add [result] and a bind operator over it in scope *)
open Odoc_utils.ResultMonad

type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorParameter.t * Component.ModuleType.expr

type ('a, 'b) either = Left of 'a | Right of 'b

type module_modifiers =
  [ `Aliased of Cpath.Resolved.module_ | `SubstMT of Cpath.Resolved.module_type ]

type module_type_modifiers = [ `AliasModuleType of Cpath.Resolved.module_type ]

(* These three functions take a fully-qualified canonical path and return
   a list of shorter possibilities to test *)
let c_mod_poss env p =
  (* canonical module paths *)
  let rec inner = function
    | `Dot (p, n) -> (
        let rest = List.map (fun p -> `Dot (p, n)) (inner p) in
        match Env.lookup_by_name Env.s_module n env with
        | Ok (`Module (id, m)) ->
            let m = Component.Delayed.get m in
            `Identifier (id, m.hidden) :: rest
        | Error _ -> rest)
    | p -> [ p ]
  in
  inner p

let c_modty_poss env p =
  (* canonical module type paths *)
  match p with
  | `Dot (p, n) -> (
      let rest = List.map (fun p -> `Dot (p, n)) (c_mod_poss env p) in
      match Env.lookup_by_name Env.s_module_type n env with
      | Ok (`ModuleType (id, _)) -> `Identifier (id, false) :: rest
      | Error _ -> rest)
  | p -> [ p ]

let c_ty_poss env p =
  (* canonical type paths *)
  match p with
  | `Dot (p, n) -> (
      let rest = List.map (fun p -> `Dot (p, n)) (c_mod_poss env p) in
      match Env.lookup_by_name Env.s_datatype n env with
      | Ok (`Type (id, _)) ->
          `Identifier ((id :> Odoc_model.Paths.Identifier.Path.Type.t), false)
          :: rest
      | Error _ -> rest)
  | p -> [ p ]

(* Small helper function for resolving canonical paths.
   [canonical_helper env resolve lang_of possibilities p2] takes the
   fully-qualified path [p2] and returns the shortest resolved path
   whose identifier is the same as the resolved fully qualified path.
   [resolve] is a function that resolves an arbitrary unresolved path,
   [lang_of] turns a resolved path into a generic resolved Lang path
   and [possibilities] is a function that, given the fully qualified
   unresolved path, returns an ordered list of all possible unresolved
   paths starting with the shortest and including the longest one. *)
let canonical_helper :
      'unresolved 'resolved.
      Env.t ->
      (Env.t -> 'unresolved -> ('resolved * 'result, _) result) ->
      ('resolved -> Odoc_model.Paths.Path.Resolved.t) ->
      (Env.t -> 'unresolved -> 'unresolved list) ->
      'unresolved ->
      ('resolved * 'result) option =
 fun env resolve lang_of possibilities p2 ->
  let resolve p =
    match resolve env p with Ok rp -> Some rp | Error _ -> None
  in
  let get_identifier cpath =
    Odoc_model.Paths.Path.Resolved.identifier (lang_of cpath)
  in
  match resolve p2 with
  | None -> None
  | Some (rp2, _) -> (
      let fallback_id = get_identifier rp2 in
      let resolved =
        Odoc_utils.List.filter_map resolve (possibilities env p2)
      in
      let find_fn (r, _) = get_identifier r = fallback_id in
      try Some (List.find find_fn resolved) with _ -> None)

let prefix_substitution path sg =
  let open Component.Signature in
  let rec get_sub sub' is =
    match is with
    | [] -> sub'
    | Type (id, _, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub
          (Subst.add_type id (`Type (path, name)) (`Type (path, name)) sub')
          rest
    | Module (id, _, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (`Module (path, name))
             (`Module (path, name))
             sub')
          rest
    | ModuleType (id, _) :: rest ->
        let name = Ident.Name.typed_module_type id in
        get_sub
          (Subst.add_module_type id
             (`ModuleType (path, name))
             (`ModuleType (path, name))
             sub')
          rest
    | ModuleTypeSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_module_type id in
        get_sub
          (Subst.add_module_type id
             (`ModuleType (path, name))
             (`ModuleType (path, name))
             sub')
          rest
    | ModuleSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (`Module (path, name))
             (`Module (path, name))
             sub')
          rest
    | TypeSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub
          (Subst.add_type id (`Type (path, name)) (`Type (path, name)) sub')
          rest
    | Exception _ :: rest
    | TypExt _ :: rest
    | Value (_, _) :: rest
    | Comment _ :: rest ->
        get_sub sub' rest
    | Class (id, _, _) :: rest ->
        let name = Ident.Name.typed_class id in
        get_sub
          (Subst.add_class id (`Class (path, name)) (`Class (path, name)) sub')
          rest
    | ClassType (id, _, _) :: rest ->
        let name = Ident.Name.typed_class_type id in
        get_sub
          (Subst.add_class_type id
             (`ClassType (path, name))
             (`ClassType (path, name))
             sub')
          rest
    | Include i :: rest -> get_sub (get_sub sub' i.expansion_.items) rest
    | Open o :: rest -> get_sub (get_sub sub' o.expansion.items) rest
  in
  get_sub Subst.identity sg.items

let prefix_signature (path, sg) =
  let open Component.Signature in
  let sub = prefix_substitution path sg in
  let items =
    List.map
      (function
        | Module (id, r, m) ->
            Module
              ( Ident.Rename.module_ id,
                r,
                Component.Delayed.put (fun () ->
                    Subst.module_ sub (Component.Delayed.get m)) )
        | ModuleType (id, mt) ->
            ModuleType
              ( Ident.Rename.module_type id,
                Component.Delayed.put (fun () ->
                    Subst.module_type sub (Component.Delayed.get mt)) )
        | Type (id, r, t) ->
            Type
              ( Ident.Rename.type_ id,
                r,
                Component.Delayed.put (fun () ->
                    Subst.type_ sub (Component.Delayed.get t)) )
        | TypeSubstitution (id, t) ->
            TypeSubstitution (Ident.Rename.type_ id, Subst.type_ sub t)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution
              (Ident.Rename.module_ id, Subst.module_substitution sub m)
        | ModuleTypeSubstitution (id, m) ->
            ModuleTypeSubstitution
              (Ident.Rename.module_type id, Subst.module_type_substitution sub m)
        | Exception (id, e) -> Exception (id, Subst.exception_ sub e)
        | TypExt t -> TypExt (Subst.extension sub t)
        | Value (id, v) ->
            Value
              ( id,
                Component.Delayed.put (fun () ->
                    Subst.value sub (Component.Delayed.get v)) )
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i -> Include (Subst.include_ sub i)
        | Open o -> Open (Subst.open_ sub o)
        | Comment c -> Comment c)
      sg.items
  in
  { sg with items }

open Errors.Tools_error

type resolve_module_result =
  ( Cpath.Resolved.module_ * Component.Module.t Component.Delayed.t,
    simple_module_lookup_error )
  Result.result

type resolve_module_type_result =
  ( Cpath.Resolved.module_type * Component.ModuleType.t,
    simple_module_type_lookup_error )
  Result.result

type resolve_type_result =
  ( Cpath.Resolved.type_ * Find.careful_type,
    simple_type_lookup_error )
  Result.result

type resolve_value_result =
  (Cpath.Resolved.value * Find.value, simple_value_lookup_error) Result.result

type resolve_class_type_result =
  ( Cpath.Resolved.class_type * Find.careful_class,
    simple_type_lookup_error )
  Result.result

type ('a, 'b, 'c) sig_map = { type_ : 'a; module_ : 'b; module_type : 'c }

let id_map = { type_ = None; module_ = None; module_type = None }

module type MEMO = sig
  type result

  include Hashtbl.HashedType
end

module MakeMemo (X : MEMO) = struct
  module M = Hashtbl.Make (X)

  let cache : (X.result * int * Env.LookupTypeSet.t) M.t = M.create 10000

  let enabled = ref true

  let memoize f env arg =
    if not !enabled then f env arg
    else
      let env_id = Env.id env in
      let no_memo () =
        let lookups, result =
          Env.with_recorded_lookups env (fun env' -> f env' arg)
        in
        M.add cache arg (result, env_id, lookups);
        result
      in
      match M.find_all cache arg with
      | [] -> no_memo ()
      | xs ->
          let rec find_fast = function
            | (result, env_id', _) :: _ when env_id' = env_id -> result
            | _ :: ys -> find_fast ys
            | [] -> find xs
          and find = function
            | (m, _, lookups) :: xs ->
                if Env.verify_lookups env lookups then m else find xs
            | [] -> no_memo ()
          in
          find_fast xs

  let clear () = M.clear cache
end

module LookupModuleMemo = MakeMemo (struct
  type t = Cpath.Resolved.module_

  type result =
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module LookupParentMemo = MakeMemo (struct
  type t = Cpath.Resolved.parent

  type result =
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module LookupAndResolveMemo = MakeMemo (struct
  type t = Cpath.module_

  type result = resolve_module_result

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module HandleCanonicalModuleMemo = MakeMemo (struct
  type t = Odoc_model.Paths.Path.Module.t

  type result = Odoc_model.Paths.Path.Module.t

  let equal x3 y3 = x3 = y3

  let hash y = Hashtbl.hash y
end)

module ExpansionOfModuleMemo = MakeMemo (struct
  type t = Cpath.Resolved.module_

  type result = (expansion, expansion_of_module_error) Result.result

  let equal = ( = )

  let hash = Hashtbl.hash
end)

let disable_all_caches () =
  LookupModuleMemo.enabled := false;
  LookupAndResolveMemo.enabled := false;
  ExpansionOfModuleMemo.enabled := false;
  LookupParentMemo.enabled := false

let reset_caches () =
  LookupModuleMemo.clear ();
  LookupAndResolveMemo.clear ();
  ExpansionOfModuleMemo.clear ();
  LookupParentMemo.clear ()

let simplify_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
    =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m with
  | `Module (`Module (`Gpath (`Identifier p)), name) -> (
      let ident = (Mk.module_ ((p :> Signature.t), name) : Path.Module.t) in
      match Env.(lookup_by_id s_module (ident :> Signature.t) env) with
      | Some _ -> `Gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let simplify_module_type :
    Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m with
  | `ModuleType (`Module (`Gpath (`Identifier p)), name) -> (
      let ident =
        (Mk.module_type ((p :> Signature.t), name) : Path.ModuleType.t)
      in
      match Env.(lookup_by_id s_module_type (ident :> Signature.t) env) with
      | Some _ -> `Gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let simplify_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m with
  | `Type (`Module (`Gpath (`Identifier p)), name) -> (
      let ident = (Mk.type_ ((p :> Signature.t), name) : Path.Type.t) in
      match Env.(lookup_by_id s_datatype (ident :> Path.Type.t) env) with
      | Some _ -> `Gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let rec handle_apply env func_path arg_path m =
  let rec find_functor mty =
    match mty with
    | Component.ModuleType.Functor (Named arg, expr) ->
        Ok (arg.Component.FunctorParameter.id, expr)
    | Component.ModuleType.Path { p_path; _ } -> (
        match resolve_module_type env p_path with
        | Ok (_, { Component.ModuleType.expr = Some mty'; _ }) ->
            find_functor mty'
        | _ -> Error `OpaqueModule)
    | _ -> Error `ApplyNotFunctor
  in
  module_type_expr_of_module env m >>= fun mty' ->
  find_functor mty' >>= fun (arg_id, result) ->
  let new_module = { m with Component.Module.type_ = ModuleType result } in
  let substitution = `Substituted arg_path in

  let path = `Apply (func_path, arg_path) in
  let subst =
    Subst.add_module
      (arg_id :> Ident.path_module)
      (`Resolved substitution) substitution Subst.identity
  in
  let subst = Subst.unresolve_opaque_paths subst in
  Ok (path, Subst.module_ subst new_module)

and add_canonical_path :
    Component.Module.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
 fun m p ->
  match p with
  | `Canonical _ -> p
  | _ -> (
      match m.Component.Module.canonical with
      | Some cp -> `Canonical (p, cp)
      | None -> p)

and add_canonical_path_mt :
    Component.ModuleType.t ->
    Cpath.Resolved.module_type ->
    Cpath.Resolved.module_type =
 fun m p ->
  match p with
  | `CanonicalModuleType _ -> p
  | _ -> (
      match m.canonical with
      | Some cp -> `CanonicalModuleType (p, cp)
      | None -> p)

and get_substituted_module_type :
    Env.t -> Component.ModuleType.expr -> Cpath.Resolved.module_type option =
 fun env expr ->
  match expr with
  | Component.ModuleType.Path { p_path; _ } ->
      if Cpath.is_module_type_substituted p_path then
        match resolve_module_type env p_path with
        | Ok (resolved_path, _) -> Some resolved_path
        | Error _ -> None
      else None
  | _ -> None

and get_module_type_path_modifiers :
    Env.t -> Component.ModuleType.t -> module_type_modifiers option =
 fun env m ->
  let alias_of expr =
    match expr with
    | Component.ModuleType.Path alias_path -> (
        match resolve_module_type env alias_path.p_path with
        | Ok (resolved_alias_path, _) -> Some resolved_alias_path
        | Error _ -> None)
    (* | Functor (_arg, res) -> alias_of res *)
    | _ -> None
  in
  match m.expr with
  | Some e -> (
      match alias_of e with Some e -> Some (`AliasModuleType e) | None -> None)
  | None -> None

and process_module_type env m p' =
  let open Component.ModuleType in
  let open Odoc_utils.OptionMonad in
  (* Loop through potential chains of module_type equalities, looking for substitutions *)
  let substpath =
    m.expr >>= get_substituted_module_type env >>= fun p ->
    Some (`SubstT (p, p'))
  in

  let p' = match substpath with Some p -> p | None -> p' in
  let p'' =
    match get_module_type_path_modifiers env m with
    | Some (`AliasModuleType e) -> `AliasModuleType (e, p')
    | None -> p'
  in
  add_canonical_path_mt m p''

and get_module_path_modifiers : Env.t -> Component.Module.t -> _ option =
 fun env m ->
  match m.type_ with
  | Alias (alias_path, _) -> (
      match resolve_module env alias_path with
      | Ok (resolved_alias_path, _) -> Some (`Aliased resolved_alias_path)
      | Error _ -> None)
  | ModuleType t -> (
      match get_substituted_module_type env t with
      | Some s -> Some (`SubstMT s)
      | None -> None)

and process_module_path env m rp =
  let rp = if m.Component.Module.hidden then `Hidden rp else rp in
  let rp' =
    match get_module_path_modifiers env m with
    | None -> rp
    | Some (`Aliased rp') ->
        let dest_hidden =
          Cpath.is_resolved_module_hidden ~weak_canonical_test:true rp'
        in
        if dest_hidden then rp
        else
          let unresolved_rp =
            try Cpath.unresolve_resolved_module_path rp with _ -> `Resolved rp
          in
          (* Keep the resolved path for the canonical processing below in handle_canonical_module.strip_alias *)
          `Alias (rp', unresolved_rp, Some rp)
    | Some (`SubstMT p') -> `Subst (p', rp)
  in
  add_canonical_path m rp'

and handle_module_lookup env id rparent sg sub =
  match Find.careful_module_in_sig sg id with
  | Some (`FModule (name, m)) ->
      let rp' = simplify_module env (`Module (rparent, name)) in
      let m' = Subst.module_ sub m in
      let md' = Component.Delayed.put_val m' in
      Ok (process_module_path env m' rp', md')
  | Some (`FModule_removed p) -> resolve_module env p
  | None -> Error `Find_failure

and handle_module_type_lookup env id p sg sub =
  let open Odoc_utils.OptionMonad in
  Find.module_type_in_sig sg id >>= fun (`FModuleType (name, mt)) ->
  let mt = Subst.module_type sub mt in
  let p' = simplify_module_type env (`ModuleType (p, name)) in
  let p'' = process_module_type env mt p' in
  Some (p'', mt)

and handle_type_lookup env id p sg =
  match Find.careful_type_in_sig sg id with
  | Some (`FClass (name, _) as t) -> Ok (`Class (p, name), t)
  | Some (`FClassType (name, _) as t) -> Ok (`ClassType (p, name), t)
  | Some (`FType (name, _) as t) -> Ok (simplify_type env (`Type (p, name)), t)
  | Some (`FType_removed (name, _, _) as t) -> Ok (`Type (p, name), t)
  | None -> Error `Find_failure

and handle_value_lookup _env id p sg =
  match Find.value_in_sig sg id with
  | (`FValue (name, _) as v) :: _ -> Ok (`Value (p, name), v)
  | _ -> Error `Find_failure

and handle_class_type_lookup id p sg =
  match Find.careful_class_in_sig sg id with
  | Some (`FClass (name, _) as t) -> Ok (`Class (p, name), t)
  | Some (`FClassType (name, _) as t) -> Ok (`ClassType (p, name), t)
  | Some (`FType_removed (_name, _, _) as _t) -> Error `Class_replaced
  | None -> Error `Find_failure

and lookup_module_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun env path ->
  match path with
  | `Identifier i ->
      of_option ~error:(`Lookup_failure i) (Env.(lookup_by_id s_module) i env)
      >>= fun (`Module (_, m)) -> Ok m
  | `Apply (functor_path, argument_path) ->
      lookup_module_gpath env functor_path >>= fun functor_module ->
      let functor_module = Component.Delayed.get functor_module in
      handle_apply env (`Gpath functor_path) (`Gpath argument_path)
        functor_module
      |> map_error (fun e -> `Parent (`Parent_expr e))
      >>= fun (_, m) -> Ok (Component.Delayed.put_val m)
  | `Module (parent, name) ->
      let find_in_sg sg sub =
        match Find.careful_module_in_sig sg (ModuleName.to_string name) with
        | None -> Error `Find_failure
        | Some (`FModule (_, m)) ->
            Ok (Component.Delayed.put_val (Subst.module_ sub m))
        | Some (`FModule_removed p) ->
            resolve_module env p >>= fun (_, m) -> Ok m
      in
      lookup_parent_gpath env parent
      |> map_error (fun e -> (e :> simple_module_lookup_error))
      >>= fun (sg, sub) -> find_in_sg sg sub
  | `Alias (p, _) -> lookup_module_gpath env p
  | `Subst (_, p) -> lookup_module_gpath env p
  | `Hidden p -> lookup_module_gpath env p
  | `Canonical (p, _) -> lookup_module_gpath env p
  | `OpaqueModule m -> lookup_module_gpath env m
  | `Substituted m -> lookup_module_gpath env m

and lookup_module :
    Env.t ->
    Cpath.Resolved.module_ ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun env' path' ->
  let lookup env (path : ExpansionOfModuleMemo.M.key) =
    match path with
    | `Local lpath -> Error (`Local (env, lpath))
    | `Gpath p -> lookup_module_gpath env p
    | `Substituted x -> lookup_module env x
    | `Apply (functor_path, argument_path) ->
        lookup_module env functor_path >>= fun functor_module ->
        let functor_module = Component.Delayed.get functor_module in
        handle_apply env functor_path argument_path functor_module
        |> map_error (fun e -> `Parent (`Parent_expr e))
        >>= fun (_, m) -> Ok (Component.Delayed.put_val m)
    | `Module (parent, name) ->
        let find_in_sg sg sub =
          match Find.careful_module_in_sig sg (ModuleName.to_string name) with
          | None -> Error `Find_failure
          | Some (`FModule (_, m)) ->
              Ok (Component.Delayed.put_val (Subst.module_ sub m))
          | Some (`FModule_removed p) ->
              resolve_module env p >>= fun (_, m) -> Ok m
        in
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_module_lookup_error))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `Alias (_, cs, _) -> (
        match resolve_module env cs with
        | Ok (_, r) -> Ok r
        | Error e -> Error e)
    | `Subst (_, p) -> lookup_module env p
    | `Hidden p -> lookup_module env p
    | `Canonical (p, _) -> lookup_module env p
    | `OpaqueModule m -> lookup_module env m
  in
  LookupModuleMemo.memoize lookup env' path'

and lookup_module_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t ->
    (Component.ModuleType.t, simple_module_type_lookup_error) Result.result =
 fun env path ->
  match path with
  | `Identifier i ->
      of_option ~error:(`Lookup_failureMT i)
        (Env.(lookup_by_id s_module_type) i env)
      >>= fun (`ModuleType (_, mt)) -> Ok mt
  | `CanonicalModuleType (s, _) | `SubstT (_, s) ->
      lookup_module_type_gpath env s
  | `ModuleType (parent, name) ->
      let find_in_sg sg sub =
        match Find.module_type_in_sig sg (ModuleTypeName.to_string name) with
        | None -> Error `Find_failure
        | Some (`FModuleType (_, mt)) -> Ok (Subst.module_type sub mt)
      in
      lookup_parent_gpath env parent
      |> map_error (fun e -> (e :> simple_module_type_lookup_error))
      >>= fun (sg, sub) -> find_in_sg sg sub
  | `AliasModuleType (_, mt) -> lookup_module_type_gpath env mt
  | `OpaqueModuleType m -> lookup_module_type_gpath env m
  | `SubstitutedMT m -> lookup_module_type_gpath env m

and lookup_module_type :
    Env.t ->
    Cpath.Resolved.module_type ->
    (Component.ModuleType.t, simple_module_type_lookup_error) Result.result =
 fun env path ->
  let lookup env =
    match path with
    | `Local l -> Error (`LocalMT (env, l))
    | `Gpath p -> lookup_module_type_gpath env p
    | `Substituted s | `CanonicalModuleType (s, _) | `SubstT (_, s) ->
        lookup_module_type env s
    | `ModuleType (parent, name) ->
        let find_in_sg sg sub =
          match Find.module_type_in_sig sg (ModuleTypeName.to_string name) with
          | None -> Error `Find_failure
          | Some (`FModuleType (_, mt)) -> Ok (Subst.module_type sub mt)
        in
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_module_type_lookup_error))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `AliasModuleType (_, mt) -> lookup_module_type env mt
    | `OpaqueModuleType m -> lookup_module_type env m
  in
  lookup env

and lookup_parent :
    Env.t ->
    Cpath.Resolved.parent ->
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result =
 fun env' parent' ->
  let lookup env parent =
    match parent with
    | `Module p ->
        lookup_module env p |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun m ->
        let m = Component.Delayed.get m in
        expansion_of_module env m
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= assert_not_functor
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `ModuleType p ->
        lookup_module_type env p
        |> map_error (fun e -> `Parent (`Parent_module_type e))
        >>= fun mt ->
        expansion_of_module_type env mt
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= assert_not_functor
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `FragmentRoot ->
        Env.lookup_fragment_root env
        |> of_option ~error:(`Parent `Fragment_root)
        >>= fun (_, sg) -> Ok (sg, prefix_substitution parent sg)
  in
  LookupParentMemo.memoize lookup env' parent'

and lookup_parent_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result =
 fun env parent ->
  lookup_module_gpath env parent
  |> map_error (fun e -> `Parent (`Parent_module e))
  >>= fun m ->
  let m = Component.Delayed.get m in
  expansion_of_module env m
  |> map_error (fun e -> `Parent (`Parent_sig e))
  >>= assert_not_functor
  >>= fun sg -> Ok (sg, prefix_substitution (`Module (`Gpath parent)) sg)

and lookup_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Type.t ->
    (Find.careful_type, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent_gpath env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    match Find.careful_type_in_sig sg name with
    | Some (`FClass (name, c)) -> Ok (`FClass (name, Subst.class_ sub c))
    | Some (`FClassType (name, ct)) ->
        Ok (`FClassType (name, Subst.class_type sub ct))
    | Some (`FType (name, t)) -> Ok (`FType (name, Subst.type_ sub t))
    | Some (`FType_removed (name, texpr, eq)) ->
        Ok (`FType_removed (name, Subst.type_expr sub texpr, eq))
    | None -> Error `Find_failure
  in
  let res =
    match p with
    | `Identifier { iv = `CoreType name; _ } ->
        (* CoreTypes aren't put into the environment, so they can't be handled
           by the next clause. They are already resolved. *)
        Ok
          (`FType
            ( name,
              Component.Of_Lang.(
                type_decl (empty ())
                  (Odoc_model.Predefined.type_of_core_type
                     (TypeName.to_string name))) ))
    | `Identifier ({ iv = `Type _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_datatype) i env)
        >>= fun (`Type ({ iv = `CoreType name | `Type (_, name); _ }, t)) ->
        Ok (`FType (name, t))
    | `Identifier ({ iv = `Class _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class ({ iv = `Class (_, name); _ }, t)) ->
        Ok (`FClass (name, t))
    | `Identifier ({ iv = `ClassType _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType ({ iv = `ClassType (_, name); _ }, t)) ->
        Ok (`FClassType (name, t))
    | `CanonicalType (t1, _) -> lookup_type_gpath env t1
    | `Type (p, id) -> do_type p (TypeName.to_string id)
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
    | `SubstitutedT t -> lookup_type_gpath env t
    | `SubstitutedCT t ->
        lookup_type_gpath env (t :> Odoc_model.Paths.Path.Resolved.Type.t)
  in
  res

and lookup_value_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Value.t ->
    (Find.value, simple_value_lookup_error) Result.result =
 fun env p ->
  let do_value p name =
    lookup_parent_gpath env p
    |> map_error (fun e -> (e :> simple_value_lookup_error))
    >>= fun (sg, sub) ->
    match Find.value_in_sig sg name with
    | `FValue (name, t) :: _ -> Ok (`FValue (name, Subst.value sub t))
    | [] -> Error `Find_failure
  in
  let res =
    match p with
    | `Identifier ({ iv = `Value _; _ } as i) ->
        of_option ~error:(`Lookup_failureV i) (Env.(lookup_by_id s_value) i env)
        >>= fun (`Value ({ iv = `Value (_, name); _ }, t)) ->
        Ok (`FValue (name, t))
    | `Value (p, id) -> do_value p (ValueName.to_string id)
  in
  res

and lookup_class_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ClassType.t ->
    (Find.careful_class, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent_gpath env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    match Find.careful_class_in_sig sg name with
    | Some (`FClass (name, c)) -> Ok (`FClass (name, Subst.class_ sub c))
    | Some (`FClassType (name, ct)) ->
        Ok (`FClassType (name, Subst.class_type sub ct))
    | Some (`FType_removed (name, texpr, eq)) ->
        Ok (`FType_removed (name, Subst.type_expr sub texpr, eq))
    | None -> Error `Find_failure
  in
  let res =
    match p with
    | `Identifier ({ iv = `Class _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class ({ iv = `Class (_, name); _ }, t)) ->
        Ok (`FClass (name, t))
    | `Identifier ({ iv = `ClassType _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType ({ iv = `ClassType (_, name); _ }, t)) ->
        Ok (`FClassType (name, t))
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
    | `SubstitutedCT c -> lookup_class_type_gpath env c
  in
  res

and lookup_type :
    Env.t ->
    Cpath.Resolved.type_ ->
    (Find.careful_type, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent env p |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    handle_type_lookup env name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
      | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
      | `FType (name, t) -> `FType (name, Subst.type_ sub t)
      | `FType_removed (name, texpr, eq) ->
          `FType_removed (name, Subst.type_expr sub texpr, eq)
    in
    Ok t
  in
  let res =
    match p with
    | `Local id -> Error (`LocalType (env, id))
    | `Gpath p -> lookup_type_gpath env p
    | `CanonicalType (t1, _) -> lookup_type env t1
    | `Substituted s -> lookup_type env s
    | `Type (p, id) -> do_type p (TypeName.to_string id)
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and lookup_value :
    Env.t ->
    Cpath.Resolved.value ->
    (_, simple_value_lookup_error) Result.result =
 fun env p ->
  match p with
  | `Value (p, id) ->
      lookup_parent env p
      |> map_error (fun e -> (e :> simple_value_lookup_error))
      >>= fun (sg, sub) ->
      handle_value_lookup env (ValueName.to_string id) p sg
      >>= fun (_, `FValue (name, c)) -> Ok (`FValue (name, Subst.value sub c))
  | `Gpath p -> lookup_value_gpath env p

and lookup_class_type :
    Env.t ->
    Cpath.Resolved.class_type ->
    (Find.careful_class, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent env p |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    handle_class_type_lookup name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
      | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
      | `FType_removed (name, texpr, eq) ->
          `FType_removed (name, Subst.type_expr sub texpr, eq)
    in
    Ok t
  in
  let res =
    match p with
    | `Local id -> Error (`LocalType (env, (id :> Ident.path_type)))
    | `Gpath p -> lookup_class_type_gpath env p
    | `Substituted s -> lookup_class_type env s
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and resolve_and_lookup_parent :
    Env.t ->
    Cpath.module_ ->
    ( Cpath.Resolved.parent * Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    result =
 fun env parent ->
  resolve_module env parent |> map_error (fun e -> `Parent (`Parent_module e))
  >>= fun (parent, _) ->
  lookup_parent env (`Module parent) >>= fun (parent_sig, sub) ->
  Ok (`Module parent, parent_sig, sub)

and resolve_module : Env.t -> Cpath.module_ -> resolve_module_result =
 fun env' path ->
  let id = path in
  let resolve env p =
    match p with
    | `Dot (parent, id) ->
        resolve_and_lookup_parent env parent
        |> map_error (fun e -> (e :> simple_module_lookup_error))
        >>= fun (parent, parent_sig, sub) ->
        handle_module_lookup env id parent parent_sig sub
    | `Module (parent, id) ->
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_module_lookup_error))
        >>= fun (parent_sig, sub) ->
        handle_module_lookup env (ModuleName.to_string id) parent parent_sig sub
    | `Apply (m1, m2) -> (
        let func = resolve_module env m1 in
        let arg = resolve_module env m2 in
        match (func, arg) with
        | Ok (func_path', m), Ok (arg_path', _) -> (
            let m = Component.Delayed.get m in
            match handle_apply env func_path' arg_path' m with
            | Ok (p, m) -> Ok (p, Component.Delayed.put_val m)
            | Error e -> Error (`Parent (`Parent_expr e)))
        | Error e, _ -> Error e
        | _, Error e -> Error e)
    | `Identifier (i, hidden) ->
        of_option ~error:(`Lookup_failure i) (Env.(lookup_by_id s_module) i env)
        >>= fun (`Module (_, m)) ->
        let rp =
          if hidden then `Hidden (`Gpath (`Identifier i))
          else `Gpath (`Identifier i)
        in
        Ok (process_module_path env (Component.Delayed.get m) rp, m)
    | `Local (p, _) -> Error (`Local (env, p))
    | `Resolved r -> lookup_module env r >>= fun m -> Ok (r, m)
    | `Substituted s ->
        resolve_module env s |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun (p, m) -> Ok (`Substituted p, m)
    | `Root r -> (
        match Env.lookup_root_module r env with
        | Some (Env.Resolved (_, p, m)) ->
            let p =
              `Gpath
                (`Identifier (p :> Odoc_model.Paths.Identifier.Path.Module.t))
            in
            let p = process_module_path env m p in
            Ok (p, Component.Delayed.put_val m)
        | Some Env.Forward ->
            Error (`Parent (`Parent_sig `UnresolvedForwardPath))
        | None -> Error (`Lookup_failure_root r))
    | `Forward f ->
        resolve_module env (`Root f)
        |> map_error (fun e -> `Parent (`Parent_module e))
  in
  LookupAndResolveMemo.memoize resolve env' id

and resolve_module_type :
    Env.t -> Cpath.module_type -> resolve_module_type_result =
 fun env p ->
  match p with
  | `Dot (parent, id) ->
      resolve_and_lookup_parent env parent
      |> map_error (fun e -> (e :> simple_module_type_lookup_error))
      >>= fun (parent, parent_sig, sub) ->
      of_option ~error:`Find_failure
        (handle_module_type_lookup env id parent parent_sig sub)
      >>= fun (p', mt) -> Ok (p', mt)
  | `ModuleType (parent, id) ->
      lookup_parent env parent
      |> map_error (fun e -> (e :> simple_module_type_lookup_error))
      >>= fun (parent_sig, sub) ->
      handle_module_type_lookup env
        (ModuleTypeName.to_string id)
        parent parent_sig sub
      |> of_option ~error:`Find_failure
  | `Identifier (i, _) ->
      of_option ~error:(`Lookup_failureMT i)
        (Env.(lookup_by_id s_module_type) i env)
      >>= fun (`ModuleType (_, mt)) ->
      let p = `Gpath (`Identifier i) in
      let p' = process_module_type env mt p in
      Ok (p', mt)
  | `Local (l, _) -> Error (`LocalMT (env, l))
  | `Resolved r -> lookup_module_type env r >>= fun m -> Ok (r, m)
  | `Substituted s ->
      resolve_module_type env s
      |> map_error (fun e -> `Parent (`Parent_module_type e))
      >>= fun (p, m) -> Ok (`Substituted p, m)

and resolve_type : Env.t -> Cpath.type_ -> resolve_type_result =
 fun env p ->
  let result =
    match p with
    | `Dot (parent, id) ->
        resolve_and_lookup_parent env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent, parent_sig, sub) ->
        handle_type_lookup env id parent parent_sig >>= fun (p', t') ->
        let t =
          match t' with
          | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
          | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
          | `FType (name, t) -> `FType (name, Subst.type_ sub t)
          | `FType_removed (name, texpr, eq) ->
              `FType_removed (name, Subst.type_expr sub texpr, eq)
        in
        Ok (p', t)
    | `Type (parent, id) ->
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sig, sub) ->
        let result =
          match Find.datatype_in_sig parent_sig (TypeName.to_string id) with
          | Some (`FType (name, t)) ->
              Some (`Type (parent, name), `FType (name, Subst.type_ sub t))
          | None -> None
        in
        of_option ~error:`Find_failure result
    | `Class (parent, id) ->
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sig, sub) ->
        let t =
          match Find.type_in_sig parent_sig (ClassName.to_string id) with
          | Some (`FClass (name, t)) ->
              Some (`Class (parent, name), `FClass (name, Subst.class_ sub t))
          | Some _ -> None
          | None -> None
        in
        of_option ~error:`Find_failure t
    | `ClassType (parent, id) ->
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sg, sub) ->
        handle_type_lookup env (ClassTypeName.to_string id) parent parent_sg
        >>= fun (p', t') ->
        let t =
          match t' with
          | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
          | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
          | `FType (name, t) -> `FType (name, Subst.type_ sub t)
          | `FType_removed (name, texpr, eq) ->
              `FType_removed (name, Subst.type_expr sub texpr, eq)
        in
        Ok (p', t)
    | `Identifier (i, _) ->
        let i' = `Identifier i in
        lookup_type env (`Gpath i') >>= fun t -> Ok (`Gpath i', t)
    | `Resolved r -> lookup_type env r >>= fun t -> Ok (r, t)
    | `Local (l, _) -> Error (`LocalType (env, l))
    | `Substituted s ->
        resolve_type env s >>= fun (p, m) -> Ok (`Substituted p, m)
  in
  result >>= fun (p, t) ->
  match t with
  | `FType (_, { canonical = Some c; _ }) -> Ok (`CanonicalType (p, c), t)
  | _ -> result

and resolve_value : Env.t -> Cpath.value -> resolve_value_result =
 fun env p ->
  let result =
    match p with
    | `Dot (parent, id) ->
        resolve_module env parent
        |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun (p, m) ->
        let m = Component.Delayed.get m in
        expansion_of_module_cached env p m
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= assert_not_functor
        >>= fun sg ->
        let sub = prefix_substitution (`Module p) sg in
        handle_value_lookup env id (`Module p) sg
        >>= fun (p', `FValue (name, c)) ->
        Ok (p', `FValue (name, Subst.value sub c))
    | `Value (parent, id) ->
        lookup_parent env parent
        |> map_error (fun e -> (e :> simple_value_lookup_error))
        >>= fun (parent_sig, sub) ->
        let result =
          match Find.value_in_sig parent_sig (ValueName.to_string id) with
          | `FValue (name, t) :: _ ->
              Some (`Value (parent, name), `FValue (name, Subst.value sub t))
          | [] -> None
        in
        of_option ~error:`Find_failure result
    | `Resolved r -> lookup_value env r >>= fun t -> Ok (r, t)
    | `Identifier (i, _) ->
        let i' = `Identifier i in
        lookup_value env (`Gpath i') >>= fun t -> Ok (`Gpath i', t)
  in
  result

and resolve_class_type : Env.t -> Cpath.class_type -> resolve_class_type_result
    =
 fun env p ->
  match p with
  | `Dot (parent, id) ->
      resolve_and_lookup_parent env parent
      |> map_error (fun e -> (e :> simple_type_lookup_error))
      >>= fun (parent, parent_sig, sub) ->
      handle_class_type_lookup id parent parent_sig >>= fun (p', t') ->
      let t =
        match t' with
        | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
        | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
        | `FType_removed (name, texpr, eq) ->
            `FType_removed (name, Subst.type_expr sub texpr, eq)
      in
      Ok (p', t)
  | `Identifier (i, _) ->
      let i' = `Identifier i in
      let id = `Gpath i' in
      lookup_class_type env id >>= fun t -> Ok (id, t)
  | `Resolved r -> lookup_class_type env r >>= fun t -> Ok (r, t)
  | `Local (l, _) -> Error (`LocalType (env, (l :> Ident.path_type)))
  | `Substituted s ->
      resolve_class_type env s >>= fun (p, m) -> Ok (`Substituted p, m)
  | `Class (parent, id) ->
      lookup_parent env parent
      |> map_error (fun e -> (e :> simple_type_lookup_error))
      >>= fun (parent_sig, sub) ->
      let t =
        match Find.type_in_sig parent_sig (ClassName.to_string id) with
        | Some (`FClass (name, t)) ->
            Some (`Class (parent, name), `FClass (name, Subst.class_ sub t))
        | Some _ -> None
        | None -> None
      in
      of_option ~error:`Find_failure t
  | `ClassType (parent, id) ->
      lookup_parent env parent
      |> map_error (fun e -> (e :> simple_type_lookup_error))
      >>= fun (parent_sg, sub) ->
      handle_class_type_lookup (ClassTypeName.to_string id) parent parent_sg
      >>= fun (p', t') ->
      let t =
        match t' with
        | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
        | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
        | `FType_removed (name, texpr, eq) ->
            `FType_removed (name, Subst.type_expr sub texpr, eq)
      in
      Ok (p', t)

and reresolve_module_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    Odoc_model.Paths.Path.Resolved.Module.t =
 fun env path ->
  match path with
  | `Identifier _ -> path
  | `Apply (functor_path, argument_path) ->
      `Apply
        ( reresolve_module_gpath env functor_path,
          reresolve_module_gpath env argument_path )
  | `Module (parent, name) -> `Module (reresolve_module_gpath env parent, name)
  | `Alias (p1, p2) ->
      let dest' = reresolve_module_gpath env p1 in
      if
        Odoc_model.Paths.Path.Resolved.Module.is_hidden
          ~weak_canonical_test:false dest'
      then
        let cp2 = Component.Of_Lang.(module_path (empty ()) p2) in
        match resolve_module env cp2 with
        | Ok (`Alias (_, _, Some p3), _) ->
            let p = reresolve_module env p3 in
            Lang_of.(Path.resolved_module (empty ()) p)
        | _ -> `Alias (dest', p2)
      else `Alias (dest', p2)
  | `Subst (p1, p2) ->
      `Subst (reresolve_module_type_gpath env p1, reresolve_module_gpath env p2)
  | `Hidden p ->
      let p' = reresolve_module_gpath env p in
      `Hidden p'
  | `Canonical (p, `Resolved p2) ->
      `Canonical
        (reresolve_module_gpath env p, `Resolved (reresolve_module_gpath env p2))
  | `Canonical (p, p2) ->
      `Canonical (reresolve_module_gpath env p, handle_canonical_module env p2)
  | `OpaqueModule m -> `OpaqueModule (reresolve_module_gpath env m)
  | `Substituted m -> `Substituted (reresolve_module_gpath env m)

and strip_canonical :
    c:Odoc_model.Paths.Path.Module.t ->
    Cpath.Resolved.module_ ->
    Cpath.Resolved.module_ =
 fun ~c path ->
  match path with
  | `Canonical (x, y) when y = c -> strip_canonical ~c x
  | `Canonical (x, y) -> `Canonical (strip_canonical ~c x, y)
  | `Alias (x, y, z) -> `Alias (strip_canonical ~c x, y, z)
  | `Subst (x, y) -> `Subst (x, strip_canonical ~c y)
  | `Hidden x -> `Hidden (strip_canonical ~c x)
  | `OpaqueModule x -> `OpaqueModule (strip_canonical ~c x)
  | `Substituted x -> `Substituted (strip_canonical ~c x)
  | `Gpath p -> `Gpath (strip_canonical_gpath ~c p)
  | `Local _ | `Apply _ | `Module _ -> path

and strip_canonical_gpath :
    c:Odoc_model.Paths.Path.Module.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    Odoc_model.Paths.Path.Resolved.Module.t =
 fun ~c path ->
  match path with
  | `Canonical (x, y) when y = c -> strip_canonical_gpath ~c x
  | `Canonical (x, y) -> `Canonical (strip_canonical_gpath ~c x, y)
  | `Alias (x, y) -> `Alias (strip_canonical_gpath ~c x, y)
  | `Subst (x, y) -> `Subst (x, strip_canonical_gpath ~c y)
  | `Hidden x -> `Hidden (strip_canonical_gpath ~c x)
  | `OpaqueModule x -> `OpaqueModule (strip_canonical_gpath ~c x)
  | `Apply _ | `Module _ | `Identifier _ -> path
  | `Substituted x -> `Substituted (strip_canonical_gpath ~c x)

and reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
    =
 fun env path ->
  match path with
  | `Local _ -> path
  | `Gpath g -> `Gpath (reresolve_module_gpath env g)
  | `Substituted x -> `Substituted (reresolve_module env x)
  | `Apply (functor_path, argument_path) ->
      `Apply
        (reresolve_module env functor_path, reresolve_module env argument_path)
  | `Module (parent, name) -> `Module (reresolve_parent env parent, name)
  | `Alias (p1, p2, p3opt) ->
      let dest' = reresolve_module env p1 in
      if Cpath.is_resolved_module_hidden ~weak_canonical_test:false dest' then
        match p3opt with
        | Some p3 -> reresolve_module env p3
        | None -> (
            match resolve_module env p2 with
            | Ok (`Alias (_, _, Some p3), _) -> reresolve_module env p3
            | _ -> `Alias (dest', p2, None))
      else `Alias (dest', p2, p3opt)
  | `Subst (p1, p2) ->
      `Subst (reresolve_module_type env p1, reresolve_module env p2)
  | `Hidden p ->
      let p' = reresolve_module env p in
      `Hidden p'
  | `Canonical (p, `Resolved p2) ->
      let cp2 = Component.Of_Lang.(resolved_module_path (empty ()) p2) in
      let cp2' = reresolve_module env cp2 in
      let p2' = Lang_of.(Path.resolved_module (empty ()) cp2') in
      `Canonical (reresolve_module env p, `Resolved p2')
  | `Canonical (p, p2) -> (
      match handle_canonical_module env p2 with
      | `Resolved _ as r -> `Canonical (p, r)
      | r -> `Canonical (reresolve_module env p, r))
  | `OpaqueModule m -> `OpaqueModule (reresolve_module env m)

and handle_canonical_module_real env p2 =
  (* Canonical paths are always fully qualified, but this isn't
     necessarily good for rendering, as the full path would
     always be written out whenever a canonical module path is
     encountered.

     Instead the intent of this code is to try to find the shortest
     path that still correctly references the canonical module.

     It works by starting with the fully qualified path, e.g.
     A.B.C.D where A is a root module. It then makes the list
     of possibilities (A).B.C.D (A.B).C.D (A.B.C).D and (A.B.C.D)
     where brackets represent the part that's an identifier.
     It then resolved each one in turn and calculates the
     identifier of the resolved path. The shortest path that
     has the same identifier as the fully-qualified path is
     chosen as the canonical path.

     When doing this, we end up resolving each possibility.
     Additionally, we need to 'reresolve' - resolve the canonical
     references - while we're doing this. This is because the
     parent parts of the resolved path can contain aliases and
     canonical paths themselves which require resolving in order
     to check the identifier is the same.

     However, we first need to strip off any alias/canonical paths
     in the resolved module, as we want the identifier for the
     module itself, not any aliased module, and the canonical path
     _ought_ to be the same as the one we're _currently_ resolving
     anyway, so we'd end up looping forever. Note that the alias
     chain may include modules that have already been resolved and
     hence have canonical constructors in their resolved paths.
  *)

  (* [strip p] strips the top-level aliases and canonical paths from
     the path [p]. Any aliases/canonicals in parents are left as is. *)
  let rec strip : Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
   fun x ->
    match x with
    | `Canonical (x, _) -> strip x
    | `Alias (_, _, Some p) -> strip (strip_canonical ~c:p2 p)
    | _ -> x
  in

  (* Resolve the path, then 'reresolve', making sure to strip off the
     top-level alias and canonicals to avoid looping forever *)
  let resolve env p =
    (* Format.eprintf "Resolve: path=%a\n%!" Component.Fmt.module_path p; *)
    resolve_module env p >>= fun (p, m) ->
    (* Format.eprintf "Resolve: resolved_path=%a\n%!" Component.Fmt.resolved_module_path (strip p); *)
    Ok (reresolve_module env (strip p), m)
  in

  let lang_of cpath =
    (Lang_of.(Path.resolved_module (empty ()) cpath)
      :> Odoc_model.Paths.Path.Resolved.t)
  in

  let cp2 = Component.Of_Lang.(module_path (empty ()) p2) in
  match canonical_helper env resolve lang_of c_mod_poss cp2 with
  | None -> p2
  | Some (rp, m) ->
      let m = Component.Delayed.get m in
      (* Need to check if the module we're going to link to has been expanded.
         ModuleTypes are always expanded if possible, but Aliases are only expanded
         if they're an alias to a hidden module or if they're self canonical.

         Checking if a module is self canonical is a bit tricky, since this function
         is itself part of the process of resolving any canonical reference. Hence
         what we do here is to look through alias chains looking for one that's marked
         with the same _unresolved_ canonical path that we're currently trying to resolve.

         This is particularly important because some modules don't know they're canonical!
         For example the module Caml in base, which is marked as the canonical path for
         all references to the standard library in the file [import0.ml], but is itself just
         defined by including [Stdlib].

         If a module doesn't know it's canonical, it will fail the self-canonical check, and
         therefore not necessarily be expanded. If this happens, we call [process_module_path]
         to stick the [`Alias] constructor back on so we'll link to the correct place. *)
      let expanded =
        match m.type_ with
        | Component.Module.Alias (_, Some _) -> true
        | Alias (p, None) -> (
            (* Format.eprintf "Going to resolve %a\n%!" Component.Fmt.module_path p; *)
            match resolve_module env p with
            | Ok (rp, _) ->
                (* we're an alias - check to see if we're marked as the canonical path.
                   If not, check for an alias chain with us as canonical in it... *)
                let rec check m =
                  match m.Component.Module.canonical with
                  | Some p ->
                      p = p2
                      (* The canonical path is the same one we're trying to resolve *)
                  | None -> (
                      match m.type_ with
                      | Component.Module.Alias (p, _) -> (
                          (* Format.eprintf "Going to resolve %a\n%!" Component.Fmt.module_path p; *)
                          match resolve_module env p with
                          | Ok (rp, _) -> (
                              match lookup_module env rp with
                              | Error _ -> false
                              | Ok m ->
                                  let m = Component.Delayed.get m in
                                  check m)
                          | _ -> false)
                      | _ -> false)
                in
                let self_canonical () = check m in
                let hidden =
                  Cpath.is_resolved_module_hidden ~weak_canonical_test:true
                    (strip rp)
                in
                hidden || self_canonical ()
            | _ -> false)
        | ModuleType _ -> true
      in
      let cpath = if expanded then rp else process_module_path env m rp in

      (* Format.eprintf "result: %a\n%!" Component.Fmt.resolved_module_path cpath; *)
      Lang_of.(Path.module_ (empty ()) (`Resolved cpath))

and handle_canonical_module env p2 =
  HandleCanonicalModuleMemo.memoize handle_canonical_module_real env p2

and handle_canonical_module_type env p2 =
  let cp2 = Component.Of_Lang.(module_type_path (empty ()) p2) in
  let rec strip : Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
    function
    | `AliasModuleType (_, p) -> strip p
    | `CanonicalModuleType (p, _) -> strip p
    | p -> p
  in
  let resolve env p =
    resolve_module_type env p >>= fun (p, m) ->
    (* Note, we reresolve here in case any parent module has a canonical
       constructor to deal with *)
    Ok (reresolve_module_type env (strip p), m)
  in
  let lang_of cpath =
    (Lang_of.(Path.resolved_module_type (empty ()) cpath)
      :> Odoc_model.Paths.Path.Resolved.t)
  in
  match canonical_helper env resolve lang_of c_modty_poss cp2 with
  | None -> p2
  | Some (rp, _) -> `Resolved Lang_of.(Path.resolved_module_type (empty ()) rp)

and handle_canonical_type env p2 =
  let cp2 = Component.Of_Lang.(type_path (empty ()) p2) in
  let lang_of cpath =
    (Lang_of.(Path.resolved_type (empty ()) cpath)
      :> Odoc_model.Paths.Path.Resolved.t)
  in

  let rec strip : Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
   fun x -> match x with `CanonicalType (x, _) -> strip x | _ -> x
  in

  let resolve env p =
    match resolve_type env p with
    | Ok (_, `FType_removed _) -> Error `Find_failure
    | Ok (x, y) ->
        (* See comment in handle_canonical_module_type for why we're reresolving here *)
        let r = reresolve_type env (strip x) in
        Ok (r, y)
    | Error y -> Error y
  in
  match canonical_helper env resolve lang_of c_ty_poss cp2 with
  | None -> p2
  | Some (rp, _) -> `Resolved Lang_of.(Path.resolved_type (empty ()) rp)

and reresolve_module_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t =
 fun env path ->
  match path with
  | `Identifier _ -> path
  | `ModuleType (parent, name) ->
      `ModuleType (reresolve_module_gpath env parent, name)
  | `CanonicalModuleType (p1, (`Resolved _ as p2)) ->
      `CanonicalModuleType (reresolve_module_type_gpath env p1, p2)
  | `CanonicalModuleType (p1, p2) ->
      `CanonicalModuleType
        (reresolve_module_type_gpath env p1, handle_canonical_module_type env p2)
  | `SubstT (p1, p2) ->
      `SubstT
        (reresolve_module_type_gpath env p1, reresolve_module_type_gpath env p2)
  | `AliasModuleType (p1, p2) ->
      `AliasModuleType
        (reresolve_module_type_gpath env p1, reresolve_module_type_gpath env p2)
  | `OpaqueModuleType m -> `OpaqueModuleType (reresolve_module_type_gpath env m)
  | `SubstitutedMT m -> `SubstitutedMT (reresolve_module_type_gpath env m)

and reresolve_module_type :
    Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun env path ->
  match path with
  | `Local _ -> path
  | `Gpath g -> `Gpath (reresolve_module_type_gpath env g)
  | `Substituted x -> `Substituted (reresolve_module_type env x)
  | `ModuleType (parent, name) -> `ModuleType (reresolve_parent env parent, name)
  | `CanonicalModuleType (p1, (`Resolved _ as p2')) ->
      `CanonicalModuleType (reresolve_module_type env p1, p2')
  | `CanonicalModuleType (p1, p2) ->
      `CanonicalModuleType
        (reresolve_module_type env p1, handle_canonical_module_type env p2)
  | `SubstT (p1, p2) ->
      `SubstT (reresolve_module_type env p1, reresolve_module_type env p2)
  | `AliasModuleType (p1, p2) ->
      `AliasModuleType
        (reresolve_module_type env p1, reresolve_module_type env p2)
  | `OpaqueModuleType m -> `OpaqueModuleType (reresolve_module_type env m)

and reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun env path ->
  let result =
    match path with
    | `Gpath _ | `Local _ -> path
    | `Substituted s -> `Substituted (reresolve_type env s)
    | `CanonicalType (p1, p2) ->
        `CanonicalType (reresolve_type env p1, handle_canonical_type env p2)
    | `Type (p, n) -> `Type (reresolve_parent env p, n)
    | `Class (p, n) -> `Class (reresolve_parent env p, n)
    | `ClassType (p, n) -> `ClassType (reresolve_parent env p, n)
  in
  result

and reresolve_value : Env.t -> Cpath.Resolved.value -> Cpath.Resolved.value =
 fun env p ->
  match p with
  | `Value (p, n) -> `Value (reresolve_parent env p, n)
  | `Gpath _ -> p

and reresolve_class_type :
    Env.t -> Cpath.Resolved.class_type -> Cpath.Resolved.class_type =
 fun env path ->
  let result =
    match path with
    | `Gpath _ | `Local _ -> path
    | `Substituted s -> `Substituted (reresolve_class_type env s)
    | `Class (p, n) -> `Class (reresolve_parent env p, n)
    | `ClassType (p, n) -> `ClassType (reresolve_parent env p, n)
  in
  result

and reresolve_parent : Env.t -> Cpath.Resolved.parent -> Cpath.Resolved.parent =
 fun env path ->
  match path with
  | `Module m -> `Module (reresolve_module env m)
  | `ModuleType mty -> `ModuleType (reresolve_module_type env mty)
  | `FragmentRoot -> path

(* *)
and module_type_expr_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    ( Component.ModuleType.expr,
      simple_module_type_expr_of_module_error )
    Result.result =
 fun env decl ->
  match decl with
  | Component.Module.Alias (`Resolved r, _) ->
      lookup_module env r |> map_error (fun e -> `Parent (`Parent_module e))
      >>= fun m ->
      let m = Component.Delayed.get m in
      module_type_expr_of_module_decl env m.type_
  | Component.Module.Alias (path, _) -> (
      match resolve_module env path with
      | Ok (_, m) ->
          let m = Component.Delayed.get m in
          module_type_expr_of_module env m
      | Error _ when Cpath.is_module_forward path ->
          Error `UnresolvedForwardPath
      | Error e -> Error (`UnresolvedPath (`Module (path, e))))
  | Component.Module.ModuleType expr -> Ok expr

and module_type_expr_of_module :
    Env.t ->
    Component.Module.t ->
    ( Component.ModuleType.expr,
      simple_module_type_expr_of_module_error )
    Result.result =
 fun env m -> module_type_expr_of_module_decl env m.type_

and expansion_of_module_path :
    Env.t ->
    strengthen:bool ->
    Cpath.module_ ->
    (expansion, expansion_of_module_error) Result.result =
 fun env ~strengthen path ->
  match resolve_module env path with
  | Ok (p', m) -> (
      let m = Component.Delayed.get m in
      (* p' is the path to the aliased module *)
      let strengthen =
        strengthen
        && not (Cpath.is_resolved_module_hidden ~weak_canonical_test:true p')
      in
      expansion_of_module_cached env p' m >>= function
      | Signature sg ->
          let sg =
            if strengthen then Strengthen.signature (`Resolved p') sg else sg
          in
          Ok (Signature sg)
      | Functor _ as f -> Ok f)
  | Error _ when Cpath.is_module_forward path -> Error `UnresolvedForwardPath
  | Error e -> Error (`UnresolvedPath (`Module (path, e)))

and handle_signature_with_subs :
    Env.t ->
    Component.Signature.t ->
    Component.ModuleType.substitution list ->
    (Component.Signature.t, expansion_of_module_error) Result.result =
 fun env sg subs ->
  let open Odoc_utils.ResultMonad in
  List.fold_left
    (fun sg_opt sub -> sg_opt >>= fun sg -> fragmap env sub sg)
    (Ok sg) subs

and assert_not_functor :
    type err. expansion -> (Component.Signature.t, err) Result.result = function
  | Signature sg -> Ok sg
  | _ -> assert false

and unresolve_subs subs =
  List.map
    (function
      | Component.ModuleType.ModuleEq (`Resolved f, m) ->
          Component.ModuleType.ModuleEq (Cfrag.unresolve_module f, m)
      | ModuleSubst (`Resolved f, m) -> ModuleSubst (Cfrag.unresolve_module f, m)
      | TypeEq (`Resolved f, t) -> TypeEq (Cfrag.unresolve_type f, t)
      | TypeSubst (`Resolved f, t) -> TypeSubst (Cfrag.unresolve_type f, t)
      | x -> x)
    subs

and signature_of_module_type_of :
    Env.t ->
    Component.ModuleType.type_of_desc ->
    original_path:Cpath.module_ ->
    (expansion, expansion_of_module_error) Result.result =
 fun env desc ~original_path:_ ->
  let p, strengthen =
    match desc with ModPath p -> (p, false) | StructInclude p -> (p, true)
  in
  (*
  match Cpath.original_path_cpath original_path with
  | None -> Error (`UnresolvedOriginalPath (original_path, `Find_failure))
  | Some cp ->
  expansion_of_module_path env ~strengthen p >>= fun exp ->
  match expansion_of_module_path env ~strengthen:false cp with
  | Ok _orig_exp -> Ok exp
    | Error _lookup_error ->
    Ok (exp)
  *)
  expansion_of_module_path env ~strengthen p

and signature_of_u_module_type_expr :
    Env.t ->
    Component.ModuleType.U.expr ->
    (Component.Signature.t, expansion_of_module_error) Result.result =
 fun env m ->
  match m with
  | Component.ModuleType.U.Path p -> (
      match resolve_module_type env p with
      | Ok (_, mt) -> expansion_of_module_type env mt >>= assert_not_functor
      | Error e -> Error (`UnresolvedPath (`ModuleType (p, e))))
  | Signature s -> Ok s
  | With (subs, s) ->
      signature_of_u_module_type_expr env s >>= fun sg ->
      let subs = unresolve_subs subs in
      handle_signature_with_subs env sg subs
  | TypeOf (desc, original_path) ->
      signature_of_module_type_of env desc ~original_path >>= assert_not_functor

and expansion_of_simple_expansion :
    Component.ModuleType.simple_expansion -> expansion =
  let rec helper :
      Component.ModuleType.simple_expansion -> Component.ModuleType.expr =
    function
    | Signature sg -> Signature sg
    | Functor (arg, e) -> Functor (arg, helper e)
  in
  function
  | Signature sg -> Signature sg | Functor (arg, e) -> Functor (arg, helper e)

and expansion_of_module_type_expr :
    Env.t ->
    Component.ModuleType.expr ->
    (expansion, expansion_of_module_error) Result.result =
 fun env m ->
  match m with
  | Component.ModuleType.Path { p_expansion = Some e; _ } ->
      Ok (expansion_of_simple_expansion e)
  | Component.ModuleType.Path { p_path; _ } -> (
      match resolve_module_type env p_path with
      | Ok (_, mt) -> expansion_of_module_type env mt
      | Error e -> Error (`UnresolvedPath (`ModuleType (p_path, e))))
  | Component.ModuleType.Signature s -> Ok (Signature s)
  | Component.ModuleType.With { w_expansion = Some e; _ } ->
      Ok (expansion_of_simple_expansion e)
  | Component.ModuleType.With { w_substitutions; w_expr; _ } ->
      signature_of_u_module_type_expr env w_expr >>= fun sg ->
      let subs = unresolve_subs w_substitutions in
      handle_signature_with_subs env sg subs >>= fun sg -> Ok (Signature sg)
  | Component.ModuleType.Functor (arg, expr) -> Ok (Functor (arg, expr))
  | Component.ModuleType.TypeOf { t_expansion = Some (Signature sg); _ } ->
      Ok (Signature sg)
  | Component.ModuleType.TypeOf { t_desc; _ } ->
      let p, strengthen =
        match t_desc with
        | ModPath p -> (p, false)
        | StructInclude p -> (p, true)
      in
      expansion_of_module_path env ~strengthen p

and expansion_of_module_type :
    Env.t ->
    Component.ModuleType.t ->
    (expansion, expansion_of_module_error) Result.result =
 fun env m ->
  match m.expr with
  | None -> Error `OpaqueModule
  | Some expr -> expansion_of_module_type_expr env expr

and expansion_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    (expansion, expansion_of_module_error) Result.result =
 fun env decl ->
  match decl with
  (* | Component.Module.Alias (_, Some e) -> Ok (expansion_of_simple_expansion e) *)
  | Component.Module.Alias (p, _) ->
      expansion_of_module_path env ~strengthen:true p
  | Component.Module.ModuleType expr -> expansion_of_module_type_expr env expr

and expansion_of_module :
    Env.t ->
    Component.Module.t ->
    (expansion, expansion_of_module_error) Result.result =
 fun env m ->
  expansion_of_module_decl env m.type_ >>= function
  | Signature sg ->
      let sg =
        (* Override the signature's documentation when the module also has
           a comment attached. *)
        match m.doc with [] -> sg | doc -> { sg with doc }
      in
      Ok (Signature sg)
  | Functor _ as f -> Ok f

and expansion_of_module_cached :
    Env.t ->
    Cpath.Resolved.module_ ->
    Component.Module.t ->
    (expansion, expansion_of_module_error) Result.result =
 fun env' path m ->
  let id = path in
  let run env _id = expansion_of_module env m in
  ExpansionOfModuleMemo.memoize run env' id

and _ascribe _env ~expansion ~original_expansion =
  match (expansion, original_expansion) with
  | Functor _, _ -> expansion
  | Signature sg, Signature sg' ->
      let items =
        List.fold_right
          (fun item acc ->
            match item with
            | Component.Signature.Module (id, _r, _m) ->
                if Find.module_in_sig sg' (Ident.Name.module_ id) = None then
                  acc
                else item :: acc
            | Component.Signature.ModuleType (id, _m) ->
                if
                  Find.module_type_in_sig sg' (Ident.Name.module_type id) = None
                then acc
                else item :: acc
            | Component.Signature.Type (id, _r, _t) ->
                if Find.type_in_sig sg' (Ident.Name.type_ id) = None then acc
                else item :: acc
            | Component.Signature.Value (id, _v) ->
                if Find.value_in_sig sg' (Ident.Name.value id) = [] then acc
                else item :: acc
            | _ -> item :: acc)
          sg.items []
      in
      Signature { sg with items }
  | _ -> expansion

and umty_of_mty : Component.ModuleType.expr -> Component.ModuleType.U.expr =
  function
  | Signature sg -> Signature sg
  | Path { p_path; _ } -> Path p_path
  | TypeOf { t_desc; t_original_path; _ } -> TypeOf (t_desc, t_original_path)
  | With { w_substitutions; w_expr; _ } -> With (w_substitutions, w_expr)
  | Functor _ -> assert false

and fragmap :
    Env.t ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    (Component.Signature.t, expansion_of_module_error) Result.result =
 fun env sub sg ->
  (* Used when we haven't finished the substitution. For example, if the
     substitution is `M.t = u`, this function is used to map the declaration
     of `M` to be `M : ... with type t = u` *)
  let map_module_decl decl subst =
    let open Component.Module in
    match decl with
    | Alias (path, _) ->
        Ok
          (ModuleType
             (With
                {
                  w_substitutions = [ subst ];
                  w_expansion = None;
                  w_expr =
                    TypeOf (StructInclude path, Cpath.unresolve_module_path path);
                }))
    | ModuleType mty' ->
        Ok
          (ModuleType
             (With
                {
                  w_substitutions = [ subst ];
                  w_expansion = None;
                  w_expr = umty_of_mty mty';
                }))
  in
  let map_include_decl decl subst =
    let open Component.Include in
    match decl with
    | Alias p ->
        expansion_of_module_path env ~strengthen:true p >>= assert_not_functor
        >>= fun sg ->
        fragmap env subst sg >>= fun sg -> Ok (ModuleType (Signature sg))
    | ModuleType mty' -> Ok (ModuleType (With ([ subst ], mty')))
  in
  let map_module m new_subst =
    let open Component.Module in
    map_module_decl m.type_ new_subst >>= fun type_ ->
    Ok (Left { m with type_ })
  in
  let map_removed = function
    | `RModule (id, p) ->
        Component.Signature.RModule (Ident.Name.typed_module id, `Resolved p)
    | `RType (id, x, y) -> RType (Ident.Name.typed_type id, x, y)
    | `RModuleType (id, x) -> RModuleType (Ident.Name.typed_module_type id, x)
  in

  let rec map_signature map items =
    List.fold_right
      (fun item acc ->
        acc >>= fun (items, handled, subbed_modules, removed) ->
        match (item, map) with
        | Component.Signature.Type (id, r, t), { type_ = Some (id', fn); _ }
          when Ident.Name.type_ id = id' -> (
            fn (Component.Delayed.get t) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.Type
                      (id, r, Component.Delayed.put (fun () -> x))
                    :: items,
                    true,
                    subbed_modules,
                    removed )
            | Right (texpr, eq) ->
                Ok
                  ( items,
                    true,
                    subbed_modules,
                    `RType (id, texpr, eq) :: removed ))
        | Component.Signature.Module (id, r, m), { module_ = Some (id', fn); _ }
          when Ident.Name.module_ id = id' -> (
            fn (Component.Delayed.get m) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.Module
                      (id, r, Component.Delayed.put (fun () -> x))
                    :: items,
                    true,
                    id :: subbed_modules,
                    removed )
            | Right y ->
                Ok (items, true, subbed_modules, `RModule (id, y) :: removed))
        | Component.Signature.Include ({ expansion_; _ } as i), _ ->
            map_signature map expansion_.items
            >>= fun (items', handled', subbed_modules', removed') ->
            let component =
              if handled' then
                map_include_decl i.decl sub >>= fun decl ->
                let expansion_ =
                  Component.Signature.
                    {
                      expansion_ with
                      items = items';
                      removed = List.map map_removed removed';
                      compiled = false;
                    }
                in
                Ok
                  (Component.Signature.Include
                     { i with decl; expansion_; strengthened = None })
              else Ok item
            in
            component >>= fun c ->
            Ok
              ( c :: items,
                handled' || handled,
                subbed_modules' @ subbed_modules,
                removed' @ removed )
        | ( Component.Signature.ModuleType (id, mt),
            { module_type = Some (id', fn); _ } )
          when Ident.Name.module_type id = id' -> (
            fn (Component.Delayed.get mt) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.ModuleType
                      (id, Component.Delayed.put (fun () -> x))
                    :: items,
                    true,
                    subbed_modules,
                    removed )
            | Right y ->
                Ok (items, true, subbed_modules, `RModuleType (id, y) :: removed)
            )
        | x, _ -> Ok (x :: items, handled, subbed_modules, removed))
      items
      (Ok ([], false, [], []))
  in
  let handle_intermediate name new_subst =
    let modmaps = Some (name, fun m -> map_module m new_subst) in
    map_signature { id_map with module_ = modmaps } sg.items
  in
  let new_sg =
    match sub with
    | ModuleEq (frag, type_) -> (
        match Cfrag.module_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleEq (frag', type_) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn m =
              let type_ =
                let open Component.Module in
                match type_ with
                | Alias (`Resolved p, _) ->
                    let new_p = `Substituted p in
                    Alias (`Resolved new_p, None)
                | Alias _ | ModuleType _ -> type_
              in
              Ok (Left { m with Component.Module.type_ })
            in
            map_signature { id_map with module_ = Some (name, mapfn) } sg.items)
    | ModuleSubst (frag, p) -> (
        match Cfrag.module_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleSubst (frag', p) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _ =
              match resolve_module env p with
              | Ok (`Canonical (p, _), _) | Ok (p, _) -> Ok (Right p)
              | Error e -> Error (`UnresolvedPath (`Module (p, e)))
            in
            map_signature { id_map with module_ = Some (name, mapfn) } sg.items)
    | ModuleTypeEq (frag, mtye) -> (
        match Cfrag.module_type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleTypeEq (frag', mtye) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn t =
              Ok (Left { t with Component.ModuleType.expr = Some mtye })
            in
            map_signature
              { id_map with module_type = Some (name, mapfn) }
              sg.items)
    | ModuleTypeSubst (frag, mtye) -> (
        match Cfrag.module_type_split frag with
        | name, Some frag' ->
            let new_subst =
              Component.ModuleType.ModuleTypeSubst (frag', mtye)
            in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _t = Ok (Right mtye) in
            map_signature
              { id_map with module_type = Some (name, mapfn) }
              sg.items)
    | TypeEq (frag, equation) -> (
        match Cfrag.type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.TypeEq (frag', equation) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn t = Ok (Left { t with Component.TypeDecl.equation }) in
            map_signature { id_map with type_ = Some (name, mapfn) } sg.items)
    | TypeSubst
        ( frag,
          ({ Component.TypeDecl.Equation.manifest = Some x; _ } as equation) )
      -> (
        match Cfrag.type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.TypeSubst (frag', equation) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _t = Ok (Right (x, equation)) in
            map_signature { id_map with type_ = Some (name, mapfn) } sg.items)
    | TypeSubst (_, { Component.TypeDecl.Equation.manifest = None; _ }) ->
        failwith "Unhandled condition: TypeSubst with no manifest"
  in
  new_sg >>= fun (items, _handled, subbed_modules, removed) ->
  let sub_of_removed removed sub =
    match removed with
    | `RModule (id, p) ->
        Subst.add_module (id :> Ident.path_module) (`Resolved p) p sub
    | `RType (id, r_texpr, r_eq) ->
        Subst.add_type_replacement (id :> Ident.path_type) r_texpr r_eq sub
    | `RModuleType (id, e) ->
        Subst.add_module_type_replacement (id :> Ident.module_type) e sub
  in

  let sub = List.fold_right sub_of_removed removed Subst.identity in

  (* Invalidate resolved paths containing substituted idents - See the `With11`
     test for an example of why this is necessary *)
  let sub_of_substituted x sub =
    let x = (x :> Ident.path_module) in
    Subst.add_module_substitution x sub |> Subst.path_invalidate_module x
  in

  let substituted_sub =
    List.fold_right sub_of_substituted subbed_modules Subst.identity
  in

  (* Need to call `apply_sig_map_items` directly as we're substituting for an item
     that's declared within the signature *)
  let items = Subst.apply_sig_map_items substituted_sub items in

  let res =
    Subst.signature sub
      {
        Component.Signature.items;
        removed = List.map map_removed removed @ sg.removed;
        compiled = false;
        doc = sg.doc;
      }
  in
  Ok res

and find_external_module_path :
    Cpath.Resolved.module_ -> Cpath.Resolved.module_ option =
 fun p ->
  let open Odoc_utils.OptionMonad in
  match p with
  | `Subst (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (`Subst (x, y))
  | `Module (p, n) ->
      find_external_parent_path p >>= fun p -> Some (`Module (p, n))
  | `Local x -> Some (`Local x)
  | `Substituted x ->
      find_external_module_path x >>= fun x -> Some (`Substituted x)
  | `Canonical (x, y) ->
      find_external_module_path x >>= fun x -> Some (`Canonical (x, y))
  | `Hidden x -> find_external_module_path x >>= fun x -> Some (`Hidden x)
  | `Alias _ -> None
  | `Apply (x, y) ->
      find_external_module_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (`Apply (x, y))
  | `Gpath x -> Some (`Gpath x)
  | `OpaqueModule m ->
      find_external_module_path m >>= fun x -> Some (`OpaqueModule x)

and find_external_module_type_path :
    Cpath.Resolved.module_type -> Cpath.Resolved.module_type option =
 fun p ->
  let open Odoc_utils.OptionMonad in
  match p with
  | `ModuleType (p, name) ->
      find_external_parent_path p >>= fun p -> Some (`ModuleType (p, name))
  | `Local _ -> Some p
  | `SubstT (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_type_path y >>= fun y -> Some (`SubstT (x, y))
  | `CanonicalModuleType (x, _) | `Substituted x ->
      find_external_module_type_path x >>= fun x -> Some (`Substituted x)
  | `Gpath _ -> Some p
  | `AliasModuleType (x, y) -> (
      match
        (find_external_module_type_path x, find_external_module_type_path y)
      with
      | Some x, Some y -> Some (`AliasModuleType (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None)
  | `OpaqueModuleType m ->
      find_external_module_type_path m >>= fun x -> Some (`OpaqueModuleType x)

and find_external_parent_path :
    Cpath.Resolved.parent -> Cpath.Resolved.parent option =
 fun p ->
  let open Odoc_utils.OptionMonad in
  match p with
  | `Module m -> find_external_module_path m >>= fun m -> Some (`Module m)
  | `ModuleType m ->
      find_external_module_type_path m >>= fun m -> Some (`ModuleType m)
  | `FragmentRoot -> None

and fixup_module_cfrag (f : Cfrag.resolved_module) : Cfrag.resolved_module =
  match f with
  | `Subst (path, frag) -> (
      match find_external_module_type_path path with
      | Some p -> `Subst (p, frag)
      | None -> frag)
  | `Alias (path, frag) -> (
      match find_external_module_path path with
      | Some p -> `Alias (p, frag)
      | None -> frag)
  | `Module (parent, name) -> `Module (fixup_signature_cfrag parent, name)
  | `OpaqueModule m -> `OpaqueModule (fixup_module_cfrag m)

and fixup_module_type_cfrag (f : Cfrag.resolved_module_type) :
    Cfrag.resolved_module_type =
  match f with
  | `ModuleType (parent, name) ->
      `ModuleType (fixup_signature_cfrag parent, name)

and fixup_signature_cfrag (f : Cfrag.resolved_signature) =
  match f with
  | `Root x -> `Root x
  | (`OpaqueModule _ | `Subst _ | `Alias _ | `Module _) as f ->
      (fixup_module_cfrag f :> Cfrag.resolved_signature)

and fixup_type_cfrag (f : Cfrag.resolved_type) : Cfrag.resolved_type =
  match f with
  | `Type (p, x) -> `Type (fixup_signature_cfrag p, x)
  | `Class (p, x) -> `Class (fixup_signature_cfrag p, x)
  | `ClassType (p, x) -> `ClassType (fixup_signature_cfrag p, x)

and find_module_with_replacement :
    Env.t ->
    Component.Signature.t ->
    string ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun env sg name ->
  match Find.careful_module_in_sig sg name with
  | Some (`FModule (_, m)) -> Ok (Component.Delayed.put_val m)
  | Some (`FModule_removed path) ->
      resolve_module env path >>= fun (_, m) -> Ok m
  | None -> Error `Find_failure

and find_module_type_with_replacement :
    Env.t ->
    Component.Signature.t ->
    string ->
    ( Component.ModuleType.t Component.Delayed.t,
      simple_module_type_lookup_error )
    Result.result =
 fun _env sg name ->
  match Find.careful_module_type_in_sig sg name with
  | Some (`FModuleType (_, m)) -> Ok (Component.Delayed.put_val m)
  | None -> Error `Find_failure
  | Some (`FModuleType_removed _mty) -> Error `Find_failure

and resolve_signature_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.signature ->
    (Cfrag.resolved_signature * Cpath.Resolved.parent * Component.Signature.t)
    option =
 fun env (p, sg) frag ->
  match frag with
  | `Root ->
      let sg = prefix_signature (`FragmentRoot, sg) in
      Some (`Root p, `FragmentRoot, sg)
  | `Resolved _r -> None
  | `Dot (parent, name) ->
      let open Odoc_utils.OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = ModuleName.make_std name in
      let new_path = `Module (ppath, mname) in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.Delayed.get m' in
      let modifier = get_module_path_modifiers env m' in
      let cp', f' =
        match modifier with
        | None -> (new_path, new_frag)
        | Some (`Aliased (`Canonical (p', _))) | Some (`Aliased p') ->
            (`Alias (p', `Resolved new_path, None), `Alias (p', new_frag))
        | Some (`SubstMT p') -> (`Subst (p', new_path), `Subst (p', new_frag))
      in
      (* Don't use the cached one - `FragmentRoot` is not unique *)
      of_result
        Odoc_utils.ResultMonad.(
          expansion_of_module env m' >>= assert_not_functor)
      >>= fun parent_sg ->
      let sg = prefix_signature (`Module cp', parent_sg) in
      Some (f', `Module cp', sg)

and resolve_module_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_ ->
    Cfrag.resolved_module option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open Odoc_utils.OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = ModuleName.make_std name in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.Delayed.get m' in
      let modifier = get_module_path_modifiers env m' in
      let f' =
        match modifier with
        | None -> new_frag
        | Some (`Aliased (`Canonical (p', _))) | Some (`Aliased p') ->
            `Alias (p', new_frag)
        | Some (`SubstMT p') -> `Subst (p', new_frag)
      in
      let f'' =
        match expansion_of_module env m' with
        | Ok (_m : expansion) -> f'
        | Error `OpaqueModule -> `OpaqueModule f'
        | Error
            ( `UnresolvedForwardPath | `UnresolvedPath _
            | `UnresolvedOriginalPath _ ) ->
            f'
      in
      Some (fixup_module_cfrag f'')

and resolve_module_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_type ->
    Cfrag.resolved_module_type option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open Odoc_utils.OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, sg) ->
      of_result (find_module_type_with_replacement env sg name) >>= fun mt' ->
      let mtname = ModuleTypeName.make_std name in
      let f' = `ModuleType (pfrag, mtname) in
      let m' = Component.Delayed.get mt' in
      let f'' =
        match expansion_of_module_type env m' with
        | Ok (_m : expansion) -> f'
        | Error
            ( `UnresolvedForwardPath | `UnresolvedPath _ | `OpaqueModule
            | `UnresolvedOriginalPath _ ) ->
            f'
      in
      Some (fixup_module_type_cfrag f'')

and resolve_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.type_ ->
    Cfrag.resolved_type option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open Odoc_utils.OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, _sg) ->
      let result = fixup_type_cfrag (`Type (pfrag, TypeName.make_std name)) in
      Some result

let rec reresolve_signature_fragment :
    Env.t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
 fun env m ->
  match m with
  | `Root (`ModuleType p) -> `Root (`ModuleType (reresolve_module_type env p))
  | `Root (`Module p) -> `Root (`Module (reresolve_module env p))
  | (`OpaqueModule _ | `Subst _ | `Alias _ | `Module _) as x ->
      (reresolve_module_fragment env x :> Cfrag.resolved_signature)

and reresolve_module_fragment :
    Env.t -> Cfrag.resolved_module -> Cfrag.resolved_module =
 fun env m ->
  match m with
  | `Subst (p, f) ->
      let p' = reresolve_module_type env p in
      `Subst (p', reresolve_module_fragment env f)
  | `Alias (p, f) ->
      let p' = reresolve_module env p in
      `Alias (p', reresolve_module_fragment env f)
  | `OpaqueModule m -> `OpaqueModule (reresolve_module_fragment env m)
  | `Module (sg, m) -> `Module (reresolve_signature_fragment env sg, m)

and reresolve_type_fragment :
    Env.t -> Cfrag.resolved_type -> Cfrag.resolved_type =
 fun env m ->
  match m with
  | `Type (p, n) -> `Type (reresolve_signature_fragment env p, n)
  | `ClassType (p, n) -> `ClassType (reresolve_signature_fragment env p, n)
  | `Class (p, n) -> `Class (reresolve_signature_fragment env p, n)

and reresolve_module_type_fragment :
    Env.t -> Cfrag.resolved_module_type -> Cfrag.resolved_module_type =
 fun env m ->
  match m with
  | `ModuleType (p, n) -> `ModuleType (reresolve_signature_fragment env p, n)

let rec class_signature_of_class :
    Env.t -> Component.Class.t -> Component.ClassSignature.t option =
 fun env c ->
  let rec inner decl =
    match decl with
    | Component.Class.ClassType e -> class_signature_of_class_type_expr env e
    | Arrow (_, _, d) -> inner d
  in
  inner c.type_

and class_signature_of_class_type_expr :
    Env.t -> Component.ClassType.expr -> Component.ClassSignature.t option =
 fun env e ->
  match e with
  | Signature s -> Some s
  | Constr (p, _) -> (
      match resolve_type env (p :> Cpath.type_) with
      | Ok (_, `FClass (_, c)) -> class_signature_of_class env c
      | Ok (_, `FClassType (_, c)) -> class_signature_of_class_type env c
      | _ -> None)

and class_signature_of_class_type :
    Env.t -> Component.ClassType.t -> Component.ClassSignature.t option =
 fun env c -> class_signature_of_class_type_expr env c.expr

let resolve_module_path env p =
  resolve_module env p >>= fun (p, m) ->
  match p with
  | `Gpath (`Identifier { iv = `Root _; _ })
  | `Hidden (`Gpath (`Identifier { iv = `Root _; _ })) ->
      Ok p
  | _ -> (
      let m = Component.Delayed.get m in
      match expansion_of_module_cached env p m with
      | Ok _ -> Ok p
      | Error `OpaqueModule -> Ok (`OpaqueModule p)
      | Error
          ( `UnresolvedForwardPath | `UnresolvedPath _
          | `UnresolvedOriginalPath _ ) ->
          Ok p)

let resolve_module_type_path env p =
  resolve_module_type env p >>= fun (p, mt) ->
  match expansion_of_module_type env mt with
  | Ok _ -> Ok p
  | Error `OpaqueModule -> Ok (`OpaqueModuleType p)
  | Error
      (`UnresolvedForwardPath | `UnresolvedPath _ | `UnresolvedOriginalPath _)
    ->
      Ok p

let resolve_type_path env p = resolve_type env p >>= fun (p, _) -> Ok p

let resolve_value_path env p = resolve_value env p >>= fun (p, _) -> Ok p

let resolve_class_type_path env p =
  resolve_class_type env p >>= fun (p, _) -> Ok p
