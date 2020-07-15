open Odoc_model.Names

(* Add [result] and a bind operator over it in scope *)
open Utils
open ResultMonad

exception ModuleLookup of Cpath.module_

type ('a, 'b) either = Left of 'a | Right of 'b

type module_modifiers =
  [ `Aliased of Cpath.Resolved.module_
  | `SubstAliased of Cpath.Resolved.module_
  | `SubstMT of Cpath.Resolved.module_type ]

module Fmt = struct
  let rec error : Format.formatter -> Errors.any -> unit =
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
    | `LocalMT (_, id) ->
        Format.fprintf fmt "Local id found: %a"
          Component.Fmt.resolved_module_type_path id
    | `Local (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `LocalType (_, id) ->
        Format.fprintf fmt "Local id found: %a" Component.Fmt.resolved_type_path
          id
    | `Unresolved_apply -> Format.fprintf fmt "Unresolved apply"
    | `Find_failure -> Format.fprintf fmt "Find failure"
    | `Lookup_failure m ->
        Format.fprintf fmt "Lookup failure (module): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureMT m ->
        Format.fprintf fmt "Lookup failure (module type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureT m ->
        Format.fprintf fmt "Lookup failure (type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `ApplyNotFunctor -> Format.fprintf fmt "Apply module is not a functor"
    | `Parent_sig e ->
        Format.fprintf fmt "Parent_sig: %a" error (e :> Errors.any)
    | `Parent_module_type e ->
        Format.fprintf fmt "Parent_module_type: %a" error (e :> Errors.any)
    | `Parent_expr e ->
        Format.fprintf fmt "Parent_expr: %a" error (e :> Errors.any)
    | `Parent_module e ->
        Format.fprintf fmt "Parent_module: %a" error (e :> Errors.any)
    | `Fragment_root -> Format.fprintf fmt "Fragment root"
    | `Class_replaced -> Format.fprintf fmt "Class replaced"
end

module ResolvedMonad = struct
  type ('a, 'b) t = Resolved of 'a | Unresolved of 'b

  let return x = Resolved x

  let bind m f = match m with Resolved x -> f x | Unresolved y -> Unresolved y

  let ( >>= ) = bind

  let map_unresolved f m =
    match m with Resolved x -> Resolved x | Unresolved y -> Unresolved (f y)

  let of_result ~unresolved = function
    | Ok x -> Resolved x
    | Error _ -> Unresolved unresolved

  let of_option ~unresolved = function
    | Some x -> Resolved x
    | None -> Unresolved unresolved
end

let core_types =
  let open Odoc_model.Lang.TypeDecl in
  let open Odoc_model.Paths in
  List.map
    (fun decl ->
      (Identifier.name decl.id, Component.Of_Lang.(type_decl empty decl)))
    Odoc_model.Predefined.core_types

let prefix_substitution path sg =
  let open Component.Signature in
  let rec get_sub sub' is =
    match is with
    | [] -> sub'
    | Type (id, _, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub (Subst.add_type id (`Type (path, name)) sub') rest
    | Module (id, _, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (`Module (path, name))
             sub')
          rest
    | ModuleType (id, _) :: rest ->
        let name = Ident.Name.typed_module_type id in
        get_sub (Subst.add_module_type id (`ModuleType (path, name)) sub') rest
    | ModuleSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (`Module (path, name))
             sub')
          rest
    | TypeSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub (Subst.add_type id (`Type (path, name)) sub') rest
    | Exception _ :: rest
    | TypExt _ :: rest
    | Value (_, _) :: rest
    | External (_, _) :: rest
    | Comment _ :: rest ->
        get_sub sub' rest
    | Class (id, _, _) :: rest ->
        let name = Ident.Name.typed_class id in
        get_sub (Subst.add_class id (`Class (path, name)) sub') rest
    | ClassType (id, _, _) :: rest ->
        let name = Ident.Name.typed_class_type id in
        get_sub (Subst.add_class_type id (`ClassType (path, name)) sub') rest
    | Include i :: rest -> get_sub (get_sub sub' i.expansion_.items) rest
    | Open o :: rest -> get_sub (get_sub sub' o.expansion.items) rest
  in
  let extend_sub_removed removed sub =
    List.fold_right
      (fun item map ->
        match item with
        | Component.Signature.RModule (id, _) ->
            let name = Ident.Name.typed_module id in
            Subst.add_module
              (id :> Ident.path_module)
              (`Module (path, name))
              map
        | Component.Signature.RType (id, _) ->
            let name = Ident.Name.typed_type id in
            Subst.add_type id (`Type (path, name)) map)
      removed sub
  in
  get_sub Subst.identity sg.items |> extend_sub_removed sg.removed

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
        | Exception (id, e) -> Exception (id, Subst.exception_ sub e)
        | TypExt t -> TypExt (Subst.extension sub t)
        | Value (id, v) ->
            Value
              ( id,
                Component.Delayed.put (fun () ->
                    Subst.value sub (Component.Delayed.get v)) )
        | External (id, e) -> External (id, Subst.external_ sub e)
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i -> Include (Subst.include_ sub i)
        | Open o -> Open (Subst.open_ sub o)
        | Comment c -> Comment c)
      sg.items
  in
  { items; removed = sg.removed }

let simplify_resolved_module_path :
    Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
 fun env cpath ->
  let path = Lang_of.(Path.resolved_module empty cpath) in
  let id = Odoc_model.Paths.Path.Resolved.Module.identifier path in
  let rec check_ident id =
    match Env.(lookup_by_id s_module) id env with
    | Some _ -> `Identifier id
    | None -> (
        match id with
        | `Module ((#Odoc_model.Paths_types.Identifier.module_ as parent), name)
          ->
            `Module (`Module (check_ident parent), name)
        | _ -> failwith "Bad canonical path" )
  in
  check_ident id

type resolve_module_result =
  ( Cpath.Resolved.module_ * Component.Module.t Component.Delayed.t,
    Cpath.module_ )
  ResolvedMonad.t

type resolve_module_type_result =
  ( Cpath.Resolved.module_type * Component.ModuleType.t,
    Cpath.module_type )
  ResolvedMonad.t

type resolve_type_result =
  ( Cpath.Resolved.type_ * (Find.type_, Component.TypeExpr.t) Find.found,
    Cpath.type_ )
  ResolvedMonad.t

type resolve_class_type_result =
  ( Cpath.Resolved.class_type
    * (Find.class_type, Component.TypeExpr.t) Find.found,
    Cpath.class_type )
  ResolvedMonad.t

open Errors

module type MEMO = sig
  type result

  include Hashtbl.HashedType
end

module MakeMemo (X : MEMO) = struct
  module M = Hashtbl.Make (X)

  let cache : (X.result * int * Env.lookup_type list) M.t = M.create 10000

  let cache_hits : int M.t = M.create 10000

  let enabled = ref true

  let bump_counter arg =
    try
      let new_val = M.find cache_hits arg + 1 in
      M.replace cache_hits arg new_val;
      new_val
    with _ ->
      M.add cache_hits arg 1;
      1

  let memoize f env arg =
    if not !enabled then f env arg
    else
      let env_id = Env.id env in
      let n = bump_counter arg in
      let no_memo () =
        let lookups, result =
          Env.with_recorded_lookups env (fun env' -> f env' arg)
        in
        if n > 1 then M.add cache arg (result, env_id, lookups);
        result
      in
      match M.find_all cache arg with
      | [] -> no_memo ()
      | xs ->
          let rec find_fast = function
            | (result, env_id', _) :: _ when env_id' = env_id ->
                M.replace cache_hits arg (M.find cache_hits arg + 1);
                result
            | _ :: ys -> find_fast ys
            | [] -> find xs
          and find = function
            | (m, _, lookups) :: xs ->
                if Env.verify_lookups env lookups then m else find xs
            | [] -> no_memo ()
          in
          find_fast xs

  let clear () =
    M.clear cache;
    M.clear cache_hits
end

module LookupModuleMemo = MakeMemo (struct
  type t = bool * Cpath.Resolved.module_

  type result =
    ( Component.Module.t Component.Delayed.t,
      [ simple_module_lookup_error | parent_lookup_error ] )
    Result.result

  let equal = ( = )

  let hash (b, m) = Hashtbl.hash (b, Cpath.resolved_module_hash m)
end)

module LookupParentMemo = MakeMemo (struct
  type t = bool * Cpath.Resolved.parent

  type result =
    ( Component.Signature.t * Component.Substitution.t,
      parent_lookup_error )
    Result.result

  let equal = ( = )

  let hash (b, p) = Hashtbl.hash (b, Cpath.resolved_parent_hash p)
end)

module LookupAndResolveMemo = MakeMemo (struct
  type t = bool * bool * Cpath.module_

  type result = resolve_module_result

  let equal = ( = )

  let hash (b1, b2, p) = Hashtbl.hash (b1, b2, Cpath.module_hash p)
end)

module SignatureOfModuleMemo = MakeMemo (struct
  type t = Cpath.Resolved.module_

  type result = (Component.Signature.t, signature_of_module_error) Result.result

  let equal = ( = )

  let hash p = Cpath.resolved_module_hash p
end)

let disable_all_caches () =
  LookupModuleMemo.enabled := false;
  LookupAndResolveMemo.enabled := false;
  SignatureOfModuleMemo.enabled := false;
  LookupParentMemo.enabled := false

let reset_caches () =
  LookupModuleMemo.clear ();
  LookupAndResolveMemo.clear ();
  SignatureOfModuleMemo.clear ();
  LookupParentMemo.clear ()

let rec handle_apply ~mark_substituted env func_path arg_path m =
  let rec find_functor mty =
    match mty with
    | Component.ModuleType.Functor (Named arg, expr) ->
        Ok (arg.Component.FunctorParameter.id, expr)
    | Component.ModuleType.Path mty_path -> (
        match resolve_module_type ~mark_substituted:false env mty_path with
        | ResolvedMonad.Resolved
            (_, { Component.ModuleType.expr = Some mty'; _ }) ->
            find_functor mty'
        | _ -> Error `OpaqueModule )
    | _ -> Error `ApplyNotFunctor
  in
  module_type_expr_of_module env m >>= fun mty' ->
  find_functor mty' >>= fun (arg_id, result) ->
  let new_module = { m with Component.Module.type_ = ModuleType result } in
  let substitution =
    if mark_substituted then `Substituted arg_path else arg_path
  in

  let path = `Apply (func_path, `Resolved substitution) in
  Ok
    ( path,
      Subst.module_
        (Subst.add_module
           (arg_id :> Ident.path_module)
           substitution Subst.identity)
        new_module )

and add_canonical_path :
    Env.t ->
    Component.Module.t ->
    Cpath.Resolved.module_ ->
    Cpath.Resolved.module_ =
 fun _env m p ->
  match p with
  | `Canonical _ -> p
  | _ -> (
      match m.Component.Module.canonical with
      | Some (cp, _cr) -> `Canonical (p, cp)
      | None -> p )

and get_substituted_module_type :
    Env.t -> Component.ModuleType.expr -> Cpath.Resolved.module_type option =
 fun env expr ->
  (* Format.fprintf Format.err_formatter ">>>expr=%a\n%!"  Component.Fmt.module_type_expr expr; *)
  match expr with
  | Component.ModuleType.Path p' ->
      if Cpath.is_module_type_substituted p' then
        match resolve_module_type ~mark_substituted:true env p' with
        | Resolved (resolved_path, _) -> Some resolved_path
        | Unresolved _ ->
            (* Format.fprintf Format.err_formatter "<<<Unresolved!?\n%!";*) None
      else None
  | _ -> (* Format.fprintf Format.err_formatter "<<<wtf!?\n%!"; *) None

and process_module_type env m p' =
  let open Component.ModuleType in
  let open OptionMonad in
  (* Format.fprintf Format.err_formatter "Processing module_type %a\n%!" Component.Fmt.resolved_module_type_path p'; *)
  (* Loop through potential chains of module_type equalities, looking for substitutions *)
  let substpath =
    m.expr >>= get_substituted_module_type env >>= fun p ->
    Some (`SubstT (p, p'))
  in
  let p' = match substpath with Some p -> p | None -> p' in
  p'

and get_module_path_modifiers :
    Env.t -> add_canonical:bool -> Component.Module.t -> _ option =
 fun env ~add_canonical m ->
  match m.type_ with
  | Alias alias_path -> (
      (* Format.fprintf Format.err_formatter "alias to path: %a\n%!" Component.Fmt.module_path alias_path; *)
      match
        resolve_module ~mark_substituted:true ~add_canonical env alias_path
      with
      | Resolved (resolved_alias_path, _) -> Some (`Aliased resolved_alias_path)
      | Unresolved _ -> None )
  | ModuleType t -> (
      match get_substituted_module_type env t with
      | Some s -> Some (`SubstMT s)
      | None -> None )

and process_module_path env ~add_canonical m p =
  let p = if m.Component.Module.hidden then `Hidden p else p in
  let p' =
    match get_module_path_modifiers env ~add_canonical m with
    | None -> p
    | Some (`SubstAliased p') -> `SubstAlias (p', p)
    | Some (`Aliased p') -> `Alias (p', p)
    | Some (`SubstMT p') -> `Subst (p', p)
  in
  let p'' = if add_canonical then add_canonical_path env m p' else p' in
  p''

and handle_module_lookup env ~add_canonical id parent sg sub =
  match Find.careful_module_in_sig sg id with
  | Some (Find.Found (name, m)) ->
      let p' = `Module (parent, name) in
      let m' = Subst.module_ sub m in
      let md' = Component.Delayed.put_val m' in
      Some (process_module_path env ~add_canonical m' p', md')
  | Some (Replaced p) -> (
      match lookup_module ~mark_substituted:false env p with
      | Ok m -> Some (p, m)
      | Error _ -> None )
  | None -> None

and handle_module_type_lookup env id p sg sub =
  let open OptionMonad in
  Find.module_type_in_sig sg id >>= fun mt ->
  let p' = `ModuleType (p, Odoc_model.Names.ModuleTypeName.of_string id) in
  let p'' = process_module_type env mt p' in
  Some (p'', Subst.module_type sub mt)

and handle_type_lookup id p sg =
  match Find.careful_type_in_sig sg id with
  | Some (Found (`C (name, _)) as t) -> Ok (`Class (p, name), t)
  | Some (Found (`CT (name, _)) as t) -> Ok (`ClassType (p, name), t)
  | Some (Found (`T (name, _)) as t) -> Ok (`Type (p, name), t)
  | Some (Replaced (name, _) as t) -> Ok (`Type (p, name), t)
  | None -> Error `Find_failure

and handle_class_type_lookup id p sg =
  match Find.careful_class_type_in_sig sg id with
  | Some (Found (`C (name, _)) as t) -> Ok (`Class (p, name), t)
  | Some (Found (`CT (name, _)) as t) -> Ok (`ClassType (p, name), t)
  | Some (Replaced (_name, _) as _t) -> Error `Class_replaced
  | None -> Error `Find_failure

and lookup_module :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_ ->
    ( Component.Module.t Component.Delayed.t,
      [ simple_module_lookup_error | parent_lookup_error ] )
    Result.result =
 fun ~mark_substituted:m env' path' ->
  let lookup env (mark_substituted, (path : SignatureOfModuleMemo.M.key)) =
    match path with
    | `Local lpath -> Error (`Local (env, lpath))
    | `Identifier i ->
        of_option ~error:(`Lookup_failure i) (Env.(lookup_by_id s_module) i env)
        >>= fun (`Module (_, m)) -> Ok m
    | `Substituted x -> lookup_module ~mark_substituted env x
    | `Apply (functor_path, `Resolved argument_path) -> (
        match lookup_module ~mark_substituted env functor_path with
        | Ok functor_module ->
            let functor_module = Component.Delayed.get functor_module in
            handle_apply ~mark_substituted:false env functor_path argument_path
              functor_module
            |> map_error (function
                 | #simple_module_type_expr_of_module_error as e ->
                     `Parent_expr e
                 | #parent_lookup_error as x -> x)
            >>= fun (_, m) -> Ok (Component.Delayed.put_val m)
        | Error _ as e -> e )
    | `Module (parent, name) ->
        let find_in_sg sg sub =
          match Find.careful_module_in_sig sg (ModuleName.to_string name) with
          | None -> Error `Find_failure
          | Some (Find.Found (_, m)) ->
              Ok (Component.Delayed.put_val (Subst.module_ sub m))
          | Some (Replaced p) -> lookup_module ~mark_substituted env p
        in
        lookup_parent ~mark_substituted env parent
        |> map_error (fun e ->
               (e :> [ simple_module_lookup_error | parent_lookup_error ]))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `Alias (_, p) -> lookup_module ~mark_substituted env p
    | `Subst (_, p) -> lookup_module ~mark_substituted env p
    | `SubstAlias (_, p) -> lookup_module ~mark_substituted env p
    | `Hidden p -> lookup_module ~mark_substituted env p
    | `Canonical (p, _) -> lookup_module ~mark_substituted env p
    | `Apply (_, _) -> Error `Unresolved_apply
    | `OpaqueModule m -> lookup_module ~mark_substituted env m
  in
  LookupModuleMemo.memoize lookup env' (m, path')

and lookup_module_type :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_type ->
    ( Component.ModuleType.t,
      [ simple_module_type_lookup_error | parent_lookup_error ] )
    Result.result =
 fun ~mark_substituted env path ->
  let lookup env =
    match path with
    | `Local _ -> Error (`LocalMT (env, path))
    | `Identifier i ->
        of_option ~error:(`Lookup_failureMT i)
          (Env.(lookup_by_id s_module_type) i env)
        >>= fun (`ModuleType (_, mt)) -> Ok mt
    | `Substituted s | `SubstT (_, s) ->
        lookup_module_type ~mark_substituted env s
    | `ModuleType (parent, name) ->
        let find_in_sg sg sub =
          match Find.module_type_in_sig sg (ModuleTypeName.to_string name) with
          | None -> Error `Find_failure
          | Some mt -> Ok (Subst.module_type sub mt)
        in
        lookup_parent ~mark_substituted:true env parent
        |> map_error (fun e ->
               (e :> [ simple_module_type_lookup_error | parent_lookup_error ]))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `OpaqueModuleType m -> lookup_module_type ~mark_substituted env m
  in
  lookup env

and lookup_parent :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.parent ->
    ( Component.Signature.t * Component.Substitution.t,
      parent_lookup_error )
    Result.result =
 fun ~mark_substituted:m env' parent' ->
  let lookup env (mark_substituted, parent) =
    match parent with
    | `Module p ->
        lookup_module ~mark_substituted env p
        |> map_error (function
             | #parent_lookup_error as p -> p
             | #simple_module_lookup_error as p -> `Parent_module p)
        >>= fun m ->
        let m = Component.Delayed.get m in
        signature_of_module env m |> map_error (fun e -> `Parent_sig e)
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `ModuleType p ->
        lookup_module_type ~mark_substituted env p
        |> map_error (function
             | #parent_lookup_error as p -> p
             | #simple_module_type_lookup_error as p -> `Parent_module_type p)
        >>= fun mt ->
        signature_of_module_type env mt |> map_error (fun e -> `Parent_sig e)
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `FragmentRoot ->
        Env.lookup_fragment_root env |> of_option ~error:`Fragment_root
        >>= fun (_, sg) -> Ok (sg, prefix_substitution parent sg)
  in
  LookupParentMemo.memoize lookup env' (m, parent')

and lookup_type :
    Env.t ->
    Cpath.Resolved.type_ ->
    ( (Find.type_, Component.TypeExpr.t) Find.found,
      [ simple_type_lookup_error | parent_lookup_error ] )
    Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent ~mark_substituted:true env p
    |> map_error (fun e ->
           (e :> [ simple_type_lookup_error | parent_lookup_error ]))
    >>= fun (sg, sub) ->
    handle_type_lookup name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | Find.Found (`C (_, c)) -> Find.Found (`C (Subst.class_ sub c))
      | Find.Found (`CT (_, ct)) -> Find.Found (`CT (Subst.class_type sub ct))
      | Find.Found (`T (_, t)) -> Find.Found (`T (Subst.type_ sub t))
      | Find.Replaced (_, texpr) -> Find.Replaced (Subst.type_expr sub texpr)
    in
    Ok t
  in
  let res =
    match p with
    | `Local _id -> Error (`LocalType (env, p))
    | `Identifier (`CoreType name) ->
        (* CoreTypes aren't put into the environment, so they can't be handled by the
              next clause. We just look them up here in the list of core types *)
        Ok (Find.Found (`T (List.assoc (TypeName.to_string name) core_types)))
    | `Identifier (`Type _ as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_type) i env)
        >>= fun (`Type (_, t)) -> Ok (Find.Found (`T t))
    | `Identifier (`Class _ as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class (_, t)) -> Ok (Find.Found (`C t))
    | `Identifier (`ClassType _ as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType (_, t)) -> Ok (Find.Found (`CT t))
    | `Substituted s -> lookup_type env s
    | `Type (p, id) -> do_type p (TypeName.to_string id)
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and lookup_class_type :
    Env.t ->
    Cpath.Resolved.class_type ->
    ( (Find.class_type, Component.TypeExpr.t) Find.found,
      [ simple_type_lookup_error | parent_lookup_error ] )
    Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent ~mark_substituted:true env p
    |> map_error (fun e ->
           (e :> [ simple_type_lookup_error | parent_lookup_error ]))
    >>= fun (sg, sub) ->
    handle_class_type_lookup name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | Find.Found (`C (_, c)) -> Find.Found (`C (Subst.class_ sub c))
      | Find.Found (`CT (_, ct)) -> Find.Found (`CT (Subst.class_type sub ct))
      | Find.Replaced (_, texpr) -> Find.Replaced (Subst.type_expr sub texpr)
    in
    Ok t
  in
  let res =
    match p with
    | `Local _id -> Error (`LocalType (env, (p :> Cpath.Resolved.type_)))
    | `Identifier (`Class _ as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class (_, t)) -> Ok (Find.Found (`C t))
    | `Identifier (`ClassType _ as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType (_, t)) -> Ok (Find.Found (`CT t))
    | `Substituted s -> lookup_class_type env s
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and resolve_module :
    mark_substituted:bool ->
    add_canonical:bool ->
    Env.t ->
    Cpath.module_ ->
    resolve_module_result =
 fun ~mark_substituted ~add_canonical env' path ->
  let open ResolvedMonad in
  let id = (mark_substituted, add_canonical, path) in
  (* Format.fprintf Format.err_formatter "resolve_module: looking up %a\n%!" Component.Fmt.path p; *)
  let resolve env (mark_substituted, add_canonical, p) =
    match p with
    | `Dot (parent, id) as unresolved ->
        resolve_module ~mark_substituted ~add_canonical env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let m = Component.Delayed.get m in
        signature_of_module_cached env p m |> of_result ~unresolved
        >>= fun parent_sig ->
        let sub = prefix_substitution (`Module p) parent_sig in
        handle_module_lookup env ~add_canonical id (`Module p) parent_sig sub
        |> of_option ~unresolved
    | `Apply (m1, m2) -> (
        let func = resolve_module ~mark_substituted ~add_canonical env m1 in
        let arg = resolve_module ~mark_substituted ~add_canonical env m2 in
        match (func, arg) with
        | Resolved (func_path', m), Resolved (arg_path', _) -> (
            let m = Component.Delayed.get m in
            match handle_apply ~mark_substituted env func_path' arg_path' m with
            | Ok (p, m) -> return (p, Component.Delayed.put_val m)
            | Error _ ->
                Unresolved (`Apply (`Resolved func_path', `Resolved arg_path'))
            )
        | Unresolved func_path', Resolved (arg_path', _) ->
            Unresolved (`Apply (func_path', `Resolved arg_path'))
        | Resolved (func_path', _), Unresolved arg_path' ->
            Unresolved (`Apply (`Resolved func_path', arg_path'))
        | Unresolved func_path', Unresolved arg_path' ->
            Unresolved (`Apply (func_path', arg_path')) )
    | `Identifier (i, hidden) as unresolved ->
        of_option ~unresolved (Env.(lookup_by_id s_module) i env)
        >>= fun (`Module (_, m)) ->
        let p = if hidden then `Hidden (`Identifier i) else `Identifier i in
        return
          (process_module_path env ~add_canonical (Component.Delayed.get m) p, m)
    | `Local _ as unresolved -> Unresolved unresolved
    | `Resolved (`Identifier i as resolved_path) as unresolved ->
        of_option ~unresolved (Env.(lookup_by_id s_module) i env)
        >>= fun (`Module (_, m)) -> return (resolved_path, m)
    | `Resolved r as unresolved -> (
        match lookup_module ~mark_substituted env r with
        | Ok m -> return (r, m)
        | Error _ -> Unresolved unresolved )
    | `Substituted s ->
        resolve_module ~mark_substituted ~add_canonical env s
        |> map_unresolved (fun p -> `Substituted p)
        >>= fun (p, m) -> return (`Substituted p, m)
    | `Root r -> (
        (* Format.fprintf Format.err_formatter "Looking up module %s by name...%!" r; *)
        match Env.lookup_root_module r env with
        | Some (Env.Resolved (_, p, hidden, m)) ->
            let p =
              `Identifier (p :> Odoc_model.Paths.Identifier.Path.Module.t)
            in
            let p = if hidden then `Hidden p else p in
            return (p, m)
        | Some Env.Forward ->
            (* Format.fprintf Format.err_formatter "Forward :-(!\n%!"; *)
            Unresolved (`Forward r)
        | None ->
            (* Format.fprintf Format.err_formatter "Unresolved!\n%!"; *)
            Unresolved p )
    | `Forward f ->
        resolve_module ~mark_substituted ~add_canonical env (`Root f)
        |> map_unresolved (fun _ -> `Forward f)
  in
  LookupAndResolveMemo.memoize resolve env' id

and resolve_module_type :
    mark_substituted:bool ->
    Env.t ->
    Cpath.module_type ->
    resolve_module_type_result =
  let open ResolvedMonad in
  fun ~mark_substituted env p ->
    (* Format.fprintf Format.err_formatter "resolve_module_type: looking up %a\n%!" Component.Fmt.module_type_path p; *)
    match p with
    | `Dot (parent, id) as unresolved ->
        resolve_module ~mark_substituted ~add_canonical:true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let m = Component.Delayed.get m in
        of_result ~unresolved (signature_of_module_cached env p m)
        >>= fun parent_sg ->
        let sub = prefix_substitution (`Module p) parent_sg in
        of_option ~unresolved
          (handle_module_type_lookup env id (`Module p) parent_sg sub)
        >>= fun (p', mt) -> return (p', mt)
    | `Identifier (i, _) as unresolved ->
        of_option ~unresolved (Env.(lookup_by_id s_module_type) i env)
        >>= fun (`ModuleType (_, mt)) ->
        let p = `Identifier i in
        let p' = process_module_type env mt p in
        return (p', mt)
    | `Local _ as unresolved -> Unresolved unresolved
    | `Resolved r as unresolved ->
        of_result ~unresolved (lookup_module_type ~mark_substituted env r)
        >>= fun m -> return (r, m)
    | `Substituted s ->
        resolve_module_type ~mark_substituted env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and resolve_type : Env.t -> Cpath.type_ -> resolve_type_result =
  let open ResolvedMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) as unresolved ->
        (* let start_time = Unix.gettimeofday () in *)
        resolve_module ~mark_substituted:true ~add_canonical:true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let m = Component.Delayed.get m in
        (* let time1 = Unix.gettimeofday () in *)
        of_result ~unresolved (signature_of_module_cached env p m) >>= fun sg ->
        (* let time1point5 = Unix.gettimeofday () in *)
        let sub = prefix_substitution (`Module p) sg in
        (* let time2 = Unix.gettimeofday () in *)
        of_result ~unresolved (handle_type_lookup id (`Module p) sg)
        >>= fun (p', t') ->
        let t =
          match t' with
          | Find.Found (`C (_, c)) -> Find.Found (`C (Subst.class_ sub c))
          | Find.Found (`CT (_, ct)) ->
              Find.Found (`CT (Subst.class_type sub ct))
          | Find.Found (`T (_, t)) -> Find.Found (`T (Subst.type_ sub t))
          | Find.Replaced (_, texpr) ->
              Find.Replaced (Subst.type_expr sub texpr)
        in
        (* let time3 = Unix.gettimeofday () in *)
        (* Format.fprintf Format.err_formatter "lookup: %f vs sig_of_mod: %f vs prefix_sub: %f vs rest: %f\n%!" (time1 -. start_time) (time1point5 -. time1) (time2 -. time1point5) (time3 -. time2); *)
        return (p', t)
    | `Identifier (i, _) as unresolved ->
        of_result ~unresolved (lookup_type env (`Identifier i)) >>= fun t ->
        return (`Identifier i, t)
    | `Resolved r as unresolved ->
        of_result ~unresolved (lookup_type env r) >>= fun t -> return (r, t)
    | `Local _ as unresolved -> Unresolved unresolved
    | `Substituted s ->
        resolve_type env s |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and resolve_class_type : Env.t -> Cpath.class_type -> resolve_class_type_result
    =
  let open ResolvedMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) as unresolved ->
        (* let start_time = Unix.gettimeofday () in *)
        resolve_module ~mark_substituted:true ~add_canonical:true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let m = Component.Delayed.get m in
        (* let time1 = Unix.gettimeofday () in *)
        of_result ~unresolved (signature_of_module_cached env p m) >>= fun sg ->
        (* let time1point5 = Unix.gettimeofday () in *)
        let sub = prefix_substitution (`Module p) sg in
        (* let time2 = Unix.gettimeofday () in *)
        of_result ~unresolved (handle_class_type_lookup id (`Module p) sg)
        >>= fun (p', t') ->
        let t =
          match t' with
          | Find.Found (`C (_, c)) -> Find.Found (`C (Subst.class_ sub c))
          | Find.Found (`CT (_, ct)) ->
              Find.Found (`CT (Subst.class_type sub ct))
          | Find.Replaced (_, texpr) ->
              Find.Replaced (Subst.type_expr sub texpr)
        in
        (* let time3 = Unix.gettimeofday () in *)
        (* Format.fprintf Format.err_formatter "lookup: %f vs sig_of_mod: %f vs prefix_sub: %f vs rest: %f\n%!" (time1 -. start_time) (time1point5 -. time1) (time2 -. time1point5) (time3 -. time2); *)
        return (p', t)
    | `Identifier (i, _) as unresolved ->
        of_result ~unresolved (lookup_class_type env (`Identifier i))
        >>= fun t -> return (`Identifier i, t)
    | `Resolved r as unresolved ->
        of_result ~unresolved (lookup_class_type env r) >>= fun t ->
        return (r, t)
    | `Local _ as unresolved -> Unresolved unresolved
    | `Substituted s ->
        resolve_class_type env s |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
    =
 fun env path ->
  match path with
  | `Local _ | `Identifier _ -> path
  | `Substituted x -> `Substituted (reresolve_module env x)
  | `Apply (functor_path, `Resolved argument_path) ->
      `Apply
        ( reresolve_module env functor_path,
          `Resolved (reresolve_module env argument_path) )
  | `Module (parent, name) -> `Module (reresolve_parent env parent, name)
  | `Alias (p1, p2) -> `Alias (reresolve_module env p1, reresolve_module env p2)
  | `Subst (p1, p2) ->
      `Subst (reresolve_module_type env p1, reresolve_module env p2)
  | `SubstAlias (p1, p2) ->
      `SubstAlias (reresolve_module env p1, reresolve_module env p2)
  | `Hidden p ->
      let p' = reresolve_module env p in
      `Hidden p'
  | `Canonical (p, `Resolved p2) ->
      `Canonical (reresolve_module env p, `Resolved (reresolve_module env p2))
  | `Canonical (p, p2) -> (
      match
        resolve_module ~mark_substituted:true ~add_canonical:false env p2
      with
      | Resolved (`Alias (_, p2'), _) ->
          `Canonical
            ( reresolve_module env p,
              `Resolved (simplify_resolved_module_path env p2') )
      | Resolved (p2', _) ->
          (* See, e.g. Base.Sexp for an example of where the canonical path might not be
             a simple alias *)
          `Canonical
            ( reresolve_module env p,
              `Resolved (simplify_resolved_module_path env p2') )
      | Unresolved _ -> `Canonical (reresolve_module env p, p2)
      | exception _ -> `Canonical (reresolve_module env p, p2) )
  | `Apply (p, p2) -> (
      match
        resolve_module ~mark_substituted:true ~add_canonical:false env p2
      with
      | Resolved (p2', _) -> `Apply (reresolve_module env p, `Resolved p2')
      | Unresolved p2' -> `Apply (reresolve_module env p, p2') )
  | `OpaqueModule m -> `OpaqueModule (reresolve_module env m)

and reresolve_module_type :
    Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun env path ->
  match path with
  | `Local _ | `Identifier _ -> path
  | `Substituted x -> `Substituted (reresolve_module_type env x)
  | `ModuleType (parent, name) -> `ModuleType (reresolve_parent env parent, name)
  | `SubstT (p1, p2) ->
      `SubstT (reresolve_module_type env p1, reresolve_module_type env p2)
  | `OpaqueModuleType m -> `OpaqueModuleType (reresolve_module_type env m)

and reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun env path ->
  let result =
    match path with
    | `Identifier _ | `Local _ -> path
    | `Substituted s -> `Substituted (reresolve_type env s)
    | `Type (p, n) -> `Type (reresolve_parent env p, n)
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
      [ simple_module_type_expr_of_module_error | parent_lookup_error ] )
    Result.result =
 fun env decl ->
  match decl with
  | Component.Module.Alias (`Resolved r) ->
      lookup_module ~mark_substituted:false env r
      |> map_error (function
           | #parent_lookup_error as e -> e
           | #simple_module_lookup_error as e -> `Parent_module e)
      >>= fun m ->
      let m = Component.Delayed.get m in
      module_type_expr_of_module_decl env m.type_
  | Component.Module.Alias path -> (
      match
        resolve_module ~mark_substituted:false ~add_canonical:true env path
      with
      | Resolved (_, m) ->
          let m = Component.Delayed.get m in
          module_type_expr_of_module env m
      | Unresolved p when Cpath.is_module_forward p ->
          Error `UnresolvedForwardPath
      | Unresolved p' -> Error (`UnresolvedPath (`Module p')) )
  | Component.Module.ModuleType expr -> Ok expr

and module_type_expr_of_module :
    Env.t ->
    Component.Module.t ->
    ( Component.ModuleType.expr,
      [ simple_module_type_expr_of_module_error | parent_lookup_error ] )
    Result.result =
 fun env m -> module_type_expr_of_module_decl env m.type_

and signature_of_module_alias :
    Env.t ->
    Cpath.module_ ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env path ->
  match resolve_module ~mark_substituted:false ~add_canonical:true env path with
  | Resolved (p', m) ->
      let m = Component.Delayed.get m in
      (* p' is the path to the aliased module *)
      signature_of_module_cached env p' m >>= fun sg ->
      Ok (Strengthen.signature (`Resolved p') sg)
  | Unresolved p when Cpath.is_module_forward p -> Error `UnresolvedForwardPath
  | Unresolved p' -> Error (`UnresolvedPath (`Module p'))

and handle_signature_with_subs :
    mark_substituted:bool ->
    Env.t ->
    Component.Signature.t ->
    Component.ModuleType.substitution list ->
    (Component.Signature.t, handle_subs_error) Result.result =
 fun ~mark_substituted env sg subs ->
  let open ResultMonad in
  List.fold_left
    (fun sg_opt sub ->
      sg_opt >>= fun sg ->
      match sub with
      | Component.ModuleType.ModuleEq (frag, _) ->
          fragmap_module ~mark_substituted env frag sub sg
      | ModuleSubst (frag, _) ->
          fragmap_module ~mark_substituted env frag sub sg
      | TypeEq (frag, _) -> Ok (fragmap_type env frag sub sg)
      | TypeSubst (frag, _) -> Ok (fragmap_type env frag sub sg))
    (Ok sg) subs

and signature_of_module_type_expr :
    mark_substituted:bool ->
    Env.t ->
    Component.ModuleType.expr ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun ~mark_substituted env m ->
  match m with
  | Component.ModuleType.Path p -> (
      match resolve_module_type ~mark_substituted env p with
      | Resolved (_, mt) -> signature_of_module_type env mt
      | Unresolved _p -> Error (`UnresolvedPath (`ModuleType p)) )
  | Component.ModuleType.Signature s -> Ok s
  | Component.ModuleType.With (s, subs) -> (
      signature_of_module_type_expr ~mark_substituted env s >>= fun sg ->
      match handle_signature_with_subs ~mark_substituted env sg subs with
      | Ok x -> Ok x
      | Error y -> Error (y :> signature_of_module_error) )
  | Component.ModuleType.Functor (Unit, expr) ->
      signature_of_module_type_expr ~mark_substituted env expr
  | Component.ModuleType.Functor (Named arg, expr) ->
      ignore arg;
      signature_of_module_type_expr ~mark_substituted env expr
  | Component.ModuleType.TypeOf decl -> signature_of_module_decl env decl

and signature_of_module_type :
    Env.t ->
    Component.ModuleType.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env m ->
  match m.expr with
  | None -> Error `OpaqueModule
  | Some expr -> signature_of_module_type_expr ~mark_substituted:false env expr

and signature_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env decl ->
  match decl with
  | Component.Module.Alias path -> signature_of_module_alias env path
  | Component.Module.ModuleType expr ->
      signature_of_module_type_expr ~mark_substituted:false env expr

and signature_of_module :
    Env.t ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env m -> signature_of_module_decl env m.type_

and signature_of_module_cached :
    Env.t ->
    Cpath.Resolved.module_ ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env' path m ->
  let id = path in
  let run env _id = signature_of_module env m in
  SignatureOfModuleMemo.memoize run env' id

and fragmap_module :
    mark_substituted:bool ->
    Env.t ->
    Cfrag.module_ ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    (Component.Signature.t, handle_subs_error) Result.result =
 fun ~mark_substituted env frag sub sg ->
  let name, frag' = Cfrag.module_split frag in
  let map_module m =
    match (frag', sub) with
    | None, ModuleEq (_, type_) ->
        let type_ =
          match type_ with
          | Alias (`Resolved p) ->
              let new_p = if mark_substituted then `Substituted p else p in
              Component.Module.Alias (`Resolved new_p)
          | Alias _ | ModuleType _ -> type_
        in
        (* Finished the substitution *)
        Left { m with Component.Module.type_; expansion = None }
    | None, ModuleSubst (_, p) -> (
        match
          resolve_module ~mark_substituted:true ~add_canonical:false env p
        with
        | Resolved (p, _) -> Right p
        | Unresolved p ->
            Format.fprintf Format.err_formatter "failed to resolve path: %a\n%!"
              Component.Fmt.module_path p;
            raise (ModuleLookup p) )
    | Some f, subst -> (
        let new_subst =
          match subst with
          | ModuleEq (_, type_) -> Component.ModuleType.ModuleEq (f, type_)
          | ModuleSubst (_, path) -> ModuleSubst (f, path)
          | TypeEq _ | TypeSubst _ -> failwith "Can't happen"
        in
        match m.type_ with
        | Alias path ->
            Left
              {
                m with
                type_ =
                  ModuleType
                    Component.(
                      ModuleType.(
                        With (TypeOf (Module.Alias path), [ new_subst ])));
                expansion = None;
              }
            (* Can this one happen? *)
        | ModuleType (With (mty', subs')) ->
            Left
              {
                m with
                type_ =
                  ModuleType
                    (Component.ModuleType.With (mty', subs' @ [ new_subst ]));
                expansion = None;
              }
        | ModuleType mty ->
            Left
              {
                m with
                type_ =
                  ModuleType (Component.ModuleType.With (mty, [ new_subst ]));
                expansion = None;
              } )
    | _, TypeEq _ | _, TypeSubst _ -> failwith "Can't happen"
  in
  let map_include i expansion_ =
    let decl =
      match i.Component.Include.decl with
      | Component.Module.Alias p ->
          Component.Module.ModuleType (With (TypeOf (Alias p), [ sub ]))
      | Component.Module.ModuleType (With (p, subs)) ->
          ModuleType (With (p, sub :: subs))
      | Component.Module.ModuleType expr -> ModuleType (With (expr, [ sub ]))
    in
    { i with decl; expansion_ }
  in
  let rec handle_items items =
    List.fold_right
      (fun item (items, handled, removed, sub) ->
        match item with
        | Component.Signature.Module (id, r, m)
          when Ident.Name.module_ id = name -> (
            let m = Component.Delayed.get m in
            match map_module m with
            | Left m ->
                ( Component.Signature.Module
                    (id, r, Component.Delayed.put (fun () -> m))
                  :: items,
                  true,
                  removed,
                  id :: sub )
            | Right p ->
                ( items,
                  true,
                  Component.Signature.RModule (id, p) :: removed,
                  sub ) )
        | Component.Signature.Include i ->
            let items', handled', removed', sub' =
              handle_items i.expansion_.items
            in
            let expansion =
              Component.Signature.{ items = items'; removed = removed' }
            in
            let component =
              if handled' then
                Component.Signature.Include (map_include i expansion)
              else Component.Signature.Include { i with expansion_ = expansion }
            in
            ( component :: items,
              handled || handled',
              removed @ removed',
              sub @ sub' )
        | x -> (x :: items, handled, removed, sub))
      items ([], false, [], [])
  in
  try
    let items, _handled, removed, substituted = handle_items sg.items in

    let sub_of_removed removed sub =
      match removed with
      | Component.Signature.RModule (id, p) ->
          Subst.add_module (id :> Ident.path_module) p sub
      | _ -> sub
    in
    let sub = List.fold_right sub_of_removed removed Subst.identity in
    let items =
      if not mark_substituted then items
      else
        (* Mark things that have been substituted as such - See the `With11`
           test for an example of why this is necessary *)
        let sub_of_substituted x sub =
          let x = (x :> Ident.path_module) in
          Subst.add_module x (`Substituted (`Local x)) sub
        in
        let substituted_sub =
          List.fold_right sub_of_substituted substituted Subst.identity
        in
        (* Need to call `apply_sig_map` directly as we're substituting for an item
           that's declared within the signature *)
        let sg = Subst.apply_sig_map substituted_sub items [] in
        (* Finished marking substituted stuff *)
        sg.items
    in
    let res =
      Subst.signature sub
        { Component.Signature.items; removed = removed @ sg.removed }
    in
    Ok res
    (* Format.(
       fprintf err_formatter "after sig=%a\n%!" Component.Fmt.(signature) res); *)
  with ModuleLookup p -> Error (`UnresolvedPath (`Module p))

and fragmap_type :
    Env.t ->
    Cfrag.type_ ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    Component.Signature.t =
 fun _env frag sub sg ->
  let name, frag' = Cfrag.type_split frag in
  let map_include i expansion_ =
    let decl =
      match i.Component.Include.decl with
      | Component.Module.Alias p ->
          Component.Module.ModuleType (With (TypeOf (Alias p), [ sub ]))
      | Component.Module.ModuleType (With (p, subs)) ->
          ModuleType (With (p, sub :: subs))
      | Component.Module.ModuleType expr -> ModuleType (With (expr, [ sub ]))
    in
    { i with decl; expansion_ }
  in
  match frag' with
  | None ->
      let mapfn t =
        match sub with
        | TypeEq (_, equation) ->
            (* Finished the substitution *)
            Left { t with Component.TypeDecl.equation }
        | TypeSubst (_, { Component.TypeDecl.Equation.manifest = Some x; _ }) ->
            Right x
        | _ -> failwith "Can't happen"
      in
      let rec handle_items items init =
        List.fold_right
          (fun item (items, handled, removed) ->
            match item with
            | Component.Signature.Type (id, r, t)
              when Ident.Name.unsafe_type id = name -> (
                match mapfn (Component.Delayed.get t) with
                | Left x ->
                    ( Component.Signature.Type
                        (id, r, Component.Delayed.put (fun () -> x))
                      :: items,
                      true,
                      removed )
                | Right y ->
                    (items, true, Component.Signature.RType (id, y) :: removed)
                )
            | Component.Signature.Include ({ expansion_; _ } as i) ->
                let items', handled', removed' =
                  handle_items expansion_.items ([], false, [])
                in
                let expansion_ =
                  Component.Signature.{ items = items'; removed = removed' }
                in
                let component =
                  if handled' then
                    Component.Signature.Include (map_include i expansion_)
                  else Component.Signature.Include { i with expansion_ }
                in
                (component :: items, handled' || handled, removed' @ removed)
            | x -> (x :: items, handled, removed))
          items init
      in
      let items, _, removed = handle_items sg.items ([], false, []) in
      let subst =
        List.fold_right
          (fun ty subst ->
            match ty with
            | Component.Signature.RType (id, replacement) ->
                Subst.add_type_replacement
                  (id :> Ident.path_type)
                  replacement subst
            | _ -> subst)
          removed Subst.identity
      in
      Subst.signature subst { items; removed = removed @ sg.removed }
  | Some f ->
      let mapfn m =
        let new_subst =
          match sub with
          | ModuleEq _ | ModuleSubst _ -> failwith "Can't happen"
          | TypeEq (_, eqn) -> Component.ModuleType.TypeEq (f, eqn)
          | TypeSubst (_, eqn) -> Component.ModuleType.TypeSubst (f, eqn)
        in
        match m.Component.Module.type_ with
        | Alias path ->
            {
              m with
              type_ =
                ModuleType
                  Component.(
                    ModuleType.(
                      With (TypeOf (Module.Alias path), [ new_subst ])));
              expansion = None;
            }
            (* Can this one happen? *)
        | ModuleType (With (mty', subs')) ->
            {
              m with
              type_ =
                ModuleType
                  (Component.ModuleType.With (mty', subs' @ [ new_subst ]));
              expansion = None;
            }
        | ModuleType mty ->
            {
              m with
              type_ =
                ModuleType (Component.ModuleType.With (mty, [ new_subst ]));
              expansion = None;
            }
      in
      let rec handle_items items =
        List.fold_right
          (fun item (items, handled) ->
            match item with
            | Component.Signature.Module (id, r, m)
              when Ident.Name.module_ id = name ->
                let m = Component.Delayed.get m in
                let item =
                  Component.Signature.Module
                    (id, r, Component.Delayed.put (fun () -> mapfn m))
                in
                (item :: items, true)
            | Component.Signature.Include ({ expansion_; _ } as i) ->
                let items', handled' = handle_items expansion_.items in
                let expansion_ =
                  Component.Signature.
                    { items = items'; removed = expansion_.removed }
                in
                let component =
                  if handled' then
                    Component.Signature.Include (map_include i expansion_)
                  else Component.Signature.Include { i with expansion_ }
                in
                (component :: items, handled' || handled)
            | x -> (x :: items, handled))
          items ([], false)
      in
      let items, _ = handle_items sg.items in
      { sg with items }

and find_external_module_path :
    Cpath.Resolved.module_ -> Cpath.Resolved.module_ option =
 fun p ->
  let open OptionMonad in
  match p with
  | `Subst (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (`Subst (x, y))
  | `Module (p, n) ->
      find_external_parent_path p >>= fun p -> Some (`Module (p, n))
  | `Local x -> Some (`Local x)
  | `Substituted x ->
      find_external_module_path x >>= fun x -> Some (`Substituted x)
  | `SubstAlias (x, y) -> (
      match (find_external_module_path x, find_external_module_path y) with
      | Some x, Some y -> Some (`SubstAlias (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None )
  | `Canonical (x, y) ->
      find_external_module_path x >>= fun x -> Some (`Canonical (x, y))
  | `Hidden x -> find_external_module_path x >>= fun x -> Some (`Hidden x)
  | `Alias (x, y) -> (
      match (find_external_module_path x, find_external_module_path y) with
      | Some x, Some y -> Some (`Alias (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None )
  | `Apply (x, `Resolved y) ->
      find_external_module_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (`Apply (x, `Resolved y))
  | `Apply (x, y) ->
      find_external_module_path x >>= fun x -> Some (`Apply (x, y))
  | `Identifier x -> Some (`Identifier x)
  | `OpaqueModule m ->
      find_external_module_path m >>= fun x -> Some (`OpaqueModule x)

and find_external_module_type_path :
    Cpath.Resolved.module_type -> Cpath.Resolved.module_type option =
 fun p ->
  let open OptionMonad in
  match p with
  | `ModuleType (p, name) ->
      find_external_parent_path p >>= fun p -> Some (`ModuleType (p, name))
  | `Local _ -> Some p
  | `SubstT (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_type_path y >>= fun y -> Some (`SubstT (x, y))
  | `Substituted x ->
      find_external_module_type_path x >>= fun x -> Some (`Substituted x)
  | `Identifier _ -> Some p
  | `OpaqueModuleType m ->
      find_external_module_type_path m >>= fun x -> Some (`OpaqueModuleType x)

and find_external_parent_path :
    Cpath.Resolved.parent -> Cpath.Resolved.parent option =
 fun p ->
  let open OptionMonad in
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
      | None -> frag )
  | `SubstAlias (path, frag) -> (
      match find_external_module_path path with
      | Some p -> `SubstAlias (p, frag)
      | None -> frag )
  | `Module (parent, name) -> `Module (fixup_signature_cfrag parent, name)
  | `OpaqueModule m -> `OpaqueModule (fixup_module_cfrag m)

and fixup_signature_cfrag (f : Cfrag.resolved_signature) =
  match f with
  | `Root x -> `Root x
  | (`OpaqueModule _ | `Subst _ | `SubstAlias _ | `Module _) as f ->
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
      [ simple_module_lookup_error | parent_lookup_error ] )
    Result.result =
 fun env sg name ->
  match Find.careful_module_in_sig sg name with
  | Some (Found (_, m)) -> Ok (Component.Delayed.put_val m)
  | Some (Replaced path) -> lookup_module ~mark_substituted:false env path
  | None -> Error `Find_failure

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
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = Odoc_model.Names.ModuleName.of_string name in
      let new_path = `Module (ppath, mname) in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.Delayed.get m' in
      let modifier = get_module_path_modifiers env ~add_canonical:false m' in
      let cp', f' =
        match modifier with
        | None ->
            (* Format.fprintf Format.err_formatter "No modifier for frag %a\n%!" Component.Fmt.resolved_signature_fragment new_frag; *)
            (new_path, new_frag)
        | Some (`SubstAliased p') ->
            (* Format.fprintf Format.err_formatter "SubstAlias for frag %a\n%!" Component.Fmt.resolved_signature_fragment new_frag; *)
            (`SubstAlias (p', new_path), `SubstAlias (p', new_frag))
        | Some (`Aliased p') ->
            (* Format.fprintf Format.err_formatter "Alias for frag %a\n%!" Component.Fmt.resolved_signature_fragment new_frag; *)
            (`Alias (p', new_path), `SubstAlias (p', new_frag))
        | Some (`SubstMT p') ->
            (* Format.fprintf Format.err_formatter "SubstMT for frag %a\n%!" Component.Fmt.resolved_signature_fragment new_frag; *)
            (`Subst (p', new_path), `Subst (p', new_frag))
      in
      (* Don't use the cached one - `FragmentRoot` is not unique *)
      of_result (signature_of_module env m') >>= fun parent_sg ->
      (* Format.eprintf "Dot (%s): sig: %a\n%!" name Component.Fmt.signature sg; *)
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
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = Odoc_model.Names.ModuleName.of_string name in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.Delayed.get m' in
      let modifier = get_module_path_modifiers env ~add_canonical:false m' in
      let f' =
        match modifier with
        | None -> new_frag
        | Some (`SubstAliased p') -> `SubstAlias (p', new_frag)
        | Some (`Aliased p') -> `SubstAlias (p', new_frag)
        | Some (`SubstMT p') -> `Subst (p', new_frag)
      in
      let f'' =
        match signature_of_module env m' with
        | Ok (_m : Component.Signature.t) -> f'
        | Error `OpaqueModule -> `OpaqueModule f'
        | Error (`UnresolvedForwardPath | `UnresolvedPath _) -> f'
      in
      Some (fixup_module_cfrag f'')

and resolve_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.type_ ->
    Cfrag.resolved_type option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, _sg) ->
      let result = fixup_type_cfrag (`Type (pfrag, TypeName.of_string name)) in
      (* Format.fprintf Format.err_formatter "resolve_type_fragment: fragment=%a\n%!" Component.Fmt.resolved_type_fragment result; *)
      Some result

let rec reresolve_signature_fragment :
    Env.t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
 fun env m ->
  match m with
  | `Root (`ModuleType p) -> `Root (`ModuleType (reresolve_module_type env p))
  | `Root (`Module p) -> `Root (`Module (reresolve_module env p))
  | (`OpaqueModule _ | `Subst _ | `SubstAlias _ | `Module _) as x ->
      (reresolve_module_fragment env x :> Cfrag.resolved_signature)

and reresolve_module_fragment :
    Env.t -> Cfrag.resolved_module -> Cfrag.resolved_module =
 fun env m ->
  match m with
  | `Subst (p, f) ->
      let p' = reresolve_module_type env p in
      `Subst (p', reresolve_module_fragment env f)
  | `SubstAlias (p, f) ->
      let p' = reresolve_module env p in
      `SubstAlias (p', reresolve_module_fragment env f)
  | `OpaqueModule m -> `OpaqueModule (reresolve_module_fragment env m)
  | `Module (sg, m) -> `Module (reresolve_signature_fragment env sg, m)

and reresolve_type_fragment :
    Env.t -> Cfrag.resolved_type -> Cfrag.resolved_type =
 fun env m ->
  match m with
  | `Type (p, n) -> `Type (reresolve_signature_fragment env p, n)
  | `ClassType (p, n) -> `ClassType (reresolve_signature_fragment env p, n)
  | `Class (p, n) -> `Class (reresolve_signature_fragment env p, n)

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
      | Resolved (_, Found (`C c)) -> class_signature_of_class env c
      | Resolved (_, Found (`CT c)) -> class_signature_of_class_type env c
      | _ -> None )

and class_signature_of_class_type :
    Env.t -> Component.ClassType.t -> Component.ClassSignature.t option =
 fun env c -> class_signature_of_class_type_expr env c.expr

let resolve_module_path env p =
  let open ResolvedMonad in
  (* Format.fprintf Format.err_formatter "resolve_module: %a\n%!" Component.Fmt.module_path p; *)
  resolve_module ~mark_substituted:true ~add_canonical:true env p
  >>= fun (p, m) ->
  match p with
  | `Identifier (`Root _) | `Hidden (`Identifier (`Root _)) -> return p
  | _ -> (
      let m = Component.Delayed.get m in
      match signature_of_module_cached env p m with
      | Ok _ -> return p
      | Error `OpaqueModule -> return (`OpaqueModule p)
      | Error (`UnresolvedForwardPath | `UnresolvedPath _) -> return p )

let resolve_module_type_path env p =
  let open ResolvedMonad in
  resolve_module_type ~mark_substituted:true env p >>= fun (p, mt) ->
  match signature_of_module_type env mt with
  | Ok _ -> return p
  | Error `OpaqueModule -> return (`OpaqueModuleType p)
  | Error (`UnresolvedForwardPath | `UnresolvedPath _) -> return p

let resolve_type_path env p =
  let open ResolvedMonad in
  resolve_type env p >>= fun (p, _) -> return p

let resolve_class_type_path env p =
  let open ResolvedMonad in
  resolve_class_type env p >>= fun (p, _) -> return p
