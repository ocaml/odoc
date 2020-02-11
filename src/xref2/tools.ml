open Odoc_model.Names
open Odoc_model.Paths

exception OpaqueModule

exception UnresolvedForwardPath

let is_compile = ref true

let num_times = ref 0

let time_wasted_start = ref 0.0

let time_wasted = ref 0.0


type ('a, 'b) either = Left of 'a | Right of 'b

module OptionMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let bind m f = match m with Some x -> f x | None -> None

  let ( >>= ) = bind
end

module ResultMonad = struct
  type ('a, 'b) t = Resolved of 'a | Unresolved of 'b

  let return x = Resolved x

  let bind m f = match m with Resolved x -> f x | Unresolved y -> Unresolved y

  let ( >>= ) = bind

  let map_unresolved f m =
    match m with Resolved x -> Resolved x | Unresolved y -> Unresolved (f y)

  let get_resolved = function
    | Resolved r -> r
    | Unresolved _ -> failwith "Unresolved"
end

let add_subst p = function
  | None, x -> (p, x)
  | Some p', x -> (`Subst (p', p), x)

let filter_map_record_removed f l =
  let rec inner (kept, removed) = function
    | x :: xs -> (
        match f x with
        | Some y -> inner (y :: kept, removed) xs
        | None -> inner (kept, x :: removed) xs )
    | [] -> (List.rev kept, removed)
  in
  inner ([], []) l

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
    List.fold_right
      (fun item map ->
        match item with
        | Type (id, _, _) ->
            let name = TypeName.of_string (Ident.Name.type_ id) in
            Subst.add_type id
              (`Type (path,name))
              map
        | Module (id, _, _) ->
            let name = ModuleName.of_string (Ident.Name.module_ id) in
            Subst.add_module id
              (`Module (path, name))
              map
        | ModuleType (id, _) ->
            let name = ModuleTypeName.of_string (Ident.Name.module_type id) in
            Subst.add_module_type id
              (`ModuleType (path, name))
              map
        | ModuleSubstitution (id, _) ->
            let name = ModuleName.of_string (Ident.Name.module_ id) in
            Subst.add_module id
            (`Module (path, name))
              map
        | TypeSubstitution (id, _) ->
            let name = TypeName.of_string (Ident.Name.type_ id) in
            Subst.add_type id
              (`Type (path, name))
              map
        | Exception _ | TypExt _ | Value (_, _) | External (_, _) | Comment _ ->
            map
        | Class (id, _, _) ->
            let name = ClassName.of_string (Ident.Name.class_ id) in
            Subst.add_class id
              (`Class (path, name))
              map
        | ClassType (id, _, _) ->
            let name = ClassTypeName.of_string (Ident.Name.class_type id) in
            Subst.add_class_type id
              (`ClassType (path, name))
              map
        | Include i -> get_sub map i.expansion_.items)
      is sub'
  in
  let extend_sub_removed removed sub =
    List.fold_right
      (fun item map ->
        match item with
        | Component.Signature.RModule (id, _) ->
            let name = ModuleName.of_string (Ident.Name.module_ id) in
            Subst.add_module id
              (`Module (path, name))
              map
        | Component.Signature.RType (id, _) ->
            let name = TypeName.of_string (Ident.Name.type_ id) in
            Subst.add_type id
              (`Type (path,name))
              map)
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
                Subst.compose_delayed m sub )
        | ModuleType (id, mt) ->
            ModuleType
              ( Ident.Rename.module_type id,
                Subst.compose_delayed mt sub )
        | Type (id, r, t) ->
            Type
              ( Ident.Rename.type_ id,
                r,
                Subst.compose_delayed t sub )
        | TypeSubstitution (id, t) ->
            TypeSubstitution (Ident.Rename.type_ id, Subst.type_ sub t)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution
              (Ident.Rename.module_ id, Subst.module_substitution sub m)
        | Exception (id, e) -> Exception (id, Subst.exception_ sub e)
        | TypExt t -> TypExt (Subst.extension sub t)
        | Value (id, v) -> Value (id, Subst.value sub v)
        | External (id, e) -> External (id, Subst.external_ sub e)
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i -> Include (Subst.include_ sub i)
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
    try ignore (Env.lookup_module id env); `Identifier id
    with _ ->
      match id with
      | `Module (#Odoc_model.Paths_types.Identifier.module_ as parent, name) ->
        `Module (`Module (check_ident parent), name)
      | _ -> failwith "Bad canonical path"
  in
  check_ident id

let rec get_canonical_path :
    Cpath.Resolved.module_ -> Cpath.Resolved.module_ option =
 fun cp ->
  match cp with
  | `Canonical (_, `Resolved r) -> Some r
  | `Module (`Module parent, _) -> get_canonical_path parent
  | `Module _ -> None
  | `Local _ -> None
  | `Identifier _ -> None
  | `Substituted _ -> None
  | `Subst _ -> None
  | `SubstAlias _ -> None
  | `Hidden p -> get_canonical_path p
  | `Apply _ -> None
  | `Alias _ -> None
  | `Canonical _ -> None

type module_lookup_result = Cpath.Resolved.module_ * Component.Module.t

type module_type_lookup_result =
  Cpath.Resolved.module_type * Component.ModuleType.t

type type_lookup_result =
  Cpath.Resolved.type_
  * (Find.type_, Component.TypeExpr.t) Find.found

type class_type_lookup_result =
  Cpath.Resolved.class_type * Find.class_type

exception Type_lookup_failure of Env.t * Cpath.Resolved.type_

exception Module_lookup_failure of Env.t * Cpath.Resolved.module_

exception ModuleType_lookup_failure of Env.t * Cpath.Resolved.module_type

exception ClassType_lookup_failure of Env.t * Cpath.Resolved.class_type

exception MyFailure of Component.Module.t * Component.Module.t

exception Couldnt_find_functor_argument

module Hashable = struct
  type t = Cpath.Resolved.module_

  let equal = ( = )

  let hash m = Cpath.resolved_module_hash m
end

module Memos1 = Hashtbl.Make (Hashable)

let module_lookup_cache = Memos1.create 10000

module Hashable2 = struct
  type t = bool * bool * Cpath.module_

  let equal = Stdlib.( = )

  let hash (b1, b2, p) = Hashtbl.hash (b1, b2, Cpath.module_hash p)
end


module Memos2 = Hashtbl.Make (Hashable2)

module Hashable3 = struct
  type t = bool * Cpath.Resolved.module_

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash
end

module Memos3 = Hashtbl.Make (Hashable3)


let module_resolve_cache : ((module_lookup_result, Cpath.module_) ResultMonad.t * int * Env.lookup_type list) Memos2.t = Memos2.create 10000

let module_signature_cache : (Component.Signature.t * int * Env.lookup_type list) Memos3.t = Memos3.create 10000


let reset_cache () = Memos1.clear module_lookup_cache; Memos2.clear module_resolve_cache


let rec handle_apply is_resolve env func_path arg_path m =
  let mty' = module_type_expr_of_module env m in
  let rec find_functor mty =
    match mty with
    | Component.ModuleType.Functor (Named arg, expr) ->
        (arg.Component.FunctorParameter.id, expr)
    | Component.ModuleType.Path mty_path -> begin
        match lookup_and_resolve_module_type_from_path false env mty_path with
        | Resolved (_, { Component.ModuleType.expr = Some mty'; _ }) ->
          find_functor mty'
        | _ ->
          raise OpaqueModule
      end
    | _ -> 
      Format.fprintf Format.err_formatter "Got this instead: %a\n%!" Component.Fmt.module_type_expr mty;
      failwith "Application must take a functor"
  in
  let arg_id, result = find_functor mty' in
  let new_module = { m with Component.Module.type_ = ModuleType result } in
  let path = `Apply (func_path, (`Resolved (`Substituted arg_path))) in
  let substitution = if is_resolve then `Substituted arg_path else arg_path in
  ( path,
    Subst.module_
      (Subst.add_module arg_id substitution Subst.identity)
      new_module )

and add_canonical_path : Env.t -> Component.Module.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
  fun env m p ->
  match p with
  | `Canonical _ -> p
  | _ -> (
      match m.Component.Module.canonical with
      | Some (cp, _cr) -> (
          if !is_compile
          then (
            (* Format.fprintf Format.err_formatter "XXX skipping canonical resolution\n%!"; *)
            `Canonical (p, cp)
          )
          else (
            (* Format.fprintf Format.err_formatter "XXX Handling canonical path for %a\n%!" (Component.Fmt.resolved_module_path) p; *)
            match lookup_and_resolve_module_from_path true false env cp with
            | Resolved (`Alias (_, p2'), _) ->
                  `Canonical ( p, `Resolved (simplify_resolved_module_path env p2'))
            | Resolved (p2', _) ->
                  `Canonical ( p, `Resolved (simplify_resolved_module_path env p2'))
            | Unresolved p' ->
                (* Format.fprintf Format.err_formatter "XXX No idea :/\n%!"; *)
                `Canonical (p, p')
            | exception _e ->
                Format.fprintf Format.err_formatter
                  "Tools.add_canonical_path: Warning: Failed to look up \
                   canonical path for module %a\n\
                   %s\n\
                   %!"
                  Component.Fmt.resolved_module_path p
                  (Printexc.get_backtrace ());
                p ))
      | None -> p )

and get_substituted_module_type : Env.t -> Component.ModuleType.expr -> Cpath.Resolved.module_type option =
  fun env expr ->
      (* Format.fprintf Format.err_formatter ">>>expr=%a\n%!"  Component.Fmt.module_type_expr expr; *)
      match expr with
        | Component.ModuleType.Path p' -> begin
          match lookup_and_resolve_module_type_from_path true env p' with
          | Resolved (resolved_path, _) ->
            (* Format.fprintf Format.err_formatter "<<<resolved_path: %a\n%!" Component.Fmt.resolved_module_type_path resolved_path; *)
            if Cpath.is_resolved_module_type_substituted resolved_path
            then Some resolved_path
            else None
          | Unresolved _ ->
            (* Format.fprintf Format.err_formatter "<<<Unresolved!?\n%!";*) None
          end
        | _ ->
          (* Format.fprintf Format.err_formatter "<<<wtf!?\n%!"; *) None

and process_module_type env m p' =
  let open Component.ModuleType in
  let open OptionMonad in
  (* Format.fprintf Format.err_formatter "Processing module_type %a\n%!" Component.Fmt.resolved_module_type_path p'; *)
  (* Loop through potential chains of module_type equalities, looking for substitutions *)
  let substpath =
    m.expr >>= get_substituted_module_type env >>= fun p -> Some (`SubstT(p, p'))
  in
  match substpath with
  | Some p -> p
  | None -> p'

and get_module_path_modifiers : Env.t -> bool -> Component.Module.t -> _ option =
  fun env add_canonical m ->
    match m.type_ with
    | Alias alias_path -> begin
      (* Format.fprintf Format.err_formatter "alias to path: %a\n%!" Component.Fmt.module_path alias_path; *)
      match lookup_and_resolve_module_from_path true add_canonical env alias_path with
      | Resolved (resolved_alias_path, _) -> 
        if Cpath.is_resolved_module_substituted resolved_alias_path
        then Some (`SubstAliased resolved_alias_path)
        else Some (`Aliased resolved_alias_path)
      | Unresolved _ -> None
    end
    | ModuleType t -> begin
        match get_substituted_module_type env t with
        | Some s -> Some (`SubstMT s)
        | None -> None
    end
and process_module_path env add_canonical m p =
  let p = if m.Component.Module.hidden then `Hidden p else p in
  let p' = match get_module_path_modifiers env add_canonical m with
    | None -> p
    | Some (`SubstAliased p') -> `SubstAlias (p', p)
    | Some (`Aliased p') -> `Alias (p', p)
    | Some (`SubstMT p') -> `Subst (p', p)
  in
  if add_canonical then add_canonical_path env m p' else p'
  

and handle_module_lookup env add_canonical id parent sg =
  match Find.careful_module_in_sig sg id with
  | Find.Found m' ->
      let p' = `Module (parent, Odoc_model.Names.ModuleName.of_string id) in
      Some (process_module_path env add_canonical m' p', m')
  | Replaced p ->
      Some (p, lookup_module env p)
  | exception _e -> None

and handle_module_type_lookup env id p sg =
  let mt = Find.module_type_in_sig sg id in
  let p' = (`ModuleType (p, Odoc_model.Names.ModuleTypeName.of_string id)) in
  let p'' = process_module_type env mt p' in
  (p'', mt)

and handle_type_lookup id p sg : type_lookup_result =
  try
    let mt = Find.careful_type_in_sig sg id in
    (`Type (p, Odoc_model.Names.TypeName.of_string id), mt)
  with e ->
    Format.fprintf Format.err_formatter
      "failed to find type in path: %a\n%!"
      Component.Fmt.resolved_parent_path p;
    Format.fprintf Format.err_formatter "Signature: %a\n%!"
      Component.Fmt.signature sg;
    raise e

and handle_class_type_lookup env id p m =
  let sg = prefix_signature (`Module p, signature_of_module_cached env p true m) in
  let c = Find.class_type_in_sig sg id in
  (`ClassType (`Module p, Odoc_model.Names.ClassTypeName.of_string id), c)

and lookup_module : Env.t -> Cpath.Resolved.module_ -> Component.Module.t =
  fun env' path ->
  let id = path in
  let env_id = Env.id env' in
  let lookup env =
    match path with
    | `Local _ -> raise (Module_lookup_failure (env, path))
    | `Identifier i -> Env.lookup_module i env
    | `Substituted x -> lookup_module env x
    | `Apply (functor_path, `Resolved argument_path) ->
      let functor_module = lookup_module env functor_path in
      snd (handle_apply false env functor_path argument_path functor_module)
    | `Module (parent, name) -> begin
      let sg = match parent with
        | `Module mp ->
          let parent_module = lookup_module env mp in
          prefix_signature (parent, signature_of_module_cached env mp false parent_module)
        | `ModuleType mtyp ->
          let parent_module_type = lookup_module_type env mtyp in
          prefix_signature (parent, signature_of_module_type env parent_module_type)
        | `FragmentRoot ->
          let _,sg = Env.lookup_fragment_root env in
          sg
      in
      match Find.careful_module_in_sig sg (ModuleName.to_string name) with
      | Find.Found m -> m
      | Replaced p -> lookup_module env p
      end
    | `Alias (_, p) -> lookup_module env p
    | `Subst (_, p) -> lookup_module env p
    | `SubstAlias (_, p) -> lookup_module env p
    | `Hidden p -> lookup_module env p
    | `Canonical (p, _) -> lookup_module env p
    | `Apply (_, _) -> failwith "Unresolved argument to apply"
  in
  match Memos1.find_all module_lookup_cache id with
  | [] ->
      let lookups, resolved = Env.with_recorded_lookups env' lookup in
      Memos1.add module_lookup_cache id (resolved, env_id, lookups);
      resolved
  | xs ->
      let rec find_fast = function
        | (result, id, _lookups) :: _ when id = env_id -> result
        | _ :: ys -> find_fast ys
        | [] -> find xs
      and find = function
        | (m, _, lookups) :: xs ->
            if Env.verify_lookups env' lookups then m else find xs
        | [] ->
            let lookups, m = Env.with_recorded_lookups env' lookup in
            Memos1.add module_lookup_cache id (m, env_id, lookups);
            m
      in
      find_fast xs


and reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
fun env path ->
  match path with
  | `Local _
  | `Identifier _ -> path
  | `Substituted x -> `Substituted (reresolve_module env x)
  | `Apply (functor_path, `Resolved argument_path) -> `Apply (reresolve_module env functor_path, `Resolved (reresolve_module env argument_path))
  | `Module (parent, name) -> `Module (reresolve_parent env parent, name)
  | `Alias (p1, p2) -> `Alias (reresolve_module env p1, reresolve_module env p2)
  | `Subst (p1, p2) -> `Subst (reresolve_module_type env p1, reresolve_module env p2)
  | `SubstAlias (p1, p2) -> `SubstAlias (reresolve_module env p1, reresolve_module env p2)
  | `Hidden p ->
    let p' = reresolve_module env p in
    `Hidden p'
  | `Canonical (p, `Resolved p2) -> `Canonical (reresolve_module env p, `Resolved (reresolve_module env p2))
  | `Canonical (p, p2) -> begin
    match lookup_and_resolve_module_from_path true false env p2 with
    | Resolved (`Alias (_, p2'),_) ->
      `Canonical (reresolve_module env p, `Resolved (simplify_resolved_module_path env p2'))
    | Resolved (p2', _) ->
      (* See, e.g. Base.Sexp for an example of where the canonical path might not be
        a simple alias *)
      `Canonical (reresolve_module env p, `Resolved (simplify_resolved_module_path env p2'))
    | Unresolved _
    | exception _ ->
      `Canonical (reresolve_module env p, p2)
    end
  | `Apply (p, p2) -> begin
    match lookup_and_resolve_module_from_path true false env p2 with
    | Resolved (p2',_) ->  `Apply (reresolve_module env p, `Resolved p2')
    | Unresolved p2' -> `Apply (reresolve_module env p, p2')
    end

and reresolve_parent : Env.t -> Cpath.Resolved.parent -> Cpath.Resolved.parent =
    fun env path ->
    match path with
    | `Module m -> `Module (reresolve_module env m)
    | `ModuleType mty -> `ModuleType (reresolve_module_type env mty)
    | `FragmentRoot -> path

and reresolve_module_type : Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
    fun env path ->
      match path with
      | `Local _
      | `Identifier _ -> path
      | `Substituted x -> `Substituted (reresolve_module_type env x)
      | `ModuleType (parent, name) -> `ModuleType (reresolve_parent env parent, name)
      | `SubstT (p1, p2) -> `SubstT (reresolve_module_type env p1, reresolve_module_type env p2)

and reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
    fun env path ->
      let result =
        match path with
        | `Identifier _
        | `Local _ -> path
        | `Substituted s -> `Substituted (reresolve_type env s)
        | `Type (p, n) -> `Type (reresolve_parent env p, n)
        | `Class (p, n) -> `Class (reresolve_parent env p, n)
        | `ClassType (p, n) -> `ClassType (reresolve_parent env p, n)
      in
      result
and lookup_module_type : Env.t -> Cpath.Resolved.module_type -> Component.ModuleType.t =
  fun env path ->
    let lookup env =
      match path with
      | `Local _ -> raise (ModuleType_lookup_failure (env, path))
      | `Identifier i -> Env.lookup_module_type i env
      | `Substituted s -> lookup_module_type env s
      | `SubstT (_, p2) -> lookup_module_type env p2
      | `ModuleType (parent, name) -> begin
        let sg =
          match parent with
          | `Module mp ->
            let parent_module = lookup_module env mp in
            prefix_signature (parent, signature_of_module_cached env mp false parent_module)
          | `ModuleType mtyp ->
            let parent_module_type = lookup_module_type env mtyp in
            prefix_signature (parent, signature_of_module_type env parent_module_type)
          | `FragmentRoot ->
            let _,sg = Env.lookup_fragment_root env in
            sg
        in
        Find.module_type_in_sig sg (ModuleTypeName.to_string name)
        end
    in lookup env

(* *)

and lookup_and_resolve_module_from_path :
    bool ->
    bool ->
    Env.t ->
    Cpath.module_ ->
    (module_lookup_result, Cpath.module_) ResultMonad.t =
 fun is_resolve add_canonical env' p ->
  let open ResultMonad in
  let id = (is_resolve, add_canonical, p) in
  let env_id = Env.id env' in
  (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_from_path: looking up %a\n%!" Component.Fmt.path p; *)
  let resolve : Env.t -> (module_lookup_result, Cpath.module_) ResultMonad.t =
    fun env ->
    match p with
    | `Dot (parent, id) -> (
        lookup_and_resolve_module_from_path is_resolve add_canonical env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let sg = prefix_signature (`Module p, signature_of_module_cached env p is_resolve m) in
        match handle_module_lookup env add_canonical id (`Module p) sg with
        | Some (p'', m') -> return (p'', m')
        | None -> Unresolved (`Dot (parent, id))
    )
    | `Apply (m1, m2) -> (
        let func =
          lookup_and_resolve_module_from_path is_resolve add_canonical env m1
        in
        let arg =
          lookup_and_resolve_module_from_path is_resolve add_canonical env m2
        in
        match (func, arg) with
        | Resolved (func_path', m), Resolved (arg_path', _) ->
            return (handle_apply is_resolve env func_path' arg_path' m)
        | Unresolved func_path', Resolved (arg_path', _) ->
            Unresolved (`Apply (func_path', `Resolved arg_path'))
        | Resolved (_func_path', _), Unresolved _arg_path' ->
            failwith "Unable to find argument"
            (* Unresolved (`Apply (`Resolved func_path', arg_path')) *)
        | Unresolved _func_path', Unresolved _arg_path' ->
            failwith "Unable to find argument"    
            (* Unresolved (`Apply (func_path', arg_path')) ) *)
    )
    | `Resolved (`Identifier i as resolved_path) ->
        let m = Env.lookup_module i env in
        return (process_module_path env add_canonical m resolved_path, m)
    | `Resolved r ->
        let m = lookup_module env r in
        return (process_module_path env add_canonical m r, m)
    | `Substituted s ->
        lookup_and_resolve_module_from_path is_resolve add_canonical env s
        |> map_unresolved (fun p -> `Substituted p)
        >>= fun (p, m) -> return (`Substituted p, m)
    | `Root r -> (
          (* Format.fprintf Format.err_formatter "Looking up module %s by name...%!" r; *)
        match Env.lookup_root_module r env with
        | Some (Env.Resolved (p, m)) ->
          (* Format.fprintf Format.err_formatter "Got it!\n%!"; *)
          return (process_module_path env add_canonical m (`Identifier p), m)
        | Some Env.Forward ->
          (* Format.fprintf Format.err_formatter "Forward :-(!\n%!"; *)
          Unresolved (`Forward r)
        | None ->
          (* Format.fprintf Format.err_formatter "Unresolved!\n%!"; *)
          Unresolved p )
    | `Forward f ->
        lookup_and_resolve_module_from_path is_resolve add_canonical env
          (`Root f)
        |> map_unresolved (fun _ -> `Forward f)
  in
  match Memos2.find_all module_resolve_cache id with
  | [] ->
      (* Format.fprintf Format.err_formatter "Looking up path (no bindings) %a\n%!" Component.Fmt.module_path p; *)
      (* Format.fprintf Format.err_formatter "Uncached\n%!"; *)
      let lookups, resolved =
        try
          Env.with_recorded_lookups env' resolve
        with e ->
          Format.fprintf Format.err_formatter "Caught exception: %s\n%!" (Printexc.to_string e);
          raise e
          in
      (* Format.fprintf Format.err_formatter "Adding into hashtbl\n%!"; *)
      Memos2.add module_resolve_cache id (resolved, env_id, lookups);
      resolved
  | xs ->
      let rec find_fast = function
        | (result, id, _lookups) :: _ when id = env_id ->
          (* Format.fprintf Format.err_formatter "cached\n%!";*) result 
        | _ :: ys -> find_fast ys
        | [] -> find xs
      and find = function
        | (r, _, lookups) :: xs ->
            if Env.verify_lookups env' lookups
            then (
              (* Format.fprintf Format.err_formatter "cached\n%!"; *)r) 
            else find xs
        | [] ->
            let lookups, result = Env.with_recorded_lookups env' resolve in
            Memos2.add module_resolve_cache id (result, env_id, lookups);
            result
      in
      find_fast xs

and resolve_module env p =
  let open ResultMonad in
  (* Format.fprintf Format.err_formatter "resolve_module: %a\n%!" Component.Fmt.module_path p; *)
  lookup_and_resolve_module_from_path true true env p >>= fun (p, _) -> return p
  

and lookup_and_resolve_module_type_from_path :
    bool ->
    Env.t ->
    Cpath.module_type ->
    (module_type_lookup_result, Cpath.module_type) ResultMonad.t =
  let open ResultMonad in
  fun is_resolve env p ->
    (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_type_from_path: looking up %a\n%!" Component.Fmt.module_type_path p; *)
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path is_resolve true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let sg = prefix_signature (`Module p, signature_of_module_cached env p is_resolve m) in
        let p', mt = handle_module_type_lookup env id (`Module p) sg in
        return (p', mt)
    | `Resolved r ->
        return (r, lookup_module_type env r)
    | `Substituted s ->
        lookup_and_resolve_module_type_from_path is_resolve env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and lookup_type_from_resolved_path :
    Env.t -> Cpath.Resolved.type_ -> type_lookup_result =
 fun env p ->
  (* let start_time = Unix.gettimeofday () in *)
  (* Format.fprintf Format.err_formatter "lookup_type_from_resolved_path\n%!"; *)
  let res = match p with
  | `Local _id -> raise (Type_lookup_failure (env, p))
  | `Identifier (`CoreType name) ->
      (* CoreTypes aren't put into the environment, so they can't be handled by the
            next clause. We just look them up here in the list of core types *)
      ( `Identifier (`CoreType name),
        Find.Found (`T (List.assoc (TypeName.to_string name) core_types)) )
  | `Identifier (`Type _ as i) ->
      let t = Env.lookup_type i env in
      (`Identifier i, Found (`T t))
  | `Identifier (`Class _ as i) ->
      let t = Env.lookup_class i env in
      (`Identifier i, Found (`C t))
  | `Identifier (`ClassType _ as i) ->
      let t = Env.lookup_class_type i env in
      (`Identifier i, Found (`CT t))
  | `Substituted s ->
      let p, t = lookup_type_from_resolved_path env s in
      (`Substituted p, t)
  | `Type (`Module p, id) -> (
      try
        (* let t0 = Unix.gettimeofday () in *)
        let m = lookup_module env p in
        (* let t1 = Unix.gettimeofday () in *)
        let sg = signature_of_module_cached env p true m in
        (* let t2 = Unix.gettimeofday () in *)
        let sub = prefix_substitution (`Module p) sg in
        (* let t3 = Unix.gettimeofday () in *)
        let p', t' = handle_type_lookup (TypeName.to_string id) (`Module p) sg in
        (* let t4 = Unix.gettimeofday () in *)
        let t = match t' with
          | Find.Found (`C c) -> Find.Found (`C (Subst.class_ sub c))
          | Find.Found (`CT ct) -> Find.Found (`CT (Subst.class_type sub ct))
          | Find.Found (`T t) -> Find.Found (`T (Subst.type_ sub t))
          | Find.Replaced texpr -> Find.Replaced (Subst.type_expr sub texpr) in 
        (* let t5 = Unix.gettimeofday () in *)
        (* Format.fprintf Format.err_formatter "timing stats: %f %f %f %f %f\n%!" (t1 -. t0) (t2 -. t1) (t3 -. t2) (t4 -. t3) (t5 -. t4); *)
          (p', t) 
      with e ->
        Format.fprintf Format.err_formatter "Here...\n%s\n%!"
          (Printexc.get_backtrace ());
        ( match p with
        | `Identifier _ident ->
            Format.fprintf Format.err_formatter "Identifier\n%!"
        | _ -> Format.fprintf Format.err_formatter "Not ident\n%!" );
        raise e )
  | `Class (`Module p, id) ->
  let m = lookup_module env p in
  let sg = signature_of_module_cached env p true m in
  let sub = prefix_substitution (`Module p) sg in
  let p', t' = handle_type_lookup (ClassName.to_string id) (`Module p) sg in
  let t = match t' with
    | Find.Found (`C c) -> Find.Found (`C (Subst.class_ sub c))
    | Find.Found (`CT ct) -> Find.Found (`CT (Subst.class_type sub ct))
    | Find.Found (`T t) -> Find.Found (`T (Subst.type_ sub t))
    | Find.Replaced texpr -> Find.Replaced (Subst.type_expr sub texpr) in 
  (p', t) 
  | `ClassType (`Module p, id) ->
  let m = lookup_module env p in
  let sg = signature_of_module_cached env p true m in
  let sub = prefix_substitution (`Module p) sg in
  let p', t' = handle_type_lookup (ClassTypeName.to_string id) (`Module p) sg in
  let t = match t' with
    | Find.Found (`C c) -> Find.Found (`C (Subst.class_ sub c))
    | Find.Found (`CT ct) -> Find.Found (`CT (Subst.class_type sub ct))
    | Find.Found (`T t) -> Find.Found (`T (Subst.type_ sub t))
    | Find.Replaced texpr -> Find.Replaced (Subst.type_expr sub texpr) in 
  (p', t) 
  | `Type (`ModuleType _, _) -> failwith "Unhandled 3"
  | `ClassType (`ModuleType _, _) -> failwith "Unhandled 4"
  | `Class (`ModuleType _, _) -> failwith "Unhandled 5"
  | `Type (`FragmentRoot, _) -> failwith "Unhandled 11"
  | `ClassType (`FragmentRoot, _) -> failwith "Unhandled 12"
  | `Class (`FragmentRoot, _) -> failwith "Unhandled 13"
  in
        (* let end_time = Unix.gettimeofday () in *)
        (* Format.fprintf Format.err_formatter "lookup_type_from_resolved_path finished: %f\n%!" (end_time -. start_time); *)
        res

and lookup_type_from_path :
    Env.t -> Cpath.type_ -> (type_lookup_result, Cpath.type_) ResultMonad.t =
  let open ResultMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) ->
        (* let start_time = Unix.gettimeofday () in *)
        lookup_and_resolve_module_from_path true true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        (* let time1 = Unix.gettimeofday () in *)
        let sg = signature_of_module_cached env p true m in
        (* let time1point5 = Unix.gettimeofday () in *)
        let sub = prefix_substitution (`Module p) sg  in
        (* let time2 = Unix.gettimeofday () in *)
        let p', t' = handle_type_lookup id (`Module p) sg in
        let t = match t' with
          | Find.Found (`C c) -> Find.Found (`C (Subst.class_ sub c))
          | Find.Found (`CT ct) -> Find.Found (`CT (Subst.class_type sub ct))
          | Find.Found (`T t) -> Find.Found (`T (Subst.type_ sub t))
          | Find.Replaced texpr -> Find.Replaced (Subst.type_expr sub texpr) in 
        (* let time3 = Unix.gettimeofday () in *)
        (* Format.fprintf Format.err_formatter "lookup: %f vs sig_of_mod: %f vs prefix_sub: %f vs rest: %f\n%!" (time1 -. start_time) (time1point5 -. time1) (time2 -. time1point5) (time3 -. time2); *)
        return (p', t) 
    | `Resolved r -> return (lookup_type_from_resolved_path env r)
    | `Substituted s ->
        lookup_type_from_path env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and lookup_class_type_from_resolved_path :
    Env.t -> Cpath.Resolved.class_type -> class_type_lookup_result =
 fun env p ->
  match p with
  | `Local _id -> raise (ClassType_lookup_failure (env, p))
  | `Identifier (`Class _ as c) ->
      let t = Env.lookup_class c env in
      (`Identifier c, `C t)
  | `Identifier (`ClassType _ as c) ->
      let t = Env.lookup_class_type c env in
      (`Identifier c, `CT t)
  | `Substituted s ->
      let p, t = lookup_class_type_from_resolved_path env s in
      (`Substituted p, t)
  | `Class (`Module p, id) ->
      let m = lookup_module env p in
      let p', t = handle_class_type_lookup env (ClassName.to_string id) p m in
      (p', t)
  | `ClassType (`Module p, id) ->
      let m = lookup_module env p in
      let p', t = handle_class_type_lookup env (ClassTypeName.to_string id) p m in
      (p', t)
  | `ClassType (`ModuleType _, _) -> failwith "Unhandled 6"
  | `Class (`ModuleType _, _) -> failwith "Unhandled 7"
  | `ClassType (`FragmentRoot, _) -> failwith "Unhandled 14"
  | `Class (`FragmentRoot, _) -> failwith "Unhandled 15"
    
and lookup_class_type_from_path :
    Env.t ->
    Cpath.class_type ->
    (class_type_lookup_result, Cpath.class_type) ResultMonad.t =
  let open ResultMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path true true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let p', c = handle_class_type_lookup env id p m in
        return (p', c)
    | `Resolved r -> return (lookup_class_type_from_resolved_path env r)
    | `Substituted s ->
        lookup_class_type_from_path env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, c) -> return (`Substituted p, c)

and lookup_signature_from_resolved_fragment :
    Env.t ->
    Cpath.Resolved.module_ ->
    Fragment.Resolved.Signature.t ->
    Component.Signature.t ->
    Cpath.Resolved.module_ * Component.Signature.t =
 fun env p f s ->
  match f with
  | `Root _ -> (p, s)
  | #Fragment.Resolved.Module.t as frag ->
      let _, p, m = lookup_module_from_resolved_fragment env p frag s in
      p, signature_of_module env m

and lookup_module_from_resolved_fragment :
    Env.t ->
    Cpath.Resolved.module_ ->
    Fragment.Resolved.Module.t ->
    Component.Signature.t ->
    Ident.module_ * Cpath.Resolved.module_ * Component.Module.t =
 fun env p f s ->
  match f with
  | `Subst (_, _) | `SubstAlias (_, _) -> failwith "What do we do with these?"
  | `Module (parent, name) ->
      let ppath, sg = lookup_signature_from_resolved_fragment env p parent s in
      let rec find = function
        | Component.Signature.Module (id, _, m') :: _
          when Ident.Name.module_ id
               = Odoc_model.Names.ModuleName.to_string name ->
            ( id,
              `Module (`Module ppath, ModuleName.of_string (Ident.Name.module_ id)),
              Subst.delayed_get_module m' )
        | _ :: xs -> find xs
        | [] -> failwith "Can't find it"
      in
      find sg.items

and lookup_module_from_fragment :
    Env.t ->
    Cpath.Resolved.module_ ->
    Fragment.Module.t ->
    Component.Signature.t ->
    Ident.module_ * Cpath.Resolved.module_ * Component.Module.t =
 fun env p f s ->
  match f with
  | `Dot (parent, name) ->
      let ppath, sg = lookup_signature_from_fragment env p parent s in
      let rec find = function
        | Component.Signature.Module (id, _, m') :: _
          when Ident.Name.module_ id = name ->
            (id, `Module (`Module ppath, (ModuleName.of_string name)), Subst.delayed_get_module m')
        | _ :: xs -> find xs
        | [] -> failwith "Can't find it"
      in
      find sg.items
  | `Resolved r -> lookup_module_from_resolved_fragment env p r s

and lookup_signature_from_fragment :
    Env.t ->
    Cpath.Resolved.module_ ->
    Fragment.Signature.t ->
    Component.Signature.t ->
    Cpath.Resolved.module_ * Component.Signature.t =
 fun env p f s ->
  match f with
  | `Root -> (p, s)
  | `Dot (_, _) as f' ->
      let _, p, m = lookup_module_from_fragment env p f' s in
      p, signature_of_module env m
  | `Resolved r -> lookup_signature_from_resolved_fragment env p r s

and module_type_expr_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    Component.ModuleType.expr =
 fun env decl ->
  match decl with
  | Component.Module.Alias (`Resolved r) ->
      let m = lookup_module env r in
      module_type_expr_of_module_decl env m.type_
  | Component.Module.Alias path -> (
      match lookup_and_resolve_module_from_path false true env path with
      | Resolved (_, y) -> module_type_expr_of_module env y
      | Unresolved p when Cpath.is_module_forward p ->
          raise UnresolvedForwardPath
      | Unresolved p' ->
          let err =
            Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
              Component.Fmt.module_path path Component.Fmt.module_path p'
          in
          failwith err )
  | Component.Module.ModuleType expr -> expr

and module_type_expr_of_module :
    Env.t ->
    Component.Module.t ->
    Component.ModuleType.expr =
 fun env m -> module_type_expr_of_module_decl env m.type_

and signature_of_module_alias :
    Env.t -> Cpath.module_ -> Component.Signature.t =
 fun env path ->
  match lookup_and_resolve_module_from_path false true env path with
  | Resolved (p', m) ->
      (* p' is the path to the aliased module *)
      let m' = signature_of_module_cached env p' false m in
      let m'' = Strengthen.signature p' m' in
      m''
  | Unresolved p when Cpath.is_module_forward p -> raise UnresolvedForwardPath
  | Unresolved p' ->
      let err =
        Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
          Component.Fmt.module_path path Component.Fmt.module_path p'
      in
      failwith err

and handle_signature_with_subs :
    Env.t ->
    Component.Signature.t ->
    Component.ModuleType.substitution list ->
    Component.Signature.t =
 fun env sg subs ->
  List.fold_left
    (fun sg sub ->
      match sub with
      | Component.ModuleType.ModuleEq (frag, _) ->
          fragmap_module env frag sub sg
      | ModuleSubst (frag, _) -> fragmap_module env frag sub sg
      | TypeEq (frag, _) -> fragmap_type env frag sub sg
      | TypeSubst (frag, _) -> fragmap_type env frag sub sg)
    sg subs

and signature_of_module_type_expr :
    Env.t -> Component.ModuleType.expr -> Component.Signature.t =
 fun env m ->
  match m with
  | Component.ModuleType.Path p -> (
      match lookup_and_resolve_module_type_from_path false env p with
      | Resolved (_, mt) -> signature_of_module_type env mt
      | Unresolved _p ->
          let p = Component.Fmt.(string_of module_type_path p) in
          failwith (Printf.sprintf "Couldn't find signature: %s" p) )
  | Component.ModuleType.Signature s -> s
  | Component.ModuleType.With (s, subs) ->
      let sg = signature_of_module_type_expr env s in
      handle_signature_with_subs env sg subs
  | Component.ModuleType.Functor (Unit, expr) ->
      signature_of_module_type_expr env expr
  | Component.ModuleType.Functor (Named arg, expr) ->
      ignore arg;
      signature_of_module_type_expr env expr
  | Component.ModuleType.TypeOf decl -> signature_of_module_decl env decl

and signature_of_module_type :
    Env.t -> Component.ModuleType.t -> Component.Signature.t =
 fun env m ->
  match m.expr with
  | None -> raise OpaqueModule
  | Some expr -> signature_of_module_type_expr env expr

and signature_of_module_decl :
    Env.t -> Component.Module.decl -> Component.Signature.t =
 fun env decl ->
  match decl with
  | Component.Module.Alias path -> signature_of_module_alias env path
  | Component.Module.ModuleType expr ->
      signature_of_module_type_expr env expr

and signature_of_module :
    Env.t -> Component.Module.t -> Component.Signature.t =
 fun env m -> signature_of_module_decl env m.type_

and signature_of_module_cached :
    Env.t -> Cpath.Resolved.module_ -> bool -> Component.Module.t -> Component.Signature.t =
  fun env' path is_resolve m ->
    let id = (is_resolve, path) in
    let run env = signature_of_module env m in
    let env_id = Env.id env' in
    match Memos3.find_all module_signature_cache id with
    | [] ->
      let lookups, sg = Env.with_recorded_lookups env' run in
      Memos3.add module_signature_cache id (sg, env_id, lookups);
      sg
    | xs ->
      let rec find_fast = function
        | (result, id, _lookups) :: _ when id = env_id ->
            (* let cached = Format.asprintf "%a" Component.Fmt.signature result in *)
          (* let uncached = Format.asprintf "%a" Component.Fmt.signature (run env') in *)
          (* if (String.compare cached uncached) <> 0 then (
            Format.fprintf Format.err_formatter "failed with path: %a\n%!" Component.Fmt.resolved_module_path path;
            Format.fprintf Format.err_formatter "cached sig:\n%s\nuncached sig:\n%s\n\n%!" cached uncached;
            Format.fprintf Format.err_formatter "lookups: %a\n%!" Env.pp_lookup_type_list _lookups;
            Format.fprintf Format.err_formatter "env_id: %d\n%!" env_id;
            failwith "bah"
          ); *)
          result
        | _ :: ys -> find_fast ys
        | [] -> find xs
      and find = function
        | (result, _, lookups) :: xs ->
            if Env.verify_lookups env' lookups
            then (
              (* let cached = Format.asprintf "%a" Component.Fmt.signature result in
              let uncached = Format.asprintf "%a" Component.Fmt.signature (run env') in
              if (String.compare cached uncached) <> 0 then (
                Format.fprintf Format.err_formatter "failed with path: %a\n%!" Component.Fmt.resolved_module_path path;
                Format.fprintf Format.err_formatter "cached sig:\n%s\nuncached sig:\n%s\n\n%!" cached uncached;
                Format.fprintf Format.err_formatter "lookups: %a\n%!" Env.pp_lookup_type_list lookups;
                failwith "bah"
              ); *)
              result)
            else find xs
        | [] ->
            let lookups, sg = Env.with_recorded_lookups env' run in
            Memos3.add module_signature_cache id (sg, env_id, lookups);
            sg
      in
      find_fast xs


and opt_map f = function None -> None | Some x -> Some (f x)

and fragmap_module :
    Env.t ->
    Cfrag.module_ ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    Component.Signature.t =
 fun env frag sub sg ->
  let name, frag' = Cfrag.module_split frag in
  let map_module m =
    match (frag', sub) with
    | None, ModuleEq (_, type_) ->
        let type_ = match type_ with
          | Alias (`Resolved p) -> Component.Module.Alias (`Resolved (`Substituted p))
          | Alias _
          | ModuleType _ -> type_
        in
        (* Finished the substitution *)
        Left { m with Component.Module.type_; expansion = None }
    | None, ModuleSubst (_, p) -> (
        match lookup_and_resolve_module_from_path false false env p with
        | Resolved (p, _) -> Right p
        | Unresolved p ->
          Format.fprintf Format.err_formatter "failed to resolve path: %a\n%!" Component.Fmt.module_path p;
          failwith "Can't resolve module substitution path" )
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
      (fun item (items, handled, removed) ->
        match item with
        | Component.Signature.Module (id, r, m)
          when Ident.Name.module_ id = name -> (
            let m = Subst.delayed_get_module m in
            match map_module m with
            | Left m ->
                ( Component.Signature.Module
                    (id, r, Component.(Substitution.NoSubst m))
                  :: items,
                  true,
                  removed )
            | Right p ->
                (items, true, Component.Signature.RModule (id, p) :: removed) )
        | Component.Signature.Include i ->
            let items', handled', removed' = handle_items i.expansion_.items in
            let expansion =
              Component.Signature.{ items = items'; removed = removed' }
            in
            let component =
              if handled' then
                Component.Signature.Include (map_include i expansion)
              else Component.Signature.Include { i with expansion_ = expansion }
            in
            (component :: items, handled || handled', removed @ removed')
        | x -> (x :: items, handled, removed))
      items ([], false, [])
  in
  let items, _handled, removed = handle_items sg.items in

  let sub_of_removed removed sub =
    match removed with
    | Component.Signature.RModule (id, p) -> Subst.add_module id p sub
    | _ -> sub
  in
  let sub = List.fold_right sub_of_removed removed Subst.identity in
  let res =
    Subst.signature sub
      { Component.Signature.items; removed = removed @ sg.removed }
  in
  (* Format.(
     fprintf err_formatter "after sig=%a\n%!" Component.Fmt.(signature) res); *)
  res

and fragmap_type :
    Env.t ->
    Cfrag.type_ ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    Component.Signature.t =
 fun _env frag sub sg ->
  let name, frag' =  Cfrag.type_split frag in
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
              when Ident.Name.type_ id = name -> (
                match mapfn (Subst.delayed_get_type t) with
                | Left x ->
                    (Component.Signature.Type (id, r, (Component.Substitution.NoSubst x)) :: items, true, removed)
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
                let m = Subst.delayed_get_module m in
                let item =
                  Component.Signature.Module
                    (id, r, Component.Substitution.NoSubst (mapfn m))
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

and find_external_module_path : Cpath.Resolved.module_ -> Cpath.Resolved.module_ option =
  fun p ->
    let open OptionMonad in
    match p with
    | `Subst (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_path y >>= fun y ->
      Some (`Subst (x, y))
    | `Module (p, n) ->
      find_external_parent_path p >>= fun p ->
      Some (`Module (p, n))
    | `Local x -> Some (`Local x)
    | `Substituted x ->
      find_external_module_path x >>= fun x ->
      Some (`Substituted x)
    | `SubstAlias (x,y) -> begin
      match (find_external_module_path x, find_external_module_path y) with
      | Some x, Some y -> Some (`SubstAlias (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None
      end
    | `Canonical (x, y) ->
      find_external_module_path x >>= fun x -> Some (`Canonical (x, y))
    | `Hidden x ->
      find_external_module_path x >>= fun x -> Some (`Hidden x)
    | `Alias (x,y) -> begin
      match (find_external_module_path x, find_external_module_path y) with
      | Some x, Some y -> Some (`Alias (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None
      end
    | `Apply (x,`Resolved y) ->
      find_external_module_path x >>= fun x ->
      find_external_module_path y >>= fun y -> 
      Some (`Apply (x, `Resolved y))
    | `Apply (x, y) ->
      find_external_module_path x >>= fun x ->
      Some (`Apply (x, y))
    | `Identifier x -> Some (`Identifier x)

and find_external_module_type_path : Cpath.Resolved.module_type -> Cpath.Resolved.module_type option =
  fun p ->
    let open OptionMonad in
    match p with
    | `ModuleType (p, name) ->
      find_external_parent_path p >>= fun p -> Some (`ModuleType (p, name))
    | `Local _ -> Some p
    | `SubstT (x,y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_type_path y >>= fun y ->
      Some (`SubstT(x,y))
    | `Substituted x ->
      find_external_module_type_path x >>= fun x ->
      Some (`Substituted x)
    | `Identifier _ -> Some p

and find_external_parent_path : Cpath.Resolved.parent -> Cpath.Resolved.parent option =
  fun p ->
    let open OptionMonad in
    match p with
    | `Module m ->
      find_external_module_path m >>= fun m -> Some (`Module m)
    | `ModuleType m ->
      find_external_module_type_path m >>= fun m -> Some (`ModuleType m)
    | `FragmentRoot -> None

and fixup_module_cfrag (f : Cfrag.resolved_module) : Cfrag.resolved_module  =
  match f with
  | `Subst (path, frag) -> begin
    match find_external_module_type_path path with
    | Some p -> `Subst (p, frag)
    | None -> frag
    end
  | `SubstAlias (path, frag) -> begin
    match find_external_module_path path with
    | Some p -> `SubstAlias (p, frag)
    | None -> frag
    end 
  | `Module (parent, name) -> `Module (fixup_signature_cfrag parent, name)

and fixup_signature_cfrag (f : Cfrag.resolved_signature) =
  match f with
  | `Root x -> `Root x
  | `Subst _ | `SubstAlias _ | `Module _ as f -> (fixup_module_cfrag f :> Cfrag.resolved_signature)

and fixup_type_cfrag (f : Cfrag.resolved_type) : Cfrag.resolved_type =
  match f with
  | `Type (p, x) -> `Type (fixup_signature_cfrag p, x)
  | `Class (p, x) -> `Class (fixup_signature_cfrag p, x)
  | `ClassType (p, x) -> `ClassType (fixup_signature_cfrag p, x)


and find_module_with_replacement :
    Env.t -> Component.Signature.t -> string -> Component.Module.t =
 fun env sg name ->
  match Find.careful_module_in_sig sg name with
  | Found m -> m
  | Replaced path -> lookup_module env path 

and resolve_mt_signature_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.signature ->
    Cfrag.resolved_signature
    * Cpath.Resolved.parent
    * Component.Signature.t =
 fun env (p, sg) frag ->
  match frag with
  | `Root ->
    let sg = prefix_signature (`FragmentRoot, sg) in
    (`Root p, `FragmentRoot, sg)
  | `Resolved _r -> failwith "Shouldn't happen"
  | `Dot (parent, name) ->
      let pfrag, ppath, sg = resolve_mt_signature_fragment env (p, sg) parent in
      let m' = find_module_with_replacement env sg name in
      let mname = Odoc_model.Names.ModuleName.of_string name in
      let new_path = `Module (ppath, mname) in
      let new_frag = `Module (pfrag, mname) in
      let modifier = get_module_path_modifiers env false m' in
      let (cp', f') =
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
      let sg = prefix_signature (`Module cp', signature_of_module env m') in
      (f', `Module cp', sg)

and resolve_mt_module_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_ ->
    Cfrag.resolved_module =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> r
  | `Dot (parent, name) ->
    let pfrag, _ppath, sg = resolve_mt_signature_fragment env (p, sg) parent in
    let m' = find_module_with_replacement env sg name in
    let mname = Odoc_model.Names.ModuleName.of_string name in
    let new_frag = `Module (pfrag, mname) in
    let modifier = get_module_path_modifiers env false m' in
    let (f') =
      match modifier with
      | None -> (new_frag)
      | Some (`SubstAliased p') -> `SubstAlias (p', new_frag)
      | Some (`Aliased p') -> `SubstAlias (p', new_frag)
      | Some (`SubstMT p') -> `Subst (p', new_frag)
    in
    fixup_module_cfrag f'

and resolve_mt_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.type_ ->
    Cfrag.resolved_type =
  fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> r
  | `Dot (parent, name) ->
    let pfrag, ppath, _sg =
      resolve_mt_signature_fragment env (p, sg) parent
    in
    let _new_id = `Type (ppath, Odoc_model.Names.ModuleName.of_string name) in    
    let result = fixup_type_cfrag (`Type (pfrag, (TypeName.of_string name))) in
    (* Format.fprintf Format.err_formatter "resolve_mt_type_fragment: fragment=%a\n%!" Component.Fmt.resolved_type_fragment result; *)
    result
let rec reresolve_signature_fragment : Env.t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
  fun env m ->
    match m with
    | `Root (`ModuleType p) -> `Root (`ModuleType (reresolve_module_type env p))
    | `Root (`Module p) -> `Root (`Module (reresolve_module env p))
    | `Subst _ | `SubstAlias _ | `Module _ as x -> (reresolve_module_fragment env x :> Cfrag.resolved_signature)

and reresolve_module_fragment : Env.t -> Cfrag.resolved_module -> Cfrag.resolved_module =
  fun env m ->
    match m with
    | `Subst (p, f) ->
      let p' = reresolve_module_type env p in
      `Subst (p', reresolve_module_fragment env f)
    | `SubstAlias (p, f) ->
      let p' = reresolve_module env p in
      `SubstAlias (p', reresolve_module_fragment env f)
    | `Module (sg, m) -> `Module (reresolve_signature_fragment env sg, m)

and reresolve_type_fragment : Env.t -> Cfrag.resolved_type -> Cfrag.resolved_type =
  fun env m ->
    match m with
    | `Type (p, n) -> `Type (reresolve_signature_fragment env p, n)
    | `ClassType (p, n) -> `ClassType (reresolve_signature_fragment env p, n)
    | `Class (p, n) -> `Class (reresolve_signature_fragment env p, n)

let rec class_signature_of_class :
    Env.t ->
    Component.Class.t ->
    Component.ClassSignature.t =
 fun env c ->
  let rec inner decl =
    match decl with
    | Component.Class.ClassType e ->
        class_signature_of_class_type_expr env e
    | Arrow (_, _, d) -> inner d
  in
  inner c.type_

and class_signature_of_class_type_expr :
    Env.t ->
    Component.ClassType.expr ->
    Component.ClassSignature.t =
 fun env e ->
  match e with
  | Signature s -> s
  | Constr (p, _) -> (
      let c =
        match lookup_class_type_from_path env p with
        | Resolved (_, c) -> c
        | _ -> failwith "error"
      in
      match c with
      | `C c -> class_signature_of_class env c
      | `CT c -> class_signature_of_class_type env c )

and class_signature_of_class_type :
    Env.t ->
    Component.ClassType.t ->
    Component.ClassSignature.t =
 fun env c -> class_signature_of_class_type_expr env c.expr
