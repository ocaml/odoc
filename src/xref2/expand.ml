open Odoc_model
open Lang

type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorParameter.t * Component.ModuleType.expr

let rec aux_expansion_of_module : Env.t -> Component.Module.t -> expansion =
  let open Component.Module in
  fun env m ->
    aux_expansion_of_module_decl env m.type_

and aux_expansion_of_module_decl env ty =
  let open Component.Module in
  match ty with
  | Alias path -> aux_expansion_of_module_alias env path
  | ModuleType expr -> aux_expansion_of_module_type_expr env expr

and aux_expansion_of_module_alias env path =
  match Tools.lookup_and_resolve_module_from_path false false env path with
  | Resolved (p, m) -> begin
    match aux_expansion_of_module env m with
    | Signature sg ->
      Signature (Strengthen.signature p sg)
    | Functor _ as x -> x
  end
  | Unresolved p ->
    let err = Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
    Component.Fmt.module_path path
    Component.Fmt.module_path p
    in
    failwith err

and aux_expansion_of_module_type_expr env expr : expansion =
  let open Component.ModuleType in
  match expr with
  | Path p -> begin
    match Tools.lookup_and_resolve_module_type_from_path false env p with
    | Resolved (_, mt) -> aux_expansion_of_module_type env mt
    | Unresolved p ->
      let p = Component.Fmt.(string_of module_type_path p) in
      failwith (Printf.sprintf "Couldn't find signature: %s" p)
    end
  | Signature s -> Signature s
  | With (s, subs) -> begin
    let expn = aux_expansion_of_module_type_expr env s in
    match expn with
    | Functor _ -> failwith "This shouldn't be possible!"
    | Signature sg ->
    let sg = Tools.handle_signature_with_subs env sg subs in
    Signature sg
    end
  | Functor (arg, expr) ->
    Functor (arg, expr)
  | TypeOf decl -> aux_expansion_of_module_decl env decl

and aux_expansion_of_module_type env mt =
    let open Component.ModuleType in
    match mt.expr with
    | None -> raise Tools.OpaqueModule
    | Some expr -> aux_expansion_of_module_type_expr env expr
    

let rec unit (resolver : Env.resolver) forward_references t =
  let open Compilation_unit in
  let initial_env =
    let m = Env.module_of_unit t in
    Env.empty |> Env.add_module t.id m
    |> Env.add_root (Paths.Identifier.name t.id) (Env.Resolved (t.id, m))
  in
  let initial_env =
    if forward_references 
    then Env.set_resolver initial_env resolver
    else initial_env
  in
  let rec handle_import (imports, env) import =
    if List.exists (fun i -> i = import) imports then (imports, env)
    else
      match import with
      | Import.Resolved root ->
          let unit = resolver.resolve_unit root in
          let m = Env.module_of_unit unit in
          let env = Env.add_module unit.id m env in
          let env =
            Env.add_root
              (Odoc_model.Root.Odoc_file.name root.Odoc_model.Root.file)
              (Env.Resolved (unit.id, m))
              env
          in
          List.fold_left handle_import (import :: imports, env) unit.imports
      | Import.Unresolved (str, _) -> (
          match resolver.lookup_unit str with
          | Forward_reference ->
              let env = Env.add_root str Env.Forward env in
              (import :: imports, env)
          | Found f ->
              let unit = resolver.resolve_unit f.root in
              let m = Env.module_of_unit unit in
              let env = Env.add_module unit.id m env in
              let env =
                Env.add_root
                  (Odoc_model.Root.Odoc_file.name f.root.Odoc_model.Root.file)
                  (Env.Resolved (unit.id, m))
                  env
              in
              List.fold_left handle_import (import :: imports, env) unit.imports
          | Not_found ->
              (* Format.fprintf Format.err_formatter "Can't find: %s\n%!" str ;*)
              (import :: imports, env) )
  in
  let imports, env = List.fold_left handle_import ([], initial_env) t.imports in
  { t with content = content env t.content; imports }

and content env =
  let open Compilation_unit in
  function
  | Module m -> Module (signature env m)
  | Pack _ -> failwith "Unhandled content"

and include_ : Env.t -> Include.t -> Include.t =
 fun env i ->
  let open Include in
  let expansion =
    { i.expansion with content = signature env i.expansion.content }
  in
  { i with expansion }

and signature : Env.t -> Signature.t -> _ =
 fun env s ->
  let open Signature in
  let env = Env.open_signature s env in
  let _, items' =
    List.fold_left
      (fun (env, items) item ->
        match item with
        | Module (r, m) ->
            let env' =
              Env.add_functor_args (m.id :> Paths.Identifier.Signature.t) env
            in
            let m' = module_ env' m in
            (env, Module (r, m') :: items)
        | ModuleType mt ->
            (*Format.fprintf Format.err_formatter "Expanding module_type %a\n%!" (Component.Fmt.model_identifier) (mt.id :> Paths.Identifier.t);*)
            let env' =
              Env.add_functor_args (mt.id :> Paths.Identifier.Signature.t) env
            in
            let mt' = module_type env' mt in
            (env, ModuleType mt' :: items)
        | Class (r, c) ->
            let c' = class_ env c in
            (env, Class (r, c') :: items)
        | ClassType (r, c) ->
            let c' = class_type env c in
            (env, ClassType (r, c') :: items)
        | Include i ->
            let i' = include_ env i in
            (env, Include i' :: items)
        | x -> (env, x :: items))
      (env, []) s
  in
  List.rev items'

and handle_expansion env id expansion =
  let handle_argument parent arg expr env =
    (* If there's an argument, extend the environment with the argument, then
      do the substitution on the signature to replace the local identifier with
      the global one *)
    match arg with
      | Component.FunctorParameter.Unit -> (env, expr)
      | Named arg ->
        let identifier =
          `Parameter
            ( parent,
              Odoc_model.Names.ParameterName.of_string
                (Ident.Name.module_ arg.Component.FunctorParameter.id) )
        in
        let env' = Env.add_module identifier (Component.module_of_functor_argument arg) env in
        let subst = Subst.add_module arg.id (`Identifier identifier) Subst.identity in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env args expansion =
    match expansion with
    | Signature sg -> begin
      match args with
      | [] -> Component.Module.Signature sg
      | args -> Component.Module.Functor (args, sg)
    end 
    | Functor (arg, expr) -> begin
      let (env', expr') = handle_argument id arg expr env in
      expand (`Result id) env' (arg :: args) (aux_expansion_of_module_type_expr env expr')
    end
  in
  Lang_of.(module_expansion empty id (expand id env [] expansion))

and expansion_of_module_type_expr (id : Paths_types.Identifier.signature) env
    expr =
    handle_expansion env id (aux_expansion_of_module_type_expr env expr)

and module_decl env (id : Paths_types.Identifier.module_) decl =
  let open Module in
  match decl with
  | Alias path -> Alias path
  | ModuleType mty ->
      ModuleType
        (module_type_expr env (id :> Paths_types.Identifier.signature) mty)

and module_type_expr env (id : Paths_types.Identifier.signature) expr =
  let open ModuleType in
  match expr with
  | Path _ | Signature _ -> expr
  | With (expr, subs) -> With (module_type_expr env id expr, subs)
  | TypeOf decl -> TypeOf decl
  | Functor (Named arg, expr) ->
      Functor
        ( Named (functor_argument env arg),
          module_type_expr env id expr )
  | Functor (Unit, expr) -> Functor (Unit, module_type_expr env id expr)

and functor_argument env arg =
  let open FunctorParameter in
  let functor_arg = Env.lookup_module arg.id env in
  try
    let expansion =
      match functor_arg.type_ with
      | ModuleType expr ->
          (try Some (expansion_of_module_type_expr
            (arg.id :> Paths_types.Identifier.signature)
            env expr) with _ -> None)
      | _ -> failwith "error"
    in
    {
      arg with
      expansion = expansion;
      expr = module_type_expr env (arg.id :> Odoc_model.Paths.Identifier.Signature.t) arg.expr;
    }
  with e ->
    Format.fprintf Format.err_formatter
      "Error expanding functor argument: %s\nArgment: %a\n%!" (Printexc.to_string e) Component.Fmt.module_ functor_arg;
  raise e

and module_ env m =
  let open Module in
  let id = m.id in
  let expansion_needed =
    match m.type_ with
    | Alias p when Paths.Path.is_hidden (p :> Paths.Path.t) -> true
    | Alias (`Resolved p) -> (
        match Paths.Path.Resolved.Module.canonical_ident p with
        | Some i -> i = m.id (* Self-canonical *)
        | None -> false )
    | ModuleType _ -> true
    | Alias _ -> false
  in
  if not expansion_needed then m
  else
    let type_ = module_decl env id m.type_ in
    let m' = Env.lookup_module m.id env in
    try
      match m'.type_ with
      | Alias _ ->
          let expn = aux_expansion_of_module env m' in
          let expansion = handle_expansion env (id :> Odoc_model.Paths.Identifier.Signature.t) expn in
          {
            m with
            type_;
            expansion = Some expansion;
            display_type = None; (* Some (ModuleType (ModuleType.Signature sg')); *)
          }
      | ModuleType expr ->
          let expansion =
            expansion_of_module_type_expr
              (id :> Paths_types.Identifier.signature)
              env expr
          in
          { m with type_; expansion = Some expansion }
    with 
    | Tools.OpaqueModule -> m
    | Tools.UnresolvedForwardPath -> m
    | e ->
      let bt = Printexc.get_backtrace () in
      Format.fprintf Format.err_formatter
        "Failed during expansion: %s (of module %a)\n%!" (Printexc.to_string e)
        Component.Fmt.model_identifier
        (id :> Paths.Identifier.t);
      Printf.fprintf stderr "backtrace:\n%s\n%!" bt;
      m

and module_type env m =
  let id = (m.id :> Odoc_model.Paths.Identifier.Signature.t) in
  let expr = Component.Opt.map (module_type_expr env id) m.expr in
  match expr with
  | None -> { m with expr; expansion = Some (Signature []) }
  | _ -> (
      let m' = Env.lookup_module_type m.id env in
      try
        let expn = aux_expansion_of_module_type env m' in
        let expansion = handle_expansion env (m.id :> Odoc_model.Paths.Identifier.Signature.t) expn in
        { m with expr; expansion = Some (expansion) }
      with e ->
        Format.fprintf Format.err_formatter
          "Got exception %s expading module type %a" (Printexc.to_string e)
          Component.Fmt.model_identifier
          (id :> Paths.Identifier.t);
        { m with expr } )

and class_ : Env.t -> Odoc_model.Lang.Class.t -> Odoc_model.Lang.Class.t =
 fun env c ->
  try
    let c' = Env.lookup_class c.id env in
    let _p, sg =
      Tools.class_signature_of_class env
        (`Identifier (c.id :> Paths_types.Identifier.path_class_type), c')
    in
    let expansion =
      Lang_of.class_signature Lang_of.empty
        (c.id :> Paths_types.Identifier.path_class_type)
        sg
    in
    { c with expansion = Some expansion }
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter "Failed to expand class: %s\n%s\n%!"
      (Printexc.to_string e) bt;
    c

and class_type :
    Env.t -> Odoc_model.Lang.ClassType.t -> Odoc_model.Lang.ClassType.t =
 fun env c ->
  let c' = Env.lookup_class_type c.id env in
  let _p, sg =
    Tools.class_signature_of_class_type env
      (`Identifier (c.id :> Paths_types.Identifier.path_class_type), c')
  in
  let expansion =
    Lang_of.class_signature Lang_of.empty
      (c.id :> Paths_types.Identifier.path_class_type)
      sg
  in
  { c with expansion = Some expansion }

let expand resolver y =
  let before = y in
  let after = unit resolver false before in
  after

let expand2 resolver y =
  let before = y in
  let after = unit resolver true before in
  after

let resolve_page _ x = x
