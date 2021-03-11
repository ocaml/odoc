open Utils.ResultMonad
open Odoc_model
open Errors.Tools_error

type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorParameter.t * Component.ModuleType.expr

let rec module_needs_recompile : Component.Module.t -> bool =
 fun m -> module_decl_needs_recompile m.type_

and module_decl_needs_recompile : Component.Module.decl -> bool = function
  | Alias _ -> false
  | ModuleType expr -> module_type_expr_needs_recompile expr

and module_type_expr_needs_recompile : Component.ModuleType.expr -> bool =
  function
  | Path _ -> false
  | Signature _ -> false
  | With _ -> true
  | Functor (_, expr) -> module_type_expr_needs_recompile expr
  | TypeOf _ -> false

and module_type_needs_recompile : Component.ModuleType.t -> bool =
 fun m ->
  match m.expr with
  | None -> false
  | Some expr -> module_type_expr_needs_recompile expr

let rec aux_expansion_of_module :
    Env.t ->
    strengthen:bool ->
    Component.Module.t ->
    (expansion, signature_of_module_error) Result.result =
  let open Component.Module in
  fun env ~strengthen m -> aux_expansion_of_module_decl env ~strengthen m.type_

and aux_expansion_of_module_decl env ~strengthen ty =
  let open Component.Module in
  match ty with
  | Alias (path, _) -> aux_expansion_of_module_alias env ~strengthen path
  | ModuleType expr -> aux_expansion_of_module_type_expr env expr

and aux_expansion_of_module_alias env ~strengthen path =
  (* Format.eprintf "aux_expansion_of_module_alias (strengthen=%b, path=%a)\n%!"
     strengthen Component.Fmt.module_path path; *)
  match
    Tools.resolve_module env ~mark_substituted:false ~add_canonical:false path
  with
  | Ok (p, m) -> (
      (* Don't strengthen if the alias is definitely hidden. We can't always resolve canonical
         paths at this stage so use the weak canonical test that assumes all canonical paths
         will resolve correctly *)
      let strengthen =
        strengthen
        && not (Cpath.is_resolved_module_hidden ~weak_canonical_test:true p)
      in
      let m = Component.Delayed.get m in
      match (aux_expansion_of_module env ~strengthen:true m, m.doc) with
      | (Error _ as e), _ -> e
      | Ok (Signature sg), [] ->
          (* Format.eprintf "Maybe strenthening now...\n%!"; *)
          let sg' =
            if strengthen then
              Strengthen.signature ?canonical:m.canonical (`Resolved p) sg
            else sg
          in
          Ok (Signature sg')
      | Ok (Signature sg), docs ->
          (* Format.eprintf "Maybe strenthening now...\n%!"; *)
          let sg' =
            if strengthen then
              Strengthen.signature ?canonical:m.canonical (`Resolved p) sg
            else sg
          in
          (* Format.eprintf "Before:\n%a\n\n%!After\n%a\n\n%!"
             Component.Fmt.signature sg
             Component.Fmt.signature sg'; *)
          Ok (Signature { sg' with items = Comment (`Docs docs) :: sg'.items })
      | Ok (Functor _ as x), _ -> Ok x)
  | Error e -> Error (`UnresolvedPath (`Module (path, e)))

(* We need to reresolve fragments in expansions as the root of the fragment
   may well change - so we turn resolved fragments back into unresolved ones
   here *)
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

and aux_expansion_of_module_type_type_of_desc env t :
    (expansion, signature_of_module_error) Result.result =
  match t with
  | Component.ModuleType.ModPath p ->
      aux_expansion_of_module_alias env ~strengthen:false p
  | StructInclude p -> aux_expansion_of_module_alias env ~strengthen:true p

and assert_not_functor = function Signature sg -> Ok sg | _ -> assert false

and aux_expansion_of_u_module_type_expr env expr :
    (Component.Signature.t, signature_of_module_error) Result.result =
  let open Utils.ResultMonad in
  match expr with
  | Component.ModuleType.U.Path p ->
      Tools.resolve_module_type ~mark_substituted:false ~add_canonical:true env
        p
      |> map_error (fun e -> `UnresolvedPath (`ModuleType (p, e)))
      >>= fun (_, mt) ->
      aux_expansion_of_module_type env mt >>= assert_not_functor
  | Signature sg -> Ok sg
  | With (subs, s) ->
      aux_expansion_of_u_module_type_expr env s >>= fun sg ->
      let subs = unresolve_subs subs in
      Tools.handle_signature_with_subs ~mark_substituted:false env sg subs
  | TypeOf { t_expansion = Some (Signature sg); _ } -> Ok sg
  | TypeOf { t_desc; _ } -> Error (`UnexpandedTypeOf t_desc)

and aux_expansion_of_module_type_expr env expr :
    (expansion, signature_of_module_error) Result.result =
  match expr with
  | Path { p_path; _ } ->
      Tools.resolve_module_type ~mark_substituted:false ~add_canonical:true env
        p_path
      |> map_error (fun e -> `UnresolvedPath (`ModuleType (p_path, e)))
      >>= fun (_, mt) -> aux_expansion_of_module_type env mt
  | Signature s -> Ok (Signature s)
  | With { w_substitutions; w_expr; _ } ->
      ( aux_expansion_of_u_module_type_expr env w_expr >>= fun sg ->
        let subs = unresolve_subs w_substitutions in
        Tools.handle_signature_with_subs ~mark_substituted:false env sg subs )
      >>= fun sg -> Ok (Signature sg)
  | Functor (arg, expr) -> Ok (Functor (arg, expr))
  | TypeOf { t_expansion = Some (Signature sg); _ } -> Ok (Signature sg)
  | TypeOf { t_desc; _ } -> Error (`UnexpandedTypeOf t_desc)

and aux_expansion_of_module_type env mt =
  let open Component.ModuleType in
  match mt.expr with
  | None -> Error `OpaqueModule
  | Some expr -> aux_expansion_of_module_type_expr env expr

and handle_expansion env id expansion =
  let handle_argument parent arg_opt expr env =
    (* If there's an argument, extend the environment with the argument, then
       do the substitution on the signature to replace the local identifier with
       the global one *)
    match arg_opt with
    | Component.FunctorParameter.Unit -> (env, expr)
    | Named arg ->
        let identifier =
          `Parameter
            ( parent,
              Ident.Name.typed_functor_parameter
                arg.Component.FunctorParameter.id )
        in
        let m = Component.module_of_functor_argument arg in
        let env' =
          Env.add_module identifier (Component.Delayed.put_val m) m.doc env
        in
        let subst =
          Subst.add_module
            (arg.id :> Ident.path_module)
            (`Resolved (`Identifier identifier))
            (`Identifier identifier) Subst.identity
        in
        let subst =
          Subst.mto_invalidate_module (arg.id :> Ident.path_module) subst
        in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env expansion :
      (Env.t * Component.ModuleType.simple_expansion, _) Result.result =
    match expansion with
    | Signature sg ->
        Ok
          ( env,
            (Component.ModuleType.Signature sg
              : Component.ModuleType.simple_expansion) )
    | Functor (arg, expr) ->
        let env', expr' = handle_argument id arg expr env in
        aux_expansion_of_module_type_expr env' expr' >>= fun res ->
        expand (`Result id) env res >>= fun (env, res) ->
        Ok
          ( env,
            (Component.ModuleType.Functor (arg, res)
              : Component.ModuleType.simple_expansion) )
  in
  expand id env expansion

let expansion_of_module_type env id m =
  let open Paths.Identifier in
  aux_expansion_of_module_type env m
  >>= handle_expansion env (id : ModuleType.t :> Signature.t)
  >>= fun (env, e) -> Ok (env, module_type_needs_recompile m, e)

let expansion_of_module_type_expr env id expr =
  aux_expansion_of_module_type_expr env expr >>= handle_expansion env id
  >>= fun (env, e) -> Ok (env, module_type_expr_needs_recompile expr, e)

let expansion_of_u_module_type_expr env id expr =
  aux_expansion_of_u_module_type_expr env expr >>= fun sg ->
  handle_expansion env id (Signature sg) >>= fun (env, e) -> Ok (env, false, e)

(* Nb. [strengthen=false] here because the only time we are ever expanding module aliases is when either
   the module is the canonical one or it's an alias to a hidden module. In neither of these cases do we want
   to strengthen. *)
let expansion_of_module_alias env id path =
  let open Paths.Identifier in
  aux_expansion_of_module_alias ~strengthen:false env path
  >>= handle_expansion env (id : Module.t :> Signature.t)
  >>= fun (env, r) -> Ok (env, false, r)

let expansion_of_module_type_of_desc env id t_desc =
  aux_expansion_of_module_type_type_of_desc env t_desc
  >>= handle_expansion env id

exception Clash

let rec type_expr map t =
  let open Lang.TypeExpr in
  match t with
  | Var v -> (
      try List.assoc v map
      with _ ->
        Format.eprintf "Failed to list assoc %s\n%!" v;
        failwith "bah")
  | Any -> Any
  | Alias (t, s) ->
      if List.mem_assoc s map then raise Clash else Alias (type_expr map t, s)
  | Arrow (l, t1, t2) -> Arrow (l, type_expr map t1, type_expr map t2)
  | Tuple ts -> Tuple (List.map (type_expr map) ts)
  | Constr (p, ts) -> Constr (p, List.map (type_expr map) ts)
  | Polymorphic_variant pv -> Polymorphic_variant (polymorphic_variant map pv)
  | Object o -> Object (object_ map o)
  | Class (path, ts) -> Class (path, List.map (type_expr map) ts)
  | Poly (s, t) -> Poly (s, type_expr map t)
  | Package p -> Package (package map p)

and polymorphic_variant map pv =
  let open Lang.TypeExpr.Polymorphic_variant in
  let constructor c =
    {
      c with
      Constructor.arguments = List.map (type_expr map) c.Constructor.arguments;
    }
  in
  let element = function
    | Type t -> Type (type_expr map t)
    | Constructor c -> Constructor (constructor c)
  in
  { kind = pv.kind; elements = List.map element pv.elements }

and object_ map o =
  let open Lang.TypeExpr.Object in
  let method_ m = { m with type_ = type_expr map m.type_ } in
  let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expr map t)
  in
  { o with fields = List.map field o.fields }

and package map p =
  let open Lang.TypeExpr.Package in
  let subst (frag, t) = (frag, type_expr map t) in
  { p with substitutions = List.map subst p.substitutions }

let collapse_eqns eqn1 eqn2 params =
  let open Lang.TypeDecl in
  let map =
    List.map2
      (fun v p -> match v.desc with Var x -> Some (x, p) | Any -> None)
      eqn2.Equation.params params
  in
  let map =
    List.fold_right
      (fun x xs -> match x with Some x -> x :: xs | None -> xs)
      map []
  in
  {
    eqn1 with
    Equation.manifest =
      (match eqn2.manifest with
      | None -> None
      | Some t -> Some (type_expr map t));
  }
