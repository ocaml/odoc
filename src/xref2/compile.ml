(* Phase 1 - compilation *)

(* First round of resolving only attempts to resolve paths and fragments, and then only those
   that don't contain forward paths *)

open Odoc_model
open Lang

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

let type_path : Env.t -> Paths.Path.Type.t -> Paths.Path.Type.t =
 fun env p ->
  let cp = Component.Of_Lang.(type_path empty p) in
  match Tools.lookup_type_from_path env cp with
  | Resolved (p', _) -> `Resolved (Cpath.resolved_type_path_of_cpath p')
  | Unresolved p -> Cpath.type_path_of_cpath p

and module_type_path :
    Env.t -> Paths.Path.ModuleType.t -> Paths.Path.ModuleType.t =
 fun env p ->
  let cp = Component.Of_Lang.(module_type_path empty p) in
  match Tools.resolve_module_type env cp with
  | Resolved p' -> `Resolved (Cpath.resolved_module_type_path_of_cpath p')
  | Unresolved p -> Cpath.module_type_path_of_cpath p

and module_path : Env.t -> Paths.Path.Module.t -> Paths.Path.Module.t =
 fun env p ->
  let cp = Component.Of_Lang.(module_path empty p) in
  match Tools.resolve_module env cp with
  | Resolved p' -> `Resolved (Cpath.resolved_module_path_of_cpath p')
  | Unresolved p -> Cpath.module_path_of_cpath p

let lookup_failure ~what =
  let r action =
    let r subject pp_a a =
      Lookup_failures.report "Failed to %s %s %a" action subject pp_a a
    in
    let r_id subject id =
      r subject Component.Fmt.model_identifier
        (id :> Odoc_model.Paths.Identifier.t)
    in
    let open Component.Fmt in
    match what with
    | `Functor_parameter id -> r_id "functor parameter" id
    | `Value id -> r_id "value" id
    | `Class id -> r_id "class" id
    | `Class_type id -> r_id "class type" id
    | `Module id -> r_id "module" id
    | `Module_type id -> r_id "module type" id
    | `Include decl -> r "include" module_decl decl
    | `Package path ->
        r "module package" module_type_path (path :> Cpath.module_type)
    | `Type cfrag -> r "type" type_fragment cfrag
    | `With_module frag -> r "module substitution" module_fragment frag
    | `With_type frag -> r "type substitution" type_fragment frag
  in
  function
  | `Lookup -> r "lookup"
  | `Expand -> r "compile expansion for"
  | `Resolve_module_type -> r "resolve type of"
  | `Resolve -> r "resolve"
  | `Compile -> r "compile"

let rec unit (resolver : Env.resolver) t =
  let open Compilation_unit in
  let imports, env = Env.initial_env t resolver in
  { t with content = content env t.content; imports }

and content env =
  let open Compilation_unit in
  function
  | Module m -> Module (signature env m)
  | Pack _ -> failwith "Unhandled content"

and value_ env t =
  let open Value in
  try { t with type_ = type_expression env t.type_ }
  with _ ->
    lookup_failure ~what:(`Value t.id) `Compile;
    t

and exception_ env e =
  let open Exception in
  let res = Opt.map (type_expression env) e.res in
  let args = type_decl_constructor_argument env e.args in
  { e with res; args }

and extension env t =
  let open Extension in
  let constructor c =
    let open Constructor in
    {
      c with
      args = type_decl_constructor_argument env c.args;
      res = Opt.map (type_expression env) c.res;
    }
  in
  let type_path = type_path env t.type_path in
  let constructors = List.map constructor t.constructors in
  { t with type_path; constructors }

and external_ env e =
  let open External in
  { e with type_ = type_expression env e.type_ }

and class_type_expr env =
  let open ClassType in
  function
  | Constr (path, texps) -> Constr (path, List.map (type_expression env) texps)
  | Signature s -> Signature (class_signature env s)

and class_type env c =
  let open ClassType in
  let expansion =
    match
      let open Utils.OptionMonad in
      Env.lookup_class_type c.id env >>= fun c' ->
      Tools.class_signature_of_class_type env c' >>= fun sg ->
      Some
        (Lang_of.class_signature Lang_of.empty
           (c.id :> Paths_types.Identifier.path_class_type)
           sg)
    with
    | Some _ as exp -> exp
    | None ->
        lookup_failure ~what:(`Class_type c.id) `Expand;
        c.expansion
  in
  { c with expr = class_type_expr env c.expr; expansion }

and class_signature env c =
  let open ClassSignature in
  let env = Env.open_class_signature c env in
  let map_item = function
    | Method m -> Method (method_ env m)
    | InstanceVariable i -> InstanceVariable (instance_variable env i)
    | Constraint (t1, t2) ->
        Constraint (type_expression env t1, type_expression env t2)
    | Inherit c -> Inherit (class_type_expr env c)
    | Comment c -> Comment c
  in
  {
    self = Opt.map (type_expression env) c.self;
    items = List.map map_item c.items;
  }

and method_ env m =
  let open Method in
  { m with type_ = type_expression env m.type_ }

and instance_variable env i =
  let open InstanceVariable in
  { i with type_ = type_expression env i.type_ }

and class_ env c =
  let open Class in
  let expansion =
    match
      let open Utils.OptionMonad in
      Env.lookup_class c.id env >>= fun c' ->
      Tools.class_signature_of_class env c' >>= fun sg ->
      Some
        (Lang_of.class_signature Lang_of.empty
           (c.id :> Paths_types.Identifier.path_class_type)
           sg)
    with
    | Some _ as exp -> exp
    | None ->
        lookup_failure ~what:(`Class c.id) `Expand;
        c.expansion
  in
  let rec map_decl = function
    | ClassType expr -> ClassType (class_type_expr env expr)
    | Arrow (lbl, expr, decl) ->
        Arrow (lbl, type_expression env expr, map_decl decl)
  in
  { c with type_ = map_decl c.type_; expansion }

and module_substitution env m =
  let open ModuleSubstitution in
  { m with manifest = module_path env m.manifest }

and signature_items : Env.t -> Signature.t -> _ =
 fun env s ->
  let open Signature in
  List.map
    (fun item ->
      match item with
      | Module (r, m) -> Module (r, module_ env m)
      | ModuleSubstitution m -> ModuleSubstitution (module_substitution env m)
      | Type (r, t) -> Type (r, type_decl env t)
      | TypeSubstitution t -> TypeSubstitution (type_decl env t)
      | ModuleType mt -> ModuleType (module_type env mt)
      | Value v -> Value (value_ env v)
      | Comment c -> Comment c
      | TypExt t -> TypExt (extension env t)
      | Exception e -> Exception (exception_ env e)
      | External e -> External (external_ env e)
      | Class (r, c) -> Class (r, class_ env c)
      | ClassType (r, c) -> ClassType (r, class_type env c)
      | Include i -> Include (include_ env i)
      | Open o -> Open o)
    s

and signature : Env.t -> Signature.t -> _ =
 fun env s ->
  let env = Env.open_signature s env in
  signature_items env s

and module_ : Env.t -> Module.t -> Module.t =
 fun env m ->
  let open Module in
  if m.hidden then m
  else
    let extra_expansion_needed =
      match m.type_ with
      | ModuleType (Signature _) -> false (* AlreadyASig *)
      | ModuleType _ -> true
      | Alias _ -> false
      (* Aliases are expanded if necessary during link *)
    in
    (* Format.fprintf Format.err_formatter "Handling module: %a\n" Component.Fmt.model_identifier (m.id :> Odoc_model.Paths.Identifier.t); *)
    match Env.lookup_module m.id env with
    | None ->
        lookup_failure ~what:(`Module m.id) `Lookup;
        m
    | Some m' ->
        let env' = Env.add_module_functor_args m' m.id env in
        let expansion =
          let sg_id = (m.id :> Paths.Identifier.Signature.t) in
          if not extra_expansion_needed then m.expansion
          else
            match Expand_tools.expansion_of_module env m.id m' with
            | Ok (env, ce) ->
                let e = Lang_of.(module_expansion empty sg_id ce) in
                Some (expansion env e)
            | Error `OpaqueModule -> None
            | Error _ ->
                lookup_failure ~what:(`Module m.id) `Expand;
                m.expansion
        in
        {
          m with
          type_ =
            module_decl env' (m.id :> Paths.Identifier.Signature.t) m.type_;
          expansion;
        }

and module_decl :
    Env.t -> Paths.Identifier.Signature.t -> Module.decl -> Module.decl =
 fun env id decl ->
  let open Module in
  match decl with
  | ModuleType expr -> ModuleType (module_type_expr env id expr)
  | Alias p -> (
      let cp = Component.Of_Lang.(module_path empty p) in
      match Tools.resolve_module env cp with
      | Resolved p' ->
          Alias (`Resolved (Cpath.resolved_module_path_of_cpath p'))
      | Unresolved p' -> Alias (Cpath.module_path_of_cpath p') )

and module_type : Env.t -> ModuleType.t -> ModuleType.t =
 fun env m ->
  let open ModuleType in
  let open Utils.ResultMonad in
  (* Format.fprintf Format.err_formatter "Handling module type: %a\n" Component.Fmt.model_identifier (m.id :> Odoc_model.Paths.Identifier.t); *)
  let expand m' env =
    match m.expr with
    | None -> Ok (env, None, None)
    | Some expr ->
        ( match Expand_tools.expansion_of_module_type env m.id m' with
        | Ok (env, ce) ->
            let sg_id = (m.id :> Paths.Identifier.Signature.t) in
            let e = Lang_of.(module_expansion empty sg_id ce) in
            Ok (env, Some e)
        | Error `OpaqueModule -> Ok (env, None)
        | Error _ -> Error `Expand )
        >>= fun (env, expansion) ->
        Ok
          ( env,
            expansion,
            Some
              (module_type_expr env (m.id :> Paths.Identifier.Signature.t) expr)
          )
  in
  match
    Env.lookup_module_type m.id env |> of_option ~error:`Lookup >>= fun m' ->
    let env = Env.add_module_type_functor_args m' m.id env in
    expand m' env
  with
  | Ok (env', expansion', expr') ->
      { m with expr = expr'; expansion = Opt.map (expansion env') expansion' }
  | Error e ->
      lookup_failure ~what:(`Module_type m.id) e;
      m

and find_shadowed map =
  let open Odoc_model.Names in
  let open Signature in
  let hidden_name : Odoc_model.Paths.Identifier.t -> string option = function
    | `Module (_, m) when ModuleName.is_internal m ->
        Some (ModuleName.to_string_unsafe m)
    | `Parameter (_, m) when ParameterName.is_internal m ->
        Some (ParameterName.to_string_unsafe m)
    | `ModuleType (_, m) when ModuleTypeName.is_internal m ->
        Some (ModuleTypeName.to_string_unsafe m)
    | `Type (_, t) when TypeName.is_internal t ->
        Some (TypeName.to_string_unsafe t)
    | _ -> None
  in
  function
  | Module (_, m) :: rest -> (
      match hidden_name (m.id :> Odoc_model.Paths.Identifier.t) with
      | Some n ->
          find_shadowed Lang_of.{ map with s_modules = n :: map.s_modules } rest
      | None ->
          find_shadowed map rest )
  | ModuleType m :: rest -> (
      match hidden_name (m.id :> Odoc_model.Paths.Identifier.t) with
      | Some n ->
          find_shadowed
            Lang_of.{ map with s_module_types = n :: map.s_module_types }
            rest
      | None -> find_shadowed map rest )
  | Type (_, t) :: rest -> (
      match hidden_name (t.id :> Odoc_model.Paths.Identifier.t) with
      | Some n ->
          find_shadowed Lang_of.{ map with s_types = n :: map.s_types } rest
      | None -> find_shadowed map rest )
  | _ :: rest -> find_shadowed map rest
  | [] -> map

and include_ : Env.t -> Include.t -> Include.t =
 fun env i ->
  let open Include in
  let remove_top_doc_from_signature =
    let open Signature in
    function Comment (`Docs _) :: xs -> xs | xs -> xs
  in
  let decl = Component.Of_Lang.(module_decl empty i.decl) in
  match
    let open Utils.ResultMonad in
    Expand_tools.aux_expansion_of_module_decl env decl
    >>= Expand_tools.handle_expansion env i.parent
  with
  | Error _ ->
      lookup_failure ~what:(`Include decl) `Expand;
      i
  | Ok (_, ce) ->
      let map = find_shadowed Lang_of.empty i.expansion.content in
      let e = Lang_of.(module_expansion map i.parent ce) in
      let expansion =
        match e with
        | Module.Signature sg ->
            {
              resolved = true;
              content = remove_top_doc_from_signature (signature env sg);
            }
        | _ -> failwith "Expansion shouldn't be anything other than a signature"
      in
      { i with decl = module_decl env i.parent i.decl; expansion }

and expansion : Env.t -> Module.expansion -> Module.expansion =
  let open Module in
  fun env e ->
    match e with
    | AlreadyASig -> AlreadyASig
    | Signature sg -> Signature (signature env sg)
    | Functor (args, sg) ->
        Functor (List.map (functor_parameter env) args, signature env sg)

and functor_parameter : Env.t -> FunctorParameter.t -> FunctorParameter.t =
 fun env param ->
  match param with
  | Unit -> Unit
  | Named arg -> Named (functor_parameter_parameter env arg)

and functor_parameter_parameter :
    Env.t -> FunctorParameter.parameter -> FunctorParameter.parameter =
 fun env' a ->
  let open Utils.ResultMonad in
  let get_module_type_expr = function
    | Component.Module.ModuleType expr -> Ok expr
    | _ -> Error `Resolve_module_type
  in
  match
    Env.lookup_module a.id env' |> of_option ~error:`Lookup
    >>= fun functor_arg ->
    let env = Env.add_module_functor_args functor_arg a.id env' in
    get_module_type_expr functor_arg.type_ >>= fun expr ->
    let sg_id = (a.id :> Paths.Identifier.Signature.t) in
    match Expand_tools.expansion_of_module_type_expr env sg_id expr with
    | Ok (env, ce) ->
        let e = Lang_of.(module_expansion empty sg_id ce) in
        Ok (env, Some e)
    | Error `OpaqueModule -> Ok (env, None)
    | Error _ ->
        lookup_failure ~what:(`Functor_parameter a.id) `Expand;
        Ok (env, None)
  with
  | Ok (env, expn) ->
      {
        a with
        expr =
          module_type_expr env (a.id :> Paths.Identifier.Signature.t) a.expr;
        expansion = Component.Opt.map (expansion env) expn;
      }
  | Error e ->
      lookup_failure ~what:(`Functor_parameter a.id) e;
      a

and module_type_expr :
    Env.t -> Paths.Identifier.Signature.t -> ModuleType.expr -> ModuleType.expr
    =
 fun env id expr ->
  let open ModuleType in
  let resolve_sub ~fragment_root (sg, env, subs) csub lsub =
    let lang_of_map = Lang_of.with_fragment_root fragment_root in
    let env = Env.add_fragment_root sg env in
    let sg', sub' =
      match (csub, lsub) with
      | Component.ModuleType.ModuleEq (frag, _), ModuleEq (unresolved, decl) ->
          let frag' =
            match
              Tools.resolve_mt_module_fragment env (fragment_root, sg) frag
            with
            | Some cfrag ->
                `Resolved
                  (Lang_of.Path.resolved_module_fragment lang_of_map cfrag)
            | None ->
                lookup_failure ~what:(`With_module frag) `Resolve;
                unresolved
          in
          let sg' = Tools.fragmap_module env frag csub sg in
          (sg', ModuleEq (frag', module_decl env id decl))
      | TypeEq (frag, _), TypeEq (unresolved, eqn) ->
          let frag' =
            match
              Tools.resolve_mt_type_fragment env (fragment_root, sg) frag
            with
            | Some cfrag ->
                `Resolved
                  (Lang_of.Path.resolved_type_fragment lang_of_map cfrag)
            | None ->
                lookup_failure ~what:(`With_type frag) `Compile;
                unresolved
          in
          let sg' = Tools.fragmap_type env frag csub sg in
          (sg', TypeEq (frag', type_decl_equation env eqn))
      | ModuleSubst (frag, _), ModuleSubst (unresolved, mpath) ->
          let frag' =
            match
              Tools.resolve_mt_module_fragment env (fragment_root, sg) frag
            with
            | Some cfrag ->
                `Resolved
                  (Lang_of.Path.resolved_module_fragment lang_of_map cfrag)
            | None ->
                lookup_failure ~what:(`With_module frag) `Resolve;
                unresolved
          in
          let sg' = Tools.fragmap_module env frag csub sg in
          (sg', ModuleSubst (frag', module_path env mpath))
      | TypeSubst (frag, _), TypeSubst (unresolved, eqn) ->
          let frag' =
            match
              Tools.resolve_mt_type_fragment env (fragment_root, sg) frag
            with
            | Some cfrag ->
                `Resolved
                  (Lang_of.Path.resolved_type_fragment lang_of_map cfrag)
            | None ->
                lookup_failure ~what:(`With_type frag) `Compile;
                unresolved
          in
          let sg' = Tools.fragmap_type env frag csub sg in
          (sg', TypeSubst (frag', type_decl_equation env eqn))
      | _ -> failwith "This is pretty unusual"
    in
    (sg', env, sub' :: subs)
  in

  let rec inner resolve_signatures = function
    | Signature s ->
        if resolve_signatures then Signature (signature env s) else Signature s
    | Path p -> Path (module_type_path env p)
    | With (expr, subs) -> (
        let expr = inner false expr in
        let cexpr = Component.Of_Lang.(module_type_expr empty expr) in
        let csubs =
          List.map Component.Of_Lang.(module_type_substitution empty) subs
        in
        let rec find_parent : Component.ModuleType.expr -> Cfrag.root option =
         fun expr ->
          match expr with
          | Component.ModuleType.Signature _ -> None
          | Path (`Resolved p) -> Some (`ModuleType p)
          | Path _ -> None
          | With (e, _) -> find_parent e
          | Functor _ -> failwith "Impossible"
          | TypeOf (Alias (`Resolved p)) -> Some (`Module p)
          | TypeOf (Alias _) -> None
          | TypeOf (ModuleType t) -> find_parent t
        in
        match find_parent cexpr with
        | None -> With (expr, subs)
        | Some parent -> (
            match Tools.signature_of_module_type_expr env cexpr with
            | Error _ ->
                lookup_failure ~what:(`Module_type id) `Lookup;
                With (expr, subs)
            | Ok sg ->
                let fragment_root =
                  match parent with (`ModuleType _ | `Module _) as x -> x
                in
                let _, _, subs =
                  List.fold_left2
                    (resolve_sub ~fragment_root)
                    (sg, env, []) csubs subs
                in
                let subs = List.rev subs in
                With (inner false expr, subs) ) )
    | Functor (param, res) ->
        let param' = functor_parameter env param in
        let res' = module_type_expr env id res in
        Functor (param', res')
    | TypeOf (ModuleType expr) ->
        TypeOf (ModuleType (inner resolve_signatures expr))
    | TypeOf (Alias p) -> TypeOf (Alias (module_path env p))
  in
  inner true expr

and type_decl : Env.t -> TypeDecl.t -> TypeDecl.t =
 fun env t ->
  let open TypeDecl in
  try
    let equation = type_decl_equation env t.equation in
    let representation =
      Opt.map (type_decl_representation env) t.representation
    in
    { t with equation; representation }
  with e ->
    Format.fprintf Format.err_formatter "Failed to resolve type (%a): %s"
      Component.Fmt.model_identifier
      (t.id :> Paths.Identifier.t)
      (Printexc.to_string e);
    raise e

and type_decl_equation env t =
  let open TypeDecl.Equation in
  let manifest = Opt.map (type_expression env) t.manifest in
  let constraints =
    List.map
      (fun (tex1, tex2) -> (type_expression env tex1, type_expression env tex2))
      t.constraints
  in
  { t with manifest; constraints }

and type_decl_representation :
    Env.t -> TypeDecl.Representation.t -> TypeDecl.Representation.t =
 fun env r ->
  let open TypeDecl.Representation in
  match r with
  | Variant cs -> Variant (List.map (type_decl_constructor env) cs)
  | Record fs -> Record (List.map (type_decl_field env) fs)
  | Extensible -> Extensible

and type_decl_field env f =
  let open TypeDecl.Field in
  { f with type_ = type_expression env f.type_ }

and type_decl_constructor_argument env c =
  let open TypeDecl.Constructor in
  match c with
  | Tuple ts -> Tuple (List.map (type_expression env) ts)
  | Record fs -> Record (List.map (type_decl_field env) fs)

and type_decl_constructor env c =
  let open TypeDecl.Constructor in
  let args = type_decl_constructor_argument env c.args in
  let res = Opt.map (type_expression env) c.res in
  { c with args; res }

and type_expression_polyvar env v =
  let open TypeExpr.Polymorphic_variant in
  let constructor c =
    let open Constructor in
    { c with arguments = List.map (type_expression env) c.arguments }
  in
  let element = function
    | Type t -> Type (type_expression env t)
    | Constructor c -> Constructor (constructor c)
  in
  { v with elements = List.map element v.elements }

and type_expression_object env o =
  let open TypeExpr.Object in
  let method_ m = { m with type_ = type_expression env m.type_ } in
  let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expression env t)
  in
  { o with fields = List.map field o.fields }

and type_expression_package env p =
  let open TypeExpr.Package in
  let cp = Component.Of_Lang.(module_type_path empty p.path) in
  match Tools.lookup_and_resolve_module_type_from_path true env cp with
  | Resolved (path, mt) -> (
      match Tools.signature_of_module_type env mt with
      | Error _ ->
          lookup_failure ~what:(`Package cp) `Lookup;
          p
      | Ok sg ->
          let substitution (frag, t) =
            let cfrag = Component.Of_Lang.(type_fragment empty frag) in
            let frag' =
              match
                Tools.resolve_mt_type_fragment env (`ModuleType path, sg) cfrag
              with
              | Some cfrag' ->
                  `Resolved (Lang_of.(Path.resolved_type_fragment empty) cfrag')
              | None ->
                  lookup_failure ~what:(`Type cfrag) `Compile;
                  frag
            in
            (frag', type_expression env t)
          in
          {
            path = module_type_path env p.path;
            substitutions = List.map substitution p.substitutions;
          } )
  | Unresolved p' -> { p with path = Cpath.module_type_path_of_cpath p' }

and type_expression : Env.t -> _ -> _ =
 fun env texpr ->
  let open TypeExpr in
  match texpr with
  | Var _ | Any -> texpr
  | Alias (t, str) -> Alias (type_expression env t, str)
  | Arrow (lbl, t1, t2) ->
      Arrow (lbl, type_expression env t1, type_expression env t2)
  | Tuple ts -> Tuple (List.map (type_expression env) ts)
  | Constr (path, ts') -> (
      let cp = Component.Of_Lang.(type_path empty path) in
      let ts = List.map (type_expression env) ts' in
      match Tools.lookup_type_from_path env cp with
      | Resolved (cp, Found _t) ->
          let p = Cpath.resolved_type_path_of_cpath cp in
          Constr (`Resolved p, ts)
      | Resolved (_cp, Replaced x) -> Lang_of.(type_expr empty x)
      | Unresolved p -> Constr (Cpath.type_path_of_cpath p, ts) )
  | Polymorphic_variant v -> Polymorphic_variant (type_expression_polyvar env v)
  | Object o -> Object (type_expression_object env o)
  | Class (path, ts) -> Class (path, List.map (type_expression env) ts)
  | Poly (strs, t) -> Poly (strs, type_expression env t)
  | Package p -> Package (type_expression_package env p)

type msg = [ `Msg of string ]

exception Fetch_failed of msg

let build_resolver :
    ?equal:(Root.t -> Root.t -> bool) ->
    ?hash:(Root.t -> int) ->
    string list ->
    (string -> Env.lookup_unit_result) ->
    (Root.t -> (Compilation_unit.t, _) Result.result) ->
    (string -> Root.t option) ->
    (Root.t -> (Page.t, _) Result.result) ->
    Env.resolver =
 fun ?equal:_ ?hash:_ open_units lookup_unit resolve_unit lookup_page
     resolve_page ->
  let resolve_unit root =
    match resolve_unit root with
    | Ok unit -> unit
    | Error e -> raise (Fetch_failed e)
  and resolve_page root =
    match resolve_page root with
    | Ok page -> page
    | Error e -> raise (Fetch_failed e)
  in
  { Env.lookup_unit; resolve_unit; lookup_page; resolve_page; open_units }

let compile x y = Lookup_failures.catch_failures (fun () -> unit x y)

let resolve_page _resolver y = y
