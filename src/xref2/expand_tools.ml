open Utils.ResultMonad
open Odoc_model

let handle_expansion env id expansion =
  let handle_argument parent arg_opt expr env =
    (* If there's an argument, extend the environment with the argument, then
       do the substitution on the signature to replace the local identifier with
       the global one *)
    match arg_opt with
    | Component.FunctorParameter.Unit -> (env, expr)
    | Named arg ->
        let identifier =
          Paths.Identifier.Mk.parameter
            ( parent,
              Ident.Name.typed_functor_parameter
                arg.Component.FunctorParameter.id )
        in
        let m = Component.module_of_functor_argument arg in
        let env' =
          Env.add_module identifier (Component.Delayed.put_val m) m.doc env
        in
        let rp = `Gpath (`Identifier identifier) in
        let p = `Resolved rp in
        let subst =
          Subst.add_module (arg.id :> Ident.path_module) p rp Subst.identity
        in
        let subst =
          Subst.mto_invalidate_module (arg.id :> Ident.path_module) subst
        in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env expansion :
      (Env.t * Component.ModuleType.simple_expansion, _) Result.result =
    match expansion with
    | Tools.Signature sg ->
        Ok
          ( env,
            (Component.ModuleType.Signature sg
              : Component.ModuleType.simple_expansion) )
    | Functor (arg, expr) ->
        let env', expr' = handle_argument id arg expr env in
        Tools.expansion_of_module_type_expr ~mark_substituted:false env' expr'
        >>= fun res ->
        expand (Paths.Identifier.Mk.result id) env res >>= fun (env, res) ->
        Ok
          ( env,
            (Component.ModuleType.Functor (arg, res)
              : Component.ModuleType.simple_expansion) )
  in
  expand id env expansion

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
