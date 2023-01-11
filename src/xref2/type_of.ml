(* Type_of.ml *)

(* Deals with expanding `module type of` expressions *)

open Odoc_model
open Lang
module Id = Odoc_model.Paths.Identifier

let again = ref false

let rec signature : Env.t -> Signature.t -> Signature.t =
 fun env sg ->
  let items, _ = signature_items env sg.items in
  { sg with items }

and signature_items : Env.t -> Signature.item list -> _ =
 fun initial_env s ->
  let open Signature in
  let rec loop items env xs =
    match xs with
    | [] -> (List.rev items, env)
    | item :: rest -> (
        match item with
        | Module (Nonrec, _) -> assert false
        | Module (r, m) ->
            let add_to_env env m =
              let ty =
                Component.Delayed.(
                  put (fun () -> Component.Of_Lang.(module_ (empty ()) m)))
              in
              Env.add_module (m.id :> Paths.Identifier.Path.Module.t) ty [] env
            in
            let env =
              match r with
              | Nonrec -> assert false
              | Ordinary | And -> env
              | Rec ->
                  let rec find modules rest =
                    match rest with
                    | Module (And, m') :: sgs -> find (m' :: modules) sgs
                    | Module (_, _) :: _ -> List.rev modules
                    | _ :: sgs -> find modules sgs
                    | [] -> List.rev modules
                  in
                  let modules = find [ m ] rest in
                  List.fold_left add_to_env env modules
            in
            let m' = module_ env m in
            let env'' =
              match r with
              | Nonrec -> assert false
              | And | Rec -> env
              | Ordinary -> add_to_env env m'
            in
            loop (Module (r, m') :: items) env'' rest
        | ModuleSubstitution m ->
            let env' = Env.open_module_substitution m env in
            loop (item :: items) env' rest
        | ModuleType mt ->
            let m' = module_type env mt in
            let ty = Component.Of_Lang.(module_type (empty ()) m') in
            let env' = Env.add_module_type mt.id ty env in
            loop (ModuleType (module_type env mt) :: items) env' rest
        | Include i ->
            let i', env' = include_ env i in
            loop (Include i' :: items) env' rest
        | item -> loop (item :: items) env rest)
  in
  loop [] initial_env s

and module_ env m =
  match m.type_ with
  | Alias _ -> m
  | ModuleType expr ->
      {
        m with
        type_ = ModuleType (module_type_expr env (m.id :> Id.Signature.t) expr);
      }

and module_type env m =
  match m.expr with
  | None -> m
  | Some expr ->
      {
        m with
        expr = Some (module_type_expr env (m.id :> Id.Signature.t) expr);
      }

and module_type_expr_typeof env (id : Id.Signature.t) t =
  let open Odoc_model.Lang.ModuleType in
  let p, strengthen =
    match t.t_desc with ModPath p -> (p, false) | StructInclude p -> (p, true)
  in
  let cp = Component.Of_Lang.(module_path (empty ()) p) in
  let open Expand_tools in
  let open Utils.ResultMonad in
  Tools.expansion_of_module_path env ~strengthen cp >>= fun exp ->
  handle_expansion env id exp >>= fun (_env, e) -> Ok e

and module_type_expr env (id : Id.Signature.t) expr =
  match expr with
  | Path _ -> expr
  | Functor (Unit, expr) -> Functor (Unit, module_type_expr env id expr)
  | Functor (Named p, expr) ->
      let env = Env.add_functor_parameter (Named p) env in
      Functor (Named (functor_parameter env p), module_type_expr env id expr)
  | Signature sg -> Signature (signature env sg)
  | With w -> With { w with w_expr = u_module_type_expr env id w.w_expr }
  | TypeOf t -> (
      match module_type_expr_typeof env id t with
      | Ok e ->
          let se = Lang_of.(simple_expansion (empty ()) id e) in
          TypeOf { t with t_expansion = Some (simple_expansion env se) }
      | Error e
        when Errors.is_unexpanded_module_type_of (e :> Errors.Tools_error.any)
        ->
          again := true;
          expr
      | Error _e -> expr)

and u_module_type_expr env id expr =
  match expr with
  | Path _ -> expr
  | Signature sg -> Signature (signature env sg)
  | With (subs, w) -> With (subs, u_module_type_expr env id w)
  | TypeOf t -> (
      match module_type_expr_typeof env id t with
      | Ok e ->
          let se = Lang_of.(simple_expansion (empty ()) id e) in
          TypeOf { t with t_expansion = Some (simple_expansion env se) }
      | Error e
        when Errors.is_unexpanded_module_type_of (e :> Errors.Tools_error.any)
        ->
          again := true;
          expr
      | Error _e -> expr)

and functor_parameter env p =
  { p with expr = module_type_expr env (p.id :> Id.Signature.t) p.expr }

and simple_expansion :
    Env.t -> ModuleType.simple_expansion -> ModuleType.simple_expansion =
 fun env -> function
  | Signature sg -> Signature (signature env sg)
  | Functor (Named n, sg) ->
      Functor (Named (functor_parameter env n), simple_expansion env sg)
  | Functor (Unit, sg) -> Functor (Unit, simple_expansion env sg)

and include_ env i =
  let decl =
    match i.decl with
    | Alias _ -> i.decl
    | ModuleType t -> ModuleType (u_module_type_expr env i.parent t)
  in
  let items, env' =
    let { Include.content; _ } = i.expansion in
    signature_items env content.items
  in
  ( {
      i with
      expansion =
        { i.expansion with content = { i.expansion.content with items } };
      decl;
    },
    env' )

let signature env =
  let rec loop sg =
    again := false;
    let sg' = signature env sg in
    Tools.reset_caches ();
    if !again then if sg' = sg then sg else loop sg' else sg'
  in
  loop
