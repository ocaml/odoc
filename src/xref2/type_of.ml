(* Type_of.ml *)

(* Deals with expanding `module type of` expressions *)

open Odoc_model
open Lang
module Id = Odoc_model.Paths.Identifier


let rec signature : Env.t -> Signature.t -> Signature.t =
  fun env sg ->
    let env = Env.open_signature sg env in
    signature_items env sg

and signature_items : Env.t -> Signature.t -> Signature.t =
    fun env s ->
     let open Signature in
     List.map
       (fun item ->
         match item with
         | Module (r, m) -> Module (r, module_ env m)
         | ModuleType mt -> ModuleType (module_type env mt)
         | Include i -> Include (include_ env i)
         | item -> item)
       s

and module_ env m =
  match m.type_ with
  | Alias _ -> m
  | ModuleType expr -> { m with type_ = ModuleType (module_type_expr env (m.id :> Id.Signature.t) expr) }

and module_type env m =
  match m.expr with
  | None -> m
  | Some expr -> { m with expr = Some (module_type_expr env (m.id :> Id.Signature.t) expr) }

and module_type_expr env (id : Id.Signature.t) expr =
  match expr with
  | Path _ -> expr
  | Functor (Unit, expr) -> Functor (Unit, module_type_expr env id expr)
  | Functor (Named p, expr) ->
    let env = Env.add_functor_parameter (Named p) env in
    Functor (Named (functor_parameter env p), module_type_expr env id expr)
  | Signature sg -> Signature (signature env sg)
  | With w -> With {w with w_expr = u_module_type_expr env id w.w_expr }
  | TypeOf t ->
    let cexpr = Component.Of_Lang.(module_type_expr empty expr) in
    match Expand_tools.expansion_of_module_type_expr env id cexpr with
    | Ok (_, _, cexpansion) ->
      let t_expansion = Some (Lang_of.(simple_expansion empty id cexpansion)) in
      TypeOf { t with t_expansion }
    | Error e ->
      Format.eprintf "Couldn't expand module_type_of expression: %a\n%!" Tools.Fmt.error (e :> Errors.any);
      failwith "Couldn't expand module_type_of expression"

and u_module_type_expr env id expr =
  match expr with
  | Path _ -> expr
  | Signature sg -> Signature (signature env sg)
  | With (subs, w) -> With ( subs, u_module_type_expr env id w)
  | TypeOf t ->
    let cexpr = Component.Of_Lang.(u_module_type_expr empty expr) in
    match Expand_tools.expansion_of_u_module_type_expr env id cexpr with
    | Ok (_, _, cexpansion) ->
      let t_expansion = Some (Lang_of.(simple_expansion empty id cexpansion)) in
      TypeOf { t with t_expansion }
    | Error _ ->
      failwith "Couldn't expand u_module_type_of expression"

and functor_parameter env p =
  { p with expr = module_type_expr env (p.id :> Id.Signature.t) p.expr}

and include_ env i =
  let decl =
    match i.decl with
    | Alias _ -> i.decl
    | ModuleType t -> ModuleType (u_module_type_expr env i.parent t)
  in
  { i with expansion = { i.expansion with content = signature env i.expansion.content }; decl }