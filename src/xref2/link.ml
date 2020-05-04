(* Second round of resolution tackles references and forward paths *)
open Odoc_model
open Lang

(* for < 4.03 *)
let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

exception Loop

let with_dummy_location v =
  Location_.
    {
      location =
        {
          file = "";
          start = { line = 0; column = 0 };
          end_ = { line = 0; column = 1 };
        };
      value = v;
    }

let rec is_forward : Paths.Path.Module.t -> bool = function
  | `Resolved _ -> false
  | `Root _ -> false
  | `Forward _ -> true
  | `Dot (p, _) -> is_forward p
  | `Apply (p1, p2) -> is_forward p1 || is_forward p2

let rec should_reresolve : Paths.Path.Resolved.t -> bool =
 fun p ->
  let open Paths.Path.Resolved in
  match p with
  | `Identifier _ -> false
  | `Subst (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `SubstAlias (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Hidden p -> should_reresolve (p :> t)
  | `Canonical (x, y) ->
      should_reresolve (x :> t) || should_resolve (y :> Paths.Path.t)
  | `Apply (x, y) ->
      should_reresolve (x :> t) || should_resolve (y :> Paths.Path.t)
  | `SubstT (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Alias (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Type (p, _)
  | `Class (p, _)
  | `ClassType (p, _)
  | `ModuleType (p, _)
  | `Module (p, _) ->
      should_reresolve (p :> t)
  | `OpaqueModule m -> should_reresolve (m :> t)
  | `OpaqueModuleType m -> should_reresolve (m :> t)

and should_resolve : Paths.Path.t -> bool =
 fun p -> match p with `Resolved p -> should_reresolve p | _ -> true

let add_module_docs m expn =
  let open Odoc_model.Lang in
  match expn with
  | Module.Signature sg ->
      let doc = Signature.Comment (`Docs m.Module.doc) in
      Module.Signature (doc :: sg)
  | _ -> expn

let type_path : Env.t -> Paths.Path.Type.t -> Paths.Path.Type.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(type_path empty p) in
    match cp with
    | `Resolved p ->
        let result = Tools.reresolve_type env p in
        `Resolved (result |> Cpath.resolved_type_path_of_cpath)
    | _ -> (
        match Tools.lookup_type_from_path env cp with
        | Resolved (p', _) -> `Resolved (Cpath.resolved_type_path_of_cpath p')
        | Unresolved unresolved ->
            Lookup_failures.report "Failed to lookup type %a"
              Component.Fmt.model_path
              (p :> Paths.Path.t);
            Cpath.type_path_of_cpath unresolved )

and module_type_path :
    Env.t -> Paths.Path.ModuleType.t -> Paths.Path.ModuleType.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(module_type_path empty p) in
    match cp with
    | `Resolved p ->
        `Resolved
          ( Tools.reresolve_module_type env p
          |> Cpath.resolved_module_type_path_of_cpath )
    | _ -> (
        match Tools.resolve_module_type env cp with
        | Resolved p'->
            `Resolved (Cpath.resolved_module_type_path_of_cpath p')
        | Unresolved unresolved ->
            Lookup_failures.report "Failed to resolve module type %a"
              Component.Fmt.model_path
              (p :> Paths.Path.t);
            Cpath.module_type_path_of_cpath unresolved )

and module_path : Env.t -> Paths.Path.Module.t -> Paths.Path.Module.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(module_path empty p) in
    match cp with
    | `Resolved p ->
        `Resolved
          (Tools.reresolve_module env p |> Cpath.resolved_module_path_of_cpath)
    | _ -> (
        match Tools.resolve_module env cp with
        | Resolved p' -> `Resolved (Cpath.resolved_module_path_of_cpath p')
        | Unresolved _ when is_forward p -> p
        | Unresolved unresolved ->
            Lookup_failures.report "Failed to resolve module %a"
              Component.Fmt.model_path
              (p :> Paths.Path.t);
            Cpath.module_path_of_cpath unresolved )

let rec unit (resolver : Env.resolver) t =
  let open Compilation_unit in
  let imports, env = Env.initial_env t resolver in
  Format.eprintf "Starting link\n%!";
  {
    t with
    content = content env t.content;
    doc = comment_docs env t.doc;
    imports;
  }

and content env =
  let open Compilation_unit in
  function
  | Module m -> Module (signature env m)
  | Pack _ -> failwith "Unhandled content"

and value_ env t =
  let open Value in
  (* Format.fprintf Format.err_formatter "Handling %a\n%!" Component.Fmt.model_identifier (t.id :> Paths.Identifier.t); *)
  let result =
    {
      t with
      doc = comment_docs env t.doc;
      type_ = type_expression env [] t.type_;
    }
  in
  (* Format.fprintf Format.err_formatter "Done\n%!"; *)
  result

and comment_inline_element :
    Env.t -> Comment.inline_element -> Comment.inline_element =
 fun env x ->
  match x with
  | `Styled (s, ls) ->
      `Styled (s, List.map (with_location comment_inline_element env) ls)
  | `Reference (r, []) -> (
      (* Format.fprintf Format.err_formatter "XXXXXXXXXX about to resolve reference: %a\n%!" (Component.Fmt.model_reference) r; *)
      match Ref_tools.resolve_reference env r with
      | Some (`Identifier (#Odoc_model.Paths.Identifier.Label.t as i) as r) ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX resolved reference: %a\n%!" (Component.Fmt.model_resolved_reference) r; *)
          let content =
            match Env.lookup_section_title i env with Some x -> x | None -> []
          in
          `Reference (`Resolved r, content)
      | Some x ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX resolved reference: %a\n%!" (Component.Fmt.model_resolved_reference) x; *)
          `Reference (`Resolved x, [])
      | None ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX FAILED to resolve reference: %a\n%!" (Component.Fmt.model_reference) r; *)
          `Reference (r, []) )
  | `Reference (r, content) as orig -> (
      (* Format.fprintf Format.err_formatter "XXXXXXXXXX about to resolve contentful reference: %a\n" (Component.Fmt.model_reference) r; *)
      match Ref_tools.resolve_reference env r with
      | Some x -> `Reference (`Resolved x, content)
      | None -> orig )
  | y -> y

and comment_nestable_block_element env (x : Comment.nestable_block_element) =
  match x with
  | `Paragraph elts ->
      `Paragraph (List.map (with_location comment_inline_element env) elts)
  | (`Code_block _ | `Verbatim _) as x -> x
  | `List (x, ys) ->
      `List
        ( x,
          List.map
            (List.map (with_location comment_nestable_block_element env))
            ys )
  | `Modules refs ->
      let refs =
        List.map
          (fun r ->
            match
              Ref_tools.resolve_module_reference env ~add_canonical:false r
            with
            | Some (r, _, _) -> `Resolved r
            | None -> r)
          refs
      in
      `Modules refs

and comment_block_element env (x : Comment.block_element) =
  match x with
  | #Comment.nestable_block_element as x ->
      (comment_nestable_block_element env x :> Comment.block_element)
  | `Heading _ as x -> x
  | `Tag _ as x -> x

and with_location :
    type a.
    (Env.t -> a -> a) ->
    Env.t ->
    a Location_.with_location ->
    a Location_.with_location =
 fun fn env x -> { x with Location_.value = fn env x.Location_.value }

and comment_docs env d = List.map (with_location comment_block_element env) d

and comment env = function
  | `Stop -> `Stop
  | `Docs d -> `Docs (comment_docs env d)

and exception_ env e =
  let open Exception in
  let res = Opt.map (type_expression env []) e.res in
  let args = type_decl_constructor_argument env e.args in
  let doc = comment_docs env e.doc in
  { e with res; args; doc }

and extension env t =
  let open Extension in
  let constructor c =
    let open Constructor in
    {
      c with
      args = type_decl_constructor_argument env c.args;
      res = Opt.map (type_expression env []) c.res;
      doc = comment_docs env c.doc;
    }
  in
  let type_path = type_path env t.type_path in
  let constructors = List.map constructor t.constructors in
  let doc = comment_docs env t.doc in
  { t with type_path; constructors; doc }

and external_ env e =
  let open External in
  {
    e with
    type_ = type_expression env [] e.type_;
    doc = comment_docs env e.doc;
  }

and class_type_expr env =
  let open ClassType in
  function
  | Constr (path, texps) ->
      Constr (path, List.map (type_expression env []) texps)
  | Signature s -> Signature (class_signature env s)

and class_type env c =
  let open ClassType in
  let doc = comment_docs env c.doc in
  { c with expr = class_type_expr env c.expr; doc }

and class_signature env c =
  let open ClassSignature in
  let env = Env.open_class_signature c env in
  let map_item = function
    | Method m -> Method (method_ env m)
    | InstanceVariable i -> InstanceVariable (instance_variable env i)
    | Constraint (t1, t2) ->
        Constraint (type_expression env [] t1, type_expression env [] t2)
    | Inherit c -> Inherit (class_type_expr env c)
    | Comment c -> Comment c
  in
  {
    self = Opt.map (type_expression env []) c.self;
    items = List.map map_item c.items;
  }

and method_ env m =
  let open Method in
  let doc = comment_docs env m.doc in
  { m with type_ = type_expression env [] m.type_; doc }

and instance_variable env i =
  let open InstanceVariable in
  let doc = comment_docs env i.doc in
  { i with type_ = type_expression env [] i.type_; doc }

and class_ env c =
  let open Class in
  let rec map_decl = function
    | ClassType expr -> ClassType (class_type_expr env expr)
    | Arrow (lbl, expr, decl) ->
        Arrow (lbl, type_expression env [] expr, map_decl decl)
  in
  let doc = comment_docs env c.doc in
  { c with type_ = map_decl c.type_; doc }

and module_substitution env m =
  let open ModuleSubstitution in
  let doc = comment_docs env m.doc in
  { m with manifest = module_path env m.manifest; doc }

and signature : Env.t -> Signature.t -> _ =
 fun env s ->
  let env = Env.open_signature s env in
  signature_items env s

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
      | Comment c -> Comment (comment env c)
      | TypExt t -> TypExt (extension env t)
      | Exception e -> Exception (exception_ env e)
      | External e -> External (external_ env e)
      | Class (r, c) -> Class (r, class_ env c)
      | ClassType (r, c) -> ClassType (r, class_type env c)
      | Include i -> Include (include_ env i)
      | Open o -> Open o)
    s

and module_expansion : Env.t -> Module.expansion -> Module.expansion =
 fun env m ->
  let open Module in
  match m with
  | AlreadyASig -> AlreadyASig
  | Signature sg -> Signature (signature env sg)
  | Functor (args, sg) ->
      let env' =
        List.fold_right
          (fun arg env ->
            match arg with
            | FunctorParameter.Unit -> env
            | Named arg ->
                let identifier = arg.FunctorParameter.id in
                let env' =
                  Env.add_module identifier
                    (Component.module_of_functor_argument
                       (Component.Of_Lang.functor_parameter
                          Component.Of_Lang.empty
                          (Ident.Of_Identifier.module_ arg.id)
                          arg))
                    env
                in
                env')
          args env
      in
      Functor (List.map (functor_argument env') args, signature env' sg)

and should_hide_moduletype : ModuleType.expr -> bool = function
  | Signature _ -> false
  | TypeOf x -> should_hide_module_decl x
  | With (e, _) -> should_hide_moduletype e
  | Functor (_, e) -> should_hide_moduletype e
  | Path p -> Paths.Path.is_hidden (p :> Paths.Path.t)

and build_hidden_moduletype : ModuleType.expr -> ModuleType.expr = function
  | Signature x -> Signature x
  | TypeOf _ -> Signature []
  | With (_, _) -> Signature []
  | Functor (x, e) -> Functor (x, build_hidden_moduletype e)
  | Path _ -> Signature []

and should_hide_module_decl : Module.decl -> bool = function
  | ModuleType t -> should_hide_moduletype t
  | Alias p -> Paths.Path.is_hidden (p :> Paths.Path.t)

and build_hidden_module_decl : Module.decl -> Module.decl = function
  | ModuleType t -> ModuleType (build_hidden_moduletype t)
  | Alias p -> Alias p

and module_ : Env.t -> Module.t -> Module.t =
 fun env m ->
  let open Module in
  let start_time = Unix.gettimeofday () in
  (* Format.fprintf Format.err_formatter "Processing Module %a\n%!"
     Component.Fmt.model_identifier
     (m.id :> Paths.Identifier.t); *)
  if m.hidden then m
  else
    let t1 = Unix.gettimeofday () in
    let m' =
      match Env.lookup_module m.id env with
      | Some m' -> m'
      | None ->
          kasprintf failwith "Failed to lookup module %a"
            Component.Fmt.model_identifier
            (m.id :> Paths.Identifier.t)
    in
    let env = Env.add_module_functor_args m' m.id env in
    let t2 = Unix.gettimeofday () in
    let type_ = module_decl env m.type_ in
    let t3 = Unix.gettimeofday () in
    let hidden_alias =
      match type_ with
      | Alias p when Paths.Path.is_hidden (p :> Paths.Path.t) -> true
      | _ -> false
    in
    let self_canonical =
      match type_ with
      | Alias (`Resolved p) -> (
          match Paths.Path.Resolved.Module.canonical_ident p with
          | Some i -> i = m.id (* Self-canonical *)
          | None -> false )
      | _ -> false
    in
    let expansion_needed = self_canonical || hidden_alias in
    (* Format.fprintf Format.err_formatter "moduletype_expansion=%b self_canonical=%b hidden_alias=%b expansion_needed=%b\n%!" moduletype_expansion self_canonical hidden_alias expansion_needed; *)
    let env, expansion =
      let sg_id = (m.id :> Paths.Identifier.Signature.t) in
      match (m.expansion, expansion_needed) with
      | None, true ->
          let env, expansion =
            match Expand_tools.expansion_of_module env m.id m' with
            | Ok (env, ce) ->
                let e = Lang_of.(module_expansion empty sg_id ce) in
                let compiled_e = Compile.expansion env e in
                (env, Some compiled_e)
            | Error `OpaqueModule -> (env, None)
            | Error _ ->
                kasprintf failwith "Failed to expand module %a"
                  Component.Fmt.model_identifier
                  (m.id :> Paths.Identifier.t)
          in
          (env, expansion)
      | _ -> (env, m.expansion)
    in
    let t4 = Unix.gettimeofday () in
    let expansion = Opt.map (module_expansion env) expansion in
    let doc, expansion =
      match m.doc with
      | _ :: _ -> (m.doc, expansion)
      | [] -> (
          match expansion with
          | Some
              (Signature
                (Comment (`Docs _doc) :: Comment (`Docs d2) :: expansion)) ->
              (d2, Some (Signature expansion))
          | _ -> ([], expansion) )
    in
    let override_display_type =
      self_canonical || should_hide_module_decl type_
    in
    let display_type =
      match (override_display_type, expansion) with
      | true, Some (Signature sg) -> Some (ModuleType (Signature sg))
      | true, Some (Functor _) -> Some (build_hidden_module_decl type_)
      | _ -> None
    in
    let result =
      { m with doc = comment_docs env doc; type_; display_type; expansion }
    in
    let end_time = Unix.gettimeofday () in
    let _timing =  Format.asprintf
      "%f seconds for module %a (t0-1=%f t1-2=%f t2-3=%f t3-4=%f t4-end=%f)\n%!"
      (end_time -. start_time) Component.Fmt.model_identifier
      (m.id :> Paths.Identifier.t)
      (t1 -. start_time) (t2 -. t1) (t3 -. t2) (t4 -. t3) (end_time -. t4) in
    result

and module_decl : Env.t -> Module.decl -> Module.decl =
 fun env decl ->
  let open Module in
  match decl with
  | ModuleType expr -> ModuleType (module_type_expr env expr)
  | Alias p -> Alias (module_path env p)

and module_type : Env.t -> ModuleType.t -> ModuleType.t =
 fun env m ->
  let open ModuleType in
  match Env.lookup_module_type m.id env with
  | None ->
      Lookup_failures.report "Failed to lookup module type %a"
        Component.Fmt.model_identifier
        (m.id :> Paths.Identifier.t);
      m
  | Some m' ->
      let env' = Env.add_module_type_functor_args m' m.id env in
      let expr' =
        match m.expr with
        | None -> None
        | Some expr -> Some (module_type_expr env' expr)
      in
      (* let self_canonical =
           match m.expr with
           | Some (Path (`Resolved p)) when Paths.Path.Resolved.ModuleType.canonical_ident p = Some m.id ->
             true
           | _ -> false
         in*)
      let display_expr =
        match expr' with
        | None -> None
        | Some expr -> (
            match (should_hide_moduletype expr, m.expansion) with
            | false, _ -> None
            | true, None -> None
            | true, Some Odoc_model.Lang.Module.AlreadyASig -> None
            | true, Some (Odoc_model.Lang.Module.Signature sg) ->
                Some (Some (Odoc_model.Lang.ModuleType.Signature sg))
            | true, Some (Odoc_model.Lang.Module.Functor _) ->
                Some (Some (build_hidden_moduletype expr)) )
      in
      let doc = comment_docs env m.doc in
      {
        m with
        expr = expr';
        expansion = Opt.map (module_expansion env') m.expansion;
        display_expr;
        doc;
      }

and include_ : Env.t -> Include.t -> Include.t =
 fun env i ->
  let open Include in
  let decl = module_decl env i.decl in
  let hidden_rhs = should_hide_module_decl decl in
  let doc = comment_docs env i.doc in
  let should_be_inlined =
    let is_inline_tag element =
      element.Odoc_model.Location_.value = `Tag `Inline
    in
    List.exists is_inline_tag doc
  in
  {
    i with
    decl;
    expansion =
      { resolved = true; content = signature_items env i.expansion.content };
    inline = should_be_inlined || hidden_rhs;
    doc;
  }

and functor_parameter_parameter :
    Env.t -> FunctorParameter.parameter -> FunctorParameter.parameter =
 fun env' a ->
  match
    let open Utils.ResultMonad in
    Env.lookup_module a.id env' |> of_option ~error:"lookup"
    >>= fun functor_arg ->
    let env = Env.add_module_functor_args functor_arg a.id env' in
    let sg_id = (a.id :> Paths.Identifier.Signature.t) in
    match (a.expansion, functor_arg.type_) with
    | None, ModuleType expr -> (
        match Expand_tools.expansion_of_module_type_expr env sg_id expr with
        | Ok (env, ce) ->
            let e = Lang_of.(module_expansion empty sg_id ce) in
            let compiled_e = Compile.expansion env e in
            Ok (env, Some compiled_e)
        | Error `OpaqueModule -> Ok (env, None)
        | Error _ -> Error "expand" )
    | x, _ -> Ok (env, x)
  with
  | Ok (env, expn) ->
      let expr = module_type_expr env a.expr in
      let display_expr =
        match (should_hide_moduletype expr, expn) with
        | false, _ -> None
        | true, None -> None
        | true, Some Odoc_model.Lang.Module.AlreadyASig -> None
        | true, Some (Odoc_model.Lang.Module.Signature sg) ->
            Some (Odoc_model.Lang.ModuleType.Signature sg)
        | true, Some (Odoc_model.Lang.Module.Functor _) ->
            Some (build_hidden_moduletype expr)
      in
      let expansion = Opt.map (module_expansion env) expn in
      { a with expr; display_expr; expansion }
  | Error s ->
      Lookup_failures.report "Failed to %s functor parameter %a" s
        Component.Fmt.model_identifier
        (a.id :> Odoc_model.Paths.Identifier.t);
      a

and functor_argument env a =
  match a with
  | FunctorParameter.Unit -> FunctorParameter.Unit
  | Named arg -> Named (functor_parameter_parameter env arg)

and handle_fragments env sg subs =
  let open ModuleType in
  let csubs =
    List.map Component.Of_Lang.(module_type_substitution empty) subs
  in
  (* Format.fprintf Format.err_formatter
     "Handling `With` expression for %a (expr=%a) [%a]\n%!"
     Component.Fmt.model_identifier
     (id :> Paths.Identifier.t)
     Component.Fmt.module_type_expr cexpr Component.Fmt.substitution_list
     (List.map Component.Of_Lang.(module_type_substitution empty) subs);*)
  List.fold_left2
    (fun (sg_res, subs) csub lsub ->
      (* Format.fprintf Format.err_formatter "Signature is: %a\n%!"
         Component.Fmt.signature sg; *)
      (* Format.fprintf Format.err_formatter "Handling sub: %a\n%!"
         Component.Fmt.substitution
         Component.Of_Lang.(module_type_substitution empty sub); *)
      
      match (sg_res, csub, lsub) with
      | Ok sg, Component.ModuleType.ModuleEq (cfrag, _), ModuleEq (frag, decl) ->
          let frag' =
            match cfrag with
            | `Resolved f ->
                `Resolved
                  ( Tools.reresolve_module_fragment env f
                  |> Lang_of.(Path.resolved_module_fragment empty) )
            | _ -> frag
          in
          let sg' =
            Tools.fragmap_module env cfrag
              Component.Of_Lang.(module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleEq (frag', module_decl env decl) :: subs)
      | Ok sg, TypeEq (cfrag, _), TypeEq (frag, eqn) ->
          let frag' =
            match cfrag with
            | `Resolved f ->
                `Resolved
                  ( Tools.reresolve_type_fragment env f
                  |> Lang_of.(Path.resolved_type_fragment empty) )
            | _ -> frag
          in
          let sg' =
            Tools.fragmap_type env cfrag
              Component.Of_Lang.(module_type_substitution empty lsub)
              sg
          in
          (Ok sg', TypeEq (frag', type_decl_equation env eqn) :: subs)
      | Ok sg, ModuleSubst (cfrag, _), ModuleSubst (frag, mpath) ->
          let frag' =
            match cfrag with
            | `Resolved f ->
                `Resolved
                  ( Tools.reresolve_module_fragment env f
                  |> Lang_of.(Path.resolved_module_fragment empty) )
            | _ -> frag
          in
          let sg' =
            Tools.fragmap_module env cfrag
              Component.Of_Lang.(module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleSubst (frag', module_path env mpath) :: subs)
      | Ok sg, TypeSubst (cfrag, _), TypeSubst (frag, eqn) ->
          let frag' =
            match cfrag with
            | `Resolved f ->
                `Resolved
                  ( Tools.reresolve_type_fragment env f
                  |> Lang_of.(Path.resolved_type_fragment empty) )
            | _ -> frag
          in
          let sg' =
            Tools.fragmap_type env cfrag
              Component.Of_Lang.(module_type_substitution empty lsub)
              sg
          in
          (Ok sg', TypeSubst (frag', type_decl_equation env eqn) :: subs)
      | Error _ as e, _, lsub ->
          (e, lsub::subs)
      | _ -> failwith "can't happen")
    (Ok sg, []) csubs subs
  |> snd |> List.rev

and module_type_expr : Env.t -> ModuleType.expr -> ModuleType.expr =
 fun env expr ->
  let open ModuleType in
  match expr with
  | Signature s -> Signature (signature env s)
  | Path p -> Path (module_type_path env p)
  | With (expr, subs) as unresolved -> (
      let cexpr = Component.Of_Lang.(module_type_expr empty expr) in
      match Tools.signature_of_module_type_expr env cexpr with
      | Ok sg -> With (module_type_expr env expr, handle_fragments env sg subs)
      | Error _ ->
          Lookup_failures.report "Failed to resolve module type %a"
            Component.Fmt.module_type_expr cexpr;
          unresolved )
  | Functor (arg, res) ->
      let arg' = functor_argument env arg in
      let res' = module_type_expr env res in
      Functor (arg', res')
  | TypeOf decl -> TypeOf (module_decl env decl)

and type_decl_representation :
    Env.t -> TypeDecl.Representation.t -> TypeDecl.Representation.t =
 fun env r ->
  let open TypeDecl.Representation in
  match r with
  | Variant cs -> Variant (List.map (type_decl_constructor env) cs)
  | Record fs -> Record (List.map (type_decl_field env) fs)
  | Extensible -> Extensible

and type_decl : Env.t -> TypeDecl.t -> TypeDecl.t =
 fun env t ->
  let open TypeDecl in
  let equation = type_decl_equation env t.equation in
  let doc = comment_docs env t.doc in
  let hidden_path =
    match equation.Equation.manifest with
    | Some (Constr (`Resolved path, params))
      when Paths.Path.Resolved.Type.is_hidden path ->
        Some (path, params)
    | _ -> None
  in
  let representation =
    Opt.map (type_decl_representation env) t.representation
  in
  let default = { t with equation; doc; representation } in
  let result =
    match hidden_path with
    | Some (p, params) -> (
        let p' =
          Component.Of_Lang.resolved_type_path Component.Of_Lang.empty p
        in
        match Tools.lookup_type_from_resolved_path env p' with
        | Ok (_, Found (`T t')) ->
            {
              default with
              equation =
                Expand_tools.collapse_eqns default.equation
                  (Lang_of.type_decl_equation Lang_of.empty t'.equation)
                  params;
            }
        | _ -> default )
    | None -> default
  in
  (* Format.fprintf Format.err_formatter "type_decl result: %a\n%!"
        Component.Fmt.type_decl (Component.Of_Lang.(type_decl empty result)); *)
  result

and type_decl_equation env t =
  let open TypeDecl.Equation in
  let manifest = Opt.map (type_expression env []) t.manifest in
  let constraints =
    List.map
      (fun (tex1, tex2) ->
        (type_expression env [] tex1, type_expression env [] tex2))
      t.constraints
  in
  { t with manifest; constraints }

and type_decl_field env f =
  let open TypeDecl.Field in
  let doc = comment_docs env f.doc in
  { f with type_ = type_expression env [] f.type_; doc }

and type_decl_constructor_argument env c =
  let open TypeDecl.Constructor in
  match c with
  | Tuple ts -> Tuple (List.map (type_expression env []) ts)
  | Record fs -> Record (List.map (type_decl_field env) fs)

and type_decl_constructor env c =
  let open TypeDecl.Constructor in
  let doc = comment_docs env c.doc in
  let args = type_decl_constructor_argument env c.args in
  let res = Opt.map (type_expression env []) c.res in
  { c with doc; args; res }

and type_expression_polyvar env visited v =
  let open TypeExpr.Polymorphic_variant in
  let constructor c =
    let open Constructor in
    let doc = comment_docs env c.doc in
    {
      c with
      arguments = List.map (type_expression env visited) c.arguments;
      doc;
    }
  in
  let element = function
    | Type t ->
        Type
          ( match type_expression env visited t with
          | Constr _ as x -> x
          | _ -> t )
        (* These have to remain Constrs *)
    | Constructor c -> Constructor (constructor c)
  in
  { v with elements = List.map element v.elements }

and type_expression_object env visited o =
  let open TypeExpr.Object in
  let method_ m = { m with type_ = type_expression env visited m.type_ } in
  let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expression env visited t)
  in
  { o with fields = List.map field o.fields }

and type_expression_package env visited p =
  let open TypeExpr.Package in
  let substitution (frag, t) =
    let cfrag = Component.Of_Lang.(type_fragment empty frag) in
    let frag' =
      match cfrag with
      | `Resolved f -> `Resolved (Tools.reresolve_type_fragment env f)
      | _ -> cfrag
    in
    (Lang_of.(Path.type_fragment empty frag'), type_expression env visited t)
  in
  {
    path = module_type_path env p.path;
    substitutions = List.map substitution p.substitutions;
  }

and type_expression : Env.t -> _ -> _ =
 fun env visited texpr ->
  let open TypeExpr in
  match texpr with
  | Var _ | Any -> texpr
  | Alias (t, str) -> Alias (type_expression env visited t, str)
  | Arrow (lbl, t1, t2) ->
      Arrow (lbl, type_expression env visited t1, type_expression env visited t2)
  | Tuple ts -> Tuple (List.map (type_expression env visited) ts)
  | Constr (path', ts') -> (
      let path = type_path env path' in
      let ts = List.map (type_expression env visited) ts' in
      if not (Odoc_model.Paths.Path.is_hidden (path :> Odoc_model.Paths.Path.t))
      then Constr (path, ts)
      else
        let cp = Component.Of_Lang.(type_path empty path') in
        match Tools.lookup_type_from_path env cp with
        | Resolved (cp', Found (`T t)) ->
            let p = Cpath.resolved_type_path_of_cpath cp' in
            if List.mem p visited then raise Loop
            else if Cpath.is_resolved_type_hidden cp' then
              match t.Component.TypeDecl.equation with
              | { manifest = Some expr; params; _ } -> (
                  let map =
                    List.fold_left2
                      (fun acc param sub ->
                        match param with
                        | Odoc_model.Lang.TypeDecl.Var x, _ -> (x, sub) :: acc
                        | Any, _ -> acc)
                      [] params ts
                  in
                  try
                    let t' =
                      Expand_tools.type_expr map Lang_of.(type_expr empty expr)
                    in
                    type_expression env (p :: visited) t'
                  with
                  | Loop ->
                      Constr (`Resolved p, ts)
                  | _ -> Constr (`Resolved p, ts) )
              | _ -> Constr (`Resolved p, ts)
            else Constr (`Resolved p, ts)
        | Resolved (cp', Found _) ->
            let p = Cpath.resolved_type_path_of_cpath cp' in
            Constr (`Resolved p, ts)
        | Resolved (_cp, Replaced x) -> Lang_of.(type_expr empty x)
        | Unresolved p -> Constr (Cpath.type_path_of_cpath p, ts) )
  | Polymorphic_variant v ->
      Polymorphic_variant (type_expression_polyvar env visited v)
  | Object o -> Object (type_expression_object env visited o)
  | Class (path, ts) -> Class (path, List.map (type_expression env visited) ts)
  | Poly (strs, t) -> Poly (strs, type_expression env visited t)
  | Package p -> Package (type_expression_package env visited p)

(*
let build_resolver :
    ?equal:(Root.t -> Root.t -> bool) ->
    ?hash:(Root.t -> int) ->
    (string -> Env.lookup_unit_result) ->
    (Root.t -> Compilation_unit.t) ->
    (string -> Root.t option) ->
    (Root.t -> Page.t) ->
    Env.resolver =
 fun ?equal:_ ?hash:_ lookup_unit resolve_unit lookup_page resolve_page ->
  { Env.lookup_unit; resolve_unit; lookup_page; resolve_page }
*)
let link x y = Lookup_failures.catch_failures (fun () -> unit x y)

let resolve_page resolver y =
  let env = Env.set_resolver Env.empty resolver in
  Lookup_failures.catch_failures (fun () ->
      {
        y with
        Page.content =
          List.map (with_location comment_block_element env) y.Page.content;
      })
