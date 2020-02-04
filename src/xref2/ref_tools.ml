open Odoc_model.Paths
open Odoc_model.Names

open Reference

type module_lookup_result = Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

type module_type_lookup_result = Resolved.ModuleType.t * Cpath.Resolved.module_type * Component.ModuleType.t

type signature_lookup_result =
  Resolved.Signature.t * Cpath.Resolved.parent * Component.Signature.t

type type_lookup_result =
  Resolved.Type.t
  * [ `T of Component.TypeDecl.t
    | `C of Component.Class.t
    | `CT of Component.ClassType.t ]

type value_lookup_result =
  Resolved.Value.t * [ `V of Component.Value.t | `E of Component.External.t ]

type label_parent_lookup_result =
  Resolved.LabelParent.t
  * Cpath.Resolved.parent option
  * [ `S of Component.Signature.t | `CS of Component.ClassSignature.t | `Page of (string * Identifier.Label.t) list ]
(* 
let rec make_prefix : Resolved.Signature.t -> Cpath.Resolved.module_ option =
  let open Tools.OptionMonad in
  function
  | `Module (parent, name) ->
      make_prefix parent >>= fun p -> return (`Module (`Module p, name))
  | `Identifier (#Identifier.Module.t as i) -> return (`Identifier i)
  | `Canonical (m, _r) -> make_prefix (m :> Resolved.Signature.t)
  | `SubstAlias (_, r) -> make_prefix (r :> Resolved.Signature.t)
  | `Identifier _ | `ModuleType _ -> None

let prefix_signature r s =
  match make_prefix r with
  | Some prefix ->
      (* Format.fprintf Format.err_formatter
         "Prefixing with Cpath.resolved_module: %a\n%!"
         Component.Fmt.resolved_module_path prefix;*)
      Tools.prefix_signature (`Module prefix, s) |> snd
  | None ->
      let identifier = Resolved.Signature.identifier r in
      (* Format.fprintf Format.err_formatter "Prefixing with Identifier: %a\n%!"
         Component.Fmt.model_identifier
         (identifier :> Identifier.t);*)
      Tools.prefix_ident_signature (identifier, s) |> snd *)

let rec choose l =
  match l with
  | [] -> None
  | x :: rest -> ( match x () with Some _ as x -> x | None -> choose rest )

let signature_lookup_result_of_label_parent :
    label_parent_lookup_result -> signature_lookup_result option =
 fun (rr, cp, c) ->
  match (rr, cp, c) with
  | (`Identifier #Odoc_model.Paths.Identifier.Signature.t as rr'), Some p, `S s ->
      Some (rr', p, s)
  | ((`SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _) as rr'), Some p,  `S s ->
      Some (rr', p, s)
  | _ -> None

module Hashable = struct
  type t = bool * Resolved.Signature.t

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash
end

module Memos1 = Hashtbl.Make (Hashable)

(*  let memo = Memos1.create 91*)

module Hashable2 = struct
  type t = bool * Signature.t

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash
end

module Memos2 = Hashtbl.Make (Hashable2)

let memo2 = Memos2.create 91

let module_lookup_to_signature_lookup : Env.t -> module_lookup_result -> signature_lookup_result option =
  fun env (ref, cp, m) -> Some ((ref :> Resolved.Signature.t), `Module cp, Tools.signature_of_module env m)

let module_type_lookup_to_signature_lookup : Env.t -> module_type_lookup_result -> signature_lookup_result option =
  fun env (ref, cp, m) -> Some ((ref :> Resolved.Signature.t), `ModuleType cp, Tools.signature_of_module_type env m)

let rec gen_resolved_module_reference_of_cpath : Cpath.Resolved.module_ -> Resolved.Module.t =
  function
  | `Module (`Module parent, name) -> `Module ((gen_resolved_module_reference_of_cpath parent :> Resolved.Signature.t), name)
  | `Subst (_, p) -> gen_resolved_module_reference_of_cpath p
  | `SubstAlias (p1, p2) -> `SubstAlias (Lang_of.(Path.resolved_module empty p1), gen_resolved_module_reference_of_cpath p2)
  | `Alias (p1, p2) -> `SubstAlias (Lang_of.(Path.resolved_module empty p1), gen_resolved_module_reference_of_cpath p2)
  | `Hidden p -> gen_resolved_module_reference_of_cpath p
  | `Local _ -> failwith "boo"
  | `Identifier id -> `Identifier id
  | `Substituted p -> gen_resolved_module_reference_of_cpath p
  | `Canonical (p, `Resolved (`Alias (_, p2))) 
  | `Canonical (p, `Resolved p2) -> `Canonical (gen_resolved_module_reference_of_cpath p, `Resolved (gen_resolved_module_reference_of_cpath p2))
  | `Apply _ -> failwith "boo"
  | `Canonical (p, _) -> gen_resolved_module_reference_of_cpath p
  | `Module (_, _) -> failwith "boo"

let rec resolved_module_reference_of_cpath : Odoc_model.Paths.Reference.Resolved.Signature.t -> Cpath.Resolved.parent -> Cpath.Resolved.module_ -> Odoc_model.Paths.Reference.Resolved.Module.t = 
  fun ref_parent path_parent path ->
    match path with
    | `Module (parent, name) when parent == path_parent -> `Module (ref_parent, name )
    | `Subst (_, p) -> resolved_module_reference_of_cpath ref_parent path_parent p
    | `SubstAlias (p1, p2) -> `SubstAlias (Lang_of.(Path.resolved_module empty p1), resolved_module_reference_of_cpath ref_parent path_parent p2)
    | `Alias (p1, p2) -> `SubstAlias (Lang_of.(Path.resolved_module empty p1), resolved_module_reference_of_cpath ref_parent path_parent p2)
    | `Hidden p -> resolved_module_reference_of_cpath ref_parent path_parent p
    | `Canonical (p, `Resolved c) -> `Canonical (resolved_module_reference_of_cpath ref_parent path_parent p, `Resolved (gen_resolved_module_reference_of_cpath c))
    | `Canonical (p, _) -> resolved_module_reference_of_cpath ref_parent path_parent p
    | `Substituted p -> resolved_module_reference_of_cpath ref_parent path_parent p
    | `Apply _
    | `Identifier _ 
    | `Local _
    | `Module _ ->
      Format.fprintf Format.err_formatter "Erk! %a (path_parent=%a)\n%!"
        Component.Fmt.resolved_module_path path
        Component.Fmt.resolved_parent_path path_parent;
      failwith "Unexpected failure in Ref_tools.resolved_module_reference_of_cpath"

let rec resolved_module_type_reference_of_cpath : Odoc_model.Paths.Reference.Resolved.Signature.t -> Cpath.Resolved.parent -> Cpath.Resolved.module_type -> Odoc_model.Paths.Reference.Resolved.ModuleType.t =
  fun ref_parent path_parent path ->
    match path with
    | `ModuleType (parent, name) when parent == path_parent -> `ModuleType (ref_parent, name)
    | `Substituted s -> resolved_module_type_reference_of_cpath ref_parent path_parent s
    | `Local _
    | `Identifier _ 
    | `ModuleType _ -> failwith "Unexpected failure in Ref_tools.resolved_module_type_reference_of_cpath"

    
let rec add_canonical_path : Env.t -> Component.Module.t -> Odoc_model.Paths.Reference.Resolved.Module.t -> Odoc_model.Paths.Reference.Resolved.Module.t =
  fun env m r ->
  match m.Component.Module.canonical with
  | Some (_cp, cr) -> (
    match resolve_module_reference env cr ~add_canonical:false with
    | Some (canonical,_,_) -> `Canonical (r, `Resolved canonical)
    | None -> r
  )
  | None -> r 

and resolve_type_reference : Env.t -> Type.t -> type_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved _r -> failwith "unhandled"
    | `Root (name, _) -> (
        Env.lookup_datatype_by_name (Odoc_model.Names.UnitName.to_string name) env >>= function
        | `Type (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `T t )
        | `Class (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `C t )
        | `ClassType (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `CT t ) )
    | `Dot (parent, name) ->
        resolve_label_parent_reference env parent ~add_canonical:true
        >>= signature_lookup_result_of_label_parent
        >>= fun (parent', cp, sg) ->
        let _, sg = Tools.prefix_signature (cp, sg) in
        Find.opt_type_in_sig sg name >>= fun t ->
        return (`Type (parent', TypeName.of_string name), t)
    | `Class (parent, name) -> (
        resolve_signature_reference env parent ~add_canonical:true
        >>= fun (parent', _, sg) ->
        Find.opt_type_in_sig sg (ClassName.to_string name) >>= function
        | `C _ as c -> return (`Class (parent', name), c)
        | _ -> None )
    | `ClassType (parent, name) -> (
        resolve_signature_reference env parent ~add_canonical:true
        >>= fun (parent', cp, sg) ->
        let _, sg = Tools.prefix_signature (cp, sg) in
        Find.opt_type_in_sig sg (ClassTypeName.to_string name) >>= function
        | `CT _ as c -> return (`ClassType (parent', name), c)
        | _ -> None )
    | `Type (parent, name) -> (
        resolve_signature_reference env parent ~add_canonical:true
        >>= fun (parent', cp, sg) ->
        let _, sg = Tools.prefix_signature (cp, sg) in
        Find.opt_type_in_sig sg (TypeName.to_string name) >>= function
        | `T _ as c -> return (`Type (parent', name), c)
        | _ -> None )
 
and entry_count = ref 0

and find_module : Env.t -> LabelParent.t -> string -> add_canonical:bool -> module_lookup_result option =
  fun env parent name ~add_canonical ->
  let open Tools.OptionMonad in
  incr entry_count;
  let my_entry_count = !entry_count in
  Format.fprintf Format.err_formatter "Entering find_module: entry_count=%d\n%!" my_entry_count;
  resolve_label_parent_reference env parent ~add_canonical
  >>= signature_lookup_result_of_label_parent
  >>= fun (parent', cp, sg) ->
  let _, sg = Tools.prefix_signature (cp, sg) in
  (try Some (Tools.handle_module_lookup env add_canonical name cp sg) with _ -> None)
  >>= fun (cp', m) ->
  let resolved_ref = resolved_module_reference_of_cpath parent' cp cp' in
  Format.fprintf Format.err_formatter "find_module: parent'=%a cp=%a cp'=%a resolved_ref=%a\n%!"
    Component.Fmt.model_resolved_reference (parent' :> Resolved.t)
    Component.Fmt.resolved_parent_path cp 
    Component.Fmt.resolved_module_path cp'
    Component.Fmt.model_resolved_reference (resolved_ref :> Resolved.t);
  Format.fprintf Format.err_formatter "Leaving find_module: entry_count was %d\n%!" my_entry_count;
  return (resolved_ref, cp', m)

and find_module_type : Env.t -> LabelParent.t -> string -> add_canonical:bool -> module_type_lookup_result option =
  fun env parent name ~add_canonical ->
  let open Tools.OptionMonad in
  resolve_label_parent_reference env parent ~add_canonical
  >>= signature_lookup_result_of_label_parent
  >>= fun (parent', cp, sg) ->
  let _, sg = Tools.prefix_signature (cp, sg) in
  (try Some (Tools.handle_module_type_lookup name cp sg) with _ -> None)
  >>= fun (cp', m) ->
  let resolved_ref = resolved_module_type_reference_of_cpath parent' cp cp' in
  return (resolved_ref, cp', m)

and resolve_module_reference :
    Env.t -> Module.t -> add_canonical:bool -> module_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ~add_canonical ->
    match r with
    | `Resolved _r ->
        failwith "What's going on!?"
(*        Some (resolve_resolved_module_reference env r ~add_canonical)*)
    | `Dot (parent, name) -> find_module env parent name ~add_canonical
    | `Module (parent, name) -> find_module env (parent :> LabelParent.t) (Odoc_model.Names.ModuleName.to_string name) ~add_canonical
    | `Root (name, _) -> (
        match Env.lookup_module_by_name (UnitName.to_string name) env with
        | Some (`Module (id, m)) ->
            let path = if add_canonical then Tools.add_canonical_path env m (`Identifier id) else (`Identifier id) in
            let ref = if add_canonical then add_canonical_path env m (`Identifier id) else (`Identifier id) in
            return (ref, path, m)
        | None -> (
            let x = Env.lookup_root_module (UnitName.to_string name) env in
            match x with
            | Some (Env.Resolved (id, m)) ->
                let path = Tools.add_canonical_path env m (`Identifier id) in
                let ref = if add_canonical then add_canonical_path env m (`Identifier id) else `Identifier id in
                return (ref,path, m)
            | _ -> None ) )

and resolve_module_type_reference :
    Env.t ->
    ModuleType.t ->
    add_canonical:bool ->
    module_type_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ~add_canonical ->
    match r with
    | `Resolved _r ->
      failwith "What's going oN!?"
      (*Some (resolve_resolved_module_type_reference env r)*)
    | `Dot (parent, name) ->
       find_module_type env parent name ~add_canonical
    | `ModuleType (parent, name) ->
        find_module_type env (parent :> LabelParent.t) (Odoc_model.Names.ModuleTypeName.to_string name) ~add_canonical
    | `Root (name, _) ->
        Env.lookup_module_type_by_name (UnitName.to_string name) env >>= fun (`ModuleType (id, m)) ->
        return (`Identifier id, `Identifier id, m)

and resolve_label_parent_reference :
    Env.t ->
    LabelParent.t ->
    add_canonical:bool ->
    label_parent_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ~add_canonical ->
    let label_parent_res_of_sig_res :
        signature_lookup_result -> label_parent_lookup_result option =
     fun (r', cp, sg) ->
      return ((r' :> Resolved.LabelParent.t), Some cp, `S sg)
    in
    match r with
    | `Resolved _ -> failwith "unimplemented"
    | ( `Module _ | `ModuleType _
      | `Root (_, #Odoc_model.Paths_types.Reference.tag_module) ) as sr ->
        resolve_signature_reference env sr ~add_canonical
        >>= label_parent_res_of_sig_res
    | `Dot (parent, name) ->
        choose [
          (fun () ->
            find_module env parent name ~add_canonical
            >>= module_lookup_to_signature_lookup env
            >>= label_parent_res_of_sig_res);
          (fun () ->
            find_module_type env parent name ~add_canonical
            >>= module_type_lookup_to_signature_lookup env
            >>= label_parent_res_of_sig_res);
        ]
    | `Root (name, _) ->
        Env.lookup_page (UnitName.to_string name) env >>= fun p ->
        let labels = List.fold_right
                (fun element l ->
                  match element.Odoc_model.Location_.value with
                  | `Heading (_, (`Label (_, name) as x), _nested_elements) ->
                    (LabelName.to_string name,x) :: l
                  | _ -> l) p.Odoc_model.Lang.Page.content [] in
        return ((`Identifier (p.Odoc_model.Lang.Page.name :> Identifier.LabelParent.t)), None, `Page labels)
    | _ -> None

and resolve_signature_reference :
    Env.t -> Signature.t -> add_canonical:bool -> signature_lookup_result option
    =
  let open Tools.OptionMonad in
  fun env' r ~add_canonical ->

    let id = (add_canonical, r) in
    (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_from_resolved_path: looking up %a\n%!" Component.Fmt.resolved_path p; *)
    let resolve env =
      (* Format.fprintf Format.err_formatter "B"; *)
      match r with
      | `Resolved _r ->
        failwith "What's going on here then?"
          (* Some (resolve_resolved_signature_reference env r ~add_canonical) *)
      | `Root (_, `TModule)
      | `Module (_, _) as r ->
        resolve_module_reference env r ~add_canonical
        >>= module_lookup_to_signature_lookup env
      | `Root (_, `TModuleType)
      | `ModuleType (_, _) as r ->
        resolve_module_type_reference env r ~add_canonical
        >>= module_type_lookup_to_signature_lookup env
      | `Root (_, `TUnknown)
      | `Dot (_,_) as r ->
        choose [
          (fun () ->
            resolve_module_reference env r ~add_canonical
            >>= module_lookup_to_signature_lookup env);
          (fun () ->
            resolve_module_type_reference env r ~add_canonical
            >>= module_type_lookup_to_signature_lookup env)
        ]
    in
    (*        Memos2.add memo2 id result; *)
    match Memos2.find_all memo2 id with
    | [] ->
        let lookups, resolved = Env.with_recorded_lookups env' resolve in
        Memos2.add memo2 id (resolved, lookups);
        resolved
    | xs ->
        let rec find = function
          | [] ->
            let lookups, resolved = Env.with_recorded_lookups env' resolve in
            Memos2.add memo2 id (resolved, lookups);
            resolved
          | (resolved, lookups) :: xs ->
              if Tools.verify_lookups env' lookups then
                (*Format.fprintf Format.err_formatter "G";*) resolved
              else find xs
        in
        find xs

and resolve_value_reference : Env.t -> Value.t -> value_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Root (name, _) -> (
        Env.lookup_value_by_name (UnitName.to_string name) env >>= function
        | `Value (id, x) -> return (`Identifier id, `V x)
        | `External (id, x) -> return (`Identifier id, `E x) )
    | `Dot (parent, name) -> (
        resolve_label_parent_reference env parent ~add_canonical:true
        >>= signature_lookup_result_of_label_parent
        >>= fun (parent', _, sg) ->
        match Find.opt_value_in_sig sg name with
        | Some v -> return (`Value (parent', ValueName.of_string name), v)
        | None -> None )
    | _ -> failwith "erk"

and resolve_label_reference : Env.t -> Label.t -> Resolved.Label.t option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved r -> Some r
    | `Root (name, _) -> (
        Env.lookup_any_by_name (UnitName.to_string name) env >>= function
        | `Label id -> return (`Identifier id)
        | _ -> None )
    | `Dot (parent, name) -> (
        resolve_label_parent_reference env parent ~add_canonical:true
        >>= fun (p, _env, sg) ->
        match sg with
        | `S sg ->
            Find.opt_label_in_sig sg name >>= fun _ ->
            Some (`Label (p, LabelName.of_string name))
        | `CS _sg -> None
        | `Page p -> (
            try Some (`Identifier (List.assoc name p)) with _ -> None))
    | `Label (parent, name) -> (
        resolve_label_parent_reference env parent ~add_canonical:true
        >>= fun (p, _, sg) ->
        match sg with
        | `S sg ->
            Find.opt_label_in_sig sg (LabelName.to_string name) >>= fun _ ->
            Some (`Label (p, name))
        | `CS _sg -> None
        | `Page p -> (
            try Some (`Identifier (List.assoc (LabelName.to_string name) p)) with _ -> None)) 

and resolve_reference : Env.t -> t -> Resolved.t option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> (
        Env.lookup_any_by_name (UnitName.to_string name) env >>= function
        | `Module (_, _) -> begin
            (* Make sure we handle aliases correctly *)
            resolve_module_reference env ~add_canonical:true (`Root (name, `TModule))
            >>= fun (r, _, _) -> Some (r :> Resolved.t)
          end
        | `ModuleType (_, _) -> begin
            resolve_module_type_reference env ~add_canonical:true (`Root (name, `TModuleType))
            >>= fun (r, _, _) -> Some (r :> Resolved.t)
          end
        | `Value (id, _) ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Type (id, _) ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Label id ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Class (id, _) ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `ClassType (id, _) ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `External (id, _) ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t)) )
    | `Resolved r -> Some r
    | (`Root (_, `TModule) | `Module (_, _)) as r ->
        resolve_module_reference env r ~add_canonical:true >>= fun (x, _, _) ->
        return (x :> Resolved.t)
    | (`Root (_, `TModuleType) | `ModuleType (_, _)) as r ->
        resolve_module_type_reference env r ~add_canonical:true
        >>= fun (x, _, _) -> return (x :> Resolved.t)
    | (`Root (_, `TType) | `Type (_, _)) as r ->
        resolve_type_reference env r >>= fun (x, _) -> return (x :> Resolved.t)
    | (`Root (_, `TValue) | `Value (_, _)) as r ->
        resolve_value_reference env r >>= fun (x, _) -> return (x :> Resolved.t)
    | (`Root (_, `TLabel) | `Label (_, _)) as r ->
        resolve_label_reference env r >>= fun x -> return (x :> Resolved.t)
    | (`Root (name, `TPage)) -> (
        match Env.lookup_page (UnitName.to_string name) env with
        | Some p -> Some (`Identifier (p.Odoc_model.Lang.Page.name :> Identifier.t))
        | None -> None
      )
    | `Dot (_, _) as r ->
        choose
          [
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying type reference\n%!"; *)
              resolve_type_reference env r >>= fun (x, _) ->
              return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying module reference\n%!"; *)
              resolve_module_reference env r ~add_canonical:true
              >>= fun (x, _, _) -> return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying module_type reference\n%!"; *)
              resolve_module_type_reference env r ~add_canonical:true
              >>= fun (x, _, _) -> return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying label reference\n%!"; *)
              resolve_label_reference env r >>= fun x -> return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying value reference\n%!"; *)
              resolve_value_reference env r >>= fun (x, _) ->
              return (x :> Resolved.t));
          ]
    | _ -> None


let _ =
  Tools.resolve_module_ref := resolve_module_reference ~add_canonical:false
