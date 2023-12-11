(* A bunch of association lists. Let's hashtbl them up later *)
open Odoc_model
open Odoc_model.Names
open Odoc_model.Paths

type lookup_unit_result =
  | Forward_reference
  | Found of Lang.Compilation_unit.t
  | Not_found

type lookup_page_result = Lang.Page.t option

type root =
  | Resolved of (Odoc_model.Root.t * Identifier.Module.t * Component.Module.t)
  | Forward

type resolver = {
  open_units : string list;
  lookup_unit : string -> lookup_unit_result;
  lookup_page : string -> lookup_page_result;
}

let unique_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

type lookup_type =
  | Module of Paths.Identifier.Path.Module.t
  | ModuleType of Paths.Identifier.Path.ModuleType.t
  | RootModule of string * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of string * Paths.Identifier.Path.Module.t
  | FragmentRoot of int

let pp_lookup_type fmt =
  let fmtrm fmt = function
    | Some `Forward -> Format.fprintf fmt "Some (Forward)"
    | Some (`Resolved digest) -> Format.fprintf fmt "Some (Resolved %s)" digest
    | None -> Format.fprintf fmt "None"
  in
  function
  | Module r ->
      Format.fprintf fmt "Module %a" Component.Fmt.model_identifier
        (r :> Identifier.t)
  | ModuleType r ->
      Format.fprintf fmt "ModuleType %a" Component.Fmt.model_identifier
        (r :> Identifier.t)
  | RootModule (str, res) -> Format.fprintf fmt "RootModule %s %a" str fmtrm res
  | ModuleByName (n, r) ->
      Format.fprintf fmt "ModuleByName %s, %a" n Component.Fmt.model_identifier
        (r :> Identifier.t)
  | FragmentRoot i -> Format.fprintf fmt "FragmentRoot %d" i

let pp_lookup_type_list fmt ls =
  let rec inner fmt = function
    | [] -> Format.fprintf fmt ""
    | [ x ] -> Format.fprintf fmt "%a" pp_lookup_type x
    | x :: ys -> Format.fprintf fmt "%a; %a" pp_lookup_type x inner ys
  in
  Format.fprintf fmt "[%a]" inner ls

module LookupTypeSet = Set.Make (struct
  type t = lookup_type

  let compare = compare
end)

type recorder = { mutable lookups : LookupTypeSet.t }

module Maps = Paths.Identifier.Maps
module StringMap = Map.Make (String)

(** Used only to handle shadowing, see {!Elements}. *)
type kind =
  | Kind_Module
  | Kind_ModuleType
  | Kind_Type
  | Kind_Value
  | Kind_Label
  | Kind_Class
  | Kind_ClassType
  | Kind_Constructor
  | Kind_Exception
  | Kind_Extension
  | Kind_Field

module ElementsByName : sig
  type t

  val empty : t

  val add : kind -> string -> [< Component.Element.any ] -> t -> t

  val find_by_name :
    (Component.Element.any -> 'b option) -> string -> t -> 'b list
end = struct
  type elem = { kind : kind; elem : Component.Element.any }

  type t = elem list StringMap.t

  let empty = StringMap.empty

  let add kind name elem t =
    let elem = (elem :> Component.Element.any) in
    let tl =
      try
        let tl = StringMap.find name t in
        let not_shadow e = e.kind <> kind in
        if not (List.for_all not_shadow tl) then List.filter not_shadow tl
        else tl
      with Not_found -> []
    in
    StringMap.add name ({ kind; elem } :: tl) t

  let find_by_name f name t =
    let filter e acc = match f e.elem with Some r -> r :: acc | None -> acc in
    try List.fold_right filter (StringMap.find name t) [] with Not_found -> []
end

module ElementsById : sig
  type t

  val empty : t

  val add :
    [< Identifier.t_pv ] Paths.Identifier.id ->
    [< Component.Element.any ] ->
    t ->
    t

  val find_by_id :
    [< Identifier.t_pv ] Paths.Identifier.id ->
    t ->
    Component.Element.any option
end = struct
  module IdMap = Identifier.Maps.Any

  type t = Component.Element.any IdMap.t

  let empty = IdMap.empty

  let add identifier element t =
    IdMap.add (identifier :> Identifier.t) (element :> Component.Element.any) t

  let find_by_id identifier t =
    try Some (IdMap.find (identifier :> Identifier.t) t)
    with Not_found -> None
end

type 'a amb_err = [ `Ambiguous of 'a * 'a list ]

type t = {
  linking : bool;
  (* True if this is a linking environment - if not, we only put in modules,
     module types, types, classes and class types *)
  id : int;
  elts : ElementsByName.t;
      (** Elements mapped by their name. Queried with {!find_by_name}. *)
  ids : ElementsById.t;
      (** Elements mapped by their identifier. Queried with {!find_by_id}. *)
  ambiguous_labels : Component.Element.label amb_err Identifier.Maps.Label.t;
  resolver : resolver option;
  recorder : recorder option;
  fragmentroot : (int * Component.Signature.t) option;
}

let is_linking env = env.linking

let set_resolver t resolver = { t with resolver = Some resolver }

let has_resolver t = match t.resolver with None -> false | _ -> true

let id t = t.id

let with_recorded_lookups env f =
  let recorder = { lookups = LookupTypeSet.empty } in
  let env' = { env with recorder = Some recorder } in
  let restore () =
    match env.recorder with
    | Some r -> r.lookups <- LookupTypeSet.union recorder.lookups r.lookups
    | None -> ()
  in
  try
    let result = f env' in
    restore ();
    (recorder.lookups, result)
  with e ->
    restore ();
    raise e

let empty =
  {
    linking = true;
    id = 0;
    elts = ElementsByName.empty;
    ids = ElementsById.empty;
    resolver = None;
    recorder = None;
    ambiguous_labels = Identifier.Maps.Label.empty;
    fragmentroot = None;
  }

let add_fragment_root sg env =
  let id = unique_id () in
  { env with fragmentroot = Some (id, sg); id }

(** Implements most [add_*] functions. *)
let add_to_elts kind identifier component env =
  if not env.linking then
    assert (
      List.mem kind
        [ Kind_Module; Kind_ModuleType; Kind_Type; Kind_Class; Kind_ClassType ]);
  let _ =
    let other = ElementsById.find_by_id identifier env.ids in
    match other with
    | Some _ ->
        (* Format.eprintf "Overriding duplicate env entry: %s\n%!" (Identifier.name identifier); *)
        ()
    | None -> ()
  in
  let name = Identifier.name identifier in
  {
    env with
    id = unique_id ();
    elts = ElementsByName.add kind name component env.elts;
    ids = ElementsById.add identifier component env.ids;
  }

let add_label identifier heading env =
  assert env.linking;
  let comp = `Label (identifier, heading) in
  let name = Identifier.name identifier in
  let ambiguous_labels =
    match ElementsById.find_by_id identifier env.ids with
    | Some (#Component.Element.label as l) ->
        let err =
          try
            match
              Identifier.Maps.Label.find identifier env.ambiguous_labels
            with
            | `Ambiguous (x, others) -> `Ambiguous (x, comp :: others)
          with Not_found -> `Ambiguous (l, [ comp ])
        in

        Identifier.Maps.Label.add identifier err env.ambiguous_labels
    | Some _ -> assert false
    | None -> env.ambiguous_labels
  in
  {
    env with
    id = unique_id ();
    elts =
      ElementsByName.add Kind_Label name
        (comp :> Component.Element.any)
        env.elts;
    ambiguous_labels;
    ids = ElementsById.add identifier comp env.ids;
  }

let add_docs (docs : Comment.docs) env =
  assert env.linking;
  List.fold_left
    (fun env -> function
      | { Location_.value = `Heading (attrs, id, text); location } ->
          let label = Ident.Of_Identifier.label id in
          add_label id { Component.Label.attrs; label; text; location } env
      | _ -> env)
    env docs

let add_comment (com : Comment.docs_or_stop) env =
  match com with `Docs doc -> add_docs doc env | `Stop -> env

let add_cdocs p (docs : Component.CComment.docs) env =
  List.fold_left
    (fun env element ->
      match element.Location_.value with
      | `Heading h ->
          let (`LLabel (name, _)) = h.Component.Label.label in
          let label =
            Paths.Identifier.Mk.label (Paths.Identifier.label_parent p, name)
          in
          add_label label h env
      | _ -> env)
    env docs

let add_module identifier m docs env =
  let env' = add_to_elts Kind_Module identifier (`Module (identifier, m)) env in
  if env.linking then add_cdocs identifier docs env' else env'

let add_type (identifier : Identifier.Type.t) t env =
  let open Component in
  let open_typedecl cs =
    let add_cons env (cons : TypeDecl.Constructor.t) =
      let ident =
        Paths.Identifier.Mk.constructor
          ( (identifier :> Identifier.DataType.t),
            ConstructorName.make_std cons.name )
      in
      add_to_elts Kind_Constructor ident (`Constructor (ident, cons)) env
    and add_field env (field : TypeDecl.Field.t) =
      let ident =
        Paths.Identifier.Mk.field
          ( (identifier :> Paths.Identifier.FieldParent.t),
            FieldName.make_std field.name )
      in
      add_to_elts Kind_Field ident (`Field (ident, field)) env
    in
    let open TypeDecl in
    match t.representation with
    | Some (Variant cons) ->
        ( List.fold_left add_cons cs cons,
          List.map (fun t -> t.Constructor.doc) cons )
    | Some (Record fields) ->
        ( List.fold_left add_field cs fields,
          List.map (fun t -> t.Field.doc) fields )
    | Some Extensible | None -> (cs, [])
  in
  let env, docs = if env.linking then open_typedecl env else (env, []) in
  let env = add_to_elts Kind_Type identifier (`Type (identifier, t)) env in
  if env.linking then
    add_cdocs identifier t.doc env
    |> List.fold_right (add_cdocs identifier) docs
  else env

let add_module_type identifier (t : Component.ModuleType.t) env =
  let env' =
    add_to_elts Kind_ModuleType identifier (`ModuleType (identifier, t)) env
  in
  if env'.linking then add_cdocs identifier t.doc env' else env'

let add_value identifier (t : Component.Value.t) env =
  add_to_elts Kind_Value identifier (`Value (identifier, t)) env
  |> add_cdocs identifier t.doc

let add_class identifier (t : Component.Class.t) env =
  let env' = add_to_elts Kind_Class identifier (`Class (identifier, t)) env in
  if env'.linking then add_cdocs identifier t.doc env' else env'

let add_class_type identifier (t : Component.ClassType.t) env =
  let env' =
    add_to_elts Kind_ClassType identifier (`ClassType (identifier, t)) env
  in
  if env'.linking then add_cdocs identifier t.doc env' else env'

let add_method _identifier _t env =
  (* TODO *)
  env

let add_exception identifier (e : Component.Exception.t) env =
  add_to_elts Kind_Exception identifier (`Exception (identifier, e)) env
  |> add_cdocs identifier e.doc

let add_extension_constructor identifier
    (ec : Component.Extension.Constructor.t) te env =
  add_to_elts Kind_Extension identifier (`Extension (identifier, ec, te)) env
  |> add_cdocs identifier ec.doc

let module_of_unit : Lang.Compilation_unit.t -> Component.Module.t =
 fun unit ->
  let id = (unit.id :> Paths.Identifier.Module.t) in
  let locs =
    match unit.source_info with
    | Some src -> Some (Identifier.Mk.source_location_mod src.id)
    | None -> None
  in
  match unit.content with
  | Module s ->
      let m =
        Lang.Module.
          {
            id;
            locs;
            doc = [];
            type_ = ModuleType (Signature s);
            canonical = unit.canonical;
            hidden = unit.hidden;
          }
      in
      let ty = Component.Of_Lang.(module_ (empty ()) m) in
      ty
  | Pack _p ->
      let m =
        Lang.Module.
          {
            id;
            locs;
            doc = [];
            type_ =
              ModuleType (Signature { items = []; compiled = true; doc = [] });
            canonical = unit.canonical;
            hidden = unit.hidden;
          }
      in
      let ty = Component.Of_Lang.(module_ (empty ()) m) in
      ty

let lookup_root_module name env =
  let result =
    match env.resolver with
    | None -> None
    | Some r -> (
        match r.lookup_unit name with
        | Forward_reference -> Some Forward
        | Not_found -> None
        | Found u ->
            let ({ Odoc_model.Paths.Identifier.iv = `Root _; _ } as id) =
              u.id
            in
            let m = module_of_unit u in
            Some (Resolved (u.root, id, m)))
  in
  (match (env.recorder, result) with
  | Some r, Some Forward ->
      r.lookups <-
        LookupTypeSet.add (RootModule (name, Some `Forward)) r.lookups
  | Some r, Some (Resolved (root, _, _)) ->
      r.lookups <-
        LookupTypeSet.add
          (RootModule (name, Some (`Resolved root.digest)))
          r.lookups
  | Some r, None ->
      r.lookups <- LookupTypeSet.add (RootModule (name, None)) r.lookups
  | None, _ -> ());
  result

let lookup_page name env =
  match env.resolver with None -> None | Some r -> r.lookup_page name

let lookup_unit name env =
  match env.resolver with None -> None | Some r -> Some (r.lookup_unit name)

type 'a scope = {
  filter : Component.Element.any -> ([< Component.Element.any ] as 'a) option;
  check : (t -> ([< Component.Element.any ] as 'a) -> 'a amb_err option) option;
  root : string -> t -> 'a option;
}

type 'a maybe_ambiguous = ('a, [ 'a amb_err | `Not_found ]) Result.result

let make_scope ?(root = fun _ _ -> None) ?check
    (filter : _ -> ([< Component.Element.any ] as 'a) option) : 'a scope =
  { filter; check; root }

let lookup_by_name scope name env =
  let record_lookup_results env results =
    match env.recorder with
    | Some r ->
        List.iter
          (function
            | `Module (id, _) ->
                r.lookups <-
                  LookupTypeSet.add (ModuleByName (name, id)) r.lookups
            | _ -> ())
          (results :> Component.Element.any list)
    | None -> ()
  in
  match
    (ElementsByName.find_by_name scope.filter name env.elts, scope.check)
  with
  | ([ x ] as results), Some c -> (
      record_lookup_results env results;
      match c env x with
      | Some (`Ambiguous _ as e) -> Result.Error e
      | None -> Result.Ok x)
  | ([ x ] as results), None ->
      record_lookup_results env results;
      Result.Ok x
  | (x :: tl as results), _ ->
      record_lookup_results env results;
      Error (`Ambiguous (x, tl))
  | [], _ -> (
      match scope.root name env with Some x -> Ok x | None -> Error `Not_found)

let lookup_by_id (scope : 'a scope) id env : 'a option =
  let record_lookup_result result =
    match env.recorder with
    | Some r -> (
        match (result :> Component.Element.any) with
        | `Module (id, _) ->
            r.lookups <- LookupTypeSet.add (Module id) r.lookups
        | `ModuleType (id, _) ->
            r.lookups <- LookupTypeSet.add (ModuleType id) r.lookups
        | _ -> ())
    | None -> ()
  in
  match ElementsById.find_by_id id env.ids with
  | Some x ->
      record_lookup_result x;
      scope.filter x
  | None -> (
      (* Format.eprintf "Can't find %a\n%!" Component.Fmt.model_identifier (id :> Identifier.t); *)
      match (id :> Identifier.t) with
      | { iv = `Root (_, name); _ } ->
          scope.root (ModuleName.to_string name) env
      | _ -> None)

let lookup_root_module_fallback name t =
  match lookup_root_module name t with
  | Some (Resolved (_, id, m)) ->
      Some
        (`Module
          ((id :> Identifier.Path.Module.t), Component.Delayed.put_val m))
  | Some Forward | None -> None

let lookup_page_or_root_module_fallback name t =
  match lookup_root_module_fallback name t with
  | Some _ as x -> x
  | None -> (
      match lookup_page name t with
      | Some page -> Some (`Page (page.Lang.Page.name, page))
      | None -> None)

let s_signature : Component.Element.signature scope =
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.signature as r -> Some r
    | _ -> None)

let s_module : Component.Element.module_ scope =
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.module_ as r -> Some r
    | _ -> None)

let s_any : Component.Element.any scope =
  make_scope ~root:lookup_page_or_root_module_fallback
    ~check:(fun env -> function
      | `Label (id, _) -> (
          try
            Some
              (Identifier.Maps.Label.find id env.ambiguous_labels
                :> Component.Element.any amb_err)
          with Not_found -> None)
      | _ -> None)
    (function
      (* Reference to [A] could refer to [extension-A] or [extension-decl-A].
         The legacy behavior refers to the constructor [extension-A]. *)
      | #Component.Element.extension_decl -> None
      | r -> Some r)

let s_module_type : Component.Element.module_type scope =
  make_scope (function
    | #Component.Element.module_type as r -> Some r
    | _ -> None)

let s_type : Component.Element.type_ scope =
  make_scope (function #Component.Element.type_ as r -> Some r | _ -> None)

let s_datatype : Component.Element.datatype scope =
  make_scope (function #Component.Element.datatype as r -> Some r | _ -> None)

let s_class : Component.Element.class_ scope =
  make_scope (function #Component.Element.class_ as r -> Some r | _ -> None)

let s_class_type : Component.Element.class_type scope =
  make_scope (function
    | #Component.Element.class_type as r -> Some r
    | _ -> None)

let s_value : Component.Element.value scope =
  make_scope (function #Component.Element.value as r -> Some r | _ -> None)

let s_label : Component.Element.label scope =
  make_scope
    ~check:(fun env -> function
      | `Label (id, _) -> (
          try Some (Identifier.Maps.Label.find id env.ambiguous_labels)
          with Not_found -> None))
    (function #Component.Element.label as r -> Some r | _ -> None)

let s_constructor : Component.Element.constructor scope =
  make_scope (function
    | #Component.Element.constructor as r -> Some r
    | _ -> None)

let s_exception : Component.Element.exception_ scope =
  make_scope (function
    | #Component.Element.exception_ as r -> Some r
    | _ -> None)

let s_extension : Component.Element.extension scope =
  make_scope (function
    | #Component.Element.extension as r -> Some r
    | _ -> None)

let s_field : Component.Element.field scope =
  make_scope (function #Component.Element.field as r -> Some r | _ -> None)

let s_label_parent : Component.Element.label_parent scope =
  make_scope ~root:lookup_page_or_root_module_fallback (function
    | #Component.Element.label_parent as r -> Some r
    | _ -> None)

let s_fragment_type_parent : Component.Element.fragment_type_parent scope =
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.fragment_type_parent as r -> Some r
    | _ -> None)

let len = ref 0

let n = ref 0

let lookup_fragment_root env =
  let maybe_record_result res =
    match env.recorder with
    | Some r -> r.lookups <- LookupTypeSet.add res r.lookups
    | None -> ()
  in
  match env.fragmentroot with
  | Some (i, _) as result ->
      maybe_record_result (FragmentRoot i);
      result
  | None -> None

let mk_functor_parameter module_type =
  let type_ = Component.Module.ModuleType module_type in
  Component.Module.
    { locs = None; doc = []; type_; canonical = None; hidden = false }

let add_functor_parameter : Lang.FunctorParameter.t -> t -> t =
 fun p t ->
  match p with
  | Unit -> t
  | Named n ->
      let id = (n.id :> Paths.Identifier.Path.Module.t) in
      let m =
        let open Component.Of_Lang in
        mk_functor_parameter (module_type_expr (empty ()) n.expr)
      in
      add_module id (Component.Delayed.put_val m) [] t

let add_functor_args' :
    Paths.Identifier.Signature.t -> Component.ModuleType.expr -> t -> t =
  let open Component in
  fun id expr env ->
    let rec find_args parent mty =
      match mty with
      | ModuleType.Functor (Named arg, res) ->
          ( arg.Component.FunctorParameter.id,
            Paths.Identifier.Mk.parameter
              ( parent,
                Ident.Name.typed_functor_parameter
                  arg.Component.FunctorParameter.id ),
            mk_functor_parameter arg.expr )
          :: find_args (Paths.Identifier.Mk.result parent) res
      | ModuleType.Functor (Unit, res) ->
          find_args (Paths.Identifier.Mk.result parent) res
      | _ -> []
    in
    (* We substituted back the parameters as identifiers to maintain the
       invariant that components in the environment are 'self-contained' - that
       is, they only contain local idents for things that are declared within
       themselves *)
    let fold_fn (env, subst) (ident, identifier, m) =
      let ident, identifier =
        ((ident, identifier) :> Ident.path_module * Identifier.Path.Module.t)
      in
      let doc = m.Component.Module.doc in
      let m = Component.Delayed.put_val (Subst.module_ subst m) in
      let rp = `Gpath (`Identifier identifier) in
      let p = `Resolved rp in
      let env' = add_module identifier m doc env in
      (env', Subst.add_module ident p rp subst)
    in
    let env', _subst =
      List.fold_left fold_fn (env, Subst.identity) (find_args id expr)
    in
    env'

let add_module_functor_args m id env =
  match m.Component.Module.type_ with
  | Alias _ -> env
  | ModuleType expr ->
      add_functor_args' (id :> Paths.Identifier.Signature.t) expr env

let add_module_type_functor_args mt id env =
  match mt.Component.ModuleType.expr with
  | None -> env
  | Some expr -> add_functor_args' (id :> Paths.Identifier.Signature.t) expr env

let open_class_signature : Lang.ClassSignature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  fun s env ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Lang.ClassSignature.Method m ->
            let ty = method_ (empty ()) m in
            add_method m.Lang.Method.id ty env
        | _ -> env)
      env s.items

let rec open_signature : Lang.Signature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  let module L = Lang in
  fun s e ->
    let ident_map = empty () in
    List.fold_left
      (fun env orig ->
        match ((orig : L.Signature.item), env.linking) with
        | Type (_, t), _ ->
            let ty = type_decl ident_map t in
            add_type t.L.TypeDecl.id ty env
        | Module (_, t), _ ->
            let ty = Component.Delayed.put (fun () -> module_ ident_map t) in
            add_module
              (t.L.Module.id :> Identifier.Path.Module.t)
              ty
              (docs ident_map t.L.Module.doc)
              env
        | ModuleType t, _ ->
            let ty = module_type ident_map t in
            add_module_type t.L.ModuleType.id ty env
        | ModuleTypeSubstitution _, _
        | L.Signature.TypeSubstitution _, _
        | L.Signature.ModuleSubstitution _, _ ->
            env
        | L.Signature.Class (_, c), _ ->
            let ty = class_ ident_map c in
            add_class c.id ty env
        | L.Signature.ClassType (_, c), _ ->
            let ty = class_type ident_map c in
            add_class_type c.id ty env
        | L.Signature.Include i, _ -> open_signature i.expansion.content env
        | L.Signature.Open o, false -> open_signature o.expansion env
        (* The following are only added when linking *)
        | L.Signature.Open o, true ->
            add_comment (`Docs o.doc) (open_signature o.expansion env)
        | Comment c, true -> add_comment c env
        | TypExt te, true ->
            let doc = docs ident_map te.doc in
            let te' = extension ident_map te in
            List.fold_left
              (fun env tec ->
                let ty = extension_constructor ident_map tec in
                add_extension_constructor tec.L.Extension.Constructor.id ty te'
                  env)
              env te.L.Extension.constructors
            |> add_cdocs te.L.Extension.parent doc
        | Exception e, true ->
            let ty = exception_ ident_map e in
            add_exception e.L.Exception.id ty env
        | L.Signature.Value v, true ->
            let ty = value ident_map v in
            add_value v.L.Value.id ty env
        (* Skip when compiling *)
        | Exception _, false -> env
        | TypExt _, false -> env
        | Comment _, false -> env
        | L.Signature.Value _, false -> env)
      e s.items

let open_type_substitution : Odoc_model.Lang.TypeDecl.t -> t -> t =
 fun t env ->
  let open Component in
  let open Of_Lang in
  let ty = type_decl (empty ()) t in
  add_type t.Lang.TypeDecl.id ty env

let open_module_substitution : Odoc_model.Lang.ModuleSubstitution.t -> t -> t =
 fun m env ->
  let open Component in
  let open Of_Lang in
  let _id = Ident.Of_Identifier.module_ m.id in
  let doc = docs (empty ()) m.doc in
  let ty =
    Component.Delayed.put (fun () ->
        Of_Lang.(
          module_of_module_substitution
            (*                  { empty with modules = [ (m.id, id) ] } *)
            (empty ())
            m))
  in
  add_module (m.id :> Identifier.Path.Module.t) ty doc env

let open_module_type_substitution : Lang.ModuleTypeSubstitution.t -> t -> t =
 fun t env ->
  let open Component in
  let open Of_Lang in
  let ty =
    module_type (empty ())
      {
        id = t.id;
        locs = None;
        doc = t.doc;
        expr = Some t.manifest;
        canonical = None;
      }
  in
  add_module_type t.Lang.ModuleTypeSubstitution.id ty env

let inherit_resolver env =
  match env.resolver with Some r -> set_resolver empty r | None -> empty

let open_units resolver env =
  List.fold_left
    (fun env m ->
      match resolver.lookup_unit m with
      | Found unit -> (
          match unit.content with
          | Module sg -> open_signature sg env
          | _ -> env)
      | _ -> env)
    env resolver.open_units

let env_of_unit t ~linking resolver =
  let open Lang.Compilation_unit in
  let initial_env =
    let m = module_of_unit t in
    let dm = Component.Delayed.put (fun () -> m) in
    let env = { empty with linking } in
    env |> add_module (t.id :> Identifier.Path.Module.t) dm m.doc
  in
  set_resolver initial_env resolver |> open_units resolver

let open_page page env = add_docs page.Lang.Page.content env

let env_of_page page resolver =
  let initial_env = open_page page empty in
  set_resolver initial_env resolver |> open_units resolver

let env_for_reference resolver =
  set_resolver empty resolver |> open_units resolver

let env_for_testing ~linking = { empty with linking }

let verify_lookups env lookups =
  let bad_lookup = function
    | Module id ->
        let actually_found =
          match lookup_by_id s_module id env with
          | Some _ -> true
          | None -> false
        in
        true <> actually_found
    | RootModule (name, res) -> (
        let actual_result =
          match env.resolver with
          | None -> None
          | Some r -> (
              match r.lookup_unit name with
              | Forward_reference -> Some `Forward
              | Not_found -> None
              | Found u -> Some (`Resolved u.root.digest))
        in
        match (res, actual_result) with
        | None, None -> false
        | Some `Forward, Some `Forward -> false
        | Some (`Resolved digest1), Some (`Resolved digest2) ->
            digest1 <> digest2
        | _ -> true)
    | ModuleType id ->
        let actually_found =
          match lookup_by_id s_module_type id env with
          | Some _ -> true
          | None -> false
        in
        true <> actually_found
    | ModuleByName (name, result) -> (
        match lookup_by_name s_module name env with
        | Ok (`Module (id', _)) -> result <> id'
        | Error `Not_found -> false
        | Error (`Ambiguous (hd, tl)) ->
            not
              (List.exists (fun (`Module (id', _)) -> result = id') (hd :: tl)))
    | FragmentRoot _i -> true
    (* begin
         try
           let (i', _) = Env.lookup_fragment_root env in
           i' <> i
         with _ ->
           true
       end*)
  in
  let result = not (LookupTypeSet.exists bad_lookup lookups) in
  (* If we're recording lookups, make sure it looks like we
      looked all this stuff up *)
  (match (result, env.recorder) with
  | true, Some r -> r.lookups <- LookupTypeSet.union r.lookups lookups
  | _ -> ());
  result
