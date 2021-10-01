(* A bunch of association lists. Let's hashtbl them up later *)
open Odoc_model
open Odoc_model.Names
open Odoc_model.Paths

type lookup_unit_result =
  | Forward_reference
  | Found of Odoc_model.Lang.Compilation_unit.t
  | Not_found

type lookup_page_result = Odoc_model.Lang.Page.t option

type root =
  | Resolved of
      (Root.t * Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
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
  | Module of Odoc_model.Paths.Identifier.Path.Module.t
  | ModuleType of Odoc_model.Paths.Identifier.Path.ModuleType.t
  | RootModule of string * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of string * Odoc_model.Paths.Identifier.Path.Module.t
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
        (r :> Odoc_model.Paths.Identifier.t)
  | ModuleType r ->
      Format.fprintf fmt "ModuleType %a" Component.Fmt.model_identifier
        (r :> Odoc_model.Paths.Identifier.t)
  | RootModule (str, res) -> Format.fprintf fmt "RootModule %s %a" str fmtrm res
  | ModuleByName (n, r) ->
      Format.fprintf fmt "ModuleByName %s, %a" n Component.Fmt.model_identifier
        (r :> Odoc_model.Paths.Identifier.t)
  | FragmentRoot i -> Format.fprintf fmt "FragmentRoot %d" i

let pp_lookup_type_list fmt ls =
  let rec inner fmt = function
    | [] -> Format.fprintf fmt ""
    | [ x ] -> Format.fprintf fmt "%a" pp_lookup_type x
    | x :: ys -> Format.fprintf fmt "%a; %a" pp_lookup_type x inner ys
  in
  Format.fprintf fmt "[%a]" inner ls

type recorder = { mutable lookups : lookup_type list }

module Maps = Odoc_model.Paths.Identifier.Maps
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

module Elements : sig
  type t

  val empty : t

  val add :
    ?shadow:bool ->
    kind ->
    [< Identifier.t ] ->
    [< Component.Element.any ] ->
    t ->
    t
  (** If [shadow] is set to [false] (defaults to [true]), existing
      elements of the same name won't be shadowed. This is used for labels,
      which doesn't allow shadowing. *)

  val find_by_name :
    (Component.Element.any -> 'b option) -> string -> t -> 'b list

  val find_by_id : Identifier.t -> t -> Component.Element.any option
end = struct
  module IdMap = Identifier.Maps.Any

  type elem = { kind : kind; elem : Component.Element.any }

  type t = elem list StringMap.t * Component.Element.any IdMap.t
  (** The first map is queried with {!find_by_name}, shadowed elements are
      removed from it. The second map is queried with {!find_by_id}. *)

  let empty = (StringMap.empty, IdMap.empty)

  let add ?(shadow = true) kind identifier elem (names, ids) =
    let elem = (elem :> Component.Element.any) in
    let name = Identifier.name identifier in
    let tl =
      try
        let tl = StringMap.find name names in
        let not_shadow e = e.kind <> kind in
        if shadow && not (List.for_all not_shadow tl) then
          List.filter not_shadow tl
        else tl
      with Not_found -> []
    in
    let ids = IdMap.add (identifier :> Identifier.t) elem ids in
    let names = StringMap.add name ({ kind; elem } :: tl) names in
    (names, ids)

  let find_by_name f name (names, _) =
    let filter e acc = match f e.elem with Some r -> r :: acc | None -> acc in
    try List.fold_right filter (StringMap.find name names) []
    with Not_found -> []

  let find_by_id id (_, ids) = try Some (IdMap.find id ids) with _ -> None
end

type t = {
  id : int;
  elts : Elements.t;
  resolver : resolver option;
  recorder : recorder option;
  fragmentroot : (int * Component.Signature.t) option;
}

let set_resolver t resolver = { t with resolver = Some resolver }

let has_resolver t = match t.resolver with None -> false | _ -> true

let id t = t.id

let with_recorded_lookups env f =
  let recorder = { lookups = [] } in
  let env' = { env with recorder = Some recorder } in
  let restore () =
    match env.recorder with
    | Some r -> r.lookups <- recorder.lookups @ r.lookups
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
    id = 0;
    elts = Elements.empty;
    resolver = None;
    recorder = None;
    fragmentroot = None;
  }

let add_fragment_root sg env =
  let id = unique_id () in
  { env with fragmentroot = Some (id, sg); id }

(** Implements most [add_*] functions. *)
let add_to_elts ?shadow kind identifier component env =
  {
    env with
    id = unique_id ();
    elts = Elements.add ?shadow kind identifier component env.elts;
  }

let add_label identifier heading env =
  (* Disallow shadowing for labels. Duplicate names are disallowed and reported
     during linking. *)
  add_to_elts ~shadow:false Kind_Label identifier
    (`Label (identifier, heading))
    env

let add_docs (docs : Odoc_model.Comment.docs) env =
  List.fold_left
    (fun env -> function
      | { Odoc_model.Location_.value = `Heading (attrs, id, text); location } ->
          let label = Ident.Of_Identifier.label id in
          add_label id { Component.Label.attrs; label; text; location } env
      | _ -> env)
    env docs

let add_comment (com : Odoc_model.Comment.docs_or_stop) env =
  match com with `Docs doc -> add_docs doc env | `Stop -> env

let add_cdocs p (docs : Component.CComment.docs) env =
  List.fold_left
    (fun env element ->
      match element.Odoc_model.Location_.value with
      | `Heading h ->
          let (`LLabel (name, _)) = h.Component.Label.label in
          let label = `Label (Paths.Identifier.label_parent p, name) in
          add_label label h env
      | _ -> env)
    env docs

let add_module identifier m docs env =
  add_to_elts Kind_Module identifier (`Module (identifier, m)) env
  |> add_cdocs identifier docs

let add_type identifier t env =
  let open Component in
  let open_typedecl cs =
    let add_cons elts (cons : TypeDecl.Constructor.t) =
      let ident =
        `Constructor (identifier, ConstructorName.make_std cons.name)
      in
      Elements.add Kind_Constructor ident (`Constructor (ident, cons)) elts
    and add_field elts (field : TypeDecl.Field.t) =
      let ident =
        `Field
          ( (identifier :> Odoc_model.Paths.Identifier.Parent.t),
            FieldName.make_std field.name )
      in
      Elements.add Kind_Field ident (`Field (ident, field)) elts
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
  let elts, docs = open_typedecl env.elts in
  let elts = Elements.add Kind_Type identifier (`Type (identifier, t)) elts in
  { env with id = unique_id (); elts }
  |> add_cdocs identifier t.doc
  |> List.fold_right (add_cdocs identifier) docs

let add_module_type identifier (t : Component.ModuleType.t) env =
  add_to_elts Kind_ModuleType identifier (`ModuleType (identifier, t)) env
  |> add_cdocs identifier t.doc

let add_value identifier (t : Component.Value.t) env =
  add_to_elts Kind_Value identifier (`Value (identifier, t)) env
  |> add_cdocs identifier t.doc

let add_class identifier (t : Component.Class.t) env =
  add_to_elts Kind_Class identifier (`Class (identifier, t)) env
  |> add_cdocs identifier t.doc

let add_class_type identifier (t : Component.ClassType.t) env =
  add_to_elts Kind_ClassType identifier (`ClassType (identifier, t)) env
  |> add_cdocs identifier t.doc

let add_method _identifier _t env =
  (* TODO *)
  env

let add_exception identifier (e : Component.Exception.t) env =
  add_to_elts Kind_Exception identifier (`Exception (identifier, e)) env
  |> add_cdocs identifier e.doc

let add_extension_constructor identifier
    (ec : Component.Extension.Constructor.t) env =
  add_to_elts Kind_Extension identifier (`Extension (identifier, ec)) env
  |> add_cdocs identifier ec.doc

let module_of_unit : Odoc_model.Lang.Compilation_unit.t -> Component.Module.t =
 fun unit ->
  match unit.content with
  | Module s ->
      let m =
        Odoc_model.Lang.Module.
          {
            id = (unit.id :> Odoc_model.Paths.Identifier.Module.t);
            doc = [];
            type_ = ModuleType (Signature s);
            canonical = unit.canonical;
            hidden = unit.hidden;
          }
      in
      let ty = Component.Of_Lang.(module_ empty m) in
      ty
  | Pack _p ->
      let m =
        Odoc_model.Lang.Module.
          {
            id = (unit.id :> Odoc_model.Paths.Identifier.Module.t);
            doc = [];
            type_ =
              ModuleType (Signature { items = []; compiled = true; doc = [] });
            canonical = unit.canonical;
            hidden = unit.hidden;
          }
      in
      let ty = Component.Of_Lang.(module_ empty m) in
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
            let (`Root _ as id) = u.id in
            let m = module_of_unit u in
            Some (Resolved (u.root, id, m)))
  in
  (match (env.recorder, result) with
  | Some r, Some Forward ->
      r.lookups <- RootModule (name, Some `Forward) :: r.lookups
  | Some r, Some (Resolved (root, _, _)) ->
      r.lookups <- RootModule (name, Some (`Resolved root.digest)) :: r.lookups
  | Some r, None -> r.lookups <- RootModule (name, None) :: r.lookups
  | None, _ -> ());
  result

type 'a scope = {
  filter : Component.Element.any -> ([< Component.Element.any ] as 'a) option;
  root : string -> t -> 'a option;
}

type 'a maybe_ambiguous =
  ('a, [ `Ambiguous of 'a * 'a list | `Not_found ]) Result.result

let make_scope ?(root = fun _ _ -> None)
    (filter : _ -> ([< Component.Element.any ] as 'a) option) : 'a scope =
  { filter; root }

let lookup_by_name scope name env =
  let record_lookup_results env results =
    match env.recorder with
    | Some r ->
        List.iter
          (function
            | `Module (id, _) ->
                r.lookups <- ModuleByName (name, id) :: r.lookups
            | _ -> ())
          (results :> Component.Element.any list)
    | None -> ()
  in
  match Elements.find_by_name scope.filter name env.elts with
  | [ x ] as results ->
      record_lookup_results env results;
      Result.Ok x
  | x :: tl as results ->
      record_lookup_results env results;
      Error (`Ambiguous (x, tl))
  | [] -> (
      match scope.root name env with Some x -> Ok x | None -> Error `Not_found)

let lookup_by_id (scope : 'a scope) id env : 'a option =
  let record_lookup_result result =
    match env.recorder with
    | Some r -> (
        match (result :> Component.Element.any) with
        | `Module (id, _) -> r.lookups <- Module id :: r.lookups
        | `ModuleType (id, _) -> r.lookups <- ModuleType id :: r.lookups
        | _ -> ())
    | None -> ()
  in
  match Elements.find_by_id (id :> Identifier.t) env.elts with
  | Some x ->
      record_lookup_result x;
      scope.filter x
  | None -> (
      match (id :> Identifier.t) with
      | `Root (_, name) -> scope.root (ModuleName.to_string name) env
      | _ -> None)

let lookup_root_module_fallback name t =
  match lookup_root_module name t with
  | Some (Resolved (_, id, m)) ->
      Some
        (`Module
          ((id :> Identifier.Path.Module.t), Component.Delayed.put_val m))
  | Some Forward | None -> None

let s_signature : Component.Element.signature scope =
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.signature as r -> Some r
    | _ -> None)

let s_module : Component.Element.module_ scope =
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.module_ as r -> Some r
    | _ -> None)

let s_any : Component.Element.any scope =
  make_scope ~root:lookup_root_module_fallback (fun r -> Some r)

let s_module_type : Component.Element.module_type scope =
  make_scope (function
    | #Component.Element.module_type as r -> Some r
    | _ -> None)

let s_datatype : Component.Element.datatype scope =
  make_scope (function #Component.Element.datatype as r -> Some r | _ -> None)

let s_type : Component.Element.type_ scope =
  make_scope (function #Component.Element.type_ as r -> Some r | _ -> None)

let s_class : Component.Element.class_ scope =
  make_scope (function #Component.Element.class_ as r -> Some r | _ -> None)

let s_class_type : Component.Element.class_type scope =
  make_scope (function
    | #Component.Element.class_type as r -> Some r
    | _ -> None)

let s_value : Component.Element.value scope =
  make_scope (function #Component.Element.value as r -> Some r | _ -> None)

let s_label : Component.Element.label scope =
  make_scope (function #Component.Element.label as r -> Some r | _ -> None)

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
  make_scope ~root:lookup_root_module_fallback (function
    | #Component.Element.label_parent as r -> Some r
    | _ -> None)

let len = ref 0

let n = ref 0

let lookup_fragment_root env =
  let maybe_record_result res =
    match env.recorder with
    | Some r -> r.lookups <- res :: r.lookups
    | None -> ()
  in
  match env.fragmentroot with
  | Some (i, _) as result ->
      maybe_record_result (FragmentRoot i);
      result
  | None -> None

let lookup_page name env =
  match env.resolver with None -> None | Some r -> r.lookup_page name

let add_functor_parameter : Odoc_model.Lang.FunctorParameter.t -> t -> t =
 fun p t ->
  match p with
  | Unit -> t
  | Named n ->
      let m =
        Component.Module.
          {
            doc = [];
            type_ = ModuleType Component.Of_Lang.(module_type_expr empty n.expr);
            canonical = None;
            hidden = false;
          }
      in
      add_module
        (n.id :> Paths.Identifier.Path.Module.t)
        (Component.Delayed.put_val m)
        [] t

let add_functor_args' :
    Odoc_model.Paths.Identifier.Signature.t ->
    Component.ModuleType.expr ->
    t ->
    t =
  let open Component in
  fun id expr env ->
    let rec find_args parent mty =
      match mty with
      | ModuleType.Functor (Named arg, res) ->
          ( arg.Component.FunctorParameter.id,
            `Parameter
              ( parent,
                Ident.Name.typed_functor_parameter
                  arg.Component.FunctorParameter.id ),
            {
              Component.Module.doc = [];
              type_ = ModuleType arg.expr;
              canonical = None;
              hidden = false;
            } )
          :: find_args (`Result parent) res
      | ModuleType.Functor (Unit, res) -> find_args (`Result parent) res
      | _ -> []
    in
    (* We substituted back the parameters as identifiers to maintain the invariant that
       components in the environment are 'self-contained' - that is, they only contain
       local idents for things that are declared within themselves *)
    let fold_fn (env, subst) (ident, identifier, m) =
      let ident, identifier =
        ((ident, identifier) :> Ident.path_module * Identifier.Path.Module.t)
      in
      let doc = m.Component.Module.doc in
      let m = Component.Delayed.put_val (Subst.module_ subst m) in
      let env' = add_module identifier m doc env in
      ( env',
        Subst.add_module ident
          (`Resolved (`Identifier identifier))
          (`Identifier identifier) subst )
    in
    let env', _subst =
      List.fold_left fold_fn (env, Subst.identity) (find_args id expr)
    in
    env'

let add_module_functor_args m id env =
  match m.Component.Module.type_ with
  | Alias _ -> env
  | ModuleType expr ->
      add_functor_args' (id :> Odoc_model.Paths.Identifier.Signature.t) expr env

let add_module_type_functor_args mt id env =
  match mt.Component.ModuleType.expr with
  | None -> env
  | Some expr ->
      add_functor_args' (id :> Odoc_model.Paths.Identifier.Signature.t) expr env

let open_class_signature : Odoc_model.Lang.ClassSignature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  fun s env ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Odoc_model.Lang.ClassSignature.Method m ->
            let ty = method_ empty m in
            add_method m.Odoc_model.Lang.Method.id ty env
        | _ -> env)
      env s.items

let rec open_signature : Odoc_model.Lang.Signature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  let module L = Odoc_model.Lang in
  fun s e ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Odoc_model.Lang.Signature.Type (_, t) ->
            let ty = type_decl empty t in
            add_type t.Odoc_model.Lang.TypeDecl.id ty env
        | Odoc_model.Lang.Signature.Module (_, t) ->
            let ty = Component.Delayed.put (fun () -> module_ empty t) in
            add_module
              (t.Odoc_model.Lang.Module.id :> Identifier.Path.Module.t)
              ty
              (docs empty t.L.Module.doc)
              env
        | Odoc_model.Lang.Signature.ModuleType t ->
            let ty = module_type empty t in
            add_module_type t.Odoc_model.Lang.ModuleType.id ty env
        | Odoc_model.Lang.Signature.ModuleTypeSubstitution t ->
            let ty =
              module_type empty
                {
                  id = t.id;
                  doc = t.doc;
                  expr = Some t.manifest;
                  canonical = None;
                }
            in
            add_module_type t.Odoc_model.Lang.ModuleTypeSubstitution.id ty env
        | Odoc_model.Lang.Signature.Comment c -> add_comment c env
        | Odoc_model.Lang.Signature.TypExt te ->
            let doc = docs empty te.doc in
            List.fold_left
              (fun env tec ->
                let ty = extension_constructor empty tec in
                add_extension_constructor tec.L.Extension.Constructor.id ty env)
              env te.L.Extension.constructors
            |> add_cdocs te.L.Extension.parent doc
        | Odoc_model.Lang.Signature.Exception e ->
            let ty = exception_ empty e in
            add_exception e.Odoc_model.Lang.Exception.id ty env
        | Odoc_model.Lang.Signature.ModuleSubstitution m ->
            let _id = Ident.Of_Identifier.module_ m.id in
            let doc = docs empty m.doc in
            let ty =
              Component.Delayed.put (fun () ->
                  Of_Lang.(
                    module_of_module_substitution
                      (*                  { empty with modules = [ (m.id, id) ] } *)
                      empty m))
            in
            add_module (m.id :> Identifier.Path.Module.t) ty doc env
        | Odoc_model.Lang.Signature.TypeSubstitution t ->
            let ty = type_decl empty t in
            add_type t.Odoc_model.Lang.TypeDecl.id ty env
        | Odoc_model.Lang.Signature.Value v ->
            let ty = value empty v in
            add_value v.Odoc_model.Lang.Value.id ty env
        | Odoc_model.Lang.Signature.Class (_, c) ->
            let ty = class_ empty c in
            add_class c.id ty env
        | Odoc_model.Lang.Signature.ClassType (_, c) ->
            let ty = class_type empty c in
            add_class_type c.id ty env
        | Odoc_model.Lang.Signature.Include i ->
            open_signature i.expansion.content env
        | Odoc_model.Lang.Signature.Open o -> open_signature o.expansion env)
      e s.items

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

let env_of_unit t resolver =
  let open Odoc_model.Lang.Compilation_unit in
  let initial_env =
    let m = module_of_unit t in
    let dm = Component.Delayed.put (fun () -> m) in
    empty |> add_module (t.id :> Identifier.Path.Module.t) dm m.doc
  in
  set_resolver initial_env resolver |> open_units resolver

let env_of_page page resolver =
  let initial_env = empty |> add_docs page.Odoc_model.Lang.Page.content in
  set_resolver initial_env resolver |> open_units resolver

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
  let result = not (List.exists bad_lookup lookups) in
  (* If we're recording lookups, make sure it looks like we
      looked all this stuff up *)
  (match (result, env.recorder) with
  | true, Some r -> r.lookups <- r.lookups @ lookups
  | _ -> ());
  result
