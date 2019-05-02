(* A bunch of association lists. Let's hashtbl them up later *)

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found
 
type root =
  | Resolved of (Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
  | Forward

type resolver = {
  open_units: string list;
  lookup_unit : string -> lookup_unit_result;
  resolve_unit : Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t;
  lookup_page : string -> Odoc_model.Root.t option;
  resolve_page : Odoc_model.Root.t -> Odoc_model.Lang.Page.t;
}

let unique_id = ref 0

type t = {
  id : int;
  modules : (Odoc_model.Paths.Identifier.Module.t * Component.Module.t) list;
  module_types :
    (Odoc_model.Paths.Identifier.ModuleType.t * Component.ModuleType.t) list;
  types : (Odoc_model.Paths.Identifier.Type.t * Component.TypeDecl.t) list;
  values : (Odoc_model.Paths.Identifier.Value.t * Component.Value.t) list;
  externals : (Odoc_model.Paths.Identifier.Value.t * Component.External.t) list;
  titles :
    (Odoc_model.Paths.Identifier.Label.t * Odoc_model.Comment.link_content) list;
  classes : (Odoc_model.Paths.Identifier.Class.t * Component.Class.t) list;
  class_types :
    (Odoc_model.Paths.Identifier.ClassType.t * Component.ClassType.t) list;
  methods : (Odoc_model.Paths.Identifier.Method.t * Component.Method.t) list;
  instance_variables :
    ( Odoc_model.Paths.Identifier.InstanceVariable.t
    * Component.InstanceVariable.t )
    list;
  elts : (string * Component.Element.any) list;
  roots : (string * root) list;
  resolver : resolver option;
}

let set_resolver t resolver = {t with resolver = Some resolver }
let has_resolver t = match t.resolver with None -> false | _ -> true
let id t = t.id

let pp_modules ppf modules =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.module_ m)
    modules

let pp_module_types ppf module_types =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.module_type m)
    module_types

let pp_types ppf types =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.type_decl m)
    types

let pp_values ppf values =
  List.iter
    (fun (i, v) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
      (i :> Odoc_model.Paths.Identifier.t)
      Component.Fmt.value v)
    values

let pp_externals ppf exts =
  List.iter
    (fun (i, e) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
      (i :> Odoc_model.Paths.Identifier.t)
      Component.Fmt.external_ e)
    exts

let pp ppf env =
  Format.fprintf ppf "@[<v>@,ENV modules: %a @,ENV module_types: %a @,ENV types: %a@,ENV values: %a@,ENV externals: %a@,"
    pp_modules env.modules pp_module_types env.module_types pp_types env.types pp_values env.values pp_externals env.externals

(* Handy for extrating transient state *)
exception MyFailure of Odoc_model.Paths.Identifier.t * t

let empty =
  {
    id = 0;
    modules = [];
    module_types = [];
    types = [];
    values = [];
    externals = [];
    titles = [];
    elts = [];
    roots = [];
    classes = [];
    class_types = [];
    methods = [];
    instance_variables = [];
    resolver = None;
  }

let add_module identifier m env =
  {
    env with
    id = (incr unique_id; !unique_id);
    modules = (identifier, m) :: env.modules;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `Module (identifier, m))
      :: env.elts;
  }

let add_type identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    types = (identifier, t) :: env.types;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `Type (identifier, t))
      :: env.elts;
  }

let add_module_type identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    module_types = (identifier, t) :: env.module_types;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `ModuleType (identifier, t))
      :: env.elts;
  }

let add_value identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    values = (identifier, t) :: env.values;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `Value (identifier, t))
      :: env.elts;
  }

let add_external identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    externals = (identifier, t) :: env.externals;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `External (identifier, t))
      :: env.elts;
  }
let add_label identifier env =
  {
    env with
    id = (incr unique_id; !unique_id);
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `Label identifier)
      :: env.elts;
  }

let add_label_title label elts env =
  { env with 
  id = (incr unique_id; !unique_id);
  titles = (label, elts) :: env.titles }

let add_class identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    classes = (identifier, t) :: env.classes;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `Class (identifier, t))
      :: env.elts;
  }

let add_class_type identifier t env =
  {
    env with
    id = (incr unique_id; !unique_id);
    class_types = (identifier, t) :: env.class_types;
    elts =
      (Odoc_model.Paths.Identifier.name identifier, `ClassType (identifier, t))
      :: env.elts;
  }

let add_docs (docs : Odoc_model.Comment.docs) env =
  List.fold_right
    (fun element env ->
      match element.Odoc_model.Location_.value with
      | `Heading (_, label, nested_elements) ->
          let env = add_label label env in
          let env = add_label_title label nested_elements env in
          env
      | _ -> env)
    docs env

let add_comment (com : Odoc_model.Comment.docs_or_stop) env =
  match com with `Docs doc -> add_docs doc env | `Stop -> env

let add_method identifier m env =
  { env with 
  id = (incr unique_id; !unique_id);
  methods = (identifier, m) :: env.methods }

let add_root name ty env = { env with roots = (name, ty) :: env.roots }

let lookup_module identifier env =
  try List.assoc identifier env.modules
  with _ ->
(*    Format.fprintf Format.err_formatter
      "Failed to find module:\nIdentifier: %a\n\n"
      Component.Fmt.model_identifier
      (identifier :> Odoc_model.Paths.Identifier.t);
    List.iter
      (fun (ident, _) ->
        Format.fprintf Format.err_formatter "%a;\n"
          Component.Fmt.model_identifier
          (ident :> Odoc_model.Paths.Identifier.t))
      env.modules;*)
    raise (MyFailure ((identifier :> Odoc_model.Paths.Identifier.t), env))

let lookup_type identifier env =
  try List.assoc identifier env.types
  with Not_found ->
    Format.fprintf Format.std_formatter
      "Failed to find type:\nIdentifier: %a\n\nEnv:\n%a\n\n%!"
      Component.Fmt.model_identifier
      (identifier :> Odoc_model.Paths.Identifier.t)
      pp env;
    raise Not_found

let lookup_module_type identifier env = List.assoc identifier env.module_types

let lookup_value identifier env = List.assoc identifier env.values

let lookup_section_title identifier env =
  try Some (List.assoc identifier env.titles) with _ -> None

let lookup_class identifier env = List.assoc identifier env.classes

let lookup_class_type identifier env = List.assoc identifier env.class_types

let module_of_unit : Odoc_model.Lang.Compilation_unit.t -> Component.Module.t =
 fun unit ->
  match unit.content with
  | Module s ->
      let m =
        Odoc_model.Lang.Module.
          {
            id = unit.id;
            doc = unit.doc;
            type_ = ModuleType (Signature s);
            canonical = None;
            hidden = unit.hidden;
            display_type = None;
            expansion = Some AlreadyASig;
          }
      in
      let idents = Component.LocalIdents.(module_ m empty) in
      let ident_map = Component.Of_Lang.(map_of_idents idents empty) in
      let ty = Component.Of_Lang.(module_ ident_map m) in
      ty
  | Pack _ -> failwith "Unsupported"

let lookup_root_module name env =
  match try Some (List.assoc name env.roots) with _ -> None with
  | Some x -> Some x
  | None -> (
      match env.resolver with
      | None -> None
      | Some r -> (
          match r.lookup_unit name with
          | Forward_reference -> Some Forward
          | Not_found -> None
          | Found u ->
              let unit = r.resolve_unit u.root in
              Some (Resolved (unit.id, module_of_unit unit)) ) )

let find_map : ('a -> 'b option) -> 'a list -> 'b option =
 fun f ->
  let rec inner acc = function
    | x :: xs -> ( match f x with Some y -> Some y | None -> inner acc xs )
    | [] -> None
  in
  inner []

let lookup_any_by_name name env =
  let filter_fn : string * Component.Element.any -> Component.Element.any option
      = function
    | n, (_ as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let lookup_signature_by_name name env =
  let filter_fn :
      string * Component.Element.any -> Component.Element.signature option =
    function
    | n, (#Component.Element.signature as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let lookup_module_by_name name env =
  let filter_fn :
      string * Component.Element.any -> Component.Element.module_ option =
    function
    | n, (#Component.Element.module_ as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let lookup_module_type_by_name name env =
  let filter_fn :
      string * Component.Element.any -> Component.Element.module_type option =
    function
    | n, (#Component.Element.module_type as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let lookup_datatype_by_name name env =
  let filter_fn :
      string * Component.Element.any -> Component.Element.datatype option =
    function
    | n, (#Component.Element.datatype as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let lookup_value_by_name name env =
  let filter_fn :
      string * Component.Element.any -> [Component.Element.value | Component.Element.external_] option =
    function
    | n, (#Component.Element.value as item) when n = name -> Some item
    | n, (#Component.Element.external_ as item) when n = name -> Some item
    | _ -> None
  in
  find_map filter_fn env.elts

let add_functor_args : Odoc_model.Paths.Identifier.Signature.t -> t -> t =
  let open Component in
  fun id env ->
    let rec find_args parent mty =
      match mty with
      | ModuleType.Functor (Named arg, res) ->
          ( arg.Component.FunctorParameter.id,
            `Parameter
              ( parent,
                Odoc_model.Names.ParameterName.of_string
                  (Ident.Name.module_ arg.Component.FunctorParameter.id) ),
            {
              Component.Module.doc = [];
              display_type = None;
              type_ = ModuleType arg.expr;
              canonical = None;
              hidden = false;
              expansion = None;
            } )
          :: find_args (`Result parent) res
      | ModuleType.Functor (Unit, res) -> find_args (`Result parent) res
      | _ -> []
    in
    (* We substituted back the parameters as identifiers to maintain the invariant that
       components in the environment are 'self-contained' - that is, they only contain
       local idents for things that are declared within themselves *)
    let fold_fn (env, subst) (ident, identifier, m) =
      let env' = add_module identifier (Subst.module_ subst m) env in
      (env', Subst.add_module ident (`Identifier identifier) subst)
    in
    match id with
    | (`Module _ | `Result _ | `Parameter _) as mid -> (
        let m = lookup_module mid env in
        match m.Component.Module.type_ with
        | Alias _ -> env
        | ModuleType e ->
            let (env', _subst) = List.fold_left fold_fn
              (env, Subst.identity) (find_args id e)
            in
              env')
    | `ModuleType _ as mtyid -> (
        let m = lookup_module_type mtyid env in
        match m.Component.ModuleType.expr with
        | Some e ->
            let (env', _subst) = List.fold_left fold_fn
                            (env, Subst.identity) (find_args id e)
              in env'
        | None -> env )
    | `Root _ -> env

let rec open_component_signature :
    Odoc_model.Paths.Identifier.Signature.t -> Component.Signature.t -> t -> t =
  let open Component in
  fun id s env ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Signature.Type (tid, _, t) ->
            let new_id = `Type (id, Odoc_model.Names.TypeName.of_string (Ident.Name.type_ tid)) in
            add_type new_id t env
        | Signature.Module (mid, _, m) ->
            let new_id = `Module (id, Odoc_model.Names.ModuleName.of_string (Ident.Name.module_ mid)) in
            add_module new_id (Delayed.get m) env
        | Signature.ModuleType (mid, m) ->
            let new_id = `ModuleType (id, Odoc_model.Names.ModuleTypeName.of_string (Ident.Name.module_type mid)) in
            add_module_type new_id (Delayed.get m) env
        | Signature.Include i -> open_component_signature id i.expansion_ env
        | _ -> env)
      env s.items

let open_class_signature : Odoc_model.Lang.ClassSignature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  fun s env ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Odoc_model.Lang.ClassSignature.Method m ->
            let idents = LocalIdents.(method_ m empty) in
            let map = map_of_idents idents empty in
            let ty = method_ map m in
            add_method m.Odoc_model.Lang.Method.id ty env
        | _ -> env)
      env s.items

let rec open_signature : Odoc_model.Lang.Signature.t -> t -> t =
  let open Component in
  let open Of_Lang in
  fun s e ->
    List.fold_left
      (fun env orig ->
        match orig with
        | Odoc_model.Lang.Signature.Type (_, t) ->
            let idents = LocalIdents.(type_decl t empty) in
            let map = map_of_idents idents empty in
            let ty = type_decl map t in
            add_type t.Odoc_model.Lang.TypeDecl.id ty env
        | Odoc_model.Lang.Signature.Module (_, t) ->
            let idents = LocalIdents.(module_ t empty) in
            let map = map_of_idents idents empty in
            let ty = module_ map t in
            add_module t.Odoc_model.Lang.Module.id ty env
        | Odoc_model.Lang.Signature.ModuleType t ->
            let idents = LocalIdents.(module_type t empty) in
            let map = map_of_idents idents empty in
            let ty = module_type map t in
            add_module_type t.Odoc_model.Lang.ModuleType.id ty env
        | Odoc_model.Lang.Signature.Comment c -> add_comment c env
        | Odoc_model.Lang.Signature.TypExt _ -> env
        | Odoc_model.Lang.Signature.Exception _ -> env
        | Odoc_model.Lang.Signature.ModuleSubstitution m ->
            let id = Ident.Of_Identifier.module_ m.id in
            let ty =
              Of_Lang.(
                module_of_module_substitution
                  { empty with modules = [ (m.id, id) ] }
                  m)
            in
            add_module m.id ty env
        | Odoc_model.Lang.Signature.TypeSubstitution _ -> env
        | Odoc_model.Lang.Signature.Value v ->
            let idents = Component.LocalIdents.(value_ v empty) in
            let ident_map = Component.Of_Lang.(map_of_idents idents empty) in
            let ty = Of_Lang.(value ident_map v) in
            add_value v.Odoc_model.Lang.Value.id ty env
        | Odoc_model.Lang.Signature.External e ->
            let idents = Component.LocalIdents.(external_ e empty) in
            let ident_map = Component.Of_Lang.(map_of_idents idents empty) in
            let ty = Of_Lang.(external_ ident_map e) in
            add_external e.Odoc_model.Lang.External.id ty env
        | Odoc_model.Lang.Signature.Class (_, c) ->
            let idents = Component.LocalIdents.(class_ c empty) in
            let ident_map = Component.Of_Lang.(map_of_idents idents empty) in
            let ty = class_ ident_map c in
            add_class c.id ty env
        | Odoc_model.Lang.Signature.ClassType (_, c) ->
            let idents = Component.LocalIdents.(class_type c empty) in
            let ident_map = Component.Of_Lang.(map_of_idents idents empty) in
            let ty = class_type ident_map c in
            add_class_type c.id ty env
        | Odoc_model.Lang.Signature.Include i ->
            open_signature i.expansion.content env)
      e s

let open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t =
 fun unit env ->
  match unit.content with Module s -> open_signature s env | Pack _ -> env
