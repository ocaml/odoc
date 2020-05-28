(* A bunch of association lists. Let's hashtbl them up later *)
open Odoc_model.Names

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type root =
  | Resolved of (Digest.t * Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
  | Forward

type resolver = {
  open_units : string list;
  lookup_unit : string -> lookup_unit_result;
  resolve_unit : Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t;
  lookup_page : string -> Odoc_model.Root.t option;
  resolve_page : Odoc_model.Root.t -> Odoc_model.Lang.Page.t;
}

let unique_id = ref 0

type lookup_type =
  | Module of Odoc_model.Paths_types.Identifier.reference_module * bool
  | ModuleType of Odoc_model.Paths_types.Identifier.module_type * bool
  | RootModule of
      string
      * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of
      string * Odoc_model.Paths_types.Identifier.reference_module option
  | FragmentRoot of int

let pp_lookup_type fmt =
  let fmtrm fmt = function
    | Some `Forward -> Format.fprintf fmt "Some (Forward)"
    | Some (`Resolved digest) ->
        Format.fprintf fmt "Some (Resolved %s)" digest
    | None -> Format.fprintf fmt "None"
  in
  let id_opt fmt = function
    | Some id ->
        Format.fprintf fmt "Some %a" Component.Fmt.model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | None -> Format.fprintf fmt "None"
  in
  function
  | Module (r, b) ->
      Format.fprintf fmt "Module %a, %b" Component.Fmt.model_identifier
        (r :> Odoc_model.Paths.Identifier.t)
        b
  | ModuleType (r, b) ->
      Format.fprintf fmt "ModuleType %a, %b" Component.Fmt.model_identifier
        (r :> Odoc_model.Paths.Identifier.t)
        b
  | RootModule (str, res) -> Format.fprintf fmt "RootModule %s %a" str fmtrm res
  | ModuleByName (n, r) -> Format.fprintf fmt "ModuleByName %s, %a" n id_opt r
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

type t = {
  id : int;
  modules : Component.Module.t Maps.Module.t;
  module_types : Component.ModuleType.t Maps.ModuleType.t;
  types : Component.TypeDecl.t Maps.Type.t;
  values : Component.Value.t Maps.Value.t;
  externals : Component.External.t Maps.Value.t;
  titles : Odoc_model.Comment.link_content Maps.Label.t;
  classes : Component.Class.t Maps.Class.t;
  class_types : Component.ClassType.t Maps.ClassType.t;
  methods : Component.Method.t Maps.Method.t;
  instance_variables : Component.InstanceVariable.t Maps.InstanceVariable.t;
  constructors : Component.TypeDecl.Constructor.t Maps.Constructor.t;
  exceptions : Component.Exception.t Maps.Exception.t;
  extensions : Component.Extension.Constructor.t Maps.Extension.t;
  fields : Component.TypeDecl.Field.t Maps.Field.t;
  elts : Component.Element.any list StringMap.t;
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

let pp_modules ppf modules =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "ENV MODULE %a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.module_ m)
    (Maps.Module.bindings modules)

let pp_module_types ppf module_types =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.module_type m)
    (Maps.ModuleType.bindings module_types)

let pp_types ppf types =
  List.iter
    (fun (i, m) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.type_decl m)
    (Maps.Type.bindings types)

let pp_values ppf values =
  List.iter
    (fun (i, v) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.value v)
    (Maps.Value.bindings values)

let pp_externals ppf exts =
  List.iter
    (fun (i, e) ->
      Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier
        (i :> Odoc_model.Paths.Identifier.t)
        Component.Fmt.external_ e)
    (Maps.Value.bindings exts)

let pp ppf env =
  Format.fprintf ppf
    "@[<v>@,\
     ENV modules: %a @,\
     ENV module_types: %a @,\
     ENV types: %a@,\
     ENV values: %a@,\
     ENV externals: %a@,\
     END OF ENV" pp_modules env.modules pp_module_types env.module_types
    pp_types env.types pp_values env.values pp_externals env.externals

let empty =
  {
    id = 0;
    modules = Maps.Module.empty;
    module_types = Maps.ModuleType.empty;
    types = Maps.Type.empty;
    values = Maps.Value.empty;
    externals = Maps.Value.empty;
    titles = Maps.Label.empty;
    elts = StringMap.empty;
    classes = Maps.Class.empty;
    class_types = Maps.ClassType.empty;
    methods = Maps.Method.empty;
    instance_variables = Maps.InstanceVariable.empty;
    constructors = Maps.Constructor.empty;
    fields = Maps.Field.empty;
    exceptions = Maps.Exception.empty;
    extensions = Maps.Extension.empty;
    resolver = None;
    recorder = None;
    fragmentroot = None;
  }

let add_fragment_root sg env =
  let id =
    incr unique_id;
    !unique_id
  in
  { env with fragmentroot = Some (id, sg); id }

let add_to_elts name v elts =
  try
    let cur = StringMap.find name elts in
    StringMap.add name (v :: cur) elts
  with Not_found -> StringMap.add name [ v ] elts

let add_module identifier m env =
  {
    env with
    id =
      ( incr unique_id;
        (*Format.fprintf Format.err_formatter "unique_id=%d\n%!" !unique_id; *)
        !unique_id );
    modules = Maps.Module.add identifier m env.modules;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Module (identifier, m))
        env.elts;
  }

let add_type identifier t env =
  let open Component in
  let open_typedecl cs =
    let add_cons (constructors, fields, elts) (cons : TypeDecl.Constructor.t) =
      let ident =
        `Constructor (identifier, ConstructorName.of_string cons.name)
      in
      ( Maps.Constructor.add ident cons constructors,
        fields,
        add_to_elts
          (Odoc_model.Paths.Identifier.name ident)
          (`Constructor (ident, cons))
          elts )
    and add_field (constructors, fields, elts) (field : TypeDecl.Field.t) =
      let ident =
        `Field
          ( (identifier :> Odoc_model.Paths_types.Identifier.parent),
            FieldName.of_string field.name )
      in
      ( constructors,
        Maps.Field.add ident field fields,
        add_to_elts
          (Odoc_model.Paths.Identifier.name ident)
          (`Field (ident, field))
          elts )
    in
    match t.TypeDecl.representation with
    | Some (Variant cons) -> List.fold_left add_cons cs cons
    | Some (Record fields) -> List.fold_left add_field cs fields
    | Some Extensible | None -> cs
  in
  let constructors, fields, elts =
    open_typedecl (env.constructors, env.fields, env.elts)
  in
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    types = Maps.Type.add identifier t env.types;
    constructors;
    fields;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Type (identifier, t))
        elts;
  }

let add_module_type identifier t env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    module_types = Maps.ModuleType.add identifier t env.module_types;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`ModuleType (identifier, t))
        env.elts;
  }

let add_value identifier t env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    values = Maps.Value.add identifier t env.values;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Value (identifier, t))
        env.elts;
  }

let add_external identifier t env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    externals = Maps.Value.add identifier t env.externals;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`External (identifier, t))
        env.elts;
  }

let add_label identifier env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Label identifier) env.elts;
  }

let add_label_title label elts env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    titles = Maps.Label.add label elts env.titles;
  }

let add_class identifier t env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    classes = Maps.Class.add identifier t env.classes;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Class (identifier, t))
        env.elts;
  }

let add_class_type identifier t env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    class_types = Maps.ClassType.add identifier t env.class_types;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`ClassType (identifier, t))
        env.elts;
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
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    methods = Maps.Method.add identifier m env.methods;
  }

let add_exception identifier e env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    exceptions = Maps.Exception.add identifier e env.exceptions;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Exception (identifier, e))
        env.elts;
  }

let add_extension_constructor identifier ec env =
  {
    env with
    id =
      ( incr unique_id;
        !unique_id );
    extensions = Maps.Extension.add identifier ec env.extensions;
    elts =
      add_to_elts
        (Odoc_model.Paths.Identifier.name identifier)
        (`Extension (identifier, ec))
        env.elts;
  }

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

let lookup_type identifier env =
  try Some (Maps.Type.find identifier env.types) with _ -> None

let lookup_module_type identifier env =
  let maybe_record_result res =
    match env.recorder with
    | Some r -> r.lookups <- res :: r.lookups
    | None -> ()
  in
  match Maps.ModuleType.find identifier env.module_types with
  | result ->
      maybe_record_result (ModuleType (identifier, true));
      Some result
  | exception _ ->
      maybe_record_result (ModuleType (identifier, false));
      None

let lookup_value identifier env =
  try Some (Maps.Value.find identifier env.values) with _ -> None

let lookup_section_title identifier env =
  try Some (Maps.Label.find identifier env.titles) with _ -> None

let lookup_class identifier env =
  try Some (Maps.Class.find identifier env.classes) with _ -> None

let lookup_class_type identifier env =
  try Some (Maps.ClassType.find identifier env.class_types) with _ -> None

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
      let ty = Component.Of_Lang.(module_ empty m) in
      ty
  | Pack _ -> failwith "Unsupported"

let lookup_root_module name env =
  let result =
    match env.resolver with 
    | None -> None
    | Some r ->
          match r.lookup_unit name with
          | Forward_reference -> Some Forward
          | Not_found -> None
          | Found u ->
              let unit = r.resolve_unit u.root in
              Some (Resolved (u.root.digest, unit.id, module_of_unit unit))
  in
  ( match (env.recorder, result) with
  | Some r, Some Forward ->
      r.lookups <- RootModule (name, Some `Forward) :: r.lookups
  | Some r, Some (Resolved (digest, _, _)) ->
      r.lookups <- RootModule (name, Some (`Resolved digest)) :: r.lookups
  | Some r, None -> r.lookups <- RootModule (name, None) :: r.lookups
  | None, _ -> () );
  result

let lookup_module_internal identifier env =
  match try Some (Maps.Module.find identifier env.modules) with _ -> None with
  | Some _ as result -> result
  | None -> (
      match identifier with
      | `Root (_, name) -> (
          match lookup_root_module (UnitName.to_string name) env with
          | Some (Resolved (_, _, m)) -> Some m
          | Some Forward | None -> None )
      | _ -> None )

let lookup_module identifier env =
  let maybe_record_result res =
    match env.recorder with
    | Some r -> r.lookups <- res :: r.lookups
    | None -> ()
  in
  match lookup_module_internal identifier env with
  | Some _ as result ->
      maybe_record_result (Module (identifier, true));
      result
  | None ->
      maybe_record_result (Module (identifier, false));
      None

let lookup_page name env =
  match env.resolver with
  | None -> None
  | Some r -> (
      match r.lookup_page name with
      | None -> None
      | Some root -> Some (r.resolve_page root) )

let find : ('a -> 'b option) -> 'a list -> 'b option =
 fun f ->
  let rec inner acc = function
    | x :: xs -> ( match f x with Some y -> Some y | None -> inner acc xs )
    | [] -> None
  in
  inner []

let lookup_any_by_name name env =
  try StringMap.find name env.elts with _ -> []

let lookup_signature_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.signature option =
    function
    | #Component.Element.signature as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_module_by_name_internal name env =
  let filter_fn : Component.Element.any -> Component.Element.module_ option =
    function
    | #Component.Element.module_ as item -> Some item
    | _ -> None
  in
  match find filter_fn (lookup_any_by_name name env) with
  | None -> None
  | Some (`Module (id, m)) -> Some (id, m)

let lookup_module_by_name name env =
  let maybe_record_result res =
    match (res, env.recorder) with
    | Some (id, _), Some r ->
        r.lookups <- ModuleByName (name, Some id) :: r.lookups
    | _ -> ()
  in
  let result = lookup_module_by_name_internal name env in
  maybe_record_result result;
  result

let lookup_module_type_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.module_type option
      = function
    | #Component.Element.module_type as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_datatype_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.datatype option =
    function
    | #Component.Element.datatype as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_class_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.class_ option =
    function
    | #Component.Element.class_ as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_class_type_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.class_type option =
    function
    | #Component.Element.class_type as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_value_by_name name env =
  let filter_fn :
      Component.Element.any ->
      [ Component.Element.value | Component.Element.external_ ] option =
    function
    | #Component.Element.value as item -> Some item
    | #Component.Element.external_ as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_label_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.label option =
    function
    | #Component.Element.label as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_constructor_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.constructor option
      = function
    | #Component.Element.constructor as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_exception_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.exception_ option =
    function
    | #Component.Element.exception_ as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_extension_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.extension option =
    function
    | #Component.Element.extension as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_field_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.field option =
    function
    | #Component.Element.field as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

let lookup_label_parent_by_name name env =
  let filter_fn : Component.Element.any -> Component.Element.label_parent option
      = function
    | #Component.Element.label_parent as item -> Some item
    | _ -> None
  in
  find filter_fn (lookup_any_by_name name env)

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
            let ty = module_ empty t in
            add_module t.Odoc_model.Lang.Module.id ty env
        | Odoc_model.Lang.Signature.ModuleType t ->
            let ty = module_type empty t in
            add_module_type t.Odoc_model.Lang.ModuleType.id ty env
        | Odoc_model.Lang.Signature.Comment c -> add_comment c env
        | Odoc_model.Lang.Signature.TypExt te ->
            List.fold_left
              (fun env tec ->
                let ty = extension_constructor empty tec in
                add_extension_constructor tec.L.Extension.Constructor.id ty env)
              env te.L.Extension.constructors
        | Odoc_model.Lang.Signature.Exception e ->
            let ty = exception_ empty e in
            add_exception e.Odoc_model.Lang.Exception.id ty env
        | Odoc_model.Lang.Signature.ModuleSubstitution m ->
            let _id = Ident.Of_Identifier.module_ m.id in
            let ty =
              Of_Lang.(
                module_of_module_substitution
                  (*                  { empty with modules = [ (m.id, id) ] } *)
                  empty m)
            in
            add_module m.id ty env
        | Odoc_model.Lang.Signature.TypeSubstitution t ->
            let ty = type_decl empty t in
            add_type t.Odoc_model.Lang.TypeDecl.id ty env
        | Odoc_model.Lang.Signature.Value v ->
            let ty = value empty v in
            add_value v.Odoc_model.Lang.Value.id ty env
        | Odoc_model.Lang.Signature.External e ->
            let ty = external_ empty e in
            add_external e.Odoc_model.Lang.External.id ty env
        | Odoc_model.Lang.Signature.Class (_, c) ->
            let ty = class_ empty c in
            add_class c.id ty env
        | Odoc_model.Lang.Signature.ClassType (_, c) ->
            let ty = class_type empty c in
            add_class_type c.id ty env
        | Odoc_model.Lang.Signature.Include i ->
            open_signature i.expansion.content env
        | Odoc_model.Lang.Signature.Open o -> open_signature o.expansion env)
      e s

let open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t =
 fun unit env ->
  match unit.content with Module s -> open_signature s env | Pack _ -> env

let initial_env :
    Odoc_model.Lang.Compilation_unit.t ->
    resolver ->
    Odoc_model.Lang.Compilation_unit.Import.t list * t =
 fun t resolver ->
  let open Odoc_model.Lang.Compilation_unit in
  let initial_env =
    let m = module_of_unit t in
    empty |> add_module t.id m
  in
  let initial_env = set_resolver initial_env resolver in
  List.fold_right
    (fun import (imports, env) ->
      match import with
      | Import.Resolved root ->
          let unit = resolver.resolve_unit root in
          Component.Delayed.eager := true;
          let m = module_of_unit unit in
          Component.Delayed.eager := false;
          let env = add_module unit.id m env in
          (import :: imports, env)
      | Import.Unresolved (str, _) -> (
          match resolver.lookup_unit str with
          | Forward_reference -> (import :: imports, env)
          | Found x -> (Import.Resolved x.root :: imports, env)
          | Not_found -> (import :: imports, env) ))
    t.imports ([], initial_env)

let modules_of env = Maps.Module.bindings env.modules

let verify_lookups env lookups =
  let bad_lookup = function
    | Module (id, found) ->
        let actually_found =
          match lookup_module id env with Some _ -> true | None -> false
        in
        found <> actually_found
    | RootModule (name, res) -> (
        let actual_result =
          match env.resolver with
          | None -> None
          | Some r ->
            match r.lookup_unit name with
            | Forward_reference -> Some `Forward
            | Not_found -> None
            | Found u -> Some (`Resolved u.root.digest)
        in
        match (res, actual_result) with
        | None, None -> false
        | Some `Forward, Some `Forward -> false
        | Some (`Resolved digest1), Some (`Resolved digest2) -> digest1 <> digest2
        | _ -> true )
    | ModuleType (id, found) ->
        let actually_found =
          match lookup_module_type id env with Some _ -> true | None -> false
        in
        found <> actually_found
    | ModuleByName (name, result) -> (
        let actually_found = lookup_module_by_name name env in
        match (result, actually_found) with
        | None, None -> false
        | Some id, Some (id', _) -> id <> id'
        | _ -> true )
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
  ( match (result, env.recorder) with
  | true, Some r -> r.lookups <- r.lookups @ lookups
  | _ -> () );
  result
