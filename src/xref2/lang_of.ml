open Odoc_model
open Paths
open Names

type maps = {
  module_ : (Ident.module_ * Identifier.Module.t) list;
  module_type : (Ident.module_type * Identifier.ModuleType.t) list;
  signatures : (Ident.signature * Identifier.Signature.t) list;
  type_ : (Ident.type_ * Identifier.Type.t) list;
  path_type :
    (Ident.path_type * Odoc_model.Paths_types.Identifier.path_type) list;
  exception_ : (Ident.exception_ * Identifier.Exception.t) list;
  value_ : (Ident.value * Identifier.Value.t) list;
  method_ : (Ident.method_ * Identifier.Method.t) list;
  instance_variable :
    (Ident.instance_variable * Identifier.InstanceVariable.t) list;
  class_ : (Ident.class_ * Identifier.Class.t) list;
  class_type : (Ident.class_type * Identifier.ClassType.t) list;
  labels : (Ident.label * Identifier.Label.t) list;
  parents : (Ident.parent * Identifier.Parent.t) list;
  label_parents : (Ident.label_parent * Identifier.LabelParent.t) list;
  path_class_type :
    (Ident.path_class_type * Odoc_model.Paths_types.Identifier.path_class_type)
    list;
  fragment_root : Cfrag.root option;
  any : (Ident.any * Identifier.t) list;
  (* Shadowed items *)
  s_modules : string list;
  s_module_types : string list;
  s_types : string list;
}

let empty =
  {
    module_ = [];
    module_type = [];
    signatures = [];
    type_ = List.map (fun (x, y) -> (y, x)) Component.core_type_ids;
    path_type =
      List.map
        (fun (x, y) ->
          ( (y :> Ident.path_type),
            (x :> Odoc_model.Paths_types.Identifier.path_type) ))
        Component.core_type_ids;
    exception_ = [];
    value_ = [];
    method_ = [];
    instance_variable = [];
    class_ = [];
    class_type = [];
    labels = [];
    label_parents = [];
    parents = [];
    path_class_type = [];
    fragment_root = None;
    any = [];
    s_modules = [];
    s_module_types = [];
    s_types = [];
  }

let with_fragment_root r = { empty with fragment_root = Some r }

module Opt = Component.Opt

module Path = struct
  let rec module_ map (p : Cpath.module_) : Odoc_model.Paths.Path.Module.t =
    match p with
    | `Substituted x -> module_ map x
    | `Resolved x -> `Resolved (resolved_module map x)
    | `Root x -> `Root x
    | `Dot (p, s) -> `Dot (module_ map p, s)
    | `Forward s -> `Forward s
    | `Apply (m1, m2) -> `Apply (module_ map m1, module_ map m2)

  and module_type map (p : Cpath.module_type) :
      Odoc_model.Paths.Path.ModuleType.t =
    match p with
    | `Substituted x -> module_type map x
    | `Resolved x -> `Resolved (resolved_module_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)

  and type_ map (p : Cpath.type_) : Odoc_model.Paths.Path.Type.t =
    match p with
    | `Substituted x -> type_ map x
    | `Resolved x -> `Resolved (resolved_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)

  and class_type map (p : Cpath.class_type) : Odoc_model.Paths.Path.ClassType.t
      =
    match p with
    | `Substituted x -> class_type map x
    | `Resolved x -> `Resolved (resolved_class_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)

  and resolved_module map (p : Cpath.Resolved.module_) :
      Odoc_model.Paths.Path.Resolved.Module.t =
    match p with
    | `Local id ->
        `Identifier
          ( try List.assoc id map.module_
            with Not_found ->
              failwith (Format.asprintf "Not_found: %a" Ident.fmt id) )
    | `Substituted x -> resolved_module map x
    | `Identifier (#Odoc_model.Paths.Identifier.Module.t as y) -> `Identifier y
    | `Subst (mty, m) ->
        `Subst (resolved_module_type map mty, resolved_module map m)
    | `SubstAlias (m1, m2) ->
        `SubstAlias (resolved_module map m1, resolved_module map m2)
    | `Hidden h -> `Hidden (resolved_module map h)
    | `Module (p, n) -> `Module (resolved_parent map p, n)
    | `Canonical (r, m) -> `Canonical (resolved_module map r, module_ map m)
    | `Apply (m1, m2) -> `Apply (resolved_module map m1, module_ map m2)
    | `Alias (m1, m2) -> `Alias (resolved_module map m1, resolved_module map m2)
    | `OpaqueModule m -> `OpaqueModule (resolved_module map m)

  and resolved_parent map (p : Cpath.Resolved.parent) =
    match p with
    | `Module m -> resolved_module map m
    | `ModuleType _ -> failwith "Invalid"
    | `FragmentRoot -> (
        Format.fprintf Format.err_formatter "dereferencing fragmentroot...\n%!";
        match map.fragment_root with
        | Some r -> resolved_parent map (r :> Cpath.Resolved.parent)
        | None -> failwith "Invalid" )

  and resolved_module_type map (p : Cpath.Resolved.module_type) :
      Odoc_model.Paths.Path.Resolved.ModuleType.t =
    match p with
    | `Identifier (#Odoc_model.Paths.Identifier.ModuleType.t as y) ->
        `Identifier y
    | `Local id -> `Identifier (List.assoc id map.module_type)
    | `ModuleType (p, name) -> `ModuleType (resolved_parent map p, name)
    | `Substituted s -> resolved_module_type map s
    | `SubstT (p1, p2) ->
        `SubstT (resolved_module_type map p1, resolved_module_type map p2)
    | `OpaqueModuleType m -> `OpaqueModuleType (resolved_module_type map m)

  and resolved_type map (p : Cpath.Resolved.type_) :
      Odoc_model.Paths.Path.Resolved.Type.t =
    match p with
    | `Identifier (#Odoc_model.Paths_types.Identifier.path_type as y) ->
        `Identifier y
    | `Local id -> `Identifier (List.assoc id map.path_type)
    | `Type (p, name) -> `Type (resolved_parent map p, name)
    | `Class (p, name) -> `Class (resolved_parent map p, name)
    | `ClassType (p, name) -> `ClassType (resolved_parent map p, name)
    | `Substituted s -> resolved_type map s

  and resolved_class_type map (p : Cpath.Resolved.class_type) :
      Odoc_model.Paths.Path.Resolved.ClassType.t =
    match p with
    | `Identifier (#Odoc_model.Paths_types.Identifier.path_class_type as y) ->
        `Identifier y
    | `Local id -> `Identifier (List.assoc id map.path_class_type)
    | `Class (p, name) -> `Class (resolved_parent map p, name)
    | `ClassType (p, name) -> `ClassType (resolved_parent map p, name)
    | `Substituted s -> resolved_class_type map s

  let rec module_fragment :
      maps -> Cfrag.module_ -> Odoc_model.Paths.Fragment.Module.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_module_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)

  and signature_fragment :
      maps -> Cfrag.signature -> Odoc_model.Paths.Fragment.Signature.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_signature_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)
    | `Root -> `Root

  and type_fragment : maps -> Cfrag.type_ -> Odoc_model.Paths.Fragment.Type.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_type_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)

  and resolved_module_fragment :
      maps ->
      Cfrag.resolved_module ->
      Odoc_model.Paths.Fragment.Resolved.Module.t =
   fun map f ->
    match f with
    | `Subst (p, f) ->
        `Subst (resolved_module_type map p, resolved_module_fragment map f)
    | `SubstAlias (p, f) ->
        `SubstAlias (resolved_module map p, resolved_module_fragment map f)
    | `Module (p, n) -> `Module (resolved_signature_fragment map p, n)
    | `OpaqueModule m -> `OpaqueModule (resolved_module_fragment map m)

  and resolved_signature_fragment :
      maps ->
      Cfrag.resolved_signature ->
      Odoc_model.Paths.Fragment.Resolved.Signature.t =
   fun map f ->
    match f with
    | `Root (`ModuleType p) -> `Root (`ModuleType (resolved_module_type map p))
    | `Root (`Module p) -> `Root (`Module (resolved_module map p))
    | (`OpaqueModule _ | `Subst _ | `SubstAlias _ | `Module _) as x ->
        ( resolved_module_fragment map x
          :> Odoc_model.Paths.Fragment.Resolved.Signature.t )

  and resolved_type_fragment :
      maps -> Cfrag.resolved_type -> Odoc_model.Paths.Fragment.Resolved.Type.t =
   fun map f ->
    match f with
    | `Type (p, n) -> `Type (resolved_signature_fragment map p, n)
    | `ClassType (p, n) -> `ClassType (resolved_signature_fragment map p, n)
    | `Class (p, n) -> `Class (resolved_signature_fragment map p, n)
end

module ExtractIDs = struct
  open Component

  let rec exception_ parent map id =
    let identifier =
      `Exception (parent, ExceptionName.of_string (Ident.Name.exception_ id))
    in
    { map with exception_ = (id, identifier) :: map.exception_ }

  and type_decl parent map id =
    let name = Ident.Name.type_ id in
    let identifier =
      if List.mem name map.s_types then
        `Type (parent, TypeName.internal_of_string name)
      else `Type (parent, TypeName.of_string name)
    in
    {
      map with
      type_ = (id, identifier) :: map.type_;
      path_type = ((id :> Ident.path_type), identifier) :: map.path_type;
      label_parents =
        ((id :> Ident.label_parent), (identifier :> Identifier.LabelParent.t))
        :: map.label_parents;
      parents =
        ((id :> Ident.parent), (identifier :> Identifier.Parent.t))
        :: map.parents;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and module_ parent map id =
    let name = Ident.Name.module_ id in
    let identifier =
      if List.mem name map.s_modules then
        `Module (parent, ModuleName.internal_of_string name)
      else `Module (parent, ModuleName.of_string name)
    in
    {
      map with
      module_ = (id, identifier) :: map.module_;
      signatures =
        ((id :> Ident.signature), (identifier :> Identifier.Signature.t))
        :: map.signatures;
      label_parents =
        ((id :> Ident.label_parent), (identifier :> Identifier.LabelParent.t))
        :: map.label_parents;
      parents =
        ((id :> Ident.parent), (identifier :> Identifier.Parent.t))
        :: map.parents;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and module_type parent map id =
    let name = Ident.Name.module_type id in
    let identifier =
      if List.mem name map.s_module_types then
        `ModuleType (parent, ModuleTypeName.internal_of_string name)
      else `ModuleType (parent, ModuleTypeName.of_string name)
    in
    {
      map with
      module_type = (id, identifier) :: map.module_type;
      signatures =
        ((id :> Ident.signature), (identifier :> Identifier.Signature.t))
        :: map.signatures;
      label_parents =
        ((id :> Ident.label_parent), (identifier :> Identifier.LabelParent.t))
        :: map.label_parents;
      parents =
        ((id :> Ident.parent), (identifier :> Identifier.Parent.t))
        :: map.parents;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and value_ parent map id =
    let identifier =
      `Value (parent, ValueName.of_string (Ident.Name.value id))
    in
    {
      map with
      value_ = (id, identifier) :: map.value_;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and class_ parent map id =
    let identifier =
      `Class (parent, ClassName.of_string (Ident.Name.class_ id))
    in
    {
      map with
      class_ = (id, identifier) :: map.class_;
      path_class_type =
        ((id :> Ident.path_class_type), identifier) :: map.path_class_type;
      path_type = ((id :> Ident.path_type), identifier) :: map.path_type;
      label_parents =
        ((id :> Ident.label_parent), (identifier :> Identifier.LabelParent.t))
        :: map.label_parents;
      parents =
        ((id :> Ident.parent), (identifier :> Identifier.Parent.t))
        :: map.parents;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and class_type parent map (id : Ident.class_type) =
    let identifier =
      `ClassType (parent, ClassTypeName.of_string (Ident.Name.class_type id))
    in
    {
      map with
      class_type = ((id :> Ident.class_type), identifier) :: map.class_type;
      path_class_type =
        ((id :> Ident.path_class_type), identifier) :: map.path_class_type;
      path_type = ((id :> Ident.path_type), identifier) :: map.path_type;
      label_parents =
        ((id :> Ident.label_parent), (identifier :> Identifier.LabelParent.t))
        :: map.label_parents;
      parents =
        ((id :> Ident.parent), (identifier :> Identifier.Parent.t))
        :: map.parents;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and include_ parent map i =
    (* Shadowed items don't apply to nested includes *)
    let new_map = { map with s_modules = [] } in
    signature parent new_map i.Include.expansion_

  and open_ parent map o = signature parent map o.Open.expansion

  and docs parent map (d : Component.CComment.docs) =
    List.fold_right
      (fun item map ->
        match item.Location_.value with
        | `Heading (_, id, _) ->
            let identifier =
              `Label (parent, LabelName.of_string (Ident.Name.label id))
            in
            { map with labels = (id, identifier) :: map.labels }
        | _ -> map)
      d map

  and docs_or_stop parent map = function
    | `Docs d -> docs parent map d
    | `Stop -> map

  and method_ parent map id =
    let identifier =
      `Method (parent, MethodName.of_string (Ident.Name.method_ id))
    in
    {
      map with
      method_ = (id, identifier) :: map.method_;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and instance_variable parent map id =
    let identifier =
      `InstanceVariable
        ( parent,
          InstanceVariableName.of_string (Ident.Name.instance_variable id) )
    in
    {
      map with
      instance_variable = (id, identifier) :: map.instance_variable;
      any = ((id :> Ident.any), (identifier :> Identifier.t)) :: map.any;
    }

  and class_signature parent map sg =
    let open ClassSignature in
    List.fold_right
      (fun item map ->
        match item with
        | Method (id, _) -> method_ parent map id
        | InstanceVariable (id, _) -> instance_variable parent map id
        | Inherit _ -> map
        | Constraint _ -> map
        | Comment c -> docs_or_stop (parent :> Identifier.LabelParent.t) map c)
      sg map

  and signature_items parent map items =
    let open Signature in
    let lpp = (parent :> Identifier.LabelParent.t) in
    List.fold_right
      (fun item map ->
        match item with
        | Module (id, _, m) ->
            docs lpp (module_ parent map id) (Delayed.get m).doc
        | ModuleSubstitution (id, m) -> docs lpp (module_ parent map id) m.doc
        | ModuleType (id, mt) ->
            docs lpp (module_type parent map id) (Delayed.get mt).doc
        | Type (id, _, t) ->
            docs lpp (type_decl parent map id) (Delayed.get t).doc
        | TypeSubstitution (id, t) -> docs lpp (type_decl parent map id) t.doc
        | Exception (id, e) -> docs lpp (exception_ parent map id) e.doc
        | Value (id, v) -> docs lpp (value_ parent map id) v.doc
        | External (id, e) -> docs lpp (value_ parent map id) e.doc
        | Class (id, _, c) -> docs lpp (class_ parent map id) c.doc
        | ClassType (id, _, c) -> docs lpp (class_type parent map id) c.doc
        | Include i -> docs lpp (include_ parent map i) i.doc
        | TypExt t -> docs lpp map t.doc
        | Open o -> open_ parent map o
        | Comment d -> docs_or_stop lpp map d)
      items map

  and signature parent map sg =
    let open Signature in
    signature_items parent map sg.items
end

let rec signature_items id map items =
  let open Component.Signature in
  let map = ExtractIDs.signature_items id map items in
  List.fold_right
    (fun item acc ->
      match item with
      | Module (id, r, m) ->
          let m = Component.Delayed.get m in
          Odoc_model.Lang.Signature.Module (r, module_ map id m) :: acc
      | ModuleType (id, m) ->
          Odoc_model.Lang.Signature.ModuleType (module_type map id m) :: acc
      | Type (id, r, t) -> (
          let t = Component.Delayed.get t in
          try Odoc_model.Lang.Signature.Type (r, type_decl map id t) :: acc
          with e ->
            let bt = Printexc.get_backtrace () in
            Format.fprintf Format.err_formatter
              "Failed (%s) during type lookup: %a\nbt:\n%s\n%!"
              (Printexc.to_string e) Ident.fmt id bt;
            raise e )
      | Exception (id', e) ->
          Odoc_model.Lang.Signature.Exception
            (exception_ map
               (id :> Odoc_model.Paths_types.Identifier.parent)
               id' e)
          :: acc
      | TypExt t -> Odoc_model.Lang.Signature.TypExt (typ_ext map id t) :: acc
      | Value (id, v) ->
          Odoc_model.Lang.Signature.Value (value_ map id v) :: acc
      | Include i -> (
          try Odoc_model.Lang.Signature.Include (include_ id map i) :: acc
          with e ->
            Format.fprintf Format.err_formatter
              "Caught exception %s with include: %a" (Printexc.to_string e)
              Component.Fmt.include_ i;
            raise e )
      | Open o -> Odoc_model.Lang.Signature.Open (open_ id map o) :: acc
      | External (id, e) ->
          Odoc_model.Lang.Signature.External (external_ map id e) :: acc
      | ModuleSubstitution (id, m) ->
          Odoc_model.Lang.Signature.ModuleSubstitution
            (module_substitution map id m)
          :: acc
      | TypeSubstitution (id, t) ->
          Odoc_model.Lang.Signature.TypeSubstitution (type_decl map id t) :: acc
      | Class (id, r, c) ->
          Odoc_model.Lang.Signature.Class (r, class_ map id c) :: acc
      | ClassType (id, r, c) ->
          Odoc_model.Lang.Signature.ClassType (r, class_type map id c) :: acc
      | Comment c ->
          Odoc_model.Lang.Signature.Comment (docs_or_stop map c) :: acc)
    items []

and signature id map sg =
  let open Component.Signature in
  signature_items id map sg.items

and class_ map id c =
  let open Component.Class in
  let identifier = List.assoc id map.class_ in
  let expansion =
    Opt.map
      (class_signature map (identifier :> Identifier.ClassSignature.t))
      c.expansion
  in
  {
    id = identifier;
    doc = docs map c.doc;
    virtual_ = c.virtual_;
    params = c.params;
    type_ =
      class_decl map
        (identifier :> Paths_types.Identifier.path_class_type)
        c.type_;
    expansion;
  }

and class_decl map parent c =
  match c with
  | Component.Class.ClassType expr ->
      ClassType (class_type_expr map parent expr)
  | Arrow (lbl, t, d) -> Arrow (lbl, type_expr map t, class_decl map parent d)

and class_type_expr map parent c =
  match c with
  | Component.ClassType.Constr (p, ts) ->
      Constr (Path.class_type map p, List.map (type_expr map) ts)
  | Signature s -> Signature (class_signature map parent s)

and class_type map id c =
  let open Component.ClassType in
  let identifier = List.assoc id map.class_type in
  let expansion =
    Opt.map
      (class_signature map (identifier :> Identifier.ClassSignature.t))
      c.expansion
  in
  {
    Odoc_model.Lang.ClassType.id = identifier;
    doc = docs map c.doc;
    virtual_ = c.virtual_;
    params = c.params;
    expr =
      class_type_expr map
        (identifier :> Paths_types.Identifier.path_class_type)
        c.expr;
    expansion;
  }

and class_signature map parent sg =
  let open Component.ClassSignature in
  let map = ExtractIDs.class_signature parent map sg.items in
  let items =
    List.map
      (function
        | Method (id, m) ->
            Odoc_model.Lang.ClassSignature.Method (method_ map id m)
        | InstanceVariable (id, i) ->
            InstanceVariable (instance_variable map id i)
        | Constraint (t1, t2) -> Constraint (type_expr map t1, type_expr map t2)
        | Inherit e -> Inherit (class_type_expr map parent e)
        | Comment c -> Comment (docs_or_stop map c))
      sg.items
  in
  { self = Opt.map (type_expr map) sg.self; items }

and method_ map id m =
  let open Component.Method in
  let identifier = List.assoc id map.method_ in
  {
    id = identifier;
    doc = docs map m.doc;
    private_ = m.private_;
    virtual_ = m.virtual_;
    type_ = type_expr map m.type_;
  }

and instance_variable map id i =
  let open Component.InstanceVariable in
  let identifier = List.assoc id map.instance_variable in
  {
    id = identifier;
    doc = docs map i.doc;
    mutable_ = i.mutable_;
    virtual_ = i.virtual_;
    type_ = type_expr map i.type_;
  }

and external_ map id e =
  let open Component.External in
  let identifier = List.assoc id map.value_ in
  {
    id = identifier;
    doc = docs map e.doc;
    type_ = type_expr map e.type_;
    primitives = e.primitives;
  }

and module_expansion :
    maps ->
    Identifier.Signature.t ->
    Component.Module.expansion ->
    Lang.Module.expansion =
 fun map id e ->
  let open Component.Module in
  let open Component.FunctorParameter in
  match e with
  | AlreadyASig -> Lang.Module.AlreadyASig
  | Signature sg -> Signature (signature id map sg)
  | Functor (args, sg) ->
      let identifier, rev_args, map =
        List.fold_left
          (fun (id, args, map) arg ->
            match arg with
            | Named arg ->
                let name = Ident.Name.module_ arg.id in
                let identifier' =
                  if List.mem name map.s_modules then
                    `Parameter (id, ParameterName.internal_of_string name)
                  else `Parameter (id, ParameterName.of_string name)
                in
                let identifier_result = `Result id in
                let map =
                  { map with module_ = (arg.id, identifier') :: map.module_ }
                in
                let arg = functor_parameter map arg in
                ( identifier_result,
                  Odoc_model.Lang.FunctorParameter.Named arg :: args,
                  map )
            | Unit -> (`Result id, Unit :: args, map))
          (id, [], map) args
      in
      Functor (List.rev rev_args, signature identifier map sg)

and include_ parent map i =
  let open Component.Include in
  {
    Odoc_model.Lang.Include.parent;
    doc = docs map i.doc;
    decl = module_decl map parent i.decl;
    expansion =
      { resolved = false; content = signature parent map i.expansion_ };
    inline = false;
  }

and open_ parent map o =
  let open Component.Open in
  { Odoc_model.Lang.Open.expansion = signature parent map o.expansion }

and value_ map id v =
  let open Component.Value in
  let identifier = List.assoc id map.value_ in
  { id = identifier; doc = docs map v.doc; type_ = type_expr map v.type_ }

and typ_ext map parent t =
  let open Component.Extension in
  {
    type_path = Path.type_ map t.type_path;
    doc = docs map t.doc;
    type_params = t.type_params;
    private_ = t.private_;
    constructors = List.map (extension_constructor map parent) t.constructors;
  }

and extension_constructor map parent c =
  let open Component.Extension.Constructor in
  let identifier = `Extension (parent, ExtensionName.of_string c.name) in
  {
    id = identifier;
    doc = docs map c.doc;
    args =
      type_decl_constructor_argument map
        (parent :> Odoc_model.Paths_types.Identifier.parent)
        c.args;
    res = Opt.map (type_expr map) c.res;
  }

and module_ map id m =
  try
    let open Component.Module in
    let identifier =
      (List.assoc id map.module_ :> Odoc_model.Paths_types.Identifier.signature)
    in
    let canonical = function
      | Some (p, r) -> Some (Path.module_ map p, r)
      | None -> None
    in
    let expansion =
      Opt.map
        (module_expansion map (identifier :> Identifier.Signature.t))
        m.expansion
    in
    {
      Odoc_model.Lang.Module.id = List.assoc id map.module_;
      doc = docs map m.doc;
      type_ =
        module_decl map
          (identifier :> Odoc_model.Paths_types.Identifier.signature)
          m.type_;
      canonical = canonical m.canonical;
      hidden = m.hidden;
      display_type = Opt.map (module_decl map identifier) m.display_type;
      expansion;
    }
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter
      "Exception handling module: %a\nbacktrace:\n%s\n%!" Ident.fmt id bt;
    raise e

and module_substitution map id m =
  let open Component.ModuleSubstitution in
  {
    Odoc_model.Lang.ModuleSubstitution.id = List.assoc id map.module_;
    doc = docs map m.doc;
    manifest = Path.module_ map m.manifest;
  }

and module_decl :
    maps ->
    Odoc_model.Paths_types.Identifier.signature ->
    Component.Module.decl ->
    Odoc_model.Lang.Module.decl =
 fun map identifier d ->
  match d with
  | Component.Module.Alias p ->
      Odoc_model.Lang.Module.Alias (Path.module_ map p)
  | ModuleType mty -> ModuleType (module_type_expr map identifier mty)

and module_type_expr map identifier =
  let substitution = function
    | Component.ModuleType.ModuleEq (frag, decl) ->
        Odoc_model.Lang.ModuleType.ModuleEq
          (Path.module_fragment map frag, module_decl map identifier decl)
    | ModuleSubst (frag, path) ->
        ModuleSubst (Path.module_fragment map frag, Path.module_ map path)
    | TypeEq (frag, eqn) ->
        TypeEq (Path.type_fragment map frag, type_decl_equation map eqn)
    | TypeSubst (frag, eqn) ->
        TypeSubst (Path.type_fragment map frag, type_decl_equation map eqn)
  in
  function
  | Component.ModuleType.Path p ->
      Odoc_model.Lang.ModuleType.Path (Path.module_type map p)
  | Signature s ->
      Signature
        (signature
           (identifier :> Odoc_model.Paths.Identifier.Signature.t)
           map s)
  | With (expr, subs) ->
      With (module_type_expr map identifier expr, List.map substitution subs)
  | Functor (Named arg, expr) ->
      let name = Ident.Name.module_ arg.id in
      let identifier' =
        if List.mem name map.s_modules then
          `Parameter (identifier, ParameterName.internal_of_string name)
        else `Parameter (identifier, ParameterName.of_string name)
      in
      let map = { map with module_ = (arg.id, identifier') :: map.module_ } in
      Functor
        ( Named (functor_parameter map arg),
          module_type_expr map (`Result identifier) expr )
  | Functor (Unit, expr) ->
      Functor (Unit, module_type_expr map (`Result identifier) expr)
  | TypeOf decl -> TypeOf (module_decl map identifier decl)

and module_type map id mty =
  let mty = Component.Delayed.get mty in
  let identifier = List.assoc id map.module_type in
  let sig_id = (identifier :> Odoc_model.Paths.Identifier.Signature.t) in
  let expansion = Opt.map (module_expansion map sig_id) mty.expansion in
  {
    Odoc_model.Lang.ModuleType.id = identifier;
    doc = docs map mty.doc;
    expr = Opt.map (module_type_expr map sig_id) mty.expr;
    display_expr = None;
    expansion;
  }

and type_decl_constructor_argument :
    maps ->
    Paths_types.Identifier.parent ->
    Component.TypeDecl.Constructor.argument ->
    Odoc_model.Lang.TypeDecl.Constructor.argument =
 fun map parent a ->
  match a with
  | Tuple ls -> Tuple (List.map (type_expr map) ls)
  | Record fs -> Record (List.map (type_decl_field map parent) fs)

and type_decl_field :
    maps ->
    Paths_types.Identifier.parent ->
    Component.TypeDecl.Field.t ->
    Odoc_model.Lang.TypeDecl.Field.t =
 fun map parent f ->
  let identifier = `Field (parent, FieldName.of_string f.name) in
  {
    id = identifier;
    doc = docs map f.doc;
    mutable_ = f.mutable_;
    type_ = type_expr map f.type_;
  }

and type_decl_equation map (eqn : Component.TypeDecl.Equation.t) :
    Odoc_model.Lang.TypeDecl.Equation.t =
  {
    params = eqn.params;
    private_ = eqn.private_;
    manifest = Opt.map (type_expr map) eqn.manifest;
    constraints =
      List.map
        (fun (x, y) -> (type_expr map x, type_expr map y))
        eqn.constraints;
  }

and type_decl map id (t : Component.TypeDecl.t) : Odoc_model.Lang.TypeDecl.t =
  let identifier = List.assoc id map.type_ in
  {
    id = identifier;
    equation = type_decl_equation map t.equation;
    doc = docs map t.doc;
    representation =
      Opt.map (type_decl_representation map identifier) t.representation;
  }

and type_decl_representation map id (t : Component.TypeDecl.Representation.t) :
    Odoc_model.Lang.TypeDecl.Representation.t =
  match t with
  | Extensible -> Extensible
  | Variant cs -> Variant (List.map (type_decl_constructor map id) cs)
  | Record fs ->
      Record
        (List.map
           (type_decl_field map
              (id :> Odoc_model.Paths_types.Identifier.parent))
           fs)

and type_decl_constructor :
    maps ->
    Odoc_model.Paths_types.Identifier.type_ ->
    Component.TypeDecl.Constructor.t ->
    Odoc_model.Lang.TypeDecl.Constructor.t =
 fun map id t ->
  let identifier = `Constructor (id, ConstructorName.of_string t.name) in
  {
    id = identifier;
    doc = docs map t.doc;
    args =
      type_decl_constructor_argument map
        (id :> Odoc_model.Paths_types.Identifier.parent)
        t.args;
    res = Opt.map (type_expr map) t.res;
  }

and type_expr_package map t =
  {
    Lang.TypeExpr.Package.path =
      Path.module_type map t.Component.TypeExpr.Package.path;
    substitutions =
      List.map
        (fun (frag, texpr) ->
          (Path.type_fragment map frag, type_expr map texpr))
        t.substitutions;
  }

and type_expr map (t : Component.TypeExpr.t) : Odoc_model.Lang.TypeExpr.t =
  try
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t, str) -> Alias (type_expr map t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr map t1, type_expr map t2)
    | Tuple ts -> Tuple (List.map (type_expr map) ts)
    | Constr (path, ts) ->
        Constr (Path.type_ map path, List.map (type_expr map) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_expr_polyvar map v)
    | Object o -> Object (type_expr_object map o)
    | Class (p, ts) -> Class (Path.class_type map p, List.map (type_expr map) ts)
    | Poly (strs, t) -> Poly (strs, type_expr map t)
    | Package p -> Package (type_expr_package map p)
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter
      "Exception %s handling type_expr: %a\nbacktrace:\n%s\n%!"
      (Printexc.to_string e) Component.Fmt.type_expr t bt;
    raise e

and type_expr_polyvar map v =
  let constructor c =
    {
      Lang.TypeExpr.Polymorphic_variant.Constructor.name =
        c.Component.TypeExpr.Polymorphic_variant.Constructor.name;
      constant = c.constant;
      arguments = List.map (type_expr map) c.arguments;
      doc = docs map c.doc;
    }
  in
  let element = function
    | Component.TypeExpr.Polymorphic_variant.Type t ->
        Lang.TypeExpr.Polymorphic_variant.Type (type_expr map t)
    | Constructor c -> Constructor (constructor c)
  in
  { kind = v.kind; elements = List.map element v.elements }

and type_expr_object map o =
  let method_ m =
    {
      Lang.TypeExpr.Object.name = m.Component.TypeExpr.Object.name;
      type_ = type_expr map m.type_;
    }
  in
  let field = function
    | Component.TypeExpr.Object.Method m ->
        Lang.TypeExpr.Object.Method (method_ m)
    | Inherit i -> Inherit (type_expr map i)
  in
  { Lang.TypeExpr.Object.fields = List.map field o.fields; open_ = o.open_ }

and functor_parameter map f =
  let identifier = List.assoc f.id map.module_ in
  let expansion =
    Opt.map
      (module_expansion map (identifier :> Identifier.Signature.t))
      f.expansion
  in
  {
    Odoc_model.Lang.FunctorParameter.id = identifier;
    expr =
      module_type_expr map
        (identifier :> Odoc_model.Paths_types.Identifier.signature)
        f.expr;
    display_expr = None;
    expansion;
  }

and exception_ map parent id (e : Component.Exception.t) :
    Odoc_model.Lang.Exception.t =
  let identifier = List.assoc id map.exception_ in
  {
    id = identifier;
    doc = docs map e.doc;
    args = type_decl_constructor_argument map parent e.args;
    res = Opt.map (type_expr map) e.res;
  }

and block_element map
    (d : Component.CComment.block_element Odoc_model.Location_.with_location) :
    Odoc_model.Comment.block_element Odoc_model.Location_.with_location =
  let value =
    match d.Odoc_model.Location_.value with
    | `Heading (l, id, content) -> (
        try `Heading (l, List.assoc id map.labels, content)
        with Not_found ->
          Format.fprintf Format.err_formatter "Failed to find id: %a\n"
            Ident.fmt id;
          raise Not_found )
    | `Tag t -> `Tag t
    | #Odoc_model.Comment.nestable_block_element as n -> n
  in
  { d with Odoc_model.Location_.value }

and docs : maps -> Component.CComment.docs -> Odoc_model.Comment.docs =
 fun map ds -> List.map (fun d -> block_element map d) ds

and docs_or_stop map (d : Component.CComment.docs_or_stop) =
  match d with `Docs d -> `Docs (docs map d) | `Stop -> `Stop
