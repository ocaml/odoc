open Component

type t = Substitution.t = {
  module_ : Cpath.Resolved.module_ ModuleMap.t;
  module_type : Cpath.Resolved.module_type ModuleTypeMap.t;
  type_ : Cpath.Resolved.type_ TypeMap.t;
  class_type : Cpath.Resolved.class_type ClassTypeMap.t;
  type_replacement : TypeExpr.t TypeMap.t;
}

exception TypeReplacement of Component.TypeExpr.t

open Component
open Substitution

let identity =
  {
    module_ = ModuleMap.empty;
    module_type = ModuleTypeMap.empty;
    type_ = TypeMap.empty;
    class_type = ClassTypeMap.empty;
    type_replacement = TypeMap.empty;
  }

let add_module id subst t =
  {
    t with
    module_ = ModuleMap.add id subst t.module_;
  }

let add_module_type id subst t =
  {
    t with
    module_type = ModuleTypeMap.add id subst t.module_type;
  }

let add_type : Ident.type_ -> Cpath.Resolved.type_ -> t -> t =
 fun id subst t ->
  {
    t with
    type_ = TypeMap.add (id :> Ident.path_type) subst t.type_;
  }

let add_class : Ident.class_ -> Cpath.Resolved.class_type -> t -> t =
 fun id subst t ->
  {
    t with
    type_ =
      TypeMap.add (id :> Ident.path_type) (subst :> Cpath.Resolved.type_) t.type_;
    class_type =
      ClassTypeMap.add (id :> Ident.path_class_type) subst t.class_type;
   }

let add_class_type : Ident.class_type -> Cpath.Resolved.class_type -> t -> t =
 fun id subst t ->
  {
    t with
    type_ =
      TypeMap.add (id :> Ident.path_type) (subst :> Cpath.Resolved.type_) t.type_;
    class_type =
      ClassTypeMap.add (id :> Ident.path_class_type) subst t.class_type;
  }

let add_type_replacement : Ident.path_type -> Component.TypeExpr.t -> t -> t =
 fun id texp t ->
  { t with type_replacement = TypeMap.add id texp t.type_replacement }

let compose_delayed f : 'a Delayed.t Substitution.delayed -> t -> 'a Delayed.t Substitution.delayed =
  fun v s ->
  match v with
  | DelayedSubst (s', v) ->
    let v = Delayed.put (fun () -> f s' (Delayed.get v)) in
    DelayedSubst (s, v) (* TODO: compose s and s': Remove [f] argument *)
  | NoSubst v -> DelayedSubst (s, v)

let make_delayed f = NoSubst (Delayed.put f)

let rec resolved_module_path :
    t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ModuleMap.find id s.module_) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Apply (p1, p2) -> `Apply (resolved_module_path s p1, module_path s p2)
  | `Substituted p -> `Substituted (resolved_module_path s p)
  | `Module (p, n) -> `Module (resolved_parent_path s p, n)
  | `Alias (p1, p2) ->
      `Alias (resolved_module_path s p1, resolved_module_path s p2)
  | `Subst (p1, p2) ->
      `Subst (resolved_module_type_path s p1, resolved_module_path s p2)
  | `SubstAlias (p1, p2) ->
      `SubstAlias (resolved_module_path s p1, resolved_module_path s p2)
  | `Hidden p1 -> `Hidden (resolved_module_path s p1)
  | `Canonical (p1, p2) ->
      `Canonical (resolved_module_path s p1, module_path s p2)

and resolved_parent_path s = function
  | `Module m -> `Module (resolved_module_path s m)
  | `ModuleType m -> `ModuleType (resolved_module_type_path s m)
  | `FragmentRoot as x -> x

and module_path : t -> Cpath.module_ -> Cpath.module_ =
 fun s p ->
  match p with
  | `Resolved p' -> `Resolved (resolved_module_path s p')
  | `Dot (p', str) -> `Dot (module_path s p', str)
  | `Apply (p1, p2) -> `Apply (module_path s p1, module_path s p2)
  | `Substituted p -> `Substituted (module_path s p)
  | `Forward _ -> p
  | `Root _ -> p

and resolved_module_type_path :
    t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ModuleTypeMap.find id s.module_type) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_module_type_path s p)
  | `ModuleType (p, n) -> `ModuleType (resolved_parent_path s p, n)
  | `SubstT (m1, m2) -> `SubstT (resolved_module_type_path s m1, resolved_module_type_path s m2)

and module_type_path : t -> Cpath.module_type -> Cpath.module_type =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_module_type_path s r)
  | `Substituted p -> `Substituted (module_type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_type_path : t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun s p ->
  match p with
  | `Local id -> (
      if TypeMap.mem id s.type_replacement then
        raise (TypeReplacement (TypeMap.find id s.type_replacement));
      match try Some (TypeMap.find id s.type_) with Not_found -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_type_path s p)
  | `Type (p, n) -> `Type (resolved_parent_path s p, n)
  | `ClassType (p, n) -> `ClassType (resolved_parent_path s p, n)
  | `Class (p, n) -> `Class (resolved_parent_path s p, n)

and type_path : t -> Cpath.type_ -> Cpath.type_ =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_type_path s r)
  | `Substituted p -> `Substituted (type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_class_type_path :
    t -> Cpath.Resolved.class_type -> Cpath.Resolved.class_type =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ClassTypeMap.find id s.class_type) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_class_type_path s p)
  | `ClassType (p, n) -> `ClassType (resolved_parent_path s p, n)
  | `Class (p, n) -> `Class (resolved_parent_path s p, n)

and class_type_path : t -> Cpath.class_type -> Cpath.class_type =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_class_type_path s r)
  | `Substituted p -> `Substituted (class_type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

let rec resolved_signature_fragment : t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
  fun t r ->
  match r with
  | `Root (`ModuleType p) -> `Root (`ModuleType (resolved_module_type_path t p))
  | `Root (`Module p) -> `Root (`Module (resolved_module_path t p))
  | `Subst _
  | `SubstAlias _
  | `Module _ as x -> (resolved_module_fragment t x :> Cfrag.resolved_signature)

and resolved_module_fragment : t -> Cfrag.resolved_module -> Cfrag.resolved_module =
  fun t r ->
  match r with
  | `Subst (mty, f) -> `Subst (resolved_module_type_path t mty, resolved_module_fragment t f)
  | `SubstAlias (m, f) -> `SubstAlias (resolved_module_path t m, resolved_module_fragment t f)
  | `Module (sg, n) -> `Module (resolved_signature_fragment t sg, n)

and resolved_type_fragment : t -> Cfrag.resolved_type -> Cfrag.resolved_type =
  fun t r ->
    match r with
    | `Type (s,n) -> `Type (resolved_signature_fragment t s, n)
    | `ClassType (s,n) -> `ClassType (resolved_signature_fragment t s, n)
    | `Class (s,n) -> `Class (resolved_signature_fragment t s, n)

let rec signature_fragment : t -> Cfrag.signature -> Cfrag.signature =
  fun t r ->
  match r with
  | `Resolved f -> `Resolved (resolved_signature_fragment t f)
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)
  | `Root -> `Root

let module_fragment : t -> Cfrag.module_ -> Cfrag.module_ =
  fun t r ->
  match r with
  | `Resolved r -> `Resolved (resolved_module_fragment t r)
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)

let type_fragment : t -> Cfrag.type_ -> Cfrag.type_ =
  fun t r ->
  match r with
  | `Resolved r -> `Resolved (resolved_type_fragment t r)
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)

let option_ conv s x = match x with Some x -> Some (conv s x) | None -> None

let list conv s xs = List.map (conv s) xs

let rec type_ s t =
  let open Component.TypeDecl in
  let representation = option_ type_decl_representation s t.representation in
  { equation = type_decl_equation s t.equation; representation; doc=t.doc }

and type_decl_representation s t =
  let open Component.TypeDecl.Representation in
  match t with
  | Variant cs -> Variant (List.map (type_decl_constructor s) cs)
  | Record fs -> Record (List.map (type_decl_field s) fs)
  | Extensible -> t

and type_decl_constructor s t =
  let open Component.TypeDecl.Constructor in
  let args = type_decl_constructor_arg s t.args in
  let res = option_ type_expr s t.res in
  { t with args; res; }

and type_poly_var s v =
  let open Component.TypeExpr.Polymorphic_variant in
  let map_constr c =
    let open Constructor in
    {
      name = c.name;
      constant = c.constant;
      arguments = List.map (type_expr s) c.arguments;
      doc = c.doc;
    }
  in
  let map_element = function
    | Type t -> Type (type_expr s t)
    | Constructor c -> Constructor (map_constr c)
  in
  { kind = v.kind; elements = List.map map_element v.elements }

and type_object s o =
  let open Component.TypeExpr.Object in
  let map_field = function
    | Method m -> Method { m with type_ = type_expr s m.type_ }
    | Inherit t -> Inherit (type_expr s t)
  in
  { fields = List.map map_field o.fields; open_ = o.open_ }

and type_package s p =
  let open Component.TypeExpr.Package in
  let sub (x, y) = (type_fragment s x, type_expr s y) in
  {
    path = module_type_path s p.path;
    substitutions = List.map sub p.substitutions;
  }

and type_expr s t =
  let open Component.TypeExpr in
  try
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t, str) -> Alias (type_expr s t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr s t1, type_expr s t2)
    | Tuple ts -> Tuple (List.map (type_expr s) ts)
    | Constr (p, ts) -> Constr (type_path s p, List.map (type_expr s) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_poly_var s v)
    | Object o -> Object (type_object s o)
    | Class (p, ts) -> Class (class_type_path s p, List.map (type_expr s) ts)
    | Poly (strs, ts) -> Poly (strs, type_expr s ts)
    | Package p -> Package (type_package s p)
  with TypeReplacement y -> y

and module_expansion s t =
  let open Component.Module in
  match t with
  | AlreadyASig -> AlreadyASig
  | Signature sg -> Signature (signature s sg)
  | Functor (arg, sg) ->
      Functor (List.map (functor_parameter s) arg, signature s sg)

and module_type s t =
  let open Component.ModuleType in
  let expr =
    match t.expr with Some m -> Some (module_type_expr s m) | None -> None
  in
  let expansion = option_ module_expansion s t.expansion in
  { expr; expansion; doc=t.doc }

and functor_parameter s t =
  let open Component.FunctorParameter in
  match t with
  | Named arg ->
      let expansion = option_ module_expansion s arg.expansion in
      Named { arg with expr = module_type_expr s arg.expr; expansion }
  | Unit -> Unit

and module_type_expr s t =
  let open Component.ModuleType in
  match t with
  | Path p -> Path (module_type_path s p)
  | Signature sg -> Signature (signature s sg)
  | Functor (arg, expr) ->
      Functor (functor_parameter s arg, module_type_expr s expr)
  | With (e, args) ->
      With (module_type_expr s e, List.map (module_type_substitution s) args)
  | TypeOf decl -> TypeOf (module_decl s decl)

and module_type_substitution s sub =
  let open Component.ModuleType in
  match sub with
  | ModuleEq (f, m) -> ModuleEq (module_fragment s f, module_decl s m)
  | ModuleSubst (f, p) -> ModuleSubst (module_fragment s f, module_path s p)
  | TypeEq (f, eq) -> TypeEq (type_fragment s f, type_decl_equation s eq)
  | TypeSubst (f, eq) -> TypeSubst (type_fragment s f, type_decl_equation s eq)

and module_decl s t =
  match t with
  | Alias p -> Alias (module_path s p)
  | ModuleType t -> ModuleType (module_type_expr s t)

and module_ s t =
  let open Component.Module in
  let type_ = module_decl s t.type_ in
  let expansion = option_ module_expansion s t.expansion in
  let canonical =
    option_ (fun s (m1, m2) -> (module_path s m1, m2)) s t.canonical
  in
  { t with type_; expansion; canonical; doc=t.doc }

and module_substitution s m =
  let open Component.ModuleSubstitution in
  let manifest = module_path s m.manifest in
  { manifest; doc=m.doc }

and type_decl_field s f =
  let open Component.TypeDecl.Field in
  { f with type_ = type_expr s f.type_ }

and type_decl_constructor_arg s a =
  let open Component.TypeDecl.Constructor in
  match a with
  | Tuple ts -> Tuple (list type_expr s ts)
  | Record fs -> Record (list type_decl_field s fs)

and type_decl_equation s t =
  let open Component.TypeDecl.Equation in
  {
    t with
    manifest = option_ type_expr s t.manifest;
    constraints =
      List.map (fun (x, y) -> (type_expr s x, type_expr s y)) t.constraints;
  }

and exception_ s e =
  let open Component.Exception in
  let res = option_ type_expr s e.res in
  let args = type_decl_constructor_arg s e.args in
  { args; res; doc=e.doc }

and extension_constructor s c =
  let open Component.Extension.Constructor in
  {
    c with
    args = type_decl_constructor_arg s c.args;
    res = option_ type_expr s c.res;
  }

and extension s e =
  let open Component.Extension in
  {
    e with
    type_path = type_path s e.type_path;
    constructors = List.map (extension_constructor s) e.constructors;
  }

and external_ s e =
  let open Component.External in
  { e with type_ = type_expr s e.type_; }

and include_ s i =
  let open Component.Include in
  {
    i with
    decl = module_decl s i.decl;
    expansion_ = apply_sig_map s i.expansion_.items i.expansion_.removed;
  }

and value s v =
  let open Component.Value in
  { type_ = type_expr s v.type_; doc=v.doc }

and class_ s c =
  let open Component.Class in
  let expansion = option_ class_signature s c.expansion in
  { c with type_ = class_decl s c.type_; expansion; }

and class_decl s =
  let open Component.Class in
  function
  | ClassType e -> ClassType (class_type_expr s e)
  | Arrow (lbl, t, d) -> Arrow (lbl, type_expr s t, class_decl s d)

and class_type_expr s =
  let open Component.ClassType in
  function
  | Constr (p, ts) -> Constr (class_type_path s p, List.map (type_expr s) ts)
  | Signature sg -> Signature (class_signature s sg)

and class_type s c =
  let open Component.ClassType in
  let expansion = option_ class_signature s c.expansion in
  { c with expr = class_type_expr s c.expr; expansion; }

and class_signature_item s =
  let open Component.ClassSignature in
  function
  | Method (id, m) -> Method (id, method_ s m)
  | InstanceVariable (id, i) -> InstanceVariable (id, instance_variable s i)
  | Constraint (t1, t2) -> Constraint (type_expr s t1, type_expr s t2)
  | Inherit e -> Inherit (class_type_expr s e)
  | Comment _ as y -> y

and class_signature s sg =
  let open Component.ClassSignature in
  {
    self = option_ type_expr s sg.self;
    items = List.map (class_signature_item s) sg.items;
  }

and method_ s m =
  let open Component.Method in
  { m with type_ = type_expr s m.type_; }

and instance_variable s i =
  let open Component.InstanceVariable in
  { i with type_ = type_expr s i.type_; }

and rename_bound_idents s sg =
  let open Component.Signature in
  function
  | [] -> (s, List.rev sg)
  | Module (id, r, m) :: rest ->
      let id' = Ident.Rename.module_ id in
      rename_bound_idents
        ( add_module id (`Local id') s)
        (Module (id', r, m) :: sg)
        rest
  | ModuleSubstitution (id, m) :: rest ->
      let id' = Ident.Rename.module_ id in
      rename_bound_idents
        ( add_module id (`Local id') s)
        (ModuleSubstitution (id', m) :: sg)
        rest
  | ModuleType (id, mt) :: rest ->
      let id' = Ident.Rename.module_type id in
      rename_bound_idents
        ( add_module_type id (`Local id') s)
        (ModuleType (id', mt) :: sg)
        rest
  | Type (id, r, t) :: rest ->
      let id' = Ident.Rename.type_ id in
      rename_bound_idents
        ( add_type id (`Local (id' :> Ident.path_type)) s)
        (Type (id', r, t) :: sg)
        rest
  | TypeSubstitution (id, t) :: rest ->
      let id' = Ident.Rename.type_ id in
      rename_bound_idents
        ( add_type id (`Local (id' :> Ident.path_type)) s)
        (TypeSubstitution (id', t) :: sg)
        rest
  | Exception (id, e) :: rest ->
      let id' = Ident.Rename.exception_ id in
      rename_bound_idents s
        (Exception (id', e) :: sg)
        rest
  | TypExt e :: rest -> rename_bound_idents s (TypExt e :: sg) rest
  | Value (id, v) :: rest ->
      let id' = Ident.Rename.value id in
      rename_bound_idents s
        (Value (id', v) :: sg)
        rest
  | External (id, e) :: rest ->
      let id' = Ident.Rename.value id in
      rename_bound_idents s
        (External (id', e) :: sg)
        rest
  | Class (id, r, c) :: rest ->
      let id' = Ident.Rename.class_ id in
      rename_bound_idents
        ( add_class id (`Local (id' :> Ident.path_class_type)) s)
        (Class (id', r, c) :: sg)
        rest
  | ClassType (id, r, c) :: rest ->
      let id' = Ident.Rename.class_type id in
      rename_bound_idents
        ( add_class_type id (`Local (id' :> Ident.path_class_type)) s)
        (ClassType (id', r, c) :: sg)
        rest
  | Include i :: rest ->
      let s, items =
        rename_bound_idents s [] i.Component.Include.expansion_.items
      in
      rename_bound_idents s
        ( Include
            { i with Component.Include.expansion_ = { items; removed = [] } }
        :: sg )
        rest
  | (Comment _ as item) :: rest -> rename_bound_idents s (item :: sg) rest

and removed_items s items =
  let open Component.Signature in
  List.map
    (function
      | RModule (id, _) when ModuleMap.mem id s.module_ ->
          RModule (id, ModuleMap.find id s.module_)
      | x -> x)
    items

and signature s sg =
  let s, items = rename_bound_idents s [] sg.items in
  apply_sig_map s items sg.removed

and apply_sig_map s items removed =
  let open Component.Signature in
  let items =
    List.map
      (function
        | Module (id, r, m) ->
            Module (id, r, compose_delayed module_ m s)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution (id, module_substitution s m)
        | ModuleType (id, mt) ->
            ModuleType (id, compose_delayed module_type mt s)
        | Type (id, r, t) ->
            Type (id, r, compose_delayed type_ t s)
        | TypeSubstitution (id, t) -> TypeSubstitution (id, type_ s t)
        | Exception (id, e) -> Exception (id, exception_ s e)
        | TypExt e -> TypExt (extension s e)
        | Value (id, v) -> Value (id, value s v)
        | External (id, e) -> External (id, external_ s e)
        | Class (id, r, c) -> Class (id, r, class_ s c)
        | ClassType (id, r, c) -> ClassType (id, r, class_type s c)
        | Include i -> Include (include_ s i)
        | Comment c -> Comment c)
      items
  in
  { items; removed = removed_items s removed }

let delayed_get_module : Module.t Delayed.t Substitution.delayed -> Module.t =
  function
  | DelayedSubst (s, m) -> module_ s (Delayed.get m)
  | NoSubst m -> Delayed.get m

let delayed_get_module_type : ModuleType.t Delayed.t Substitution.delayed -> ModuleType.t =
  function
  | DelayedSubst (s, mt) -> module_type s (Delayed.get mt)
  | NoSubst mt -> Delayed.get mt

let delayed_get_type : TypeDecl.t Delayed.t Substitution.delayed -> TypeDecl.t =
  function
  | DelayedSubst (s, t) -> type_ s (Delayed.get t)
  | NoSubst t -> Delayed.get t
