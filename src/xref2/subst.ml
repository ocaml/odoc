module ModuleMap = Map.Make(struct type t = Ident.module_ let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any) end)
module ModuleTypeMap = Map.Make(struct type t = Ident.module_type let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any) end)
module TypeMap = Map.Make(struct type t = Ident.path_type let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any) end)
module ClassTypeMap = Map.Make(struct type t = Ident.path_class_type let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any) end)

module IdentMap = Map.Make(struct type t = Ident.any let compare = Ident.compare end)

type t =
  { module_ : Cpath.resolved_module ModuleMap.t
  ; module_type : Cpath.resolved_module_type ModuleTypeMap.t
  ; type_ : Cpath.resolved_type TypeMap.t
  ; class_type : Cpath.resolved_class_type ClassTypeMap.t
  ; type_replacement : Component.TypeExpr.t TypeMap.t

  (* Reference maps *)
  ; ref_module : Cref.Resolved.module_ ModuleMap.t
  ; ref_module_type : Cref.Resolved.module_type ModuleTypeMap.t
  ; ref_type : Cref.Resolved.type_ TypeMap.t
  ; ref_class_type : Cref.Resolved.class_type ClassTypeMap.t

  ; id_any : Ident.any IdentMap.t

  }

exception TypeReplacement of Component.TypeExpr.t

let identity = 
  { module_ = ModuleMap.empty
  ; module_type = ModuleTypeMap.empty
  ; type_ = TypeMap.empty
  ; class_type = ClassTypeMap.empty
  ; type_replacement = TypeMap.empty
  
  ; ref_module = ModuleMap.empty
  ; ref_module_type = ModuleTypeMap.empty
  ; ref_type = TypeMap.empty
  ; ref_class_type = ClassTypeMap.empty

  ; id_any = IdentMap.empty }

  (* hack *)
let module_ref_of_module_path : Cpath.resolved_module -> Cref.Resolved.module_ =
  function
  | `Local x -> `Local x
  | `Identifier x -> `Identifier x
  | x ->
      let p = Lang_of.(Path.resolved_module empty x) in
      `Identifier (Odoc_model.Paths.Path.Resolved.Module.identifier p)

let add_module id subst t =
  let ref_subst = module_ref_of_module_path subst in   
  { t with module_ = ModuleMap.add id subst t.module_
  ; ref_module = ModuleMap.add id ref_subst t.ref_module }

(* hack *)
let module_type_ref_of_module_type_path : Cpath.resolved_module_type -> Cref.Resolved.module_type =
  function
  | `Local x -> `Local x
  | `Identifier x -> `Identifier x
  | x ->
      let p = Lang_of.(Path.resolved_module_type empty x) in
      `Identifier (Odoc_model.Paths.Path.Resolved.ModuleType.identifier p)

let add_module_type id subst t =
  let ref_subst = module_type_ref_of_module_type_path subst in    
  { t with module_type = ModuleTypeMap.add id subst t.module_type
  ; ref_module_type = ModuleTypeMap.add id ref_subst t.ref_module_type }

(* hack *)
let type_ref_of_type_path : Cpath.resolved_type -> Cref.Resolved.type_ =
  function
  | `Local x -> `Local x
  | `Identifier x -> `Identifier x
  | x ->
      let p = Lang_of.(Path.resolved_type empty x) in
      `Identifier (Odoc_model.Paths.Path.Resolved.Type.identifier p)

let add_type : Ident.type_ -> Cpath.resolved_type -> t -> t =
 fun id subst t ->
   let ref_subst = type_ref_of_type_path subst in
  { t with type_ = TypeMap.add (id :> Ident.path_type) subst t.type_
  ; ref_type = TypeMap.add (id :> Ident.path_type) ref_subst t.ref_type }

(* hack *)
let class_ref_of_class_path : Cpath.resolved_class_type -> Cref.Resolved.class_type =
  function
  | `Local x -> `Local x
  | `Identifier x -> `Identifier x
  | x ->
      let p = Lang_of.(Path.resolved_class_type empty x) in
      `Identifier (Odoc_model.Paths.Path.Resolved.ClassType.identifier p)



let add_class : Ident.class_ -> Cpath.resolved_class_type -> t -> t =
 fun id subst t ->
 let ref_subst = class_ref_of_class_path subst in
  {
    t with
    type_ = TypeMap.add (id :> Ident.path_type) (subst :> Cpath.resolved_type) t.type_;
    class_type = ClassTypeMap.add (id :> Ident.path_class_type) subst t.class_type;
    ref_type = TypeMap.add (id :> Ident.path_type) (ref_subst :> Cref.Resolved.type_) t.ref_type;
    ref_class_type = ClassTypeMap.add (id :> Ident.path_class_type) ref_subst t.ref_class_type
    ; 
  }

let add_class_type : Ident.class_type -> Cpath.resolved_class_type -> t -> t =
 fun id subst t ->
 let ref_subst = class_ref_of_class_path subst in
  {
    t with
    type_ = TypeMap.add (id :> Ident.path_type) (subst :> Cpath.resolved_type) t.type_;
    class_type = ClassTypeMap.add (id :> Ident.path_class_type) subst t.class_type;
    ref_type = TypeMap.add (id :> Ident.path_type) (ref_subst :> Cref.Resolved.type_) t.ref_type;
    ref_class_type = ClassTypeMap.add (id :> Ident.path_class_type) ref_subst t.ref_class_type
  }

let add_type_replacement : Ident.path_type -> Component.TypeExpr.t -> t -> t =
  fun id texp t ->
    {
      t with
      type_replacement = TypeMap.add id texp t.type_replacement
    }

let add_id_map : Ident.any -> Ident.any -> t -> t =
  fun id new_id t ->
    { 
      t with
      id_any = IdentMap.add id new_id t.id_any
    }

let rec resolved_module_path :
    t -> Cpath.resolved_module -> Cpath.resolved_module =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ModuleMap.find id s.module_) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Apply (p1, p2) -> `Apply (resolved_module_path s p1, module_path s p2)
  | `Substituted p -> `Substituted (resolved_module_path s p)
  | `Module (p, n) -> `Module (resolved_module_path s p, n)
  | `Alias (p1, p2) ->
      `Alias (resolved_module_path s p1, resolved_module_path s p2)
  | `Subst (p1, p2) ->
      `Subst (resolved_module_type_path s p1, resolved_module_path s p2)
  | `SubstAlias (p1, p2) ->
      `SubstAlias (resolved_module_path s p1, resolved_module_path s p2)
  | `Hidden p1 -> `Hidden (resolved_module_path s p1)
  | `Canonical (p1, p2) ->
      `Canonical (resolved_module_path s p1, module_path s p2)

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
    t -> Cpath.resolved_module_type -> Cpath.resolved_module_type =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ModuleTypeMap.find id s.module_type) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_module_type_path s p)
  | `ModuleType (p, n) -> `ModuleType (resolved_module_path s p, n)

and module_type_path : t -> Cpath.module_type -> Cpath.module_type =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_module_type_path s r)
  | `Substituted p -> `Substituted (module_type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_type_path : t -> Cpath.resolved_type -> Cpath.resolved_type =
 fun s p ->
  match p with
  | `Local id -> (
      if TypeMap.mem id s.type_replacement then begin
        raise (TypeReplacement (TypeMap.find id s.type_replacement))
      end;
      match try Some (TypeMap.find id s.type_) with Not_found -> None with
      | Some x -> x
      | None -> `Local id)
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_type_path s p)
  | `Type (p, n) -> `Type (resolved_module_path s p, n)
  | `ClassType (p, n) -> `ClassType (resolved_module_path s p, n)
  | `Class (p, n) -> `Class (resolved_module_path s p, n)

and type_path : t -> Cpath.type_ -> Cpath.type_ =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_type_path s r)
  | `Substituted p -> `Substituted (type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_class_type_path :
    t -> Cpath.resolved_class_type -> Cpath.resolved_class_type =
 fun s p ->
  match p with
  | `Local id -> (
      match try Some (ClassTypeMap.find id s.class_type) with _ -> None with
      | Some x -> x
      | None -> `Local id )
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_class_type_path s p)
  | `ClassType (p, n) -> `ClassType (resolved_module_path s p, n)
  | `Class (p, n) -> `Class (resolved_module_path s p, n)

and class_type_path : t -> Cpath.class_type -> Cpath.class_type =
 fun s p ->
  match p with
  | `Resolved r -> `Resolved (resolved_class_type_path s r)
  | `Substituted p -> `Substituted (class_type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)

let rec module_reference : t -> Cref.module_ -> Cref.module_ =
 fun t r ->
  match r with
  | `Resolved r -> `Resolved (resolved_module_reference t r)
  | `Root (_, _) -> r
  | `Dot (parent, s) -> `Dot (label_parent_reference t parent, s)
  | `Module (parent, s) -> `Module (signature_reference t parent, s)

and resolved_module_reference :
    t -> Cref.Resolved.module_ -> Cref.Resolved.module_ =
 fun t r ->
  match r with
  | `Local id -> (
      match try Some (ModuleMap.find id t.ref_module) with _ -> None with
      | Some x -> x
      | None -> r )
  | `Identifier _ -> r
  | `SubstAlias (p1, p2) ->
      `SubstAlias (resolved_module_path t p1, resolved_module_reference t p2)
  | `Module (p, n) -> `Module (resolved_signature_reference t p, n)
  | `Canonical (m, m2) ->
      `Canonical (resolved_module_reference t m, module_reference t m2)

and signature_reference : t -> Cref.signature -> Cref.signature =
 fun t r ->
  match r with
  | (`Dot _ | `Module _) as r' -> (module_reference t r' :> Cref.signature)
  | `Root (_, _) -> r
  | `Resolved r -> `Resolved (resolved_signature_reference t r)
  | `ModuleType (parent, s) -> `ModuleType (signature_reference t parent, s)

and resolved_signature_reference :
    t -> Cref.Resolved.signature -> Cref.Resolved.signature =
 fun t r ->
  match r with
  | `Local (#Ident.module_)
  | #Cref.Resolved.module_no_id as s -> (resolved_module_reference t s :> Cref.Resolved.signature)
  | `Local (`LModuleType _ as id) -> (
      match try Some (ModuleTypeMap.find id t.ref_module_type) with _ -> None with
      | Some x -> (x :> Cref.Resolved.signature)
      | None -> r )
  | `Identifier _ -> r
  | `ModuleType (p, n) -> `ModuleType (resolved_signature_reference t p, n)

and label_parent_reference : t -> Cref.label_parent -> Cref.label_parent =
 fun t r ->
  match r with
  | (`Dot _ | `Module _) as r' -> (module_reference t r' :> Cref.label_parent)
  | `Root (_, _) -> r
  | `Resolved r -> `Resolved (resolved_label_parent_reference t r)
  | `ModuleType (parent, s) -> `ModuleType (signature_reference t parent, s)
  | `Class (p, n) -> `Class (signature_reference t p, n)
  | `ClassType (p, n) -> `ClassType (signature_reference t p, n)
  | `Type (p, n) -> `Type (signature_reference t p, n)

and resolved_class_signature_reference :
    t -> Cref.Resolved.class_signature -> Cref.Resolved.class_signature =
    fun t r ->
    match r with
    | `Local (#Ident.class_signature as c) -> (
      match try Some (ClassTypeMap.find c t.ref_class_type) with _ -> None with
      | Some x -> (x :> Cref.Resolved.class_signature)
      | None -> r )
    
    | `Class (p, n) -> `Class (resolved_signature_reference t p, n)
    | `ClassType (p, n) -> `ClassType (resolved_signature_reference t p, n)
    | `Identifier _ -> r

and resolved_type_reference :
  t -> Cref.Resolved.type_ -> Cref.Resolved.type_ =
  fun t r ->
  match r with
  | `Local (#Ident.class_signature)
  | `Class _
  | `ClassType _ as c -> (resolved_class_signature_reference t c :> Cref.Resolved.type_)
  | `Type (p, n) -> `Type (resolved_signature_reference t p, n)
  | `Identifier _ -> r
  | `Local (#Ident.type_ as ty) ->
      (
      match try Some (TypeMap.find ty t.ref_type) with _ -> None with
      | Some x -> (x :> Cref.Resolved.type_)
      | None -> r )

and resolved_parent_reference :
    t -> Cref.Resolved.parent -> Cref.Resolved.parent =
 fun t r ->
  match r with
  | `Local (#Ident.signature)
  | #Cref.Resolved.signature_no_id as l -> (resolved_signature_reference t l :> Cref.Resolved.parent)
  | `Identifier _ -> r
  | `Local (#Ident.path_type)
  | `Type (_, _) as ty -> (resolved_type_reference t ty :> Cref.Resolved.parent)
  | #Cref.Resolved.class_signature_no_id as c -> (resolved_class_signature_reference t c :> Cref.Resolved.parent)

and resolved_label_parent_reference :
  t -> Cref.Resolved.label_parent -> Cref.Resolved.label_parent =
    fun t r ->
    match r with
  | #Cref.Resolved.parent_no_id
  | `Local (#Ident.parent) as l -> (resolved_parent_reference t l :> Cref.Resolved.label_parent)
  | `Identifier _ -> r
  | `Local (`LPage _) -> r

and resolved_reference :
  t -> Cref.Resolved.any -> Cref.Resolved.any =
    fun t r ->
    match r with
    | `Local (#Ident.parent)
    | #Cref.Resolved.parent_no_id as p -> (resolved_parent_reference t p :> Cref.Resolved.any)
    | `Constructor _ -> r (* FIXME *)
    | `Field _ -> r (* FIXME *)
    | `Extension (p, name) -> `Extension (resolved_signature_reference t p, name)
    | `Exception (p, name) -> `Exception (resolved_signature_reference t p, name)
    | `Value (p, name) -> `Value (resolved_signature_reference t p, name)
    | `Method (p, name) -> `Method (resolved_class_signature_reference t p, name)
    | `InstanceVariable (p, name) -> `InstanceVariable (resolved_class_signature_reference t p, name)
    | `Label (p, name) -> `Label (resolved_label_parent_reference t p, name)
    | `Identifier _ -> r
    | `Local id -> (
        match (try Some (IdentMap.find id t.id_any) with | _ -> None) with
        | Some x -> `Local x
        | None -> r)

and reference : t -> Cref.any -> Cref.any =
  fun t r -> 
  match r with
  | `Resolved r -> `Resolved (resolved_reference t r)
  | `Dot _
  | `Module _
  | `ModuleType _
  | `Class _
  | `ClassType _
  | `Type _ as r -> (label_parent_reference t r :> Cref.any)
  | `Root _ -> r
  | `Constructor _ -> r (* FIXME *)
  | `Field _ -> r (* FIXME *)
  | `Extension (p, n) -> `Extension (signature_reference t p, n)
  | `Exception (p, n) -> `Exception (signature_reference t p, n)
  | `Value (p, n) -> `Value (signature_reference t p, n)
  | `Method _ -> r (* FIXME *)
  | `InstanceVariable _ -> r (* FIXME *)
  | `Label _ -> r
  
let option_ conv s x = match x with Some x -> Some (conv s x) | None -> None

let list conv s xs = List.map (conv s) xs

let rec type_ s t =
  let open Component.TypeDecl in
  let representation = option_ type_decl_representation s t.representation in
  let doc = docs s t.doc in
  { equation = type_decl_equation s t.equation; representation; doc }

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
  let doc = docs s t.doc in
  { t with args; res; doc }

and type_poly_var s v =
  let open Component.TypeExpr.Polymorphic_variant in
  let map_constr c =
    let open Constructor in
    {
      name = c.name;
      constant = c.constant;
      arguments = List.map (type_expr s) c.arguments;
      doc = docs s c.doc;
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
  let sub (x, y) = (x, type_expr s y) in
  {
    path = module_type_path s p.path;
    substitutions = List.map sub p.substitutions;
  }

and type_expr s t =
  let open Component.TypeExpr in
  try begin
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
  end with TypeReplacement y -> y

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
  let doc = docs s t.doc in
  { expr; expansion; doc }

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
  | ModuleEq (f, m) -> ModuleEq (f, module_decl s m)
  | ModuleSubst (f, p) -> ModuleSubst (f, module_path s p)
  | TypeEq (f, eq) -> TypeEq (f, type_decl_equation s eq)
  | TypeSubst (f, eq) -> TypeSubst (f, type_decl_equation s eq)

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
  let doc = docs s t.doc in
  { t with type_; expansion; canonical; doc }

and module_substitution s m =
  let open Component.ModuleSubstitution in
  let manifest = module_path s m.manifest in
  let doc = docs s m.doc in
  { manifest; doc }

and type_decl_field s f =
  let open Component.TypeDecl.Field in
  let doc = docs s f.doc in
  { f with type_ = type_expr s f.type_; doc }

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
  let doc = docs s e.doc in
  { args; res; doc }

and extension_constructor s c =
  let open Component.Extension.Constructor in
  let doc = docs s c.doc in
  {
    c with
    args = type_decl_constructor_arg s c.args;
    res = option_ type_expr s c.res;
    doc
  }

and extension s e =
  let open Component.Extension in
  let doc = docs s e.doc in
  {
    e with
    type_path = type_path s e.type_path;
    constructors = List.map (extension_constructor s) e.constructors;
    doc
  }

and external_ s e =
  let open Component.External in
  let doc = docs s e.doc in
  { e with type_ = type_expr s e.type_; doc }

and include_ s i =
  let open Component.Include in
  {
    i with
    decl = module_decl s i.decl;
    expansion_ = apply_sig_map s i.expansion_.items i.expansion_;
  }

and value s v =
  let open Component.Value in
  let doc = docs s v.doc in
  { type_ = type_expr s v.type_; doc }

and class_ s c =
  let open Component.Class in
  let expansion = option_ class_signature s c.expansion in
  let doc = docs s c.doc in
  { c with type_ = class_decl s c.type_; expansion; doc }

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
  let doc = docs s c.doc in
  let expansion = option_ class_signature s c.expansion in
  { c with expr = class_type_expr s c.expr; expansion; doc }

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
  let doc = docs s m.doc in
  { m with type_ = type_expr s m.type_; doc }

and instance_variable s i =
  let open Component.InstanceVariable in
  let doc = docs s i.doc in
  { i with type_ = type_expr s i.type_; doc }

and location : t -> (t -> 'a -> 'a) -> 'a Odoc_model.Location_.with_location -> 'a Odoc_model.Location_.with_location =
  fun s f l ->
    { l with Odoc_model.Location_.value = f s l.Odoc_model.Location_.value }

and inline_element s i =
  match i with
  | #Odoc_model.Comment.leaf_inline_element as n -> n
  | `Styled (e,els) -> `Styled (e, List.map (location s inline_element) els)
  | `Reference (r, content) -> `Reference (reference s r, content)
  | `Link (x, y) -> `Link (x, y)

and nestable_block_element : t -> Component.CComment.nestable_block_element -> Component.CComment.nestable_block_element = fun s n ->
  match n with
  | `Paragraph ps -> `Paragraph (list inline_element s ps)
  | `Code_block _
  | `Verbatim _ -> n
  | `List (x, yss) -> `List (x, list (list nestable_block_element) s yss)
  | `Modules ms -> `Modules (list module_reference s ms)

and tag : t -> Component.CComment.tag -> Component.CComment.tag = fun s t ->
  match t with
  | `Author _
  | `Inline
  | `Open
  | `Closed
  | `Since _
  | `Version _ -> t
  | `Deprecated ns -> `Deprecated (list nestable_block_element s ns)
  | `Param (str, ns) -> `Param (str, list nestable_block_element s ns)
  | `Raise (str, ns) -> `Raise (str, list nestable_block_element s ns)
  | `Return ns -> `Return (list nestable_block_element s ns)
  | `See (x, str, ns) -> `See (x, str, list nestable_block_element s ns)
  | `Before (x, ns) -> `Before (x, list nestable_block_element s ns)
  | `Canonical (c1, c2) -> `Canonical (module_path s c1, module_reference s c2)

and block_element : t -> Component.CComment.block_element -> Component.CComment.block_element = fun s t ->
  match t with
  | #Component.CComment.nestable_block_element as x -> (nestable_block_element s x :> Component.CComment.block_element)
  | `Heading (lvl, id, l) ->
      let id' =
        match try Some (IdentMap.find (id :> Ident.any) s.id_any) with _ -> None with 
        | Some (`LLabel _ as x) -> x
        | _ -> id
      in `Heading (lvl, id', l)
  | `Tag t -> `Tag (tag s t)

and docs s = list block_element s

and docs_or_stop s = function
    | `Docs d -> `Docs (docs s d)
    | `Stop -> `Stop

and rename_bound_idents s sg =
  let open Component.Signature in
  function
  | [] -> (s, sg)
  | Module (id, r, m) :: rest ->
      let id' = Ident.Rename.module_ id in
      rename_bound_idents
        (add_module id (`Local id') s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (Module (id', r, m) :: sg)
        rest
  | ModuleSubstitution (id, m) :: rest ->
      let id' = Ident.Rename.module_ id in
      rename_bound_idents
        (add_module id (`Local id') s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (ModuleSubstitution (id', m) :: sg)
        rest
  | ModuleType (id, mt) :: rest ->
      let id' = Ident.Rename.module_type id in
      rename_bound_idents
        (add_module_type id (`Local id') s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (ModuleType (id', mt) :: sg)
        rest
  | Type (id, r, t) :: rest ->
      let id' = Ident.Rename.type_ id in
      rename_bound_idents
        (add_type id (`Local (id' :> Ident.path_type)) s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (Type (id', r, t) :: sg)
        rest
  | TypeSubstitution (id, t) :: rest ->
      let id' = Ident.Rename.type_ id in
      rename_bound_idents
        (add_type id (`Local (id' :> Ident.path_type)) s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (TypeSubstitution (id', t) :: sg)
        rest
  | Exception (id, e) :: rest ->
      let id' = Ident.Rename.exception_ id in
      rename_bound_idents
        (add_id_map (id :> Ident.any) (id' :> Ident.any) s)
        (Exception (id', e) :: sg)
        rest
  | TypExt e :: rest -> rename_bound_idents s (TypExt e :: sg) rest
  | Value (id, v) :: rest ->
      let id' = Ident.Rename.value id in
      rename_bound_idents
        (add_id_map (id :> Ident.any) (id' :> Ident.any) s)
        (Value (id', v) :: sg)
        rest
  | External (id, e) :: rest ->
      let id' = Ident.Rename.value id in
      rename_bound_idents (add_id_map (id :> Ident.any) (id' :> Ident.any) s) (External (id', e) :: sg) rest
  | Class (id, r, c) :: rest ->
      let id' = Ident.Rename.class_ id in
      rename_bound_idents
        (add_class id (`Local (id' :> Ident.path_class_type)) s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
        (Class (id', r, c) :: sg)
        rest
  | ClassType (id, r, c) :: rest ->
      let id' = Ident.Rename.class_type id in
      rename_bound_idents
        (add_class_type id (`Local (id' :> Ident.path_class_type)) s |> add_id_map (id :> Ident.any) (id' :> Ident.any))
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
  apply_sig_map s items sg

and apply_sig_map s items sg =
  let open Component.Signature in
  let items =
    List.rev_map
      (function
        | Module (id, r, m) ->
            Module
              ( id,
                r,
                Component.Delayed.put (fun () ->
                    module_ s (Component.Delayed.get m)) )
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution (id, module_substitution s m)
        | ModuleType (id, mt) ->
            ModuleType 
              ( id, 
                Component.Delayed.put (fun () ->
                    module_type s (Component.Delayed.get mt)) )
        | Type (id, r, t) -> Type (id, r, type_ s t)
        | TypeSubstitution (id, t) -> TypeSubstitution (id, type_ s t)
        | Exception (id, e) -> Exception (id, exception_ s e)
        | TypExt e -> TypExt (extension s e)
        | Value (id, v) -> Value (id, value s v)
        | External (id, e) -> External (id, external_ s e)
        | Class (id, r, c) -> Class (id, r, class_ s c)
        | ClassType (id, r, c) -> ClassType (id, r, class_type s c)
        | Include i -> Include (include_ s i)
        | Comment c -> Comment (docs_or_stop s c))
      items
  in
  { items; removed = removed_items s sg.removed }
