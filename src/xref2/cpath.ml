open Odoc_model.Paths
open Odoc_model.Names

module rec Resolved : sig
  type parent =
    [ `Module of module_ | `ModuleType of module_type | `FragmentRoot ]

  and module_ =
    [ `Local of Ident.path_module
    | `Identifier of Identifier.Path.Module.t
    | `Substituted of module_
    | `Subst of module_type * module_
    | `SubstAlias of module_ * module_
    | `Hidden of module_
    | `Module of parent * ModuleName.t
    | `Canonical of module_ * Cpath.module_
    | `Apply of module_ * module_
    | `Alias of module_ * module_
    | `OpaqueModule of module_ ]

  and module_type =
    [ `Local of Ident.module_type
    | `Substituted of module_type
    | `Identifier of Identifier.ModuleType.t
    | `ModuleType of parent * ModuleTypeName.t
    | `SubstT of module_type * module_type
    | `CanonicalModuleType of module_type * Cpath.module_type
    | `OpaqueModuleType of module_type ]

  and type_ =
    [ `Local of Ident.path_type
    | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t
    | `Substituted of type_
    | `CanonicalType of type_ * Cpath.type_
    | `Type of parent * TypeName.t
    | `Class of parent * ClassName.t
    | `ClassType of parent * ClassTypeName.t ]

  and class_type =
    [ `Local of Ident.path_class_type
    | `Substituted of class_type
    | `Identifier of Odoc_model.Paths.Identifier.Path.ClassType.t
    | `Class of parent * ClassName.t
    | `ClassType of parent * ClassTypeName.t ]
end =
  Resolved

and Cpath : sig
  type module_ =
    [ `Resolved of Resolved.module_
    | `Substituted of module_
    | `Local of Ident.path_module * bool
    | `Identifier of Identifier.Path.Module.t * bool
    | `Root of string
    | `Forward of string
    | `Dot of module_ * string
    | `Module of Resolved.parent * ModuleName.t (* Like dot, but typed *)
    | `Apply of module_ * module_ ]

  and module_type =
    [ `Resolved of Resolved.module_type
    | `Substituted of module_type
    | `Local of Ident.module_type * bool
    | `Identifier of Identifier.ModuleType.t * bool
    | `Dot of module_ * string
    | `ModuleType of Resolved.parent * ModuleTypeName.t ]

  and type_ =
    [ `Resolved of Resolved.type_
    | `Substituted of type_
    | `Local of Ident.path_type * bool
    | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t * bool
    | `Dot of module_ * string
    | `Type of Resolved.parent * TypeName.t
    | `Class of Resolved.parent * ClassName.t
    | `ClassType of Resolved.parent * ClassTypeName.t ]

  and class_type =
    [ `Resolved of Resolved.class_type
    | `Substituted of class_type
    | `Local of Ident.path_class_type * bool
    | `Identifier of Odoc_model.Paths.Identifier.Path.ClassType.t * bool
    | `Dot of module_ * string
    | `Class of Resolved.parent * ClassName.t
    | `ClassType of Resolved.parent * ClassTypeName.t ]
end =
  Cpath

include Cpath

let rec resolved_module_hash : Resolved.module_ -> int = function
  | `Local id -> Hashtbl.hash (0, Ident.hash (id :> Ident.any))
  | `Identifier id ->
      Hashtbl.hash
        ( 1,
          Odoc_model.Paths.Identifier.hash (id :> Odoc_model.Paths.Identifier.t)
        )
  | `Substituted s -> Hashtbl.hash (2, resolved_module_hash s)
  | `Subst (mt, m) ->
      Hashtbl.hash (3, resolved_module_type_hash mt, resolved_module_hash m)
  | `SubstAlias (m1, m2) ->
      Hashtbl.hash (4, resolved_module_hash m1, resolved_module_hash m2)
  | `Hidden h -> Hashtbl.hash (5, resolved_module_hash h)
  | `Module (m, n) -> Hashtbl.hash (6, resolved_parent_hash m, n)
  | `Canonical (m, m2) ->
      Hashtbl.hash (7, resolved_module_hash m, module_hash m2)
  | `Apply (m1, m2) ->
      Hashtbl.hash (8, resolved_module_hash m1, resolved_module_hash m2)
  | `Alias (m1, m2) ->
      Hashtbl.hash (9, resolved_module_hash m1, resolved_module_hash m2)
  | `OpaqueModule m -> Hashtbl.hash (10, resolved_module_hash m)

and module_hash : module_ -> int = function
  | `Resolved r -> Hashtbl.hash (10, resolved_module_hash r)
  | `Substituted s -> Hashtbl.hash (11, module_hash s)
  | `Root r -> Hashtbl.hash (12, r)
  | `Forward f -> Hashtbl.hash (13, f)
  | `Identifier (id, b) ->
      Hashtbl.hash
        ( 14,
          Odoc_model.Paths.Identifier.hash (id :> Odoc_model.Paths.Identifier.t),
          b )
  | `Local (id, b) -> Hashtbl.hash (15, Ident.hash (id :> Ident.any), b)
  | `Dot (m, s) -> Hashtbl.hash (16, module_hash m, s)
  | `Module (m, s) -> Hashtbl.hash (17, resolved_parent_hash m, s)
  | `Apply (m1, m2) -> Hashtbl.hash (18, module_hash m1, module_hash m2)

and resolved_module_type_hash : Resolved.module_type -> int = function
  | `Local id -> Hashtbl.hash (19, Ident.hash (id :> Ident.any))
  | `Substituted m -> Hashtbl.hash (20, resolved_module_type_hash m)
  | `Identifier id ->
      Hashtbl.hash (21, Odoc_model.Paths.Identifier.(hash (id :> t)))
  | `ModuleType (p, n) -> Hashtbl.hash (22, resolved_parent_hash p, n)
  | `SubstT (p1, p2) ->
      Hashtbl.hash
        (23, resolved_module_type_hash p1, resolved_module_type_hash p2)
  | `CanonicalModuleType (p1, p2) ->
      Hashtbl.hash (24, resolved_module_type_hash p1, module_type_hash p2)
  | `OpaqueModuleType m -> Hashtbl.hash (25, resolved_module_type_hash m)

and resolved_parent_hash : Resolved.parent -> int = function
  | `Module m -> Hashtbl.hash (26, resolved_module_hash m)
  | `ModuleType m -> Hashtbl.hash (27, resolved_module_type_hash m)
  | `FragmentRoot -> Hashtbl.hash 28

and module_type_hash : module_type -> int = function
  | `Resolved r -> Hashtbl.hash (29, resolved_module_type_hash r)
  | `Substituted m -> Hashtbl.hash (30, module_type_hash m)
  | `Local (id, b) -> Hashtbl.hash (31, Ident.hash (id :> Ident.any), b)
  | `Identifier (id, b) ->
      Hashtbl.hash (32, Odoc_model.Paths.Identifier.(hash (id :> t)), b)
  | `Dot (p, s) -> Hashtbl.hash (33, module_hash p, s)
  | `ModuleType (m, s) -> Hashtbl.hash (34, resolved_parent_hash m, s)

type local_path_error =
  | ErrModule of module_
  | ErrModuleType of module_type
  | ErrType of type_

exception LocalPath of local_path_error

let rec resolved_module_path_of_cpath :
    Resolved.module_ -> Path.Resolved.Module.t = function
  | `Local _ as y -> raise (LocalPath (ErrModule (`Resolved y)))
  | `Substituted y -> resolved_module_path_of_cpath y
  | `Identifier (#Identifier.Path.Module.t as x) -> `Identifier x
  | `Subst (a, b) ->
      `Subst
        (resolved_module_type_path_of_cpath a, resolved_module_path_of_cpath b)
  | `SubstAlias (a, b) ->
      `SubstAlias
        (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
  | `Hidden x -> `Hidden (resolved_module_path_of_cpath x)
  | `Canonical (a, b) ->
      `Canonical (resolved_module_path_of_cpath a, module_path_of_cpath b)
  | `Apply (a, b) ->
      `Apply (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
  | `Alias (a, b) ->
      `Alias (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
  | `Module (p, m) -> `Module (resolved_module_path_of_cpath_parent p, m)
  | `OpaqueModule m -> `OpaqueModule (resolved_module_path_of_cpath m)

and resolved_module_path_of_cpath_parent :
    Resolved.parent -> Path.Resolved.Module.t = function
  | `Module m -> resolved_module_path_of_cpath m
  | `ModuleType _ | `FragmentRoot -> failwith "Can't do it"

and resolved_module_type_path_of_cpath :
    Resolved.module_type -> Path.Resolved.ModuleType.t = function
  | `Local _ as y -> raise (LocalPath (ErrModuleType (`Resolved y)))
  | `Identifier (#Identifier.ModuleType.t as x) -> `Identifier x
  | `Substituted y -> resolved_module_type_path_of_cpath y
  | `ModuleType (p, m) -> `ModuleType (resolved_module_path_of_cpath_parent p, m)
  | `SubstT (p1, p2) ->
      `SubstT
        ( resolved_module_type_path_of_cpath p1,
          resolved_module_type_path_of_cpath p2 )
  | `CanonicalModuleType (p1, p2) ->
      `CanonicalModuleType
        (resolved_module_type_path_of_cpath p1, module_type_path_of_cpath p2)
  | `OpaqueModuleType m ->
      `OpaqueModuleType (resolved_module_type_path_of_cpath m)

and resolved_type_path_of_cpath : Resolved.type_ -> Path.Resolved.Type.t =
  function
  | `Identifier (#Odoc_model.Paths.Identifier.Path.Type.t as x) -> `Identifier x
  | `Local _ as y -> raise (LocalPath (ErrType (`Resolved y)))
  | `Substituted y -> resolved_type_path_of_cpath y
  | `CanonicalType (t1, t2) ->
      `CanonicalType (resolved_type_path_of_cpath t1, type_path_of_cpath t2)
  | `Type (p, m) -> `Type (resolved_module_path_of_cpath_parent p, m)
  | `Class (p, m) -> `Class (resolved_module_path_of_cpath_parent p, m)
  | `ClassType (p, m) -> `ClassType (resolved_module_path_of_cpath_parent p, m)

and resolved_class_type_path_of_cpath :
    Resolved.class_type -> Path.Resolved.ClassType.t = function
  | `Identifier (#Odoc_model.Paths.Identifier.Path.ClassType.t as x) ->
      `Identifier x
  | `Local ident ->
      raise
        (LocalPath (ErrType (`Resolved (`Local (ident :> Ident.path_type)))))
  | `Substituted y -> resolved_class_type_path_of_cpath y
  | `Class (p, m) -> `Class (resolved_module_path_of_cpath_parent p, m)
  | `ClassType (p, m) -> `ClassType (resolved_module_path_of_cpath_parent p, m)

and module_path_of_cpath : module_ -> Path.Module.t = function
  | `Resolved r -> `Resolved (resolved_module_path_of_cpath r)
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
  | `Module (_p, _n) -> failwith "Probably shouldn't happen"
  | `Identifier (x, b) -> `Identifier (x, b)
  | `Local _ as y -> raise (LocalPath (ErrModule y))
  | `Substituted p -> module_path_of_cpath p
  | `Root x -> `Root x
  | `Forward x -> `Forward x
  | `Apply (m1, m2) -> `Apply (module_path_of_cpath m1, module_path_of_cpath m2)

and module_type_path_of_cpath : module_type -> Path.ModuleType.t = function
  | `Resolved r -> `Resolved (resolved_module_type_path_of_cpath r)
  | `Identifier (x, b) -> `Identifier (x, b)
  | `Local _ as y -> raise (LocalPath (ErrModuleType y))
  | `Substituted r -> module_type_path_of_cpath r
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
  | `ModuleType (_p, _n) -> failwith "Probably shouldn't happen"

and type_path_of_cpath : type_ -> Path.Type.t = function
  | `Resolved r -> `Resolved (resolved_type_path_of_cpath r)
  | `Identifier (x, b) -> `Identifier (x, b)
  | `Local _ as y -> raise (LocalPath (ErrType y))
  | `Substituted r -> type_path_of_cpath r
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
  | `Type (_, _) | `Class (_, _) | `ClassType (_, _) ->
      failwith "Probably shouldn't happen"

and class_type_path_of_cpath : class_type -> Path.ClassType.t = function
  | `Resolved r -> `Resolved (resolved_class_type_path_of_cpath r)
  | `Identifier (x, b) -> `Identifier (x, b)
  | `Local (ident, b) ->
      raise (LocalPath (ErrType (`Local ((ident :> Ident.path_type), b))))
  | `Substituted r -> class_type_path_of_cpath r
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
  | `Class (_, _) | `ClassType (_, _) -> failwith "Probably shouldn't happen"

let rec is_resolved_module_substituted : Resolved.module_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Subst (_a, _) -> false (* is_resolved_module_type_substituted a*)
  | `SubstAlias (a, _)
  | `Hidden a
  | `Canonical (a, _)
  | `Apply (a, _)
  | `Alias (a, _) ->
      is_resolved_module_substituted a
  | `Module (a, _) -> is_resolved_parent_substituted a
  | `OpaqueModule a -> is_resolved_module_substituted a

and is_resolved_parent_substituted = function
  | `Module m -> is_resolved_module_substituted m
  | `ModuleType m -> is_resolved_module_type_substituted m
  | `FragmentRoot -> false

and is_resolved_module_type_substituted : Resolved.module_type -> bool =
  function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `ModuleType (a, _) -> is_resolved_parent_substituted a
  | `SubstT _ -> false
  | `CanonicalModuleType (m, _) | `OpaqueModuleType m ->
      is_resolved_module_type_substituted m

and is_resolved_type_substituted : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `CanonicalType (t, _) -> is_resolved_type_substituted t
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_substituted : module_ -> bool = function
  | `Resolved a -> is_resolved_module_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) | `Apply (a, _) -> is_module_substituted a
  | `Forward _ -> false
  | `Root _ -> false
  | `Module (a, _) -> is_resolved_parent_substituted a

let is_module_type_substituted : module_type -> bool = function
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `ModuleType (a, _) -> is_resolved_parent_substituted a

let is_type_substituted : type_ -> bool = function
  | `Resolved a -> is_resolved_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

let is_class_type_substituted : class_type -> bool = function
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_forward : module_ -> bool = function
  | `Forward _ -> true
  | `Resolved _ -> false
  | `Root _ -> false
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_forward p
  | `Module (_, _) -> false

let rec is_module_hidden : module_ -> bool = function
  | `Resolved r -> is_resolved_module_hidden ~weak_canonical_test:false r
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_hidden p
  | `Identifier (_, b) -> b
  | `Local (_, b) -> b
  | `Forward _ -> false
  | `Root _ -> false
  | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_hidden :
    weak_canonical_test:bool -> Resolved.module_ -> bool =
 fun ~weak_canonical_test ->
  let rec inner = function
    | `Local _ -> false
    | `Identifier (`Module (_, t)) when ModuleName.is_internal t -> true
    | `Identifier (`Module _) -> false
    | `Identifier _ -> false
    | `Hidden _ -> true
    | `Canonical (_, `Resolved _) -> false
    | `Canonical (p, _) -> weak_canonical_test || inner p
    | `Substituted p | `Apply (p, _) -> inner p
    | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test p
    | `Subst (p1, p2) -> is_resolved_module_type_hidden p1 || inner p2
    | `SubstAlias (p1, p2) | `Alias (p1, p2) -> inner p1 || inner p2
    | `OpaqueModule m -> inner m
  in
  inner

and is_resolved_parent_hidden :
    weak_canonical_test:bool -> Resolved.parent -> bool =
 fun ~weak_canonical_test -> function
  | `Module m -> is_resolved_module_hidden ~weak_canonical_test m
  | `ModuleType m -> is_resolved_module_type_hidden m
  | `FragmentRoot -> false

and is_module_type_hidden : module_type -> bool = function
  | `Resolved r -> is_resolved_module_type_hidden r
  | `Identifier (id, b) -> b || is_resolved_module_type_hidden (`Identifier id)
  | `Local (_, b) -> b
  | `Substituted p -> is_module_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_type_hidden : Resolved.module_type -> bool = function
  | `Local _ -> false
  | `Identifier (`ModuleType (_, t)) when ModuleTypeName.is_internal t -> true
  | `Identifier (`ModuleType _) -> false
  | `Substituted p -> is_resolved_module_type_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p
  | `SubstT (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2
  | `CanonicalModuleType (_, `Resolved _) -> false
  | `CanonicalModuleType (p, _) -> is_resolved_module_type_hidden p
  | `OpaqueModuleType m -> is_resolved_module_type_substituted m

and is_type_hidden : type_ -> bool = function
  | `Resolved r -> is_resolved_type_hidden r
  | `Identifier (id, b) ->
      b || is_resolved_type_hidden (`Identifier (id :> Identifier.Path.Type.t))
  | `Local (_, b) -> b
  | `Substituted p -> is_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_type_hidden : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Identifier (`Type (_, t)) when TypeName.is_internal t -> true
  | `Identifier (`ClassType (_, t)) when ClassTypeName.is_internal t -> true
  | `Identifier (`Class (_, t)) when ClassName.is_internal t -> true
  | `Identifier (`Type (_, _)) -> false
  | `Identifier (`ClassType (_, _)) -> false
  | `Identifier (`Class (_, _)) -> false
  | `Identifier (`CoreType _) -> false
  | `Substituted p -> is_resolved_type_hidden p
  | `CanonicalType (_, `Resolved _) -> false
  | `CanonicalType (p, _) -> is_resolved_type_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_class_type_hidden : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Identifier (`ClassType (_, t)) when ClassTypeName.is_internal t -> true
  | `Identifier (`Class (_, t)) when ClassName.is_internal t -> true
  | `Identifier (`ClassType _) -> false
  | `Identifier (`Class (_, _)) -> false
  | `Substituted p -> is_resolved_class_type_hidden p
  | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_class_type_hidden : class_type -> bool = function
  | `Resolved r -> is_resolved_class_type_hidden r
  | `Identifier (_, b) -> b
  | `Local (_, b) -> b
  | `Substituted p -> is_class_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> Resolved.module_ = function
  | `Module (parent, name) ->
      `Module
        (`Module (resolved_module_of_resolved_signature_reference parent), name)
  | `Identifier i -> `Identifier i
  | `SubstAlias (_m1, _m2) -> failwith "gah"
  | `Hidden s -> `Hidden (resolved_module_of_resolved_module_reference s)
  | `Canonical (m1, m2) ->
      `Canonical
        ( resolved_module_of_resolved_module_reference m1,
          module_of_module_reference m2 )

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> Resolved.module_ = function
  | `Identifier (#Identifier.Module.t as i) -> `Identifier i
  | (`SubstAlias _ | `Canonical _ | `Module _ | `Hidden _) as r' ->
      resolved_module_of_resolved_module_reference r'
  | `ModuleType (_, n) ->
      failwith ("Not a module reference: " ^ ModuleTypeName.to_string n)
  | `Identifier _ -> failwith "Not a module reference : identifier"

and module_of_module_reference : Reference.Module.t -> module_ = function
  | `Resolved r -> `Resolved (resolved_module_of_resolved_module_reference r)
  | `Root (_, _) -> failwith "unhandled"
  | `Dot
      ( ( ( `Resolved (`Identifier #Identifier.Module.t)
          | `Dot (_, _)
          | `Module (_, _) ) as parent ),
        name ) ->
      `Dot (module_of_module_reference parent, name)
  | `Module
      ( ( ( `Resolved (`Identifier #Identifier.Module.t)
          | `Dot (_, _)
          | `Module (_, _) ) as parent ),
        name ) ->
      `Dot (module_of_module_reference parent, ModuleName.to_string name)
  | _ -> failwith "Not a module reference"

let rec unresolve_resolved_module_path : Resolved.module_ -> module_ = function
  | `Hidden (`Identifier x) -> `Identifier (x, true)
  | `Identifier x -> `Identifier (x, false)
  | `Hidden (`Local x) -> `Local (x, true)
  | `Local x -> `Local (x, false)
  | `Substituted x -> unresolve_resolved_module_path x
  | `Subst (_, x) -> unresolve_resolved_module_path x
  | `SubstAlias (_, x) -> unresolve_resolved_module_path x
  | `Hidden x -> unresolve_resolved_module_path x (* should assert false here *)
  | `Module (p, m) ->
      `Dot (unresolve_resolved_parent_path p, ModuleName.to_string m)
  | `Canonical (m, _) -> unresolve_resolved_module_path m
  | `Apply (m, a) ->
      `Apply (unresolve_resolved_module_path m, unresolve_resolved_module_path a)
  | `Alias (_, m) -> unresolve_resolved_module_path m
  | `OpaqueModule m -> unresolve_resolved_module_path m

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  function
  | (`Local _ | `Identifier _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_module_type_path x
  | `ModuleType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ModuleTypeName.to_string n)
  | `SubstT (_, m) -> unresolve_resolved_module_type_path m
  | `CanonicalModuleType (p, _) -> unresolve_resolved_module_type_path p
  | `OpaqueModuleType m -> unresolve_resolved_module_type_path m

and unresolve_resolved_parent_path : Resolved.parent -> module_ = function
  | `Module m -> unresolve_resolved_module_path m
  | `FragmentRoot | `ModuleType _ -> assert false

and unresolve_resolved_type_path : Resolved.type_ -> type_ = function
  | (`Identifier _ | `Local _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_type_path x
  | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
  | `Type (p, n) -> `Dot (unresolve_resolved_parent_path p, TypeName.to_string n)
  | `Class (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassName.to_string n)
  | `ClassType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_resolved_class_type_path : Resolved.class_type -> class_type =
  function
  | (`Local _ | `Identifier _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_class_type_path x
  | `Class (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassName.to_string n)
  | `ClassType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_module_path : module_ -> module_ = function
  | `Resolved m -> unresolve_resolved_module_path m
  | y -> y

and unresolve_module_type_path : module_type -> module_type = function
  | `Resolved m -> unresolve_resolved_module_type_path m
  | y -> y

and unresolve_type_path : type_ -> type_ = function
  | `Resolved m -> unresolve_resolved_type_path m
  | y -> y

and unresolve_class_type_path : class_type -> class_type = function
  | `Resolved m -> unresolve_resolved_class_type_path m
  | y -> y
