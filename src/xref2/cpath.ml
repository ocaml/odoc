open Odoc_model.Paths
open Odoc_model.Names

module rec Resolved : sig
  type parent =
  [ `Module of module_
  | `ModuleType of module_type
  | `FragmentRoot ]

  and module_ =
  [ `Local of Ident.module_
  | `Identifier of Identifier.Module.t
  | `Substituted of module_
  | `Subst of module_type * module_
  | `SubstAlias of module_ * module_
  | `Hidden of module_
  | `Module of parent * ModuleName.t
  | `Canonical of module_ * Cpath.module_
  | `Apply of module_ * Cpath.module_
  | `Alias of module_ * module_ ]

  and module_type =
  [ `Local of Ident.module_type
  | `Substituted of module_type
  | `Identifier of Identifier.ModuleType.t
  | `ModuleType of parent * ModuleTypeName.t
  | `SubstT of module_type * module_type ]

  and type_ =
  [ `Local of Ident.path_type
  | `Identifier of Odoc_model.Paths_types.Identifier.path_type
  | `Substituted of type_
  | `Type of parent * TypeName.t
  | `Class of parent * ClassName.t
  | `ClassType of parent * ClassTypeName.t ]

  and class_type =
  [ `Local of Ident.path_class_type
  | `Substituted of class_type
  | `Identifier of Odoc_model.Paths_types.Identifier.path_class_type
  | `Class of parent * ClassName.t
  | `ClassType of parent * ClassTypeName.t ]

end = Resolved

and Cpath : sig 
  type module_ =
  [ `Resolved of Resolved.module_
  | `Substituted of module_
  | `Root of string
  | `Forward of string
  | `Dot of module_ * string
  | `Apply of module_ * module_ ]


and module_type =
  [ `Resolved of Resolved.module_type
  | `Substituted of module_type
  | `Dot of module_ * string ]


and type_ =
  [ `Resolved of Resolved.type_
  | `Substituted of type_
  | `Dot of module_ * string ]

and class_type =
[ `Resolved of Resolved.class_type
| `Substituted of class_type
| `Dot of module_ * string ]

end = Cpath

include Cpath

let rec resolved_module_hash : Resolved.module_ -> int =
  function
  | `Local id -> Hashtbl.hash (0, Ident.hash (id :> Ident.any))
  | `Identifier id -> Hashtbl.hash (1, Odoc_model.Paths.Identifier.hash (id :> Odoc_model.Paths.Identifier.t ))
  | `Substituted s -> Hashtbl.hash (2, resolved_module_hash s)
  | `Subst (mt, m) -> Hashtbl.hash (3, resolved_module_type_hash mt, resolved_module_hash m)
  | `SubstAlias (m1, m2) -> Hashtbl.hash (4, resolved_module_hash m1, resolved_module_hash m2)
  | `Hidden h -> Hashtbl.hash (5, resolved_module_hash h)
  | `Module (m, n) -> Hashtbl.hash (6, resolved_parent_hash m, n)
  | `Canonical (m, m2) -> Hashtbl.hash (7, resolved_module_hash m, module_hash m2)
  | `Apply (m1, m2) -> Hashtbl.hash (8, resolved_module_hash m1, module_hash m2)
  | `Alias (m1, m2) -> Hashtbl.hash (9, resolved_module_hash m1, resolved_module_hash m2)

and module_hash : module_ -> int =
  function
  | `Resolved r -> Hashtbl.hash (10, resolved_module_hash r)
  | `Substituted s -> Hashtbl.hash (11, module_hash s)
  | `Root r -> Hashtbl.hash (12, r)
  | `Forward f -> Hashtbl.hash (13, f)
  | `Dot (m, s) -> Hashtbl.hash (14, module_hash m, s)
  | `Apply (m1, m2) -> Hashtbl.hash (15, module_hash m1, module_hash m2)

and resolved_module_type_hash : Resolved.module_type -> int =
  function
  | `Local id -> Hashtbl.hash (16, Ident.hash (id :> Ident.any))
  | `Substituted m -> Hashtbl.hash (17, resolved_module_type_hash m)
  | `Identifier id -> Hashtbl.hash (18, Odoc_model.Paths.Identifier.(hash (id :> t)))
  | `ModuleType (p, n) -> Hashtbl.hash (19, resolved_parent_hash p, n)
  | `SubstT (p1, p2) -> Hashtbl.hash (1023, resolved_module_type_hash p1, resolved_module_type_hash p2)

and resolved_parent_hash : Resolved.parent -> int =
  function
  | `Module m -> Hashtbl.hash (20,resolved_module_hash m)
  | `ModuleType m -> Hashtbl.hash (21, resolved_module_type_hash m)
  | `FragmentRoot -> Hashtbl.hash (22)


type local_path_error =
  | ErrModule of module_
  | ErrModuleType of module_type
  | ErrType of type_

exception LocalPath of local_path_error

exception TypesNeedRefining

let rec resolved_module_path_of_cpath :
    Resolved.module_ -> Path.Resolved.Module.t = function
  | `Local _ as y -> raise (LocalPath (ErrModule (`Resolved y)))
  | `Substituted y -> resolved_module_path_of_cpath y
  | `Identifier (#Identifier.Module.t as x) -> `Identifier x
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
      `Apply (resolved_module_path_of_cpath a, module_path_of_cpath b)
  | `Alias (a, b) ->
      `Alias (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
  | `Module (p, m) -> `Module (resolved_module_path_of_cpath_parent p, m)

and resolved_module_path_of_cpath_parent : Resolved.parent -> Path.Resolved.Module.t = function
| `Module m -> resolved_module_path_of_cpath m
| `ModuleType _ 
| `FragmentRoot -> failwith "Can't do it"

and resolved_module_type_path_of_cpath :
    Resolved.module_type -> Path.Resolved.ModuleType.t = function
  | `Local _ as y -> raise (LocalPath (ErrModuleType (`Resolved y)))
  | `Identifier (#Identifier.ModuleType.t as x) -> `Identifier x
  | `Substituted y -> resolved_module_type_path_of_cpath y
  | `ModuleType (p, m) -> `ModuleType (resolved_module_path_of_cpath_parent p, m)
  | `SubstT (p1, p2) -> `SubstT (resolved_module_type_path_of_cpath p1, resolved_module_type_path_of_cpath p2)

and resolved_type_path_of_cpath : Resolved.type_ -> Path.Resolved.Type.t =
  function
  | `Identifier (#Odoc_model.Paths_types.Identifier.path_type as x) ->
      `Identifier x
  | `Local _ as y -> raise (LocalPath (ErrType (`Resolved y)))
  | `Substituted y -> resolved_type_path_of_cpath y
  | `Type (p, m) -> `Type (resolved_module_path_of_cpath_parent p, m)
  | `Class (p, m) -> `Class (resolved_module_path_of_cpath_parent p, m)
  | `ClassType (p, m) -> `ClassType (resolved_module_path_of_cpath_parent p, m)

and module_path_of_cpath : module_ -> Path.Module.t = function
  | `Resolved r -> `Resolved (resolved_module_path_of_cpath r)
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
  | `Substituted p -> module_path_of_cpath p
  | `Root x -> `Root x
  | `Forward x -> `Forward x
  | `Apply (m1, m2) -> `Apply (module_path_of_cpath m1, module_path_of_cpath m2)

and module_type_path_of_cpath : module_type -> Path.ModuleType.t = function
  | `Resolved r -> `Resolved (resolved_module_type_path_of_cpath r)
  | `Substituted r -> module_type_path_of_cpath r
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)

and type_path_of_cpath : type_ -> Path.Type.t = function
  | `Resolved r -> `Resolved (resolved_type_path_of_cpath r)
  | `Substituted r -> type_path_of_cpath r
  | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)

let rec is_resolved_module_substituted : Resolved.module_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Subst (a, _) -> is_resolved_module_type_substituted a
  | `SubstAlias (a, _)
  | `Hidden a
  | `Canonical (a, _)
  | `Apply (a, _)
  | `Alias (a, _) ->
      is_resolved_module_substituted a
  | `Module (a, _) ->
      is_resolved_parent_substituted a

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
  | `SubstT _ -> true

and is_resolved_type_substituted : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_substituted : module_ -> bool = function
  | `Resolved a -> is_resolved_module_substituted a
  | `Substituted _ -> true
  | `Dot (a, _) | `Apply (a, _) -> is_module_substituted a
  | `Forward _ -> false
  | `Root _ -> false

let is_module_type_substituted : module_type -> bool = function
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a

let is_type_substituted : type_ -> bool = function
  | `Resolved a -> is_resolved_type_substituted a
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a

let is_class_type_substituted : class_type -> bool = function
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a

let rec is_module_forward : module_ -> bool = function
  | `Forward _ -> true
  | `Resolved _ -> false
  | `Root _ -> false
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_forward p

let rec is_module_hidden : module_ -> bool = function
  | `Resolved r -> is_resolved_module_hidden r
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_hidden p
  | `Forward _ -> false
  | `Root _ -> false

and is_resolved_module_hidden : Resolved.module_ -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Hidden _ -> true
  | `Canonical (_, `Resolved _) -> false
  | `Substituted p | `Canonical (p, _) | `Apply (p, _)  ->
      is_resolved_module_hidden p
  | `Module (p, _) -> is_resolved_parent_hidden p
  | `Subst (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_hidden p2
  | `SubstAlias (p1, p2) | `Alias (p1, p2) ->
      is_resolved_module_hidden p1 || is_resolved_module_hidden p2

and is_resolved_parent_hidden : Resolved.parent -> bool = function
  | `Module m -> is_resolved_module_hidden m
  | `ModuleType m -> is_resolved_module_type_hidden m
  | `FragmentRoot -> false

  and is_module_type_hidden : module_type -> bool = function
  | `Resolved r -> is_resolved_module_type_hidden r
  | `Substituted p -> is_module_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

and is_resolved_module_type_hidden : Resolved.module_type -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_module_type_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden p
  | `SubstT (p1, p2) -> is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2

and is_type_hidden : type_ -> bool = function
  | `Resolved r -> is_resolved_type_hidden r
  | `Substituted p -> is_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

and is_resolved_type_hidden : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_type_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden p

and is_resolved_class_type_hidden : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_class_type_hidden p
  | `Class (p, _) | `ClassType (p, _) -> is_resolved_parent_hidden p

and is_class_type_hidden : class_type -> bool = function
  | `Resolved r -> is_resolved_class_type_hidden r
  | `Substituted p -> is_class_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> Resolved.module_ = function
  | `Module (parent, name) ->
      `Module (`Module (resolved_module_of_resolved_signature_reference parent), name)
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
  | `ModuleType (_, n) -> failwith ("Not a module reference: " ^ (ModuleTypeName.to_string n))
  | `Identifier _ -> failwith ("Not a module reference : identifier")

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
