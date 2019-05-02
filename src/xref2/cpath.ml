open Odoc_model.Paths
open Odoc_model.Names

type resolved_module =
  [ `Local of Ident.module_
  | `Identifier of Identifier.Module.t
  | `Substituted of resolved_module
  | `Subst of resolved_module_type * resolved_module
  | `SubstAlias of resolved_module * resolved_module
  | `Hidden of resolved_module
  | `Module of resolved_module * ModuleName.t
  | `Canonical of resolved_module * module_
  | `Apply of resolved_module * module_
  | `Alias of resolved_module * resolved_module ]

and module_ =
  [ `Resolved of resolved_module
  | `Substituted of module_
  | `Root of string
  | `Forward of string
  | `Dot of module_ * string
  | `Apply of module_ * module_ ]

and resolved_module_type =
  [ `Local of Ident.module_type
  | `Substituted of resolved_module_type
  | `Identifier of Identifier.ModuleType.t
  | `ModuleType of resolved_module * ModuleTypeName.t ]

and module_type =
  [ `Resolved of resolved_module_type
  | `Substituted of module_type
  | `Dot of module_ * string ]

and resolved_type =
  [ `Local of Ident.path_type
  | `Identifier of Odoc_model.Paths_types.Identifier.path_type
  | `Substituted of resolved_type
  | `Type of resolved_module * TypeName.t
  | `Class of resolved_module * ClassName.t
  | `ClassType of resolved_module * ClassTypeName.t ]

and type_ =
  [ `Resolved of resolved_type
  | `Substituted of type_
  | `Dot of module_ * string ]

and resolved_class_type =
  [ `Local of Ident.path_class_type
  | `Substituted of resolved_class_type
  | `Identifier of Odoc_model.Paths_types.Identifier.path_class_type
  | `Class of resolved_module * ClassName.t
  | `ClassType of resolved_module * ClassTypeName.t ]

and class_type =
  [ `Resolved of resolved_class_type
  | `Substituted of class_type
  | `Dot of module_ * string ]

type local_path_error =
  | ErrModule of module_
  | ErrModuleType of module_type
  | ErrType of type_

exception LocalPath of local_path_error

exception TypesNeedRefining

let rec resolved_module_path_of_cpath :
    resolved_module -> Path.Resolved.Module.t = function
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
  | `Module (p, m) -> `Module (resolved_module_path_of_cpath p, m)

and resolved_module_type_path_of_cpath :
    resolved_module_type -> Path.Resolved.ModuleType.t = function
  | `Local _ as y -> raise (LocalPath (ErrModuleType (`Resolved y)))
  | `Identifier (#Identifier.ModuleType.t as x) -> `Identifier x
  | `Substituted y -> resolved_module_type_path_of_cpath y
  | `ModuleType (p, m) -> `ModuleType (resolved_module_path_of_cpath p, m)

and resolved_type_path_of_cpath : resolved_type -> Path.Resolved.Type.t =
  function
  | `Identifier (#Odoc_model.Paths_types.Identifier.path_type as x) ->
      `Identifier x
  | `Local _ as y -> raise (LocalPath (ErrType (`Resolved y)))
  | `Substituted y -> resolved_type_path_of_cpath y
  | `Type (p, m) -> `Type (resolved_module_path_of_cpath p, m)
  | `Class (p, m) -> `Class (resolved_module_path_of_cpath p, m)
  | `ClassType (p, m) -> `ClassType (resolved_module_path_of_cpath p, m)

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

let rec is_resolved_module_substituted : resolved_module -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Subst (a, _) -> is_resolved_module_type_substituted a
  | `SubstAlias (a, _)
  | `Hidden a
  | `Canonical (a, _)
  | `Apply (a, _)
  | `Alias (a, _)
  | `Module (a, _) ->
      is_resolved_module_substituted a

and is_resolved_module_type_substituted : resolved_module_type -> bool =
  function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `ModuleType (a, _) -> is_resolved_module_substituted a

and is_resolved_type_substituted : resolved_type -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_module_substituted a

and is_resolved_class_type_substituted : resolved_class_type -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_module_substituted a

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

and is_resolved_module_hidden : resolved_module -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Hidden _ -> true
  | `Substituted p | `Canonical (p, _) | `Apply (p, _) | `Module (p, _) ->
      is_resolved_module_hidden p
  | `Subst (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_hidden p2
  | `SubstAlias (p1, p2) | `Alias (p1, p2) ->
      is_resolved_module_hidden p1 || is_resolved_module_hidden p2

and is_module_type_hidden : module_type -> bool = function
  | `Resolved r -> is_resolved_module_type_hidden r
  | `Substituted p -> is_module_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

and is_resolved_module_type_hidden : resolved_module_type -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_module_type_hidden p
  | `ModuleType (p, _) -> is_resolved_module_hidden p

and is_type_hidden : type_ -> bool = function
  | `Resolved r -> is_resolved_type_hidden r
  | `Substituted p -> is_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

and is_resolved_type_hidden : resolved_type -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_type_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_module_hidden p

and is_resolved_class_type_hidden : resolved_class_type -> bool = function
  | `Local _ -> false
  | `Identifier _ -> false
  | `Substituted p -> is_resolved_class_type_hidden p
  | `Class (p, _) | `ClassType (p, _) -> is_resolved_module_hidden p

and is_class_type_hidden : class_type -> bool = function
  | `Resolved r -> is_resolved_class_type_hidden r
  | `Substituted p -> is_class_type_hidden p
  | `Dot (p, _) -> is_module_hidden p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> resolved_module = function
  | `Module (parent, name) ->
      `Module (resolved_module_of_resolved_signature_reference parent, name)
  | `Identifier i -> `Identifier i
  | `SubstAlias (_m1, _m2) -> failwith "gah"
  | `Canonical (m1, m2) ->
      `Canonical
        ( resolved_module_of_resolved_module_reference m1,
          module_of_module_reference m2 )

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> resolved_module = function
  | `Identifier (#Identifier.Module.t as i) -> `Identifier i
  | (`SubstAlias _ | `Canonical _ | `Module _) as r' ->
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

