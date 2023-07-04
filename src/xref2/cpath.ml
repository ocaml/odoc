open Odoc_model.Paths
open Odoc_model.Names

module rec Resolved : sig
  type parent =
    [ `Module of module_ | `ModuleType of module_type | `FragmentRoot ]

  and module_ =
    [ `Local of Ident.path_module
    | `Gpath of Path.Resolved.Module.t
    | `Substituted of module_
    | `Subst of module_type * module_
    | `Hidden of module_
    | `Module of parent * ModuleName.t
    | `Canonical of module_ * Path.Module.t
    | `Apply of module_ * module_
    | `Alias of module_ * Cpath.module_ * module_ option
    | `OpaqueModule of module_ ]

  and module_type =
    [ `Local of Ident.module_type
    | `Substituted of module_type
    | `Gpath of Path.Resolved.ModuleType.t
    | `ModuleType of parent * ModuleTypeName.t
    | `SubstT of module_type * module_type
    | `AliasModuleType of module_type * module_type
    | `CanonicalModuleType of module_type * Path.ModuleType.t
    | `OpaqueModuleType of module_type ]

  and type_ =
    [ `Local of Ident.path_type
    | `Gpath of Path.Resolved.Type.t
    | `Substituted of type_
    | `CanonicalType of type_ * Path.Type.t
    | `Type of parent * TypeName.t
    | `Class of parent * ClassName.t
    | `ClassType of parent * ClassTypeName.t ]

  and value = [ `Value of parent * ValueName.t ]

  and datatype =
    [ `Local of Ident.path_datatype
    | `Gpath of Path.Resolved.DataType.t
    | `Substituted of datatype
    | `CanonicalDataType of datatype * Path.DataType.t
    | `Type of parent * TypeName.t ]

  and constructor = [ `Constructor of datatype * ConstructorName.t ]

  and class_type =
    [ `Local of Ident.path_class_type
    | `Substituted of class_type
    | `Gpath of Path.Resolved.ClassType.t
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

  and value =
    [ `Resolved of Resolved.value
    | `Dot of module_ * string
    | `Value of Resolved.parent * ValueName.t ]

  and datatype =
    [ `Resolved of Resolved.datatype
    | `Substituted of datatype
    | `Local of Ident.path_datatype * bool
    | `Identifier of Odoc_model.Paths.Identifier.Path.DataType.t * bool
    | `Dot of module_ * string
    | `Type of Resolved.parent * TypeName.t ]

  and constructor =
    [ `Resolved of Resolved.constructor
    | `Dot of datatype * string
    | `Constructor of Resolved.datatype * ConstructorName.t ]

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

let rec is_resolved_module_substituted : Resolved.module_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Gpath _ -> false
  | `Subst (_a, _) -> false (* is_resolved_module_type_substituted a*)
  | `Hidden a | `Apply (a, _) | `Alias (a, _, _) | `Canonical (a, _) ->
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
  | `Gpath _ -> false
  | `ModuleType (a, _) -> is_resolved_parent_substituted a
  | `SubstT _ -> false
  | `AliasModuleType (m1, _) -> is_resolved_module_type_substituted m1
  | `CanonicalModuleType (m, _) | `OpaqueModuleType m ->
      is_resolved_module_type_substituted m

and is_resolved_type_substituted : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Gpath _ -> false
  | `CanonicalType (t, _) -> is_resolved_type_substituted t
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Substituted _ -> true
  | `Gpath _ -> false
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
    | `Gpath p ->
        Odoc_model.Paths.Path.Resolved.Module.is_hidden ~weak_canonical_test p
    | `Hidden _ -> true
    | `Canonical (_, `Resolved _) -> false
    | `Canonical (p, _) -> (not weak_canonical_test) && inner p
    | `Substituted p -> inner p
    | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test p
    | `Subst (p1, p2) -> is_resolved_module_type_hidden p1 || inner p2
    | `Alias (p1, `Resolved p2, _) -> inner p1 && inner p2
    | `Alias (p1, _p2, _) -> inner p1
    | `Apply (p1, p2) -> inner p1 || inner p2
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
  | `Identifier ({ iv = `ModuleType (_, t); _ }, b) ->
      b || ModuleTypeName.is_internal t
  | `Local (_, b) -> b
  | `Substituted p -> is_module_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_type_hidden : Resolved.module_type -> bool = function
  | `Local _ -> false
  | `Gpath p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
  | `Substituted p -> is_resolved_module_type_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p
  | `SubstT (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2
  | `AliasModuleType (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2
  | `CanonicalModuleType (_, `Resolved _) -> false
  | `CanonicalModuleType (p, _) -> is_resolved_module_type_hidden p
  | `OpaqueModuleType m -> is_resolved_module_type_substituted m

and is_type_hidden : type_ -> bool = function
  | `Resolved r -> is_resolved_type_hidden r
  | `Identifier ({ iv = `Type (_, t); _ }, b) -> b || TypeName.is_internal t
  | `Identifier ({ iv = `ClassType (_, t); _ }, b) ->
      b || ClassTypeName.is_internal t
  | `Identifier ({ iv = `Class (_, t); _ }, b) -> b || ClassName.is_internal t
  | `Identifier ({ iv = `CoreType _; _ }, b) -> b
  | `Local (_, b) -> b
  | `Substituted p -> is_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_type_hidden : Resolved.type_ -> bool = function
  | `Local _ -> false
  | `Gpath p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
  | `Substituted p -> is_resolved_type_hidden p
  | `CanonicalType (_, `Resolved _) -> false
  | `CanonicalType (p, _) -> is_resolved_type_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_class_type_hidden : Resolved.class_type -> bool = function
  | `Local _ -> false
  | `Gpath p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
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
  | `Identifier x -> `Gpath (`Identifier x)
  | `Alias (_m1, _m2) -> failwith "gah"
  | `Hidden s -> `Hidden (resolved_module_of_resolved_module_reference s)

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> Resolved.module_ = function
  | `Identifier ({ iv = #Identifier.Module.t_pv; _ } as i) ->
      `Gpath (`Identifier i)
  | (`Alias _ | `Module _ | `Hidden _) as r' ->
      resolved_module_of_resolved_module_reference r'
  | `ModuleType (_, n) ->
      failwith ("Not a module reference: " ^ ModuleTypeName.to_string n)
  | `AliasModuleType _ -> failwith "Not a module reference: aliasmoduletype"
  | `Identifier _ -> failwith "Not a module reference : identifier"

and module_of_module_reference : Reference.Module.t -> module_ = function
  | `Resolved r -> `Resolved (resolved_module_of_resolved_module_reference r)
  | `Root (_, _) -> failwith "unhandled"
  | `Dot
      ( (( `Resolved (`Identifier { iv = #Identifier.Module.t_pv; _ })
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, name)
  | `Module
      ( (( `Resolved (`Identifier { iv = #Identifier.Module.t_pv; _ })
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, ModuleName.to_string name)
  | _ -> failwith "Not a module reference"

let rec unresolve_resolved_module_path : Resolved.module_ -> module_ = function
  | `Hidden (`Gpath (`Identifier x)) -> `Identifier (x, true)
  | `Gpath (`Identifier x) ->
      let hidden =
        match x.iv with
        | `Module (_, n) -> Odoc_model.Names.ModuleName.is_internal n
        | _ -> false
      in
      `Identifier (x, hidden)
  | `Gpath _ as x -> `Resolved x
  | `Hidden (`Local x) -> `Local (x, true)
  | `Local x -> `Local (x, false)
  | `Substituted x -> unresolve_resolved_module_path x
  | `Subst (_, x) -> unresolve_resolved_module_path x
  | `Hidden x -> unresolve_resolved_module_path x (* should assert false here *)
  | `Module (p, m) ->
      `Dot (unresolve_resolved_parent_path p, ModuleName.to_string m)
  | `Canonical (m, _) -> unresolve_resolved_module_path m
  | `Apply (m, a) ->
      `Apply (unresolve_resolved_module_path m, unresolve_resolved_module_path a)
  | `Alias (_, `Resolved m, _) -> unresolve_resolved_module_path m
  | `Alias (_, m, _) -> m
  | `OpaqueModule m -> unresolve_resolved_module_path m

and unresolve_module_path : module_ -> module_ = function
  | `Resolved x -> unresolve_resolved_module_path x
  | `Substituted x -> unresolve_module_path x
  | `Local (_, _) as x -> x
  | `Identifier _ as x -> x
  | `Root _ as x -> x
  | `Forward _ as x -> x
  | `Dot (p, x) -> `Dot (unresolve_module_path p, x)
  | `Module (p, x) ->
      `Dot (unresolve_resolved_parent_path p, ModuleName.to_string x)
  | `Apply (x, y) -> `Apply (unresolve_module_path x, unresolve_module_path y)

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  function
  | (`Local _ | `Gpath _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_module_type_path x
  | `ModuleType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ModuleTypeName.to_string n)
  | `SubstT (_, m) -> unresolve_resolved_module_type_path m
  | `AliasModuleType (_, m2) -> unresolve_resolved_module_type_path m2
  | `CanonicalModuleType (p, _) -> unresolve_resolved_module_type_path p
  | `OpaqueModuleType m -> unresolve_resolved_module_type_path m

and unresolve_resolved_parent_path : Resolved.parent -> module_ = function
  | `Module m -> unresolve_resolved_module_path m
  | `FragmentRoot | `ModuleType _ -> assert false

and unresolve_resolved_type_path : Resolved.type_ -> type_ = function
  | (`Gpath _ | `Local _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_type_path x
  | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
  | `Type (p, n) -> `Dot (unresolve_resolved_parent_path p, TypeName.to_string n)
  | `Class (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassName.to_string n)
  | `ClassType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_resolved_class_type_path : Resolved.class_type -> class_type =
  function
  | (`Local _ | `Gpath _) as p -> `Resolved p
  | `Substituted x -> unresolve_resolved_class_type_path x
  | `Class (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassName.to_string n)
  | `ClassType (p, n) ->
      `Dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_module_type_path : module_type -> module_type = function
  | `Resolved m -> unresolve_resolved_module_type_path m
  | y -> y

and unresolve_type_path : type_ -> type_ = function
  | `Resolved m -> unresolve_resolved_type_path m
  | y -> y

and unresolve_class_type_path : class_type -> class_type = function
  | `Resolved m -> unresolve_resolved_class_type_path m
  | y -> y
