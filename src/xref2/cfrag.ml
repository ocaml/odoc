(*open Odoc_model.Paths*)
open Odoc_model.Names

type root =
  [ `ModuleType of Cpath.Resolved.module_type
  | `Module of Cpath.Resolved.module_ ]

type resolved_signature =
  [ `Root of root
  | `Subst of Cpath.Resolved.module_type * resolved_module
  | `Alias of Cpath.Resolved.module_ * resolved_module
  | `Module of resolved_signature * ModuleName.t
  | `OpaqueModule of resolved_module ]

and resolved_module =
  [ `Subst of Cpath.Resolved.module_type * resolved_module
  | `Alias of Cpath.Resolved.module_ * resolved_module
  | `Module of resolved_signature * ModuleName.t
  | `OpaqueModule of resolved_module ]

and resolved_module_type =
  [ `ModuleType of resolved_signature * ModuleTypeName.t ]

and resolved_type =
  [ `Type of resolved_signature * TypeName.t
  | `Class of resolved_signature * ClassName.t
  | `ClassType of resolved_signature * ClassTypeName.t ]

(* and signature = [ `Resolved of resolved_signature ] *)

type signature =
  [ `Resolved of resolved_signature | `Dot of signature * string | `Root ]

and module_ = [ `Resolved of resolved_module | `Dot of signature * string ]

and module_type =
  [ `Resolved of resolved_module_type | `Dot of signature * string ]

and type_ = [ `Resolved of resolved_type | `Dot of signature * string ]

type resolved_base_name =
  | RBase of root
  | RBranch of ModuleName.t * resolved_signature

type base_name = Base of root option | Branch of ModuleName.t * signature

let rec resolved_signature_split_parent :
    resolved_signature -> resolved_base_name = function
  | `Root i -> RBase i
  | `Subst (_, p) -> resolved_signature_split_parent (p :> resolved_signature)
  | `Alias (_, p) -> resolved_signature_split_parent (p :> resolved_signature)
  | `OpaqueModule m -> resolved_signature_split_parent (m :> resolved_signature)
  | `Module (p, name) -> (
      match resolved_signature_split_parent p with
      | RBase i -> RBranch (name, `Module (`Root i, name))
      | RBranch (base, m) -> RBranch (base, `Module (m, name)))

(* Note that this returns an unresolved fragment by design *)
let rec signature_split_parent : signature -> base_name = function
  | `Root -> Base None
  | `Resolved r -> (
      match resolved_signature_split_parent r with
      | RBase _ -> Base None
      | RBranch (base, m) -> Branch (base, `Resolved m))
  | `Dot (m, name) -> (
      match signature_split_parent m with
      | Base _ -> Branch (ModuleName.make_std name, `Root)
      | Branch (base, m) -> Branch (base, `Dot (m, name)))

let rec resolved_module_split :
    resolved_module -> string * resolved_module option = function
  | `Subst (_, p) -> resolved_module_split p
  | `Alias (_, p) -> resolved_module_split p
  | `Module (m, name) -> (
      match resolved_signature_split_parent m with
      | RBase _ -> (ModuleName.to_string name, None)
      | RBranch (base, m) ->
          (ModuleName.to_string base, Some (`Module (m, name))))
  | `OpaqueModule m -> resolved_module_split m

let module_split : module_ -> string * module_ option = function
  | `Resolved r ->
      let base, m = resolved_module_split r in
      let m = match m with None -> None | Some m -> Some (`Resolved m) in
      (base, m)
  | `Dot (m, name) -> (
      match signature_split_parent m with
      | Base _ -> (name, None)
      | Branch (base, m) -> (ModuleName.to_string base, Some (`Dot (m, name))))

let resolved_module_type_split :
    resolved_module_type -> string * resolved_module_type option = function
  | `ModuleType (m, name) -> (
      match resolved_signature_split_parent m with
      | RBase _ -> (ModuleTypeName.to_string name, None)
      | RBranch (base, m) ->
          (ModuleName.to_string base, Some (`ModuleType (m, name))))

let module_type_split : module_type -> string * module_type option = function
  | `Resolved r ->
      let base, m = resolved_module_type_split r in
      let m = match m with None -> None | Some m -> Some (`Resolved m) in
      (base, m)
  | `Dot (m, name) -> (
      match signature_split_parent m with
      | Base _ -> (name, None)
      | Branch (base, m) -> (ModuleName.to_string base, Some (`Dot (m, name))))

let resolved_type_split : resolved_type -> string * resolved_type option =
  function
  | `Type (m, name) -> (
      match resolved_signature_split_parent m with
      | RBase _ -> (TypeName.to_string name, None)
      | RBranch (base, m) -> (ModuleName.to_string base, Some (`Type (m, name)))
      )
  | `Class (m, name) -> (
      match resolved_signature_split_parent m with
      | RBase _ -> (ClassName.to_string name, None)
      | RBranch (base, m) -> (ModuleName.to_string base, Some (`Class (m, name)))
      )
  | `ClassType (m, name) -> (
      match resolved_signature_split_parent m with
      | RBase _ -> (ClassTypeName.to_string name, None)
      | RBranch (base, m) ->
          (ModuleName.to_string base, Some (`ClassType (m, name))))

let type_split : type_ -> string * type_ option = function
  | `Resolved r ->
      let base, m = resolved_type_split r in
      let m = match m with None -> None | Some m -> Some (`Resolved m) in
      (base, m)
  | `Dot (m, name) -> (
      match signature_split_parent m with
      | Base _ -> (name, None)
      | Branch (base, m) -> (ModuleName.to_string base, Some (`Dot (m, name))))

let rec unresolve_module : resolved_module -> module_ = function
  | `OpaqueModule m | `Subst (_, m) | `Alias (_, m) -> unresolve_module m
  | `Module (parent, m) ->
      `Dot (unresolve_signature parent, ModuleName.to_string m)

and unresolve_signature : resolved_signature -> signature = function
  | #resolved_module as m -> (unresolve_module m :> signature)
  | `Root _ -> `Root

and unresolve_type : resolved_type -> type_ = function
  | `Type (parent, name) ->
      `Dot (unresolve_signature parent, TypeName.to_string name)
  | `ClassType (parent, name) ->
      `Dot (unresolve_signature parent, ClassTypeName.to_string name)
  | `Class (parent, name) ->
      `Dot (unresolve_signature parent, ClassName.to_string name)

and unresolve_module_type : resolved_module_type -> module_type = function
  | `ModuleType (parent, name) ->
      `Dot (unresolve_signature parent, ModuleTypeName.to_string name)
