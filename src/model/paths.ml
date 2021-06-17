(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Names

module Identifier = struct
  type t = Paths_types.Identifier.any

  let rec name_aux : t -> string = function
    | `Root (_, name) -> ModuleName.to_string name
    | `RootPage name -> PageName.to_string name
    | `Page (_, name) -> PageName.to_string name
    | `LeafPage (_, name) -> PageName.to_string name
    | `Module (_, name) -> ModuleName.to_string name
    | `Parameter (_, name) -> ParameterName.to_string name
    | `Result x -> name_aux (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.to_string name
    | `Type (_, name) -> TypeName.to_string name
    | `CoreType name -> TypeName.to_string name
    | `Constructor (_, name) -> ConstructorName.to_string name
    | `Field (_, name) -> FieldName.to_string name
    | `Extension (_, name) -> ExtensionName.to_string name
    | `Exception (_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value (_, name) -> ValueName.to_string name
    | `Class (_, name) -> ClassName.to_string name
    | `ClassType (_, name) -> ClassTypeName.to_string name
    | `Method (_, name) -> MethodName.to_string name
    | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
    | `Label (_, name) -> LabelName.to_string name

  let name : [< t ] -> string = fun n -> name_aux (n :> t)

  let rec label_parent_aux =
    let open Paths_types.Identifier in
    fun (n : any) ->
      match n with
      | `Result i -> label_parent_aux (i :> any)
      | `CoreType _ | `CoreException _ -> assert false
      | `Root _ as p -> (p :> label_parent)
      | `RootPage _ as p -> (p :> label_parent)
      | `Page _ as p -> (p :> label_parent)
      | `LeafPage _ as p -> (p :> label_parent)
      | `Module (p, _)
      | `ModuleType (p, _)
      | `Parameter (p, _)
      | `Class (p, _)
      | `ClassType (p, _)
      | `Type (p, _)
      | `Extension (p, _)
      | `Exception (p, _)
      | `Value (p, _) ->
          (p : signature :> label_parent)
      | `Label (p, _) -> p
      | `Method (p, _) | `InstanceVariable (p, _) ->
          (p : class_signature :> label_parent)
      | `Constructor (p, _) -> (p : datatype :> label_parent)
      | `Field (p, _) -> (p : parent :> label_parent)

  let label_parent n = label_parent_aux (n :> t)

  let equal = ( = )

  let hash = Hashtbl.hash

  let compare = compare

  type any = t

  module Signature = struct
    type t = Paths_types.Identifier.signature

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ClassSignature = struct
    type t = Paths_types.Identifier.class_signature

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module DataType = struct
    type t = Paths_types.Identifier.datatype

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Parent = struct
    type t = Paths_types.Identifier.parent

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module LabelParent = struct
    type t = Paths_types.Identifier.label_parent

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module RootModule = struct
    type t = Paths_types.Identifier.root_module

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Module = struct
    type t = Paths_types.Identifier.module_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module FunctorParameter = struct
    type t = Paths_types.Identifier.functor_parameter

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module FunctorResult = struct
    type t = Paths_types.Identifier.functor_result

    let equal x y = equal (x :> any) (y :> any)

    let hash x = hash (x :> any)

    let compare x y = compare (x :> any) (y :> any)
  end

  module ModuleType = struct
    type t = Paths_types.Identifier.module_type

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Type = struct
    type t = Paths_types.Identifier.type_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Constructor = struct
    type t = Paths_types.Identifier.constructor

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Field = struct
    type t = Paths_types.Identifier.field

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Extension = struct
    type t = Paths_types.Identifier.extension

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Exception = struct
    type t = Paths_types.Identifier.exception_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Value = struct
    type t = Paths_types.Identifier.value

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Class = struct
    type t = Paths_types.Identifier.class_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ClassType = struct
    type t = Paths_types.Identifier.class_type

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Method = struct
    type t = Paths_types.Identifier.method_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module InstanceVariable = struct
    type t = Paths_types.Identifier.instance_variable

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Label = struct
    type t = Paths_types.Identifier.label

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Page = struct
    type t = Paths_types.Identifier.page

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ContainerPage = struct
    type t = Paths_types.Identifier.container_page

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module OdocId = struct
    type t = Paths_types.Identifier.odoc_id

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Path = struct
    module Module = struct
      type t = Paths_types.Identifier.path_module

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module ModuleType = struct
      type t = Paths_types.Identifier.path_module_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module Type = struct
      type t = Paths_types.Identifier.path_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module ClassType = struct
      type t = Paths_types.Identifier.path_class_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    type t = Paths_types.Identifier.path_any
  end

  module Sets = struct
    module Signature = Set.Make (Signature)
    module ClassSignature = Set.Make (ClassSignature)
    module DataType = Set.Make (DataType)
    module Parent = Set.Make (Parent)
    module LabelParent = Set.Make (LabelParent)
    module RootModule = Set.Make (RootModule)
    module FunctorParameter = Set.Make (FunctorParameter)
    module Module = Set.Make (Module)
    module ModuleType = Set.Make (ModuleType)
    module Type = Set.Make (Type)
    module Constructor = Set.Make (Constructor)
    module Field = Set.Make (Field)
    module Extension = Set.Make (Extension)
    module Exception = Set.Make (Exception)
    module Value = Set.Make (Value)
    module Class = Set.Make (Class)
    module ClassType = Set.Make (ClassType)
    module Method = Set.Make (Method)
    module InstanceVariable = Set.Make (InstanceVariable)
    module Label = Set.Make (Label)
    module Page = Set.Make (Page)
    module ContainerPage = Set.Make (ContainerPage)

    module Path = struct
      module Module = Set.Make (Path.Module)
      module ModuleType = Set.Make (Path.ModuleType)
      module Type = Set.Make (Path.Type)
      module ClassType = Set.Make (Path.ClassType)
    end
  end

  module Maps = struct
    module Signature = Map.Make (Signature)
    module ClassSignature = Map.Make (ClassSignature)
    module DataType = Map.Make (DataType)
    module Parent = Map.Make (Parent)
    module LabelParent = Map.Make (LabelParent)
    module RootModule = Map.Make (RootModule)
    module FunctorParameter = Map.Make (FunctorParameter)
    module Module = Map.Make (Module)
    module ModuleType = Map.Make (ModuleType)
    module Type = Map.Make (Type)
    module Constructor = Map.Make (Constructor)
    module Field = Map.Make (Field)
    module Extension = Map.Make (Extension)
    module Exception = Map.Make (Exception)
    module Value = Map.Make (Value)
    module Class = Map.Make (Class)
    module ClassType = Map.Make (ClassType)
    module Method = Map.Make (Method)
    module InstanceVariable = Map.Make (InstanceVariable)
    module Label = Map.Make (Label)
    module Page = Map.Make (Page)
    module ContainerPage = Map.Make (ContainerPage)

    module Path = struct
      module Module = Map.Make (Path.Module)
      module ModuleType = Map.Make (Path.ModuleType)
      module Type = Map.Make (Path.Type)
      module ClassType = Map.Make (Path.ClassType)
    end
  end
end

module Path = struct
  type t = Paths_types.Path.any

  let rec is_resolved_hidden : Paths_types.Resolved_path.any -> bool =
   fun x ->
    let open Paths_types.Resolved_path in
    let rec inner = function
      | `Identifier (`ModuleType (_, m)) when Names.ModuleTypeName.is_internal m
        ->
          true
      | `Identifier (`Type (_, t)) when Names.TypeName.is_internal t -> true
      | `Identifier (`Module (_, m)) when Names.ModuleName.is_internal m -> true
      | `Identifier _ -> false
      | `Canonical (_, `Resolved _) -> false
      | `Canonical (x, _) -> inner (x : module_ :> any)
      | `Hidden _ -> true
      | `Subst (p1, p2) ->
          inner (p1 : module_type :> any) || inner (p2 : module_ :> any)
      | `SubstAlias (p1, p2) ->
          inner (p1 : module_ :> any) || inner (p2 : module_ :> any)
      | `Module (p, _) -> inner (p : module_ :> any)
      | `Apply (p, _) -> inner (p : module_ :> any)
      | `ModuleType (_, m) when Names.ModuleTypeName.is_internal m -> true
      | `ModuleType (p, _) -> inner (p : module_ :> any)
      | `Type (_, t) when Names.TypeName.is_internal t -> true
      | `Type (p, _) -> inner (p : module_ :> any)
      | `Class (p, _) -> inner (p : module_ :> any)
      | `ClassType (p, _) -> inner (p : module_ :> any)
      | `Alias (p1, p2) ->
          inner (p1 : module_ :> any) && inner (p2 : module_ :> any)
      | `SubstT (p1, p2) -> inner (p1 :> any) || inner (p2 :> any)
      | `CanonicalModuleType (_, `Resolved _) -> false
      | `CanonicalModuleType (x, _) -> inner (x : module_type :> any)
      | `CanonicalType (_, `Resolved _) -> false
      | `CanonicalType (x, _) -> inner (x : type_ :> any)
      | `OpaqueModule m -> inner (m :> any)
      | `OpaqueModuleType mt -> inner (mt :> any)
    in
    inner x

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    function
    | `Resolved r -> is_resolved_hidden r
    | `Identifier (_, hidden) -> hidden
    | `Root _ -> false
    | `Forward _ -> false
    | `Dot (p, _) -> is_path_hidden (p : module_ :> any)
    | `Apply (p1, p2) ->
        is_path_hidden (p1 : module_ :> any)
        || is_path_hidden (p2 : module_ :> any)

  module Resolved = struct
    type t = Paths_types.Resolved_path.any

    let rec parent_module_type_identifier :
        Paths_types.Resolved_path.module_type -> Identifier.Signature.t =
      function
      | `Identifier id ->
          (id : Identifier.ModuleType.t :> Identifier.Signature.t)
      | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
      | `SubstT (m, _n) -> parent_module_type_identifier m
      | `CanonicalModuleType (_, `Resolved p) -> parent_module_type_identifier p
      | `CanonicalModuleType (p, _) -> parent_module_type_identifier p
      | `OpaqueModuleType mt -> parent_module_type_identifier mt

    and parent_module_identifier :
        Paths_types.Resolved_path.module_ -> Identifier.Signature.t = function
      | `Identifier id ->
          (id : Identifier.Path.Module.t :> Identifier.Signature.t)
      | `Subst (sub, _) -> parent_module_type_identifier sub
      | `SubstAlias (sub, _) -> parent_module_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module (m, n) -> `Module (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> parent_module_identifier p
      | `Canonical (p, _) -> parent_module_identifier p
      | `Apply (m, _) -> parent_module_identifier m
      | `Alias (sub, orig) ->
          if is_path_hidden (`Resolved (sub :> t)) then
            parent_module_identifier orig
          else parent_module_identifier sub
      | `OpaqueModule m -> parent_module_identifier m

    module Module = struct
      type t = Paths_types.Resolved_path.module_

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.Path.Module.t = function
        | `Identifier id -> id
        | `Subst (_, p) -> identifier p
        | `SubstAlias (_, p) -> identifier p
        | `Hidden p -> identifier p
        | `Module (m, n) -> `Module (parent_module_identifier m, n)
        | `Canonical (_, `Resolved p) -> identifier p
        | `Canonical (p, _) -> identifier p
        | `Apply (m, _) -> identifier m
        | `Alias (sub, orig) ->
            if is_path_hidden (`Resolved (sub :> Paths_types.Resolved_path.any))
            then identifier orig
            else identifier sub
        | `OpaqueModule m -> identifier m

      let rec canonical_ident : t -> Identifier.Path.Module.t option = function
        | `Identifier _id -> None
        | `Subst (_, _) -> None
        | `SubstAlias (_, _) -> None
        | `Hidden p -> canonical_ident p
        | `Module (p, n) -> (
            match canonical_ident p with
            | Some x -> Some (`Module ((x :> Identifier.Signature.t), n))
            | None -> None)
        | `Canonical (_, `Resolved p) -> Some (identifier p)
        | `Canonical (_, _) -> None
        | `Apply (_, _) -> None
        | `Alias (_, _) -> None
        | `OpaqueModule m -> canonical_ident m
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_path.module_type

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.ModuleType.t = function
        | `Identifier id -> id
        | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
        | `SubstT (s, _) -> identifier s
        | `CanonicalModuleType (_, `Resolved p) -> identifier p
        | `CanonicalModuleType (p, _) -> identifier p
        | `OpaqueModuleType mt -> identifier mt

      let rec canonical_ident : t -> Identifier.ModuleType.t option = function
        | `Identifier _id -> None
        | `ModuleType (p, n) -> (
            match Module.canonical_ident p with
            | Some x -> Some (`ModuleType ((x :> Identifier.Signature.t), n))
            | None -> None)
        | `SubstT (_, _) -> None
        | `CanonicalModuleType (_, `Resolved p) -> Some (identifier p)
        | `CanonicalModuleType (_, _) -> None
        | `OpaqueModuleType m -> canonical_ident (m :> t)
    end

    module Type = struct
      type t = Paths_types.Resolved_path.type_

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.Path.Type.t = function
        | `Identifier id -> id
        | `CanonicalType (_, `Resolved t) -> identifier t
        | `CanonicalType (t, _) -> identifier t
        | `Type (m, n) -> `Type (parent_module_identifier m, n)
        | `Class (m, n) -> `Class (parent_module_identifier m, n)
        | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)

      let canonical_ident : t -> Identifier.Path.Type.t option =
        let parent m default fn =
          match Module.canonical_ident m with
          | Some x -> fn (x :> Identifier.Signature.t)
          | None -> default
        in
        function
        | `Identifier _ -> None
        | `CanonicalType (_, `Resolved t) -> Some (identifier t)
        | `CanonicalType (_, _) -> None
        | `Type (m, n) -> parent m None (fun sg -> Some (`Type (sg, n)))
        | `Class (m, n) -> parent m None (fun sg -> Some (`Class (sg, n)))
        | `ClassType (m, n) ->
            parent m None (fun sg -> Some (`ClassType (sg, n)))
    end

    module ClassType = struct
      type t = Paths_types.Resolved_path.class_type

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `Class (m, n) -> `Class (parent_module_identifier m, n)
        | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)
    end

    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `Subst (_, p) -> identifier (p :> t)
      | `SubstAlias (_, p) -> identifier (p :> t)
      | `Hidden p -> identifier (p :> t)
      | `Module (m, n) -> `Module (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> identifier (p :> t)
      | `Canonical (p, _) -> identifier (p :> t)
      | `Apply (m, _) -> identifier (m :> t)
      | `Type (m, n) -> `Type (parent_module_identifier m, n)
      | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
      | `Class (m, n) -> `Class (parent_module_identifier m, n)
      | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)
      | `Alias (sub, orig) ->
          if is_path_hidden (`Resolved (sub :> t)) then identifier (orig :> t)
          else identifier (sub :> t)
      | `SubstT (p, _) -> identifier (p :> t)
      | `CanonicalModuleType (_, `Resolved p) -> identifier (p :> t)
      | `CanonicalModuleType (p, _) -> identifier (p :> t)
      | `CanonicalType (_, `Resolved p) -> identifier (p :> t)
      | `CanonicalType (p, _) -> identifier (p :> t)
      | `OpaqueModule m -> identifier (m :> t)
      | `OpaqueModuleType mt -> identifier (mt :> t)
  end

  module Module = struct
    type t = Paths_types.Path.module_
  end

  module ModuleType = struct
    type t = Paths_types.Path.module_type
  end

  module Type = struct
    type t = Paths_types.Path.type_
  end

  module ClassType = struct
    type t = Paths_types.Path.class_type
  end

  let is_hidden = is_path_hidden
end

module Fragment = struct
  module Resolved = struct
    type t = Paths_types.Resolved_fragment.any

    type root = Paths_types.Resolved_fragment.root

    let sig_of_mod m =
      let open Paths_types.Resolved_fragment in
      (m : module_ :> signature)

    type base_name =
      | Base of root
      | Branch of ModuleName.t * Paths_types.Resolved_fragment.signature

    let rec split_parent : Paths_types.Resolved_fragment.signature -> base_name
        = function
      | `Root i -> Base i
      | `Subst (_, p) -> split_parent (sig_of_mod p)
      | `SubstAlias (_, p) -> split_parent (sig_of_mod p)
      | `OpaqueModule m -> split_parent (sig_of_mod m)
      | `Module (p, name) -> (
          match split_parent p with
          | Base i -> Branch (name, `Root i)
          | Branch (base, m) -> Branch (base, `Module (m, name)))

    module Signature = struct
      type t = Paths_types.Resolved_fragment.signature

      let rec split : t -> string * t option = function
        | `Root _ -> ("", None)
        | `Subst (_, p) -> split (sig_of_mod p)
        | `SubstAlias (_, p) -> split (sig_of_mod p)
        | `OpaqueModule m -> split (sig_of_mod m)
        | `Module (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module (m, name))))

      let rec identifier : t -> Identifier.Signature.t = function
        | `Root (`ModuleType i) ->
            (Path.Resolved.ModuleType.identifier i :> Identifier.Signature.t)
        | `Root (`Module i) ->
            (Path.Resolved.Module.identifier i :> Identifier.Signature.t)
        | `Subst (s, _) ->
            (Path.Resolved.ModuleType.identifier s :> Identifier.Signature.t)
        | `SubstAlias (i, _) ->
            (Path.Resolved.Module.identifier i :> Identifier.Signature.t)
        | `Module (m, n) -> `Module (identifier m, n)
        | `OpaqueModule m -> identifier (sig_of_mod m)
    end

    module Module = struct
      type t = Paths_types.Resolved_fragment.module_

      let rec split : t -> string * t option = function
        | `Subst (_, p) -> split p
        | `SubstAlias (_, p) -> split p
        | `Module (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module (m, name))))
        | `OpaqueModule m -> split m
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_fragment.module_type

      let split : t -> string * t option = function
        | `Module_type (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleTypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module_type (m, name))))
    end

    module Type = struct
      type t = Paths_types.Resolved_fragment.type_

      let split : t -> string * t option = function
        | `Type (m, name) -> (
            match split_parent m with
            | Base _ -> (TypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Type (m, name))))
        | `Class (m, name) -> (
            match split_parent m with
            | Base _ -> (ClassName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Class (m, name))))
        | `ClassType (m, name) -> (
            match split_parent m with
            | Base _ -> (ClassTypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`ClassType (m, name))))
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    let rec identifier : t -> Identifier.t = function
      | `Root (`ModuleType _r) -> assert false
      | `Root (`Module _r) -> assert false
      | `Subst (s, _) -> Path.Resolved.identifier (s :> Path.Resolved.t)
      | `SubstAlias (p, _) ->
          (Path.Resolved.Module.identifier p :> Identifier.t)
      | `Module (m, n) -> `Module (Signature.identifier m, n)
      | `Module_type (m, n) -> `ModuleType (Signature.identifier m, n)
      | `Type (m, n) -> `Type (Signature.identifier m, n)
      | `Class (m, n) -> `Class (Signature.identifier m, n)
      | `ClassType (m, n) -> `ClassType (Signature.identifier m, n)
      | `OpaqueModule m -> identifier (m :> t)

    let rec is_hidden : t -> bool = function
      | `Root (`ModuleType r) -> Path.is_resolved_hidden (r :> Path.Resolved.t)
      | `Root (`Module r) -> Path.is_resolved_hidden (r :> Path.Resolved.t)
      | `Subst (s, _) -> Path.is_resolved_hidden (s :> Path.Resolved.t)
      | `SubstAlias (s, _) -> Path.is_resolved_hidden (s :> Path.Resolved.t)
      | `Module (m, _)
      | `Module_type (m, _)
      | `Type (m, _)
      | `Class (m, _)
      | `ClassType (m, _) ->
          is_hidden (m :> t)
      | `OpaqueModule m -> is_hidden (m :> t)
  end

  type t = Paths_types.Fragment.any

  type base_name =
    | Base of Resolved.root option
    | Branch of ModuleName.t * Paths_types.Fragment.signature

  let rec split_parent : Paths_types.Fragment.signature -> base_name = function
    | `Root -> Base None
    | `Resolved r -> (
        match Resolved.split_parent r with
        | Resolved.Base i -> Base (Some i)
        | Resolved.Branch (base, m) -> Branch (base, `Resolved m))
    | `Dot (m, name) -> (
        match split_parent m with
        | Base None -> Branch (ModuleName.make_std name, `Root)
        | Base (Some i) -> Branch (ModuleName.make_std name, `Resolved (`Root i))
        | Branch (base, m) -> Branch (base, `Dot (m, name)))

  module Signature = struct
    type t = Paths_types.Fragment.signature

    let split : t -> string * t option = function
      | `Root -> ("", None)
      | `Resolved r ->
          let base, m = Resolved.Signature.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module Module = struct
    type t = Paths_types.Fragment.module_

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.Module.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module ModuleType = struct
    type t = Paths_types.Fragment.module_type

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.ModuleType.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module Type = struct
    type t = Paths_types.Fragment.type_

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.Type.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  type leaf = Paths_types.Fragment.leaf
end

module Reference = struct
  module Resolved = struct
    open Paths_types.Resolved_reference

    type t = Paths_types.Resolved_reference.any

    let rec parent_signature_identifier : signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `Hidden s -> parent_signature_identifier (s :> signature)
      | `SubstAlias (sub, orig) ->
          if Path.Resolved.Module.is_hidden sub then
            parent_signature_identifier (orig :> signature)
          else (Path.Resolved.Module.identifier sub :> Identifier.Signature.t)
      | `Module (m, n) -> `Module (parent_signature_identifier m, n)
      | `Canonical (_, `Resolved r) ->
          parent_signature_identifier (r : module_ :> signature)
      | `Canonical (r, _) ->
          parent_signature_identifier (r : module_ :> signature)
      | `ModuleType (m, s) -> `ModuleType (parent_signature_identifier m, s)

    and parent_type_identifier : datatype -> Identifier.DataType.t = function
      | `Identifier id -> id
      | `Type (sg, s) -> `Type (parent_signature_identifier sg, s)

    and parent_class_signature_identifier :
        class_signature -> Identifier.ClassSignature.t = function
      | `Identifier id -> id
      | `Class (sg, s) -> `Class (parent_signature_identifier sg, s)
      | `ClassType (sg, s) -> `ClassType (parent_signature_identifier sg, s)

    and parent_identifier : parent -> Identifier.Parent.t = function
      | `Identifier id -> id
      | (`Hidden _ | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.Parent.t)
      | `Type _ as t -> (parent_type_identifier t :> Identifier.Parent.t)
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c :> Identifier.Parent.t)

    and label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | ( `Hidden _ | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _
        | `Type _ | `Class _ | `ClassType _ ) as r ->
          (parent_identifier r :> Identifier.LabelParent.t)

    and identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | ( `SubstAlias _ | `Module _ | `Canonical _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t)
      | `Field (p, n) -> `Field (parent_identifier p, n)
      | `Constructor (s, n) -> `Constructor (parent_type_identifier s, n)
      | `Extension (p, q) -> `Extension (parent_signature_identifier p, q)
      | `Exception (p, q) -> `Exception (parent_signature_identifier p, q)
      | `Value (p, q) -> `Value (parent_signature_identifier p, q)
      | `Method (p, q) -> `Method (parent_class_signature_identifier p, q)
      | `InstanceVariable (p, q) ->
          `InstanceVariable (parent_class_signature_identifier p, q)
      | `Label (p, q) -> `Label (label_parent_identifier p, q)

    module Signature = struct
      type t = Paths_types.Resolved_reference.signature
    end

    module ClassSignature = struct
      type t = Paths_types.Resolved_reference.class_signature
    end

    module DataType = struct
      type t = Paths_types.Resolved_reference.datatype
    end

    module Parent = struct
      type t = Paths_types.Resolved_reference.parent
    end

    module LabelParent = struct
      type t = Paths_types.Resolved_reference.label_parent
    end

    module Module = struct
      type t = Paths_types.Resolved_reference.module_
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_reference.module_type
    end

    module Type = struct
      type t = Paths_types.Resolved_reference.type_
    end

    module Constructor = struct
      type t = Paths_types.Resolved_reference.constructor
    end

    module Field = struct
      type t = Paths_types.Resolved_reference.field
    end

    module Extension = struct
      type t = Paths_types.Resolved_reference.extension
    end

    module Exception = struct
      type t = Paths_types.Resolved_reference.exception_
    end

    module Value = struct
      type t = Paths_types.Resolved_reference.value
    end

    module Class = struct
      type t = Paths_types.Resolved_reference.class_
    end

    module ClassType = struct
      type t = Paths_types.Resolved_reference.class_type
    end

    module Method = struct
      type t = Paths_types.Resolved_reference.method_
    end

    module InstanceVariable = struct
      type t = Paths_types.Resolved_reference.instance_variable
    end

    module Label = struct
      type t = Paths_types.Resolved_reference.label
    end

    module Page = struct
      type t = Paths_types.Resolved_reference.page
    end
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any

  module Signature = struct
    type t = Paths_types.Reference.signature
  end

  module ClassSignature = struct
    type t = Paths_types.Reference.class_signature
  end

  module DataType = struct
    type t = Paths_types.Reference.datatype
  end

  module Parent = struct
    type t = Paths_types.Reference.parent
  end

  module LabelParent = struct
    type t = Paths_types.Reference.label_parent
  end

  module Module = struct
    type t = Paths_types.Reference.module_
  end

  module ModuleType = struct
    type t = Paths_types.Reference.module_type
  end

  module Type = struct
    type t = Paths_types.Reference.type_
  end

  module Constructor = struct
    type t = Paths_types.Reference.constructor
  end

  module Field = struct
    type t = Paths_types.Reference.field
  end

  module Extension = struct
    type t = Paths_types.Reference.extension
  end

  module Exception = struct
    type t = Paths_types.Reference.exception_
  end

  module Value = struct
    type t = Paths_types.Reference.value
  end

  module Class = struct
    type t = Paths_types.Reference.class_
  end

  module ClassType = struct
    type t = Paths_types.Reference.class_type
  end

  module Method = struct
    type t = Paths_types.Reference.method_
  end

  module InstanceVariable = struct
    type t = Paths_types.Reference.instance_variable
  end

  module Label = struct
    type t = Paths_types.Reference.label
  end

  module Page = struct
    type t = Paths_types.Reference.page
  end
end
