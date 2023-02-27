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

module Ocaml_ident = Ident
module Ocaml_env = Env

open Names

module Identifier = struct
  type 'a id = 'a Paths_types.id = { iv : 'a; ihash : int; ikey : string }

  type t = Paths_types.Identifier.any

  type t_pv = Paths_types.Identifier.any_pv

  let rec name_aux : t -> string =
   fun x ->
    match x.iv with
    | `Root (_, name) -> ModuleName.to_string name
    | `Page (_, name) -> PageName.to_string name
    | `LeafPage (_, name) -> PageName.to_string name
    | `Module (_, name) -> ModuleName.to_string name
    | `Parameter (_, name) -> ModuleName.to_string name
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

  let name : [< t_pv ] id -> string = fun n -> name_aux (n :> t)

  let rec root id =
    match id.iv with
    | `Root _ as root -> Some { id with iv = root }
    | `Module (parent, _) -> root (parent :> t)
    | `Parameter (parent, _) -> root (parent :> t)
    | `Result x -> root (x :> t)
    | `ModuleType (parent, _) -> root (parent :> t)
    | `Type (parent, _) -> root (parent :> t)
    | `Constructor (parent, _) -> root (parent :> t)
    | `Field (parent, _) -> root (parent :> t)
    | `Extension (parent, _) -> root (parent :> t)
    | `Exception (parent, _) -> root (parent :> t)
    | `Value (parent, _) -> root (parent :> t)
    | `Class (parent, _) -> root (parent :> t)
    | `ClassType (parent, _) -> root (parent :> t)
    | `Method (parent, _) -> root (parent :> t)
    | `InstanceVariable (parent, _) -> root (parent :> t)
    | `Label (parent, _) -> root (parent :> t)
    | `Page _ | `LeafPage _ | `CoreType _ | `CoreException _ -> None

  let root id = root (id :> t)

  let rec label_parent_aux =
    let open Paths_types.Identifier in
    fun (n : any) ->
      match n with
      | { iv = `Result i; _ } -> label_parent_aux (i :> any)
      | { iv = `CoreType _; _ } | { iv = `CoreException _; _ } -> assert false
      | { iv = `Root _; _ } as p -> (p :> label_parent)
      | { iv = `Page _; _ } as p -> (p :> label_parent)
      | { iv = `LeafPage _; _ } as p -> (p :> label_parent)
      | { iv = `Module (p, _); _ }
      | { iv = `ModuleType (p, _); _ }
      | { iv = `Parameter (p, _); _ }
      | { iv = `Class (p, _); _ }
      | { iv = `ClassType (p, _); _ }
      | { iv = `Type (p, _); _ }
      | { iv = `Extension (p, _); _ }
      | { iv = `Exception (p, _); _ }
      | { iv = `Value (p, _); _ } ->
          (p : signature :> label_parent)
      | { iv = `Label (p, _); _ } -> p
      | { iv = `Method (p, _); _ } | { iv = `InstanceVariable (p, _); _ } ->
          (p : class_signature :> label_parent)
      | { iv = `Constructor (p, _); _ } -> (p : datatype :> label_parent)
      | { iv = `Field (p, _); _ } -> (p : parent :> label_parent)

  let label_parent n = label_parent_aux (n :> t)

  let equal x y = x.ihash = y.ihash && x.ikey = y.ikey

  let hash x = x.ihash

  let compare x y = compare x.ikey y.ikey

  type any = t

  module Any = struct
    type t = any

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Signature = struct
    type t = Paths_types.Identifier.signature

    type t_pv = Paths_types.Identifier.signature_pv

    let equal = equal

    let hash = hash

    let compare = compare

    let rec root = function
      | { iv = `Root _; _ } as root -> root
      | {
          iv =
            ( `ModuleType (parent, _)
            | `Module (parent, _)
            | `Parameter (parent, _) );
          _;
        } ->
          root parent
      | { iv = `Result x; _ } -> root x

    let root id = root (id :> t)
  end

  module ClassSignature = struct
    type t = Paths_types.Identifier.class_signature

    type t_pv = Paths_types.Identifier.class_signature_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module DataType = struct
    type t = Paths_types.Identifier.datatype

    type t_pv = Paths_types.Identifier.datatype_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Parent = struct
    type t = Paths_types.Identifier.parent

    type t_pv = Paths_types.Identifier.parent_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module LabelParent = struct
    type t = Paths_types.Identifier.label_parent

    type t_pv = Paths_types.Identifier.label_parent_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module RootModule = struct
    type t = Paths_types.Identifier.root_module

    type t_pv = Paths_types.Identifier.root_module_pv

    let equal = equal

    let hash = hash

    let compare = compare

    let name { iv = `Root (_, name); _ } = ModuleName.to_string name
  end

  module Module = struct
    type t = Paths_types.Identifier.module_

    type t_pv = Paths_types.Identifier.module_pv

    let equal = equal

    let hash = hash

    let compare = compare

    let root id = Signature.root (id :> Signature.t)
  end

  module FunctorParameter = struct
    type t = Paths_types.Identifier.functor_parameter

    type t_pv = Paths_types.Identifier.functor_parameter_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module FunctorResult = struct
    type t = Paths_types.Identifier.functor_result

    type t_pv = Paths_types.Identifier.functor_result_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module ModuleType = struct
    type t = Paths_types.Identifier.module_type

    type t_pv = Paths_types.Identifier.module_type_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Type = struct
    type t = Paths_types.Identifier.type_

    type t_pv = Paths_types.Identifier.type_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Constructor = struct
    type t = Paths_types.Identifier.constructor

    type t_pv = Paths_types.Identifier.constructor_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Field = struct
    type t = Paths_types.Identifier.field

    type t_pv = Paths_types.Identifier.field_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Extension = struct
    type t = Paths_types.Identifier.extension

    type t_pv = Paths_types.Identifier.extension_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Exception = struct
    type t = Paths_types.Identifier.exception_

    type t_pv = Paths_types.Identifier.exception_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Value = struct
    type t = Paths_types.Identifier.value

    type t_pv = Paths_types.Identifier.value_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Class = struct
    type t = Paths_types.Identifier.class_

    type t_pv = Paths_types.Identifier.class_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module ClassType = struct
    type t = Paths_types.Identifier.class_type

    type t_pv = Paths_types.Identifier.class_type_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Method = struct
    type t = Paths_types.Identifier.method_

    type t_pv = Paths_types.Identifier.method_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module InstanceVariable = struct
    type t = Paths_types.Identifier.instance_variable

    type t_pv = Paths_types.Identifier.instance_variable_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Label = struct
    type t = Paths_types.Identifier.label

    type t_pv = Paths_types.Identifier.label_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Page = struct
    type t = Paths_types.Identifier.page

    type t_pv = Paths_types.Identifier.page_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module ContainerPage = struct
    type t = Paths_types.Identifier.container_page

    type t_pv = Paths_types.Identifier.container_page_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module SourceDir = struct
    type t = Paths_types.Identifier.source_dir
    type t_pv = Paths_types.Identifier.source_dir_pv
    let equal = equal
    let hash = hash
    let compare = compare
    let rec name = function
      | { iv = `SourceDir (p, n); _ } -> name p ^ n ^ "/"
      | { iv = `SourceRoot _; _ } -> "./"
  end

  module SourcePage = struct
    type t = Paths_types.Identifier.source_page
    type t_pv = Paths_types.Identifier.source_page_pv
    let equal = equal
    let hash = hash
    let compare = compare
    let name { iv = `SourcePage (p, name); _ } = SourceDir.name p ^ name
  end

  module OdocId = struct
    type t = Paths_types.Identifier.odoc_id

    type t_pv = Paths_types.Identifier.odoc_id_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Path = struct
    module Module = struct
      type t = Paths_types.Identifier.path_module

      type t_pv = Paths_types.Identifier.path_module_pv

      let equal = equal

      let hash = hash

      let compare = compare

      let root id = Signature.root (id :> Signature.t)
    end

    module ModuleType = struct
      type t = Paths_types.Identifier.path_module_type

      let equal = equal

      let hash = hash

      let compare = compare
    end

    module Type = struct
      type t = Paths_types.Identifier.path_type

      type t_pv = Paths_types.Identifier.path_type_pv

      let equal = equal

      let hash = hash

      let compare = compare
    end

    module ClassType = struct
      type t = Paths_types.Identifier.path_class_type

      type t_pv = Paths_types.Identifier.path_class_type_pv

      let equal = equal

      let hash = hash

      let compare = compare
    end

    type t = Paths_types.Identifier.path_any
  end

  module Maps = struct
    module Any = Map.Make (Any)
    module FunctorParameter = Map.Make (FunctorParameter)
    module Module = Map.Make (Module)
    module ModuleType = Map.Make (ModuleType)
    module Type = Map.Make (Type)
    module Class = Map.Make (Class)
    module ClassType = Map.Make (ClassType)
    module Label = Map.Make (Label)

    module Path = struct
      module Module = Map.Make (Path.Module)
      module ModuleType = Map.Make (Path.ModuleType)
      module Type = Map.Make (Path.Type)
      module ClassType = Map.Make (Path.ClassType)
    end
  end

  module Mk = struct
    let mk_fresh to_str ty f x =
      let ikey = Printf.sprintf "%s_%s" ty (to_str x) in
      let ihash = Hashtbl.hash ikey in
      { iv = f x; ihash; ikey }

    let mk_parent to_str ty f (parent, x) =
      let ikey = Printf.sprintf "%s_%s.%s" ty (to_str x) parent.ikey in
      let ihash = Hashtbl.hash ikey in

      { iv = f (parent, x); ihash; ikey }

    let mk_parent_opt to_str ty f (parent_opt, x) =
      let ikey =
        match parent_opt with
        | None -> Printf.sprintf "%s_%s" ty (to_str x)
        | Some p -> Printf.sprintf "%s_%s.%s" ty (to_str x) p.ikey
      in
      let ihash = Hashtbl.hash ikey in
      { iv = f (parent_opt, x); ihash; ikey }

    let page :
        ContainerPage.t option * PageName.t ->
        [> `Page of ContainerPage.t option * PageName.t ] id =
      mk_parent_opt PageName.to_string "p" (fun (p, n) -> `Page (p, n))

    let leaf_page :
        ContainerPage.t option * PageName.t ->
        [> `LeafPage of ContainerPage.t option * PageName.t ] id =
      mk_parent_opt PageName.to_string "lp" (fun (p, n) -> `LeafPage (p, n))

    let source_page (container_page, path) =
      let rec source_dir dir =
        match dir with
        | [] ->
            mk_parent
              (fun () -> "")
              "sr"
              (fun (p, ()) -> `SourceRoot p)
              (container_page, ())
        | a :: q ->
            let parent = source_dir q in
            mk_parent
              (fun k -> k)
              "sd"
              (fun (p, dir) -> `SourceDir (p, dir))
              (parent, a)
      in
      match List.rev path with
      | [] -> assert false
      | file :: dir ->
          let parent = source_dir dir in
          mk_parent
            (fun x -> x)
            "sp"
            (fun (p, rp) -> `SourcePage (p, rp))
            (parent, file)

    let root :
        ContainerPage.t option * ModuleName.t ->
        [> `Root of ContainerPage.t option * ModuleName.t ] id =
      mk_parent_opt ModuleName.to_string "r" (fun (p, n) -> `Root (p, n))

    let module_ :
        Signature.t * ModuleName.t ->
        [> `Module of Signature.t * ModuleName.t ] id =
      mk_parent ModuleName.to_string "m" (fun (p, n) -> `Module (p, n))

    let parameter :
        Signature.t * ModuleName.t ->
        [> `Parameter of Signature.t * ModuleName.t ] id =
      mk_parent ModuleName.to_string "p" (fun (p, n) -> `Parameter (p, n))

    let result : Signature.t -> [> `Result of Signature.t ] id =
     fun s ->
      mk_parent (fun () -> "__result__") "" (fun (s, ()) -> `Result s) (s, ())

    let module_type :
        Signature.t * ModuleTypeName.t ->
        [> `ModuleType of Signature.t * ModuleTypeName.t ] id =
      mk_parent ModuleTypeName.to_string "mt" (fun (p, n) -> `ModuleType (p, n))

    let class_ :
        Signature.t * ClassName.t -> [> `Class of Signature.t * ClassName.t ] id
        =
      mk_parent ClassName.to_string "c" (fun (p, n) -> `Class (p, n))

    let class_type :
        Signature.t * ClassTypeName.t ->
        [> `ClassType of Signature.t * ClassTypeName.t ] id =
      mk_parent ClassTypeName.to_string "ct" (fun (p, n) -> `ClassType (p, n))

    let type_ :
        Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ] id =
      mk_parent TypeName.to_string "t" (fun (p, n) -> `Type (p, n))

    let core_type =
      mk_fresh (fun s -> s) "coret" (fun s -> `CoreType (TypeName.make_std s))

    let constructor :
        Type.t * ConstructorName.t ->
        [> `Constructor of Type.t * ConstructorName.t ] id =
      mk_parent ConstructorName.to_string "ctor" (fun (p, n) ->
          `Constructor (p, n))

    let field :
        Parent.t * FieldName.t -> [> `Field of Parent.t * FieldName.t ] id =
      mk_parent FieldName.to_string "fld" (fun (p, n) -> `Field (p, n))

    let extension :
        Signature.t * ExtensionName.t ->
        [> `Extension of Signature.t * ExtensionName.t ] id =
      mk_parent ExtensionName.to_string "extn" (fun (p, n) -> `Extension (p, n))

    let exception_ :
        Signature.t * ExceptionName.t ->
        [> `Exception of Signature.t * ExceptionName.t ] id =
      mk_parent ExceptionName.to_string "exn" (fun (p, n) -> `Exception (p, n))

    let core_exception =
      mk_fresh
        (fun s -> s)
        "coreexn"
        (fun s -> `CoreException (ExceptionName.make_std s))

    let value :
        Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ] id
        =
      mk_parent ValueName.to_string "v" (fun (p, n) -> `Value (p, n))

    let method_ :
        ClassSignature.t * MethodName.t ->
        [> `Method of ClassSignature.t * MethodName.t ] id =
      mk_parent MethodName.to_string "m" (fun (p, n) -> `Method (p, n))

    let instance_variable :
        ClassSignature.t * InstanceVariableName.t ->
        [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ] id =
      mk_parent InstanceVariableName.to_string "iv" (fun (p, n) ->
          `InstanceVariable (p, n))

    let label :
        LabelParent.t * LabelName.t ->
        [> `Label of LabelParent.t * LabelName.t ] id =
      mk_parent LabelName.to_string "l" (fun (p, n) -> `Label (p, n))
  end
end

module Path = struct
  type t = Paths_types.Path.any

  let rec is_resolved_hidden :
      weak_canonical_test:bool -> Paths_types.Resolved_path.any -> bool =
   fun ~weak_canonical_test x ->
    let open Paths_types.Resolved_path in
    let rec inner : Paths_types.Resolved_path.any -> bool = function
      | `Identifier { iv = `ModuleType (_, m); _ }
        when Names.ModuleTypeName.is_internal m ->
          true
      | `Identifier { iv = `Type (_, t); _ } when Names.TypeName.is_internal t
        ->
          true
      | `Identifier { iv = `Module (_, m); _ }
        when Names.ModuleName.is_internal m ->
          true
      | `Identifier _ -> false
      | `Canonical (_, `Resolved _) -> false
      | `Canonical (x, _) ->
          (not weak_canonical_test) && inner (x : module_ :> any)
      | `Hidden _ -> true
      | `Subst (p1, p2) ->
          inner (p1 : module_type :> any) || inner (p2 : module_ :> any)
      | `Module (p, _) -> inner (p : module_ :> any)
      | `Apply (p, _) -> inner (p : module_ :> any)
      | `ModuleType (_, m) when Names.ModuleTypeName.is_internal m -> true
      | `ModuleType (p, _) -> inner (p : module_ :> any)
      | `Type (_, t) when Names.TypeName.is_internal t -> true
      | `Type (p, _) -> inner (p : module_ :> any)
      | `Class (p, _) -> inner (p : module_ :> any)
      | `ClassType (p, _) -> inner (p : module_ :> any)
      | `Alias (dest, `Resolved src) ->
          inner (dest : module_ :> any) && inner (src : module_ :> any)
      | `Alias (dest, src) ->
          inner (dest : module_ :> any)
          && is_path_hidden (src :> Paths_types.Path.any)
      | `AliasModuleType (p1, p2) ->
          inner (p1 : module_type :> any) && inner (p2 : module_type :> any)
      | `SubstT (p1, p2) -> inner (p1 :> any) || inner (p2 :> any)
      | `CanonicalModuleType (_, `Resolved _) -> false
      | `CanonicalModuleType (x, _) -> inner (x : module_type :> any)
      | `CanonicalType (_, `Resolved _) -> false
      | `CanonicalType (x, _) -> inner (x : type_ :> any)
      | `OpaqueModule m -> inner (m :> any)
      | `OpaqueModuleType mt -> inner (mt :> any)
    in
    inner x

  and contains_double_underscore s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then false
      else if s.[i] = '_' && s.[i + 1] = '_' then true
      else aux (i + 1)
    in
    aux 0

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    function
    | `Resolved r -> is_resolved_hidden ~weak_canonical_test:false r
    | `Identifier (_, hidden) -> hidden
    | `Root s -> contains_double_underscore s
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
      | `ModuleType (m, n) ->
          Identifier.Mk.module_type (parent_module_identifier m, n)
      | `SubstT (m, _n) -> parent_module_type_identifier m
      | `CanonicalModuleType (_, `Resolved p) -> parent_module_type_identifier p
      | `CanonicalModuleType (p, _) -> parent_module_type_identifier p
      | `OpaqueModuleType mt -> parent_module_type_identifier mt
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden ~weak_canonical_test:false (sub :> t) then
            parent_module_type_identifier orig
          else parent_module_type_identifier sub

    and parent_module_identifier :
        Paths_types.Resolved_path.module_ -> Identifier.Signature.t = function
      | `Identifier id ->
          (id : Identifier.Path.Module.t :> Identifier.Signature.t)
      | `Subst (sub, _) -> parent_module_type_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module (m, n) -> Identifier.Mk.module_ (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> parent_module_identifier p
      | `Canonical (p, _) -> parent_module_identifier p
      | `Apply (m, _) -> parent_module_identifier m
      | `Alias (dest, `Resolved src) ->
          if is_resolved_hidden ~weak_canonical_test:false (dest :> t) then
            parent_module_identifier src
          else parent_module_identifier dest
      | `Alias (dest, _src) -> parent_module_identifier dest
      | `OpaqueModule m -> parent_module_identifier m

    module Module = struct
      type t = Paths_types.Resolved_path.module_

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.Path.Module.t =
       fun x ->
        let r = identifier in
        match x with
        | `Identifier id -> id
        | `Subst (_, x) -> r x
        | `Hidden p -> r p
        | `Module (m, n) -> Identifier.Mk.module_ (parent_module_identifier m, n)
        | `Canonical (p, _) -> r p
        | `Apply (m, _) -> r m
        | `Alias (dest, _src) -> r dest
        | `OpaqueModule m -> r m

      let rec root : t -> string option = function
        | `Identifier id -> (
            match Identifier.root (id :> Identifier.t) with
            | Some root -> Some (Identifier.name root)
            | None -> None)
        | `Subst (_, p)
        | `Hidden p
        | `Module (p, _)
        | `Canonical (p, _)
        | `Apply (p, _)
        | `Alias (p, _)
        | `OpaqueModule p ->
            root p
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_path.module_type

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)
    end

    module Type = struct
      type t = Paths_types.Resolved_path.type_

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden ~weak_canonical_test:false
          (m : t :> Paths_types.Resolved_path.any)
    end

    module ClassType = struct
      type t = Paths_types.Resolved_path.class_type

      let of_ident id = `Identifier id

      let is_hidden m =
        is_resolved_hidden ~weak_canonical_test:false
          (m : t :> Paths_types.Resolved_path.any)
    end

    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `Subst (sub, _) -> identifier (sub :> t)
      | `Hidden p -> identifier (p :> t)
      | `Module (m, n) -> Identifier.Mk.module_ (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> identifier (p :> t)
      | `Canonical (p, _) -> identifier (p :> t)
      | `Apply (m, _) -> identifier (m :> t)
      | `Type (m, n) -> Identifier.Mk.type_ (parent_module_identifier m, n)
      | `ModuleType (m, n) ->
          Identifier.Mk.module_type (parent_module_identifier m, n)
      | `Class (m, n) -> Identifier.Mk.class_ (parent_module_identifier m, n)
      | `ClassType (m, n) ->
          Identifier.Mk.class_type (parent_module_identifier m, n)
      | `Alias (dest, `Resolved src) ->
          if is_resolved_hidden ~weak_canonical_test:false (dest :> t) then
            identifier (src :> t)
          else identifier (dest :> t)
      | `Alias (dest, _src) -> identifier (dest :> t)
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden ~weak_canonical_test:false (sub :> t) then
            identifier (orig :> t)
          else identifier (sub :> t)
      | `SubstT (p, _) -> identifier (p :> t)
      | `CanonicalModuleType (_, `Resolved p) -> identifier (p :> t)
      | `CanonicalModuleType (p, _) -> identifier (p :> t)
      | `CanonicalType (_, `Resolved p) -> identifier (p :> t)
      | `CanonicalType (p, _) -> identifier (p :> t)
      | `OpaqueModule m -> identifier (m :> t)
      | `OpaqueModuleType mt -> identifier (mt :> t)

    let is_hidden r = is_resolved_hidden ~weak_canonical_test:false r
  end

  module Module = struct
    type t = Paths_types.Path.module_

    let rec root : t -> string option = function
      | `Resolved r -> Resolved.Module.root r
      | `Identifier (id, _) -> (
          match Identifier.root (id :> Identifier.t) with
          | Some root -> Some (Identifier.name root)
          | None -> None)
      | `Root s -> Some s
      | `Forward _ -> None
      | `Dot (p, _) | `Apply (p, _) -> root p
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
      | `Alias (_, p) -> split_parent (sig_of_mod p)
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
        | `Alias (_, p) -> split (sig_of_mod p)
        | `OpaqueModule m -> split (sig_of_mod m)
        | `Module (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module (m, name))))

      let rec identifier : t -> Identifier.Signature.t = function
        | `Root (`ModuleType i) -> Path.Resolved.parent_module_type_identifier i
        | `Root (`Module i) -> Path.Resolved.parent_module_identifier i
        | `Subst (s, _) -> Path.Resolved.parent_module_type_identifier s
        | `Alias (i, _) -> Path.Resolved.parent_module_identifier i
        | `Module (m, n) -> Identifier.Mk.module_ (identifier m, n)
        | `OpaqueModule m -> identifier (sig_of_mod m)
    end

    module Module = struct
      type t = Paths_types.Resolved_fragment.module_

      let rec split : t -> string * t option = function
        | `Subst (_, p) -> split p
        | `Alias (_, p) -> split p
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
      | `Alias (p, _) ->
          (Path.Resolved.parent_module_identifier p :> Identifier.t)
      | `Module (m, n) -> Identifier.Mk.module_ (Signature.identifier m, n)
      | `Module_type (m, n) ->
          Identifier.Mk.module_type (Signature.identifier m, n)
      | `Type (m, n) -> Identifier.Mk.type_ (Signature.identifier m, n)
      | `Class (m, n) -> Identifier.Mk.class_ (Signature.identifier m, n)
      | `ClassType (m, n) -> Identifier.Mk.class_type (Signature.identifier m, n)
      | `OpaqueModule m -> identifier (m :> t)

    let rec is_hidden : t -> bool = function
      | `Root (`ModuleType r) ->
          Path.is_resolved_hidden ~weak_canonical_test:false
            (r :> Path.Resolved.t)
      | `Root (`Module r) ->
          Path.is_resolved_hidden ~weak_canonical_test:false
            (r :> Path.Resolved.t)
      | `Subst (s, _) ->
          Path.is_resolved_hidden ~weak_canonical_test:false
            (s :> Path.Resolved.t)
      | `Alias (s, _) ->
          Path.is_resolved_hidden ~weak_canonical_test:false
            (s :> Path.Resolved.t)
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
      | `Alias (sub, orig) ->
          if Path.Resolved.Module.is_hidden ~weak_canonical_test:false sub then
            parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.parent_module_identifier sub
              :> Identifier.Signature.t)
      | `AliasModuleType (sub, orig) ->
          if Path.Resolved.ModuleType.is_hidden ~weak_canonical_test:false sub
          then parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.parent_module_type_identifier sub
              :> Identifier.Signature.t)
      | `Module (m, n) ->
          Identifier.Mk.module_ (parent_signature_identifier m, n)
      | `ModuleType (m, s) ->
          Identifier.Mk.module_type (parent_signature_identifier m, s)

    and parent_type_identifier : datatype -> Identifier.DataType.t = function
      | `Identifier id -> id
      | `Type (sg, s) -> Identifier.Mk.type_ (parent_signature_identifier sg, s)

    and parent_class_signature_identifier :
        class_signature -> Identifier.ClassSignature.t = function
      | `Identifier id -> id
      | `Class (sg, s) ->
          Identifier.Mk.class_ (parent_signature_identifier sg, s)
      | `ClassType (sg, s) ->
          Identifier.Mk.class_type (parent_signature_identifier sg, s)

    and parent_identifier : parent -> Identifier.Parent.t = function
      | `Identifier id -> id
      | (`Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.Parent.t)
      | `Type _ as t -> (parent_type_identifier t :> Identifier.Parent.t)
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c :> Identifier.Parent.t)

    and label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | ( `Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _
        | `Type _ | `Class _ | `ClassType _ ) as r ->
          (parent_identifier r :> Identifier.LabelParent.t)

    and identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | ( `Alias _ | `AliasModuleType _ | `Module _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t)
      | `Field (p, n) -> Identifier.Mk.field (parent_identifier p, n)
      | `Constructor (s, n) ->
          Identifier.Mk.constructor (parent_type_identifier s, n)
      | `Extension (p, q) ->
          Identifier.Mk.extension (parent_signature_identifier p, q)
      | `Exception (p, q) ->
          Identifier.Mk.exception_ (parent_signature_identifier p, q)
      | `Value (p, q) -> Identifier.Mk.value (parent_signature_identifier p, q)
      | `Method (p, q) ->
          Identifier.Mk.method_ (parent_class_signature_identifier p, q)
      | `InstanceVariable (p, q) ->
          Identifier.Mk.instance_variable
            (parent_class_signature_identifier p, q)
      | `Label (p, q) -> Identifier.Mk.label (label_parent_identifier p, q)

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
