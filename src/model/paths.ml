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
  module Id = Paths_types.Identifier

  type t = Id.any

  let rec name_aux : t -> string =
   fun x ->
    match x with
    | `Root (_, name) -> ModuleName.to_string name
    | `Page (_, name) -> PageName.to_string name
    | `LeafPage (_, name) -> PageName.to_string name
    | `Module (_, name) -> ModuleName.to_string name
    | `Parameter (_, name) -> ModuleName.to_string name
    | `Result x -> name_aux (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.to_string name
    | `Type (_, name) -> TypeName.to_string name
    | `Constructor (_, name) -> ConstructorName.to_string name
    | `Field (_, name) -> FieldName.to_string name
    | `UnboxedField (_, name) -> UnboxedFieldName.to_string name
    | `Extension (_, name) -> ExtensionName.to_string name
    | `ExtensionDecl (_, _, name) -> ExtensionName.to_string name
    | `Exception (_, name) -> ExceptionName.to_string name
    | `Value (_, name) -> ValueName.to_string name
    | `Class (_, name) -> TypeName.to_string name
    | `ClassType (_, name) -> TypeName.to_string name
    | `Method (_, name) -> MethodName.to_string name
    | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
    | `Label (_, name) -> LabelName.to_string name
    | `SourcePage (_, name) -> name
    | `SourceLocation (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ DefName.to_string anchor
    | `SourceLocationMod x -> name_aux (x :> t)
    | `SourceLocationInternal (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ LocalName.to_string anchor
    | `AssetFile (_, name) -> AssetName.to_string name

  let rec is_hidden : t -> bool =
   fun x ->
    match x with
    | `Root (_, name) -> ModuleName.is_hidden name
    | `Page (_, _) -> false
    | `LeafPage (_, _) -> false
    | `Module (_, name) -> ModuleName.is_hidden name
    | `Parameter (_, name) -> ModuleName.is_hidden name
    | `Result x -> is_hidden (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.is_hidden name
    | `Type (_, name) -> TypeName.is_hidden name
    | `Constructor (parent, _) -> is_hidden (parent :> t)
    | `Field (parent, _) -> is_hidden (parent :> t)
    | `UnboxedField (parent, _) -> is_hidden (parent :> t)
    | `Extension (parent, _) -> is_hidden (parent :> t)
    | `ExtensionDecl (parent, _, _) -> is_hidden (parent :> t)
    | `Exception (parent, _) -> is_hidden (parent :> t)
    | `Value (_, name) -> ValueName.is_hidden name
    | `Class (_, name) -> TypeName.is_hidden name
    | `ClassType (_, name) -> TypeName.is_hidden name
    | `Method (parent, _) -> is_hidden (parent :> t)
    | `InstanceVariable (parent, _) -> is_hidden (parent :> t)
    | `Label (parent, _) -> is_hidden (parent :> t)
    | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
    | `SourceLocationInternal _ | `AssetFile _ ->
        false

  let name : [< t ] -> string = fun n -> name_aux (n :> t)

  let rec full_name_aux : t -> string list =
   fun x ->
    match x with
    | `Root (_, name) -> [ ModuleName.to_string name ]
    | `Page (None, name) -> [ PageName.to_string name ]
    | `Page (Some parent, name) ->
        PageName.to_string name :: full_name_aux (parent :> t)
    | `LeafPage (None, name) -> [ PageName.to_string name ]
    | `LeafPage (Some parent, name) ->
        PageName.to_string name :: full_name_aux (parent :> t)
    | `Module (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Parameter (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Result x -> full_name_aux (x :> t)
    | `ModuleType (parent, name) ->
        ModuleTypeName.to_string name :: full_name_aux (parent :> t)
    | `Type (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `Constructor (parent, name) ->
        ConstructorName.to_string name :: full_name_aux (parent :> t)
    | `Field (parent, name) ->
        FieldName.to_string name :: full_name_aux (parent :> t)
    | `UnboxedField (parent, name) ->
        UnboxedFieldName.to_string name :: full_name_aux (parent :> t)
    | `Extension (parent, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `ExtensionDecl (parent, _, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `Exception (parent, name) ->
        ExceptionName.to_string name :: full_name_aux (parent :> t)
    | `Value (parent, name) ->
        ValueName.to_string name :: full_name_aux (parent :> t)
    | `Class (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `ClassType (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `Method (parent, name) ->
        MethodName.to_string name :: full_name_aux (parent :> t)
    | `InstanceVariable (parent, name) ->
        InstanceVariableName.to_string name :: full_name_aux (parent :> t)
    | `Label (parent, name) ->
        LabelName.to_string name :: full_name_aux (parent :> t)
    | `SourceLocation (parent, name) ->
        DefName.to_string name :: full_name_aux (parent :> t)
    | `SourceLocationInternal (parent, name) ->
        LocalName.to_string name :: full_name_aux (parent :> t)
    | `SourceLocationMod name -> full_name_aux (name :> t)
    | `SourcePage (parent, name) -> name :: full_name_aux (parent :> t)
    | `AssetFile (parent, name) ->
        AssetName.to_string name :: full_name_aux (parent :> t)

  let fullname : [< t ] -> string list =
   fun n -> List.rev @@ full_name_aux (n :> t)

  let is_hidden : [< t ] -> bool = fun n -> is_hidden (n :> t)

  let rec label_parent_aux =
    let open Id in
    fun (n : non_src) ->
      match n with
      | `Result i -> label_parent_aux (i :> non_src)
      | `Root _ as p -> (p :> label_parent)
      | `Page _ as p -> (p :> label_parent)
      | `LeafPage _ as p -> (p :> label_parent)
      | `Module (p, _)
      | `ModuleType (p, _)
      | `Parameter (p, _)
      | `Class (p, _)
      | `ClassType (p, _)
      | `Type (p, _)
      | `Extension (p, _)
      | `ExtensionDecl (p, _, _)
      | `Exception (p, _)
      | `Value (p, _) ->
          (p : signature :> label_parent)
      | `Label (p, _) -> p
      | `Method (p, _) | `InstanceVariable (p, _) ->
          (p : class_signature :> label_parent)
      | `Constructor (p, _) -> (p : datatype :> label_parent)
      | `Field (p, _) -> (p : field_parent :> label_parent)
      | `UnboxedField (p, _) -> (p : unboxed_field_parent :> label_parent)

  let label_parent n = label_parent_aux (n :> Id.non_src)

  (* An identifier is now just plain data, so structural equality is the right
     notion. *)
  let equal x y = x == y || x = y

  (* Deliberately not [Hashtbl.hash], which is [hash_param 10 100]. Hashing is a
     breadth-first walk that stops after [count] meaningful nodes (ints,
     strings) or [limit] enqueued ones, and each identifier level costs several
     of each, so a budget of 10 is exhausted a few levels down and never reaches
     the root.

     256/256 is the most reach obtainable: the runtime clamps [limit] to its
     fixed-size traversal queue (HASH_QUEUE_SIZE, runtime/hash.c), so larger
     values are silently ignored and cannot help. *)
  let hash x = Hashtbl.hash_param 256 256 x

  (* Left polymorphic on purpose: every sub-module below reuses this as its
     own [compare], each at a narrower identifier type. *)
  let compare = Stdlib.compare

  type any = t

  module type IdSig = sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Any = struct
    type t = any
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Signature = struct
    type t = Id.signature
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassSignature = struct
    type t = Id.class_signature
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module DataType = struct
    type t = Id.datatype
  end

  module FieldParent = struct
    type t = Paths_types.Identifier.field_parent
  end

  module UnboxedFieldParent = struct
    type t = Paths_types.Identifier.unboxed_field_parent
  end

  module LabelParent = struct
    type t = Id.label_parent
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module RootModule = struct
    type t = Id.root_module
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Module = struct
    type t = Id.module_
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module FunctorParameter = struct
    type t = Id.functor_parameter
    let equal = equal
    let hash = hash
    let compare = compare

    let functor_arg_pos (`Parameter (p, _)) =
      let rec inner_sig = function
        | `Result p -> 1 + inner_sig p
        | `Module _ | `ModuleType _ | `Root _ | `Parameter _ -> 1
      in
      inner_sig p
  end

  module FunctorResult = struct
    type t = Id.functor_result
  end

  module ModuleType = struct
    type t = Id.module_type
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Type = struct
    type t = Id.type_
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Constructor = struct
    type t = Id.constructor
  end

  module Field = struct
    type t = Id.field
  end

  module UnboxedField = struct
    type t = Id.unboxed_field
  end

  module Extension = struct
    type t = Id.extension
  end

  module ExtensionDecl = struct
    type t = Paths_types.Identifier.extension_decl

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Exception = struct
    type t = Id.exception_
  end

  module Value = struct
    type t = Id.value
  end

  module Class = struct
    type t = Id.class_
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassType = struct
    type t = Id.class_type
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Method = struct
    type t = Id.method_
  end

  module InstanceVariable = struct
    type t = Id.instance_variable
  end

  module Label = struct
    type t = Paths_types.Identifier.label
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Page = struct
    type t = Id.page
  end

  module LeafPage = struct
    type t = Id.leaf_page
    let equal = equal
    let hash = hash
  end

  module ContainerPage = struct
    type t = Id.container_page
    let equal = equal
    let hash = hash
  end

  module NonSrc = struct
    type t = Paths_types.Identifier.non_src

    (* [==] guard as in [equal] above: [( = )] alone never checks pointers. *)
    let equal x y = x == y || x = y

    let hash x = Hashtbl.hash_param 256 256 x
  end

  module SourcePage = struct
    type t = Id.source_page

    let equal = equal
    let hash = hash
  end

  module SourceLocation = struct
    type t = Paths_types.Identifier.source_location
  end

  module AssetFile = struct
    type t = Id.asset_file
  end

  module OdocId = struct
    type t = Id.odoc_id
  end

  module Path = struct
    module Module = struct
      type t = Id.path_module
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ModuleType = struct
      type t = Id.path_module_type
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Type = struct
      type t = Id.path_type
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Value = struct
      type t = Id.path_value
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ClassType = struct
      type t = Id.path_class_type
      let equal = equal
      let hash = hash
      let compare = compare
    end

    type t = Id.path_any
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
    let mk f x = f x

    let page :
        ContainerPage.t option * PageName.t ->
        [> `Page of ContainerPage.t option * PageName.t ] =
      mk (fun (p, n) -> `Page (p, n))

    let leaf_page :
        ContainerPage.t option * PageName.t ->
        [> `LeafPage of ContainerPage.t option * PageName.t ] =
      mk (fun (p, n) -> `LeafPage (p, n))

    let asset_file : Page.t * AssetName.t -> AssetFile.t =
      mk (fun (p, n) -> `AssetFile (p, n))

    let source_page (container_page, name) =
      mk (fun (p, rp) -> `SourcePage (p, rp)) (container_page, name)

    let root :
        ContainerPage.t option * ModuleName.t ->
        [> `Root of ContainerPage.t option * ModuleName.t ] =
      mk (fun (p, n) -> `Root (p, n))

    let implementation = mk (fun s -> `Implementation (ModuleName.make_std s))

    let module_ :
        Signature.t * ModuleName.t -> [> `Module of Signature.t * ModuleName.t ]
        =
      mk (fun (p, n) -> `Module (p, n))

    let parameter :
        Signature.t * ModuleName.t ->
        [> `Parameter of Signature.t * ModuleName.t ] =
      mk (fun (p, n) -> `Parameter (p, n))

    let result : Signature.t -> [> `Result of Signature.t ] =
     fun s -> mk (fun s -> `Result s) s

    let module_type :
        Signature.t * ModuleTypeName.t ->
        [> `ModuleType of Signature.t * ModuleTypeName.t ] =
      mk (fun (p, n) -> `ModuleType (p, n))

    let class_ :
        Signature.t * TypeName.t -> [> `Class of Signature.t * TypeName.t ] =
      mk (fun (p, n) -> `Class (p, n))

    let class_type :
        Signature.t * TypeName.t -> [> `ClassType of Signature.t * TypeName.t ]
        =
      mk (fun (p, n) -> `ClassType (p, n))

    let type_ :
        Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ] =
      mk (fun (p, n) -> `Type (p, n))

    let core_type = mk (fun s -> `CoreType (TypeName.make_std s))

    let constructor :
        DataType.t * ConstructorName.t ->
        [> `Constructor of DataType.t * ConstructorName.t ] =
      mk (fun (p, n) -> `Constructor (p, n))

    let field :
        FieldParent.t * FieldName.t -> [> `Field of FieldParent.t * FieldName.t ]
        =
      mk (fun (p, n) -> `Field (p, n))

    let unboxed_field :
        UnboxedFieldParent.t * UnboxedFieldName.t ->
        [> `UnboxedField of UnboxedFieldParent.t * UnboxedFieldName.t ] =
      mk (fun (p, n) -> `UnboxedField (p, n))

    let extension :
        Signature.t * ExtensionName.t ->
        [> `Extension of Signature.t * ExtensionName.t ] =
      mk (fun (p, n) -> `Extension (p, n))

    let extension_decl :
        Signature.t * (ExtensionName.t * ExtensionName.t) ->
        [> `ExtensionDecl of Signature.t * ExtensionName.t * ExtensionName.t ] =
      mk (fun (p, (n, m)) -> `ExtensionDecl (p, n, m))

    let exception_ :
        Signature.t * ExceptionName.t ->
        [> `Exception of Signature.t * ExceptionName.t ] =
      mk (fun (p, n) -> `Exception (p, n))

    let value :
        Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ] =
      mk (fun (p, n) -> `Value (p, n))

    let method_ :
        ClassSignature.t * MethodName.t ->
        [> `Method of ClassSignature.t * MethodName.t ] =
      mk (fun (p, n) -> `Method (p, n))

    let instance_variable :
        ClassSignature.t * InstanceVariableName.t ->
        [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ] =
      mk (fun (p, n) -> `InstanceVariable (p, n))

    let label :
        LabelParent.t * LabelName.t -> [> `Label of LabelParent.t * LabelName.t ]
        =
      mk (fun (p, n) -> `Label (p, n))

    let source_location :
        SourcePage.t * DefName.t ->
        [> `SourceLocation of SourcePage.t * DefName.t ] =
      mk (fun (p, n) -> `SourceLocation (p, n))

    let source_location_mod :
        SourcePage.t -> [> `SourceLocationMod of SourcePage.t ] =
     fun s -> mk (fun s -> `SourceLocationMod s) s

    let source_location_int :
        SourcePage.t * LocalName.t ->
        [> `SourceLocationInternal of SourcePage.t * LocalName.t ] =
      mk (fun (p, n) -> `SourceLocationInternal (p, n))
  end

  (* Counter for generating unique synthetic parents for include expressions.
     Items inside an include's module type expression need a different parent
     to avoid identifier conflicts with items in the enclosing signature. *)
  let include_parent_counter = ref 0

  (* Create a synthetic parent identifier for items inside an include's module
     type expression. Uses a lowercase module name (illegal in normal OCaml)
     to ensure no clashes with real identifiers. *)
  let fresh_include_parent (parent : Signature.t) : Signature.t =
    incr include_parent_counter;
    let name = Printf.sprintf "include%d_" !include_parent_counter in
    (Mk.module_ (parent, ModuleName.make_std name) :> Signature.t)

  let module_arg_parent_counter = ref 0

  (* Create a synthetic parent identifier for module arguments, which can't have
     unique identifier, as they can be introduced multiple times with the same
     name in a single type expression . *)
  let fresh_module_arg_parent () : Signature.t =
    incr module_arg_parent_counter;
    let name = Printf.sprintf "module_arg_%d_" !module_arg_parent_counter in
    (Mk.root (None, ModuleName.hidden_of_string name) :> Signature.t)

  module Hashtbl = struct
    module Any = Hashtbl.Make (Any)
    module ContainerPage = Hashtbl.Make (ContainerPage)
    module LeafPage = Hashtbl.Make (LeafPage)
    module RootModule = Hashtbl.Make (RootModule)
    module SourcePage = Hashtbl.Make (SourcePage)
  end
end

module Path = struct
  type t = Paths_types.Path.any

  let rec is_resolved_hidden :
      weak_canonical_test:bool -> Paths_types.Resolved_path.any -> bool =
   fun ~weak_canonical_test x ->
    let open Paths_types.Resolved_path in
    let rec inner : Paths_types.Resolved_path.any -> bool = function
      | `Identifier (`ModuleType (_, m)) when Names.ModuleTypeName.is_hidden m
        ->
          true
      | `Identifier (`Type (_, t)) when Names.TypeName.is_hidden t -> true
      | `Identifier (`Module (_, m)) when Names.ModuleName.is_hidden m -> true
      | `Identifier id -> Identifier.is_hidden id
      | `Canonical (_, `Resolved _) -> false
      | `Canonical (x, _) ->
          (not weak_canonical_test) && inner (x : module_ :> any)
      | `Hidden _ -> true
      | `Subst (p1, p2) ->
          inner (p1 : module_type :> any) || inner (p2 : module_ :> any)
      | `Module (p, _) -> inner (p : module_ :> any)
      | `Apply (p, _) -> inner (p : module_ :> any)
      | `ModuleType (_, m) when Names.ModuleTypeName.is_hidden m -> true
      | `ModuleType (p, _) -> inner (p : module_ :> any)
      | `Type (_, t) when Names.TypeName.is_hidden t -> true
      | `CoreType t -> Names.TypeName.is_hidden t
      | `Type (p, _) -> inner (p : module_ :> any)
      | `Value (_, t) when Names.ValueName.is_hidden t -> true
      | `Value (p, _) -> inner (p : module_ :> any)
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
      | `Substituted m -> inner (m :> any)
      | `SubstitutedMT m -> inner (m :> any)
      | `SubstitutedT m -> inner (m :> any)
      | `SubstitutedCT m -> inner (m :> any)
      | `CanonicalModuleType (_, `Resolved _) -> false
      | `CanonicalModuleType (x, _) -> inner (x : module_type :> any)
      | `CanonicalType (_, `Resolved _) -> false
      | `CanonicalType (x, _) -> inner (x : type_ :> any)
      | `OpaqueModule m -> inner (m :> any)
      | `OpaqueModuleType mt -> inner (mt :> any)
      | `Unbox mt -> inner (mt :> any)
    in
    inner x

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    function
    | `Resolved r -> is_resolved_hidden ~weak_canonical_test:false r
    | `Identifier (id, hidden) -> hidden || Identifier.is_hidden id
    | `Substituted r -> is_path_hidden (r :> any)
    | `SubstitutedMT r -> is_path_hidden (r :> any)
    | `SubstitutedT r -> is_path_hidden (r :> any)
    | `SubstitutedCT r -> is_path_hidden (r :> any)
    | `Unbox r -> is_path_hidden (r :> any)
    | `Root s -> ModuleName.is_hidden s
    | `Forward _ -> false
    | `Dot (p, n) ->
        ModuleName.is_hidden n || is_path_hidden (p : module_ :> any)
    | `DotMT (p, n) ->
        ModuleTypeName.is_hidden n || is_path_hidden (p : module_ :> any)
    | `DotT (p, n) ->
        TypeName.is_hidden n || is_path_hidden (p : module_ :> any)
    | `DotV (p, n) ->
        ValueName.is_hidden n || is_path_hidden (p : module_ :> any)
    | `Apply (p1, p2) ->
        is_path_hidden (p1 : module_ :> any)
        || is_path_hidden (p2 : module_ :> any)

  module Resolved = struct
    type t = Paths_types.Resolved_path.any

    let rec parent_module_type_identifier :
        Paths_types.Resolved_path.module_type -> Identifier.ModuleType.t option
        = function
      | `Identifier id -> Some (id : Identifier.ModuleType.t)
      | `ModuleType (m, n) -> (
          match parent_module_identifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_type (p, n)))
      | `SubstT (m, _n) -> parent_module_type_identifier m
      | `CanonicalModuleType (_, `Resolved p) -> parent_module_type_identifier p
      | `CanonicalModuleType (p, _) -> parent_module_type_identifier p
      | `OpaqueModuleType mt -> parent_module_type_identifier mt
      | `SubstitutedMT m -> parent_module_type_identifier m
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden ~weak_canonical_test:false (sub :> t) then
            parent_module_type_identifier orig
          else parent_module_type_identifier sub

    and parent_module_identifier :
        Paths_types.Resolved_path.module_ -> Identifier.Signature.t option =
      function
      | `Identifier id ->
          Some (id : Identifier.Path.Module.t :> Identifier.Signature.t)
      | `Subst (sub, _) ->
          (parent_module_type_identifier sub :> Identifier.Signature.t option)
      | `Hidden _ -> None
      | `Module (m, n) -> (
          match parent_module_identifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_ (p, n)))
      | `Canonical (_, `Resolved p) -> parent_module_identifier p
      | `Canonical (p, _) -> parent_module_identifier p
      | `Apply (m, _) -> parent_module_identifier m
      | `Alias (dest, `Resolved src) ->
          if is_resolved_hidden ~weak_canonical_test:false (dest :> t) then
            parent_module_identifier src
          else parent_module_identifier dest
      | `Alias (dest, _src) -> parent_module_identifier dest
      | `Substituted m -> parent_module_identifier m
      | `OpaqueModule m -> parent_module_identifier m

    module Module = struct
      type t = Paths_types.Resolved_path.module_

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_path.module_type

      let identifier : t -> Identifier.ModuleType.t option =
        parent_module_type_identifier
    end

    module Type = struct
      type t = Paths_types.Resolved_path.type_
    end

    module Value = struct
      type t = Paths_types.Resolved_path.value
    end

    module ClassType = struct
      type t = Paths_types.Resolved_path.class_type
    end

    let rec identifier : t -> Identifier.t option =
      let parent p f =
        match parent_module_identifier p with
        | None -> None
        | Some id -> Some (f id :> Identifier.t)
      in
      function
      | `Identifier id -> Some id
      | `CoreType _ -> None
      | `Subst (sub, _) -> identifier (sub :> t)
      | `Hidden _p -> None
      | `Module (m, n) -> parent m (fun p -> Identifier.Mk.module_ (p, n))
      | `Canonical (_, `Resolved p) -> identifier (p :> t)
      | `Canonical (p, _) -> identifier (p :> t)
      | `Apply (m, _) -> identifier (m :> t)
      | `Type (m, n) -> parent m (fun p -> Identifier.Mk.type_ (p, n))
      | `Value (m, n) -> parent m (fun p -> Identifier.Mk.value (p, n))
      | `ModuleType (m, n) ->
          parent m (fun p -> Identifier.Mk.module_type (p, n))
      | `Class (m, n) -> parent m (fun p -> Identifier.Mk.class_ (p, n))
      | `ClassType (m, n) -> parent m (fun p -> Identifier.Mk.class_type (p, n))
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
      | `Substituted m -> identifier (m :> t)
      | `SubstitutedMT m -> identifier (m :> t)
      | `SubstitutedCT m -> identifier (m :> t)
      | `SubstitutedT m -> identifier (m :> t)
      | `Unbox m -> identifier (m :> t)

    let is_hidden r = is_resolved_hidden ~weak_canonical_test:false r
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

  module Value = struct
    type t = Paths_types.Path.value
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

    module Signature = struct
      type t = Paths_types.Resolved_fragment.signature

      let rec sgidentifier : t -> Identifier.Signature.t option = function
        | `Root (`ModuleType i) ->
            (Path.Resolved.parent_module_type_identifier i
              :> Identifier.Signature.t option)
        | `Root (`Module i) -> Path.Resolved.parent_module_identifier i
        | `Subst (s, _) ->
            (Path.Resolved.parent_module_type_identifier s
              :> Identifier.Signature.t option)
        | `Alias (i, _) -> Path.Resolved.parent_module_identifier i
        | `Module (m, n) -> (
            match sgidentifier m with
            | None -> None
            | Some p -> Some (Identifier.Mk.module_ (p, n)))
        | `OpaqueModule m -> sgidentifier (m :> t)
    end

    module Module = struct
      type t = Paths_types.Resolved_fragment.module_
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_fragment.module_type
    end

    module Type = struct
      type t = Paths_types.Resolved_fragment.type_
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    let rec identifier : t -> Identifier.t option = function
      | `Root (`ModuleType _r) -> assert false
      | `Root (`Module _r) -> assert false
      | `Subst (s, _) ->
          (Path.Resolved.ModuleType.identifier s :> Identifier.t option)
      | `Alias (p, _) ->
          (Path.Resolved.parent_module_identifier p :> Identifier.t option)
      | `Module (m, n) -> (
          match Signature.sgidentifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_ (p, n)))
      | `Module_type (m, n) -> (
          match Signature.sgidentifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_type (p, n)))
      | `Type (m, n) -> (
          match Signature.sgidentifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.type_ (p, n)))
      | `Class (m, n) -> (
          match Signature.sgidentifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.class_ (p, n)))
      | `ClassType (m, n) -> (
          match Signature.sgidentifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.class_type (p, n)))
      | `OpaqueModule m -> identifier (m :> t)

    let rec is_hidden : t -> bool = function
      | `Root (`ModuleType r) -> Path.Resolved.(is_hidden (r :> t))
      | `Root (`Module r) -> Path.Resolved.(is_hidden (r :> t))
      | `Subst (s, _) -> Path.Resolved.(is_hidden (s :> t))
      | `Alias (s, _) -> Path.Resolved.(is_hidden (s :> t))
      | `Module (m, _)
      | `Module_type (m, _)
      | `Type (m, _)
      | `Class (m, _)
      | `ClassType (m, _) ->
          is_hidden (m :> t)
      | `OpaqueModule m -> is_hidden (m :> t)
  end

  type t = Paths_types.Fragment.any

  module Signature = struct
    type t = Paths_types.Fragment.signature
  end

  module Module = struct
    type t = Paths_types.Fragment.module_
  end

  module ModuleType = struct
    type t = Paths_types.Fragment.module_type
  end

  module Type = struct
    type t = Paths_types.Fragment.type_
  end

  type leaf = Paths_types.Fragment.leaf
end

module Reference = struct
  module Resolved = struct
    open Paths_types.Resolved_reference

    type t = Paths_types.Resolved_reference.any

    let rec parent_signature_identifier :
        signature -> Identifier.Signature.t option = function
      | `Identifier id -> Some id
      | `Hidden _s -> None
      | `Alias (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.parent_module_identifier sub
              :> Identifier.Signature.t option)
      | `AliasModuleType (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.parent_module_type_identifier sub
              :> Identifier.Signature.t option)
      | `Module (m, n) -> (
          match parent_signature_identifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_ (p, n)))
      | `ModuleType (m, n) -> (
          match parent_signature_identifier m with
          | None -> None
          | Some p -> Some (Identifier.Mk.module_type (p, n)))

    and parent_type_identifier : datatype -> Identifier.DataType.t option =
      function
      | `Identifier id -> Some id
      | `Type (sg, s) -> (
          match parent_signature_identifier sg with
          | None -> None
          | Some p -> Some (Identifier.Mk.type_ (p, s)))

    and parent_class_signature_identifier :
        class_signature -> Identifier.ClassSignature.t option = function
      | `Identifier id -> Some id
      | `Class (sg, s) -> (
          match parent_signature_identifier sg with
          | None -> None
          | Some p -> Some (Identifier.Mk.class_ (p, s)))
      | `ClassType (sg, s) -> (
          match parent_signature_identifier sg with
          | None -> None
          | Some p -> Some (Identifier.Mk.class_type (p, s)))

    and field_parent_identifier :
        field_parent -> Identifier.FieldParent.t option = function
      | `Identifier id -> Some id
      | (`Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.FieldParent.t option)
      | `Type _ as t ->
          (parent_type_identifier t :> Identifier.FieldParent.t option)

    and unboxed_field_parent_identifier :
        unboxed_field_parent -> Identifier.UnboxedFieldParent.t option =
      function
      | `Identifier id -> Some id
      | `Type _ as t ->
          (parent_type_identifier t :> Identifier.UnboxedFieldParent.t option)

    and label_parent_identifier :
        label_parent -> Identifier.LabelParent.t option = function
      | `Identifier id -> Some id
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c
            :> Identifier.LabelParent.t option)
      | ( `Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _
        | `Type _ ) as r ->
          (field_parent_identifier r :> Identifier.LabelParent.t option)

    and identifier : t -> Identifier.t option = function
      | `Identifier id -> Some id
      | `UnboxedField (p, n) -> (
          match unboxed_field_parent_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.unboxed_field (p, n)))
      | ( `Alias _ | `AliasModuleType _ | `Module _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t option)
      | `Field (p, n) -> (
          match field_parent_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.field (p, n)))
      | `PolyConstructor (s, n) -> (
          (* Uses an identifier for constructor even though it is not
             one. Document must make the links correspond. *)
          match parent_type_identifier s with
          | None -> None
          | Some p -> Some (Identifier.Mk.constructor (p, n)))
      | `Constructor (s, n) -> (
          match parent_type_identifier s with
          | None -> None
          | Some p -> Some (Identifier.Mk.constructor (p, n)))
      | `Extension (p, q) -> (
          match parent_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.extension (p, q)))
      | `ExtensionDecl (p, q, r) -> (
          match parent_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.extension_decl (p, (q, r))))
      | `Exception (p, q) -> (
          match parent_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.exception_ (p, q)))
      | `Value (p, q) -> (
          match parent_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.value (p, q)))
      | `Method (p, q) -> (
          match parent_class_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.method_ (p, q)))
      | `InstanceVariable (p, q) -> (
          match parent_class_signature_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.instance_variable (p, q)))
      | `Label (p, q) -> (
          match label_parent_identifier p with
          | None -> None
          | Some p -> Some (Identifier.Mk.label (p, q)))

    module Signature = struct
      type t = Paths_types.Resolved_reference.signature
    end

    module ClassSignature = struct
      type t = Paths_types.Resolved_reference.class_signature
    end

    module DataType = struct
      type t = Paths_types.Resolved_reference.datatype
    end

    module FieldParent = struct
      type t = Paths_types.Resolved_reference.field_parent
    end

    module UnboxedFieldParent = struct
      type t = Paths_types.Resolved_reference.unboxed_field_parent
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

    module UnboxedField = struct
      type t = Paths_types.Resolved_reference.unboxed_field
    end

    module Extension = struct
      type t = Paths_types.Resolved_reference.extension
    end

    module ExtensionDecl = struct
      type t = Paths_types.Resolved_reference.extension_decl
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

    module Asset = struct
      let identifier = function `Identifier id -> id

      type t = Paths_types.Resolved_reference.asset
    end
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any
  type tag_hierarchy = Paths_types.Reference.tag_hierarchy

  module Signature = struct
    type t = Paths_types.Reference.signature
  end

  module ClassSignature = struct
    type t = Paths_types.Reference.class_signature
  end

  module DataType = struct
    type t = Paths_types.Reference.datatype
  end

  module FragmentTypeParent = struct
    type t = Paths_types.Reference.fragment_type_parent
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

  module UnboxedField = struct
    type t = Paths_types.Reference.unboxed_field
  end

  module Extension = struct
    type t = Paths_types.Reference.extension
  end

  module ExtensionDecl = struct
    type t = Paths_types.Reference.extension_decl
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

  module Asset = struct
    type t = Paths_types.Reference.asset
  end

  module Hierarchy = struct
    type t = Paths_types.Reference.hierarchy
  end
end
