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

  module Id = Paths_types.Identifier

  type t = Id.any

  type t_pv = Id.any_pv

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
    | `ExtensionDecl (_, _, name) -> ExtensionName.to_string name
    | `Exception (_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value (_, name) -> ValueName.to_string name
    | `Class (_, name) -> ClassName.to_string name
    | `ClassType (_, name) -> ClassTypeName.to_string name
    | `Method (_, name) -> MethodName.to_string name
    | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
    | `Label (_, name) -> LabelName.to_string name
    | `SourcePage (dir, name) -> name_aux (dir :> t) ^ name
    | `SourceDir (({ iv = `SourceDir _; _ } as p), n) ->
        name_aux (p :> t) ^ n ^ "/"
    | `SourceDir (_, n) -> "./" ^ n ^ "/"
    | `SourceLocation (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ DefName.to_string anchor
    | `SourceLocationMod x -> name_aux (x :> t)
    | `SourceLocationInternal (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ LocalName.to_string anchor
    | `AssetFile (_, name) -> name

  let rec is_internal : t -> bool =
   fun x ->
    match x.iv with
    | `Root (_, name) -> ModuleName.is_internal name
    | `Page (_, _) -> false
    | `LeafPage (_, _) -> false
    | `Module (_, name) -> ModuleName.is_internal name
    | `Parameter (_, name) -> ModuleName.is_internal name
    | `Result x -> is_internal (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.is_internal name
    | `Type (_, name) -> TypeName.is_internal name
    | `CoreType name -> TypeName.is_internal name
    | `Constructor (parent, _) -> is_internal (parent :> t)
    | `Field (parent, _) -> is_internal (parent :> t)
    | `Extension (parent, _) -> is_internal (parent :> t)
    | `ExtensionDecl (parent, _, _) -> is_internal (parent :> t)
    | `Exception (parent, _) -> is_internal (parent :> t)
    | `CoreException _ -> false
    | `Value (_, name) -> ValueName.is_internal name
    | `Class (_, name) -> ClassName.is_internal name
    | `ClassType (_, name) -> ClassTypeName.is_internal name
    | `Method (parent, _) -> is_internal (parent :> t)
    | `InstanceVariable (parent, _) -> is_internal (parent :> t)
    | `Label (parent, _) -> is_internal (parent :> t)
    | `SourceDir _ | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
    | `SourceLocationInternal _ | `AssetFile _ ->
        false

  let name : [< t_pv ] id -> string = fun n -> name_aux (n :> t)

  let rec full_name_aux : t -> string list =
   fun x ->
    match x.iv with
    | `Root (_, name) -> [ ModuleName.to_string name ]
    | `Page (_, name) -> [ PageName.to_string name ]
    | `LeafPage (_, name) -> [ PageName.to_string name ]
    | `Module (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Parameter (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Result x -> full_name_aux (x :> t)
    | `ModuleType (parent, name) ->
        ModuleTypeName.to_string name :: full_name_aux (parent :> t)
    | `Type (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `CoreType name -> [ TypeName.to_string name ]
    | `Constructor (parent, name) ->
        ConstructorName.to_string name :: full_name_aux (parent :> t)
    | `Field (parent, name) ->
        FieldName.to_string name :: full_name_aux (parent :> t)
    | `Extension (parent, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `ExtensionDecl (parent, _, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `Exception (parent, name) ->
        ExceptionName.to_string name :: full_name_aux (parent :> t)
    | `CoreException name -> [ ExceptionName.to_string name ]
    | `Value (parent, name) ->
        ValueName.to_string name :: full_name_aux (parent :> t)
    | `Class (parent, name) ->
        ClassName.to_string name :: full_name_aux (parent :> t)
    | `ClassType (parent, name) ->
        ClassTypeName.to_string name :: full_name_aux (parent :> t)
    | `Method (parent, name) ->
        MethodName.to_string name :: full_name_aux (parent :> t)
    | `InstanceVariable (parent, name) ->
        InstanceVariableName.to_string name :: full_name_aux (parent :> t)
    | `Label (parent, name) ->
        LabelName.to_string name :: full_name_aux (parent :> t)
    | `AssetFile (parent, name) -> name :: full_name_aux (parent :> t)
    | `SourceDir _ | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
    | `SourceLocationInternal _ ->
        []

  let fullname : [< t_pv ] id -> string list =
   fun n -> List.rev @@ full_name_aux (n :> t)

  let is_internal : [< t_pv ] id -> bool = fun n -> is_internal (n :> t)

  let rec label_parent_aux =
    let open Id in
    fun (n : non_src) ->
      match n with
      | { iv = `Result i; _ } -> label_parent_aux (i :> non_src)
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
      | { iv = `ExtensionDecl (p, _, _); _ }
      | { iv = `Exception (p, _); _ }
      | { iv = `Value (p, _); _ } ->
          (p : signature :> label_parent)
      | { iv = `Label (p, _); _ } -> p
      | { iv = `Method (p, _); _ } | { iv = `InstanceVariable (p, _); _ } ->
          (p : class_signature :> label_parent)
      | { iv = `Constructor (p, _); _ } -> (p : datatype :> label_parent)
      | { iv = `Field (p, _); _ } -> (p : field_parent :> label_parent)

  let label_parent n = label_parent_aux (n :> Id.non_src)

  let equal x y = x.ihash = y.ihash && x.ikey = y.ikey

  let hash x = x.ihash

  let compare x y = compare x.ikey y.ikey

  type any = t

  type any_pv = t_pv

  module type IdSig = sig
    type t
    type t_pv
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Any = struct
    type t = any
    type t_pv = any_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Signature = struct
    type t = Id.signature
    type t_pv = Id.signature_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassSignature = struct
    type t = Id.class_signature
    type t_pv = Id.class_signature_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module DataType = struct
    type t = Id.datatype
    type t_pv = Id.datatype_pv
  end

  module FieldParent = struct
    type t = Paths_types.Identifier.field_parent
    type t_pv = Paths_types.Identifier.field_parent_pv
  end

  module LabelParent = struct
    type t = Id.label_parent
    type t_pv = Id.label_parent_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module RootModule = struct
    type t = Id.root_module
    type t_pv = Id.root_module_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Module = struct
    type t = Id.module_
    type t_pv = Id.module_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module FunctorParameter = struct
    type t = Id.functor_parameter
    type t_pv = Id.functor_parameter_pv
    let equal = equal
    let hash = hash
    let compare = compare

    let functor_arg_pos { iv = `Parameter (p, _); _ } =
      let rec inner_sig = function
        | `Result { iv = p; _ } -> 1 + inner_sig p
        | `Module _ | `ModuleType _ | `Root _ | `Parameter _ -> 1
      in
      inner_sig p.iv
  end

  module FunctorResult = struct
    type t = Id.functor_result
    type t_pv = Id.functor_result_pv
  end

  module ModuleType = struct
    type t = Id.module_type
    type t_pv = Id.module_type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Type = struct
    type t = Id.type_
    type t_pv = Id.type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Constructor = struct
    type t = Id.constructor
    type t_pv = Id.constructor_pv
  end

  module Field = struct
    type t = Id.field
    type t_pv = Id.field_pv
  end

  module Extension = struct
    type t = Id.extension
    type t_pv = Id.extension_pv
  end

  module ExtensionDecl = struct
    type t = Paths_types.Identifier.extension_decl

    type t_pv = Paths_types.Identifier.extension_decl_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Exception = struct
    type t = Id.exception_
    type t_pv = Id.exception_pv
  end

  module Value = struct
    type t = Id.value
    type t_pv = Id.value_pv
  end

  module Class = struct
    type t = Id.class_
    type t_pv = Id.class_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassType = struct
    type t = Id.class_type
    type t_pv = Id.class_type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Method = struct
    type t = Id.method_
    type t_pv = Id.method_pv
  end

  module InstanceVariable = struct
    type t = Id.instance_variable
    type t_pv = Id.instance_variable_pv
  end

  module Label = struct
    type t = Paths_types.Identifier.label
    type t_pv = Paths_types.Identifier.label_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Page = struct
    type t = Id.page
    type t_pv = Id.page_pv
  end

  module ContainerPage = struct
    type t = Id.container_page
    type t_pv = Id.container_page_pv
  end

  module NonSrc = struct
    type t = Paths_types.Identifier.non_src
    type t_pv = Paths_types.Identifier.non_src_pv
  end

  module SourceDir = struct
    type t = Id.source_dir
    type t_pv = Id.source_dir_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module SourcePage = struct
    type t = Id.source_page
    type t_pv = Id.source_page_pv
  end

  module SourceLocation = struct
    type t = Paths_types.Identifier.source_location
    type t_pv = Paths_types.Identifier.source_location_pv
  end

  module AssetFile = struct
    type t = Id.asset_file
    type t_pv = Id.asset_file_pv
  end

  module OdocId = struct
    type t = Id.odoc_id
    type t_pv = Id.odoc_id_pv
  end

  module Path = struct
    module Module = struct
      type t = Id.path_module
      type t_pv = Id.path_module_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ModuleType = struct
      type t = Id.path_module_type
      type t_pv = Id.module_type_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Type = struct
      type t = Id.path_type
      type t_pv = Id.path_type_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module DataType = struct
      type t = Id.path_datatype
      type t_pv = Id.path_datatype_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Constructor = struct
      type t = Id.path_constructor
      type t_pv = Id.constructor_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Value = struct
      type t = Id.path_value
      type t_pv = Id.value_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ClassType = struct
      type t = Id.path_class_type
      type t_pv = Id.path_class_type_pv
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

    let asset_file : Page.t * string -> AssetFile.t =
      mk_parent (fun k -> k) "asset" (fun (p, n) -> `AssetFile (p, n))

    let source_page (container_page, path) =
      let rec source_dir dir =
        match dir with
        | [] -> (container_page : ContainerPage.t :> SourceDir.t)
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
        DataType.t * ConstructorName.t ->
        [> `Constructor of DataType.t * ConstructorName.t ] id =
      mk_parent ConstructorName.to_string "ctor" (fun (p, n) ->
          `Constructor (p, n))

    let field :
        FieldParent.t * FieldName.t ->
        [> `Field of FieldParent.t * FieldName.t ] id =
      mk_parent FieldName.to_string "fld" (fun (p, n) -> `Field (p, n))

    let extension :
        Signature.t * ExtensionName.t ->
        [> `Extension of Signature.t * ExtensionName.t ] id =
      mk_parent ExtensionName.to_string "extn" (fun (p, n) -> `Extension (p, n))

    let extension_decl :
        Signature.t * (ExtensionName.t * ExtensionName.t) ->
        [> `ExtensionDecl of Signature.t * ExtensionName.t * ExtensionName.t ]
        id =
      mk_parent
        (fun (n, m) ->
          ExtensionName.to_string n ^ "." ^ ExtensionName.to_string m)
        "extn-decl"
        (fun (p, (n, m)) -> `ExtensionDecl (p, n, m))

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

    let source_location :
        SourcePage.t * DefName.t ->
        [> `SourceLocation of SourcePage.t * DefName.t ] id =
      mk_parent DefName.to_string "sl" (fun (p, n) -> `SourceLocation (p, n))

    let source_location_mod :
        SourcePage.t -> [> `SourceLocationMod of SourcePage.t ] id =
     fun s ->
      mk_parent
        (fun () -> "__slm__")
        ""
        (fun (s, ()) -> `SourceLocationMod s)
        (s, ())

    let source_location_int :
        SourcePage.t * LocalName.t ->
        [> `SourceLocationInternal of SourcePage.t * LocalName.t ] id =
      mk_parent LocalName.to_string "sli" (fun (p, n) ->
          `SourceLocationInternal (p, n))
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
      | `Value (_, t) when Names.ValueName.is_internal t -> true
      | `Value (p, _) -> inner (p : module_ :> any)
      | `Constructor (p, _) -> inner (p : datatype :> any)
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
      | `CanonicalDataType (_, `Resolved _) -> false
      | `CanonicalDataType (x, _) -> inner (x : datatype :> any)
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

    and parent_datatype_identifier :
        Paths_types.Resolved_path.datatype -> Identifier.DataType.t = function
      | `Identifier id ->
          (id : Identifier.Path.DataType.t :> Identifier.DataType.t)
      | `CanonicalDataType (_, `Resolved p) -> parent_datatype_identifier p
      | `CanonicalDataType (p, _) -> parent_datatype_identifier p
      | `Type (m, n) -> Identifier.Mk.type_ (parent_module_identifier m, n)

    module Module = struct
      type t = Paths_types.Resolved_path.module_

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_path.module_type
    end

    module Type = struct
      type t = Paths_types.Resolved_path.type_
    end

    module DataType = struct
      type t = Paths_types.Resolved_path.datatype
    end

    module Constructor = struct
      type t = Paths_types.Resolved_path.constructor
    end

    module Value = struct
      type t = Paths_types.Resolved_path.value
    end

    module ClassType = struct
      type t = Paths_types.Resolved_path.class_type
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
      | `Value (m, n) -> Identifier.Mk.value (parent_module_identifier m, n)
      | `Constructor (m, n) ->
          Identifier.Mk.constructor (parent_datatype_identifier m, n)
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
      | `CanonicalDataType (_, `Resolved p) -> identifier (p :> t)
      | `CanonicalDataType (p, _) -> identifier (p :> t)
      | `OpaqueModule m -> identifier (m :> t)
      | `OpaqueModuleType mt -> identifier (mt :> t)

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

  module DataType = struct
    type t = Paths_types.Path.datatype
  end

  module Constructor = struct
    type t = Paths_types.Path.constructor
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

      let rec sgidentifier : t -> Identifier.Signature.t = function
        | `Root (`ModuleType i) -> Path.Resolved.parent_module_type_identifier i
        | `Root (`Module i) -> Path.Resolved.parent_module_identifier i
        | `Subst (s, _) -> Path.Resolved.parent_module_type_identifier s
        | `Alias (i, _) -> Path.Resolved.parent_module_identifier i
        | `Module (m, n) -> Identifier.Mk.module_ (sgidentifier m, n)
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

    let rec identifier : t -> Identifier.t = function
      | `Root (`ModuleType _r) -> assert false
      | `Root (`Module _r) -> assert false
      | `Subst (s, _) -> Path.Resolved.identifier (s :> Path.Resolved.t)
      | `Alias (p, _) ->
          (Path.Resolved.parent_module_identifier p :> Identifier.t)
      | `Module (m, n) -> Identifier.Mk.module_ (Signature.sgidentifier m, n)
      | `Module_type (m, n) ->
          Identifier.Mk.module_type (Signature.sgidentifier m, n)
      | `Type (m, n) -> Identifier.Mk.type_ (Signature.sgidentifier m, n)
      | `Class (m, n) -> Identifier.Mk.class_ (Signature.sgidentifier m, n)
      | `ClassType (m, n) ->
          Identifier.Mk.class_type (Signature.sgidentifier m, n)
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

    let rec parent_signature_identifier : signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `Hidden s -> parent_signature_identifier (s :> signature)
      | `Alias (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.parent_module_identifier sub
              :> Identifier.Signature.t)
      | `AliasModuleType (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> signature)
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

    and field_parent_identifier : field_parent -> Identifier.FieldParent.t =
      function
      | `Identifier id -> id
      | (`Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.FieldParent.t)
      | `Type _ as t -> (parent_type_identifier t :> Identifier.FieldParent.t)

    and label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c :> Identifier.LabelParent.t)
      | ( `Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _
        | `Type _ ) as r ->
          (field_parent_identifier r :> Identifier.LabelParent.t)

    and identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | ( `Alias _ | `AliasModuleType _ | `Module _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t)
      | `Field (p, n) -> Identifier.Mk.field (field_parent_identifier p, n)
      | `Constructor (s, n) ->
          Identifier.Mk.constructor
            ((parent_type_identifier s :> Identifier.DataType.t), n)
      | `Extension (p, q) ->
          Identifier.Mk.extension (parent_signature_identifier p, q)
      | `ExtensionDecl (p, q, r) ->
          Identifier.Mk.extension_decl (parent_signature_identifier p, (q, r))
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

    module FieldParent = struct
      type t = Paths_types.Resolved_reference.field_parent
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
end
