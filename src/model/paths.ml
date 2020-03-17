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
    | `Root(_, name) -> UnitName.to_string name
    | `Page(_, name) -> PageName.to_string name
    | `Module(_, name) -> ModuleName.to_string name
    | `Parameter(_, name) -> ParameterName.to_string name
    | `Result x -> name_aux (x :> t)
    | `ModuleType(_, name) -> ModuleTypeName.to_string name
    | `Type(_, name) -> TypeName.to_string name
    | `CoreType name -> TypeName.to_string name
    | `Constructor(_, name) -> ConstructorName.to_string name
    | `Field(_, name) -> FieldName.to_string name
    | `Extension(_, name) -> ExtensionName.to_string name
    | `Exception(_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value(_, name) -> ValueName.to_string name
    | `Class(_, name) -> ClassName.to_string name
    | `ClassType(_, name) -> ClassTypeName.to_string name
    | `Method(_, name) -> MethodName.to_string name
    | `InstanceVariable(_, name) -> InstanceVariableName.to_string name
    | `Label(_, name) -> LabelName.to_string name

  let name : [< t] -> string = fun n -> name_aux (n :> t)

  let rec equal : t -> t -> bool =
    let open Paths_types.Identifier in
    fun p1 p2 ->
      match p1, p2 with
      | `Root (r1, n1), `Root (r2, n2) ->
        UnitName.equal n1 n2 && Root.equal r1 r2
      | `Module (s1,n1), `Module (s2,n2) ->
        ModuleName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any) 
      | `Parameter (s1,n1), `Parameter (s2,n2) ->
        ParameterName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `ModuleType (s1,n1), `ModuleType (s2,n2) ->
        ModuleTypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Type (s1, n1), `Type (s2, n2) ->
        TypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `CoreType n1, `CoreType n2 ->
        TypeName.equal n1 n2
      | `Class (s1, n1), `Class (s2, n2) ->
        ClassName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `ClassType (s1, n1), `ClassType (s2, n2) ->
        ClassTypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Page (r1, n1), `Page (r2, n2) ->
        PageName.equal n1 n2 && Root.equal r1 r2
      | `Constructor(t1, n1), `Constructor(t2, n2) ->
        ConstructorName.equal n1 n2 && equal (t1 : type_ :> any) (t2 : type_ :> any)
      | `Field(s1, n1), `Field(s2, n2) ->
        FieldName.equal n1 n2 && equal (s1 : parent :> any) (s2 : parent :> any)
      | `Extension(s1, n1), `Extension(s2, n2) ->
        ExtensionName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Exception(s1, n1), `Exception(s2, n2) ->
        ExceptionName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `CoreException n1, `CoreException n2 ->
        ExceptionName.equal n1 n2
      | `Value(s1, n1), `Value(s2, n2) ->
        ValueName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Method(s1, n1), `Method(s2, n2) ->
        MethodName.equal n1 n2 && equal (s1 : class_signature :> any) (s2 : class_signature :> any)
      | `InstanceVariable(s1, n1), `InstanceVariable(s2, n2) ->
        InstanceVariableName.equal n1 n2 && equal (s1 : class_signature :> any) (s2 : class_signature :> any)
      | `Label(s1, n1), `Label(s2, n2) ->
        LabelName.equal n1 n2 && equal (s1 : label_parent :> any) (s2 : label_parent :> any)
      | _, _ -> false

  let rec hash (id : Paths_types.Identifier.any) =
    let open Paths_types.Identifier in
    match id with
    | `Root(r, s) ->
      Hashtbl.hash (1, Root.hash r, s)
    | `Page(r, s) ->
      Hashtbl.hash (2, Root.hash r, s)
    | `Module(id, s) ->
      Hashtbl.hash (3, hash (id : signature :> any), s)
    | `Parameter(id, s) ->
      Hashtbl.hash (4, hash (id : signature :> any), s)
    | `Result(s) ->
      Hashtbl.hash (1001, hash (s : signature :> any))
    | `ModuleType(id, s) ->
      Hashtbl.hash (5, hash (id : signature :> any), s)
    | `Type(id, s) ->
      Hashtbl.hash (6, hash (id : signature :> any), s)
    | `CoreType s ->
      Hashtbl.hash (7, s)
    | `Constructor(id, s) ->
      Hashtbl.hash (8, hash (id : type_ :> any), s)
    | `Field(id, s) ->
      Hashtbl.hash (9, hash (id : parent :> any), s)
    | `Extension(id, s) ->
      Hashtbl.hash (10, hash (id : signature :> any), s)
    | `Exception(id, s) ->
      Hashtbl.hash (11, hash (id : signature :> any), s)
    | `CoreException s ->
      Hashtbl.hash (12, s)
    | `Value(id, s) ->
      Hashtbl.hash (13, hash (id : signature :> any), s)
    | `Class(id, s) ->
      Hashtbl.hash (14, hash (id : signature :> any), s)
    | `ClassType(id, s) ->
      Hashtbl.hash (15, hash (id : signature :> any), s)
    | `Method(id, s) ->
      Hashtbl.hash (16, hash (id : class_signature :> any), s)
    | `InstanceVariable(id, s) ->
      Hashtbl.hash (17, hash (id : class_signature :> any), s)
    | `Label(id, s) ->
      Hashtbl.hash (18, hash (id : label_parent :> any ), s)

  module Signature =
  struct
    type t = Paths_types.Identifier.signature

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let rec root = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Parameter(id, _)
      | `Result(id)
      | `ModuleType(id, _) -> root id
  end

  module ClassSignature =
  struct
    type t = Paths_types.Identifier.class_signature

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root = function
      | `Class (s, _) -> Signature.root s
      | `ClassType (s, _) -> Signature.root s
  end

  module DataType =
  struct
    type t = Paths_types.Identifier.datatype

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Parent =
  struct
    type t = Paths_types.Identifier.parent

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module LabelParent =
  struct
    type t = Paths_types.Identifier.label_parent

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root : t -> Root.t = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Parameter(id, _)
      | `Result(id)
      | `ModuleType(id, _) -> Signature.root id
      | `Type(id,_) -> Signature.root id
      | `CoreType _ -> assert false
      | `Class (s, _) -> Signature.root s
      | `ClassType (s, _) -> Signature.root s
      | `Page (r, _) -> r
  end

  module Module =
  struct
    type t = Paths_types.Identifier.module_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Parameter(id, _)
      | `Result(id)
      | `Argument(id, _, _) -> Signature.root id

  end

  module ModuleType =
  struct
    type t = Paths_types.Identifier.module_type

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root : t -> Root.t = function
      | `ModuleType(id, _) -> Signature.root id

  end

  module Type =
  struct
    type t = Paths_types.Identifier.type_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Constructor =
  struct
    type t = Paths_types.Identifier.constructor

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Field =
  struct
    type t = Paths_types.Identifier.field

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Extension =
  struct
    type t = Paths_types.Identifier.extension

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Exception =
  struct
    type t = Paths_types.Identifier.exception_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Value =
  struct
    type t = Paths_types.Identifier.value

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Class =
  struct
    type t = Paths_types.Identifier.class_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Identifier.class_type

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Method =
  struct
    type t = Paths_types.Identifier.method_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module InstanceVariable =
  struct
    type t = Paths_types.Identifier.instance_variable

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Label =
  struct
    type t = Paths_types.Identifier.label

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Page =
  struct
    type t = Paths_types.Identifier.page

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Path =
  struct
    module Module =
    struct
      type t = Paths_types.Identifier.path_module

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module ModuleType =
    struct
      type t = Paths_types.Identifier.path_module_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module Type =
    struct
      type t = Paths_types.Identifier.path_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module ClassType =
    struct
      type t = Paths_types.Identifier.path_class_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    type t = Paths_types.Identifier.path_any
  end



end



module Path = struct

  type t = Paths_types.Path.any

  let rec equal_resolved_path : Paths_types.Resolved_path.any -> Paths_types.Resolved_path.any -> bool =
    let open Paths_types.Resolved_path in
    fun p1 p2 ->
      match p1, p2 with
      | `Identifier id1, `Identifier id2 ->
        Identifier.equal id1 id2
      | `Subst(sub1, p1), `Subst(sub2, p2) ->
        equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
        && equal_resolved_path (sub1 : module_type :> any) (sub2 : module_type :> any)
      | `Alias(sub1, p1), `Alias(sub2, p2) ->
        equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
        && equal_resolved_path (sub1 : module_ :> any) (sub2 : module_ :> any)
      | `SubstAlias(sub1, p1), `SubstAlias(sub2, p2) ->
        equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
        && equal_resolved_path (sub1 : module_ :> any) (sub2 : module_ :> any)
      | `Module(p1, s1), `Module(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Apply(p1, arg1), `Apply(p2, arg2) ->
        equal_path
          (arg1 : Paths_types.Path.module_ :> Paths_types.Path.any)
          (arg2 : Paths_types.Path.module_ :> Paths_types.Path.any)
        && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `ModuleType(p1, s1), `ModuleType(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Type(p1, s1), `Type(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Class(p1, s1), `Class(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `ClassType(p1, s1), `ClassType(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | _, _ -> false

  and equal_path : Paths_types.Path.any -> Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    fun p1 p2 ->
      match p1, p2 with
      | `Resolved p1, `Resolved p2 ->
        equal_resolved_path p1 p2
      | `Root s1, `Root s2 ->
        s1 = s2
      | `Dot(p1, s1), `Dot(p2, s2) ->
        s1 = s2 && equal_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Apply(p1, arg1), `Apply(p2, arg2) ->
        equal_path (arg1 : module_ :> any) (arg2 : module_ :> any)
        && equal_path (p1 : module_ :> any) (p2 : module_ :> any)
      | _, _ -> false

  let rec hash_resolved_path : Paths_types.Resolved_path.any -> int =
    fun p ->
    let open Paths_types.Resolved_path in
    match p with
    | `Identifier id ->
      Identifier.hash id
    | `Subst(sub, p) ->
      Hashtbl.hash (19, hash_resolved_path (sub : module_type :> any),
                    hash_resolved_path (p : module_ :> any))
    | `SubstAlias(sub, p) ->
      Hashtbl.hash (20, hash_resolved_path (sub : module_ :> any),
                    hash_resolved_path (p : module_ :> any))
    | `Hidden p -> Hashtbl.hash (21, hash_resolved_path (p : module_ :> any))
    | `Module(p, s) ->
      Hashtbl.hash (22, hash_resolved_path (p : module_ :> any), s)
    | `Canonical(p, canonical) ->
      Hashtbl.hash (23, hash_resolved_path (p : module_ :> any),
                    hash_path (canonical : Paths_types.Path.module_ :> Paths_types.Path.any))
    | `Apply(p, arg) ->
      Hashtbl.hash (24, hash_resolved_path (p : module_ :> any),
                    hash_path (arg : Paths_types.Path.module_ :> Paths_types.Path.any))
    | `ModuleType(p, s) ->
      Hashtbl.hash (25, hash_resolved_path (p : module_ :> any), s)
    | `SubstT(p, s) ->
      Hashtbl.hash (1006, hash_resolved_path (p :> any), hash_resolved_path (s :> any))
    | `Type(p, s) ->
      Hashtbl.hash (26, hash_resolved_path (p : module_ :> any), s)
    | `Class(p, s) ->
      Hashtbl.hash (27, hash_resolved_path (p : module_ :> any), s)
    | `ClassType(p, s) ->
      Hashtbl.hash (28, hash_resolved_path (p : module_ :> any), s)
    | `Alias(m1, m2) ->
      Hashtbl.hash (20, hash_resolved_path (m1 : module_ :> any), hash_resolved_path (m2 : module_ :> any))

  and hash_path : Paths_types.Path.any -> int =
    fun p ->
    let open Paths_types.Path in
    match p with
    | `Resolved p -> hash_resolved_path p
    | `Root s ->
      Hashtbl.hash (29, s)
    | `Forward s ->
      Hashtbl.hash (30, s)
    | `Dot(p, s) ->
      Hashtbl.hash (31, hash_path (p : module_ :> any), s)
    | `Apply(p, arg) ->
      Hashtbl.hash (32, hash_path (p : module_ :> any), hash_path (arg : module_ :> any))

  let rec is_resolved_hidden : Paths_types.Resolved_path.any -> bool =
    let open Paths_types.Resolved_path in
    function
    | `Identifier _ -> false
    | `Canonical (_, `Resolved _) -> false
    | `Canonical (x, _) -> is_resolved_hidden (x : module_ :> any)
    | `Hidden _ -> true
    | `Subst(p1, p2) -> is_resolved_hidden (p1 : module_type :> any) || is_resolved_hidden (p2 : module_ :> any)
    | `SubstAlias(p1, p2) -> is_resolved_hidden (p1 : module_ :> any) || is_resolved_hidden (p2 : module_ :> any)
    | `Module (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Apply (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `ModuleType (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Type (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Class (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `ClassType (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Alias (p1, p2) -> is_resolved_hidden (p1 : module_ :> any) && (is_resolved_hidden (p2 : module_ :> any))
    | `SubstT (p1, p2) -> is_resolved_hidden (p1 :> any) || is_resolved_hidden (p2 :> any)

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    function
    | `Resolved r -> is_resolved_hidden r
    | `Root _ -> false
    | `Forward _ -> false
    | `Dot(p, _) -> is_path_hidden (p : module_ :> any)
    | `Apply(p1, p2) -> is_path_hidden (p1 : module_ :> any) || is_path_hidden (p2 : module_ :> any)

  module Resolved = struct


    type t = Paths_types.Resolved_path.any

    let rec parent_module_type_identifier : Paths_types.Resolved_path.module_type -> Identifier.Signature.t = function
      | `Identifier id -> (id : Identifier.ModuleType.t :> Identifier.Signature.t)
      | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)
      | `SubstT(m, _n) -> parent_module_type_identifier m

    and parent_module_identifier : Paths_types.Resolved_path.module_ -> Identifier.Signature.t = function
      | `Identifier id -> (id : Identifier.Module.t :> Identifier.Signature.t)
      | `Subst(sub, _) -> parent_module_type_identifier sub
      | `SubstAlias(sub, _) -> parent_module_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module(m, n) -> `Module(parent_module_identifier m, n)
      | `Canonical(_, `Resolved p) -> parent_module_identifier p
      | `Canonical(p, _) -> parent_module_identifier p
      | `Apply(m, _) -> parent_module_identifier m
      | `Alias(sub, orig) ->
        if is_path_hidden (`Resolved (sub :> t))
        then parent_module_identifier orig
        else parent_module_identifier sub

    let equal p1 p2 = equal_resolved_path p1 p2

    let hash p = hash_resolved_path p


    let rec equal_identifier :
      Identifier.t -> t -> bool =
      fun id p ->
      match id, p with
      | _, `Identifier id' -> Identifier.equal id id'
      | `Module (id, s1), `Module (p, s2) when s1 = s2 ->
        equal_identifier
          (id : Paths_types.Identifier.signature :> Paths_types.Identifier.any)
          (p : Paths_types.Resolved_path.module_ :> Paths_types.Resolved_path.any)
      | `ModuleType (id, s1), `ModuleType (p, s2) when s1 = s2 ->
        equal_identifier
          (id : Paths_types.Identifier.signature :> Paths_types.Identifier.any)
          (p : Paths_types.Resolved_path.module_ :> Paths_types.Resolved_path.any)
      | _, _ ->
        false

    module Module = struct

      type t = Paths_types.Resolved_path.module_

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

        let rec identifier = function
        | `Identifier id -> id
        | `Subst(_, p) -> identifier p
        | `SubstAlias(_, p) -> identifier p
        | `Hidden p -> identifier p
        | `Module(m, n) -> `Module(parent_module_identifier m, n)
        | `Canonical(_, `Resolved p) -> identifier p
        | `Canonical(p, _) -> identifier p
        | `Apply(m, _) -> identifier m
        | `Alias(sub,orig) ->
          if is_path_hidden (`Resolved (sub :> Paths_types.Resolved_path.any))
          then identifier orig
          else identifier sub
          
      let rec canonical_ident = function
        | `Identifier _id -> None
        | `Subst(_,_) -> None
        | `SubstAlias(_,_) -> None
        | `Hidden p -> canonical_ident p
        | `Module(p, n) -> begin
            match canonical_ident p with | Some x -> Some (`Module((x :>Identifier.Signature.t), n)) | None -> None
        end 
        | `Canonical(_, `Resolved p) -> Some (identifier p)
        | `Canonical(_, _) -> None
        | `Apply(_,_) -> None
        | `Alias(_,_) -> None

      let equal_identifier :
        Identifier.Path.Module.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.Module.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    module ModuleType = struct

      type t = Paths_types.Resolved_path.module_type

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier = function
        | `Identifier id -> id
        | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)
        | `SubstT(s,_) -> identifier s

      let canonical_ident : t -> Identifier.ModuleType.t option = function
        | `Identifier _id -> None
        | `ModuleType (p, n) -> begin
            match Module.canonical_ident p with | Some x -> Some (`ModuleType((x :>Identifier.Signature.t), n)) | None -> None
        end 
        | `SubstT (_, _) -> None


      let equal_identifier :
        Identifier.Path.ModuleType.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.ModuleType.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    module Type = struct

      type t = Paths_types.Resolved_path.type_

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `Type(m, n) -> `Type(parent_module_identifier m, n)
        | `Class(m, n) -> `Class(parent_module_identifier m, n)
        | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)

      let equal_identifier :
        Identifier.Path.Type.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.Type.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)

    end

    module ClassType = struct

      type t = Paths_types.Resolved_path.class_type

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `Class(m, n) -> `Class(parent_module_identifier m, n)
        | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)

      let equal_identifier :
        Identifier.Path.ClassType.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.ClassType.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    let module_of_t : t -> Module.t = function
        | `Identifier (#Identifier.Path.Module.t)
        | #Paths_types.Resolved_path.module_no_id as x -> x
        | _ -> assert false

    let module_type_of_t : t -> ModuleType.t = function
        | `Identifier (#Identifier.Path.ModuleType.t)
        | #Paths_types.Resolved_path.module_type_no_id as x -> x
        | _ -> assert false

    let type_of_t : t -> Type.t = function
        | `Identifier (#Identifier.Path.Type.t)
        | #Paths_types.Resolved_path.type_no_id as x -> x
        | _ -> assert false

    let class_type_of_t : t -> ClassType.t = function
        | `Identifier (#Identifier.Path.ClassType.t)
        | #Paths_types.Resolved_path.class_type_no_id as x -> x
        | _ -> assert false


    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `Subst(_, p) -> identifier (p :> t)
      | `SubstAlias(_, p) -> identifier (p :> t)
      | `Hidden p -> identifier (p :> t)
      | `Module(m, n) -> `Module(parent_module_identifier m, n)
      | `Canonical(_, `Resolved p) -> identifier (p :> t)
      | `Canonical(p, _) -> identifier (p :> t)
      | `Apply(m, _) -> identifier (m :> t)
      | `Type(m, n) -> `Type(parent_module_identifier m, n)
      | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)
      | `Class(m, n) -> `Class(parent_module_identifier m, n)
      | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)
      | `Alias(sub, orig) ->
        if is_path_hidden (`Resolved (sub :> t))
        then identifier (orig :> t)
        else identifier (sub :> t)
      | `SubstT(p, _) -> identifier (p :> t)

  end

  module Module =
  struct
    type t = Paths_types.Path.module_

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module ModuleType =
  struct
    type t = Paths_types.Path.module_type

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module Type =
  struct
    type t = Paths_types.Path.type_

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Path.class_type

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  let module_of_t : t -> Module.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_module))
    | `Resolved (#Paths_types.Resolved_path.module_no_id)
    | `Root _
    | `Forward _
    | `Dot (_,_)
    | `Apply (_,_) as x -> x
    | _ -> assert false

  let module_type_of_t : t -> ModuleType.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_module_type))
    | `Resolved (#Paths_types.Resolved_path.module_type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let type_of_t : t -> Type.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_type))
    | `Resolved (#Paths_types.Resolved_path.type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let class_type_of_t : t -> ClassType.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_class_type))
    | `Resolved (#Paths_types.Resolved_path.class_type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let module_ : Module.t -> ModuleName.t -> Module.t = fun p name ->
    match p with
    | `Resolved p -> `Resolved (`Module(p, name))
    | p -> `Dot(p, ModuleName.to_string name)

  let apply p arg =
    match p with
    | `Resolved p -> `Resolved (`Apply(p, arg))
    | p -> `Apply(p, arg)

  let module_type p name =
    match p with
    | `Resolved p -> `Resolved (`ModuleType(p, name))
    | p -> `Dot(p, ModuleTypeName.to_string name)

  let is_hidden = is_path_hidden

  let equal = equal_path

  let hash = hash_path
end



module Fragment = struct

  module Resolved = struct

    type t = Paths_types.Resolved_fragment.any

    type root = Paths_types.Resolved_fragment.root

    let t_of_module m = (m : Paths_types.Resolved_fragment.module_ :> t)
    let t_of_signature s = (s : Paths_types.Resolved_fragment.signature :> t)

    let equal p1 p2 =
      let rec loop : t -> t -> bool =
        fun p1 p2 ->
          match p1, p2 with
          | `Root (`ModuleType i1), `Root (`ModuleType i2) -> Path.Resolved.ModuleType.equal i1 i2
          | `Root (`Module i1), `Root (`Module i2) -> Path.Resolved.Module.equal i1 i2
          | `Subst(sub1, p1), `Subst(sub2, p2) ->
            Path.Resolved.ModuleType.equal sub1 sub2
            && loop (t_of_module p1) (t_of_module p2)
          | `SubstAlias(sub1, p1), `SubstAlias(sub2, p2) ->
            Path.Resolved.Module.equal sub1 sub2
            && loop (t_of_module p1) (t_of_module p2)
          | `Module(p1, s1), `Module(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `Type(p1, s1), `Type(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `Class(p1, s1), `Class(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `ClassType(p1, s1), `ClassType(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | _, _ -> false
      in
      loop p1 p2


    let hash p =
      let rec loop : t -> int =
        fun p ->
          match p with
          | `Root (`ModuleType i) -> Hashtbl.hash (32, Path.Resolved.ModuleType.hash i)
          | `Root (`Module i) -> Hashtbl.hash (33, Path.Resolved.Module.hash i)
          | `Subst(sub, p) ->
            Hashtbl.hash (34, Path.Resolved.ModuleType.hash sub, loop (t_of_module p))
          | `SubstAlias(sub, p) ->
            Hashtbl.hash (35, Path.Resolved.Module.hash sub, loop (t_of_module p))
          | `Module(p, s) ->
            Hashtbl.hash (36, loop (t_of_signature p), s)
          | `Type(p, s) ->
            Hashtbl.hash (37, loop (t_of_signature p), s)
          | `Class(p, s) ->
            Hashtbl.hash (38, loop (t_of_signature p), s)
          | `ClassType(p, s) ->
            Hashtbl.hash (39, loop (t_of_signature p), s)
      in
      loop p

    let sig_of_mod m =
      let open Paths_types.Resolved_fragment in
      (m : module_ :> signature)

    type base_name =
      | Base of root
      | Branch of ModuleName.t * Paths_types.Resolved_fragment.signature

    let rec split_parent : Paths_types.Resolved_fragment.signature -> base_name = function
      | `Root i -> Base i
      | `Subst(_, p) -> split_parent (sig_of_mod p)
      | `SubstAlias(_, p) -> split_parent (sig_of_mod p)
      | `Module(p, name) ->
        match split_parent p with
        | Base i -> Branch(name, `Root i)
        | Branch(base, m) -> Branch(base, `Module(m, name))


    module Signature =
    struct
      type t = Paths_types.Resolved_fragment.signature

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let rec split : t -> string * t option = function
        | `Root _ -> "", None
        | `Subst(_,p) -> split (sig_of_mod p)
        | `SubstAlias(_,p) -> split (sig_of_mod p)
        | `Module (m, name) -> begin
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch(base,m) -> ModuleName.to_string base, Some (`Module(m,name))
          end

      let rec identifier : t -> Identifier.Signature.t =
        function
          | `Root (`ModuleType i) -> (Path.Resolved.ModuleType.identifier i :> Identifier.Signature.t)
          | `Root (`Module i) -> (Path.Resolved.Module.identifier i :> Identifier.Signature.t)
          | `Subst(s, _) -> (Path.Resolved.ModuleType.identifier s :> Identifier.Signature.t)
          | `SubstAlias(_, p) -> identifier (sig_of_mod p)
          | `Module(m, n) -> `Module (identifier m, n)

    end

    module Module =
    struct
      type t = Paths_types.Resolved_fragment.module_

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let rec split : t -> string * t option = function
        | `Subst(_,p) -> split p
        | `SubstAlias(_,p) -> split p
        | `Module (m, name) -> begin
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch(base,m) -> ModuleName.to_string base, Some (`Module(m,name))
          end

    end

    module Type =
    struct
      type t = Paths_types.Resolved_fragment.type_

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let split : t -> string * t option =
        function
        | `Type (m,name) -> begin
            match split_parent m with
            | Base _ -> TypeName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`Type(m, name))
          end
        | `Class(m, name) -> begin
            match split_parent m with
            | Base _ -> ClassName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`Class(m, name))
          end
        | `ClassType(m, name) -> begin
            match split_parent m with
            | Base _ -> ClassTypeName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`ClassType(m, name))
          end


    end

    let signature_of_t : t -> Signature.t = function
      | #Signature.t as x -> x
      | _ -> assert false

    let module_of_t : t -> Module.t = function
      | #Module.t as x -> x
      | _ -> assert false

    let type_of_t : t -> Type.t = function
      | #Type.t as x -> x
      | _ -> assert false

    let rec identifier : t -> Identifier.t =
       function
        | `Root (`ModuleType r) -> (Path.Resolved.ModuleType.identifier r :> Identifier.t)
        | `Root (`Module r) -> (Path.Resolved.Module.identifier r :> Identifier.t)
        | `Subst(s, _) ->
          Format.fprintf Format.err_formatter "Got a subst!\n%!";
          Path.Resolved.identifier (s :> Path.Resolved.t)
        | `SubstAlias(_, p) -> identifier (p :> t)
        | `Module(m, n) -> `Module (Signature.identifier m, n)
        | `Type(m, n) -> `Type(Signature.identifier m, n)
        | `Class(m, n) -> `Class(Signature.identifier m, n)
        | `ClassType(m, n) -> `ClassType(Signature.identifier m, n)


  end

  type t = Paths_types.Fragment.any

  type base_name =
    | Base of Resolved.root option
    | Branch of ModuleName.t * Paths_types.Fragment.signature

  let rec split_parent : Paths_types.Fragment.signature -> base_name =
    function
    | `Root -> Base None
    | `Resolved r -> begin
        match Resolved.split_parent r with
        | Resolved.Base i -> Base (Some i)
        | Resolved.Branch(base, m) -> Branch(base, `Resolved m)
      end
    | `Dot(m,name) -> begin
        match split_parent m with
        | Base None -> Branch(ModuleName.of_string name,`Root)
        | Base (Some i) -> Branch(ModuleName.of_string name, `Resolved (`Root i))
        | Branch(base,m) -> Branch(base, `Dot(m,name))
      end

  let equal p1 p2 =
    let rec loop : t -> t -> bool =
      fun p1 p2 ->
        match p1, p2 with
        | `Resolved p1, `Resolved p2 ->
          Resolved.equal p1 p2
        | `Dot(p1, s1), `Dot(p2, s2) ->
          s1 = s2 && loop (p1 : Paths_types.Fragment.signature :> t) (p2 : Paths_types.Fragment.signature :> t)
        | _, _ -> false
    in
    loop p1 p2

  let hash p =
    let rec loop : t -> int =
      fun p ->
        match p with
        | `Resolved p -> Resolved.hash p
        | `Dot(p, s) ->
          Hashtbl.hash (40, loop (p : Paths_types.Fragment.signature :> t), s)
        | `Root ->
          Hashtbl.hash (41)
    in
    loop p

  module Signature = struct
    type t = Paths_types.Fragment.signature

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Root -> "", None
      | `Resolved r ->
        let base, m = Resolved.Signature.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base _ -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))
      
  end

  module Module = struct
    type t = Paths_types.Fragment.module_

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Resolved r ->
        let base, m = Resolved.Module.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base _ -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

  end

  module Type = struct
    type t = Paths_types.Fragment.type_

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Resolved r ->
        let base, m = Resolved.Type.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base _ -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

  end

  let signature_of_t : t -> Signature.t = function
    | `Resolved (#Resolved.Signature.t)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let module_of_t : t -> Module.t = function
    | `Resolved (#Resolved.Module.t)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let type_of_t : t -> Type.t = function
    | `Resolved (#Resolved.Type.t)
    | `Dot (_,_) as x -> x
    | _ -> assert false

end


module Reference = struct

  let rec hash_resolved : Paths_types.Resolved_reference.any -> int =
    fun p ->
      let open Paths_types.Resolved_reference in
      match p with
      | `Identifier id ->
        Identifier.hash id
      | `SubstAlias (r1, r2) ->
        Hashtbl.hash (41, Path.Resolved.Module.hash r1, hash_resolved (r2 : module_ :> any ))
      | `Module(p, s) ->
        Hashtbl.hash (42, hash_resolved (p : signature :> any), s)
      | `Canonical (rp, p) ->
        Hashtbl.hash (43, hash_resolved (rp : module_ :> any), hash_reference (p : Paths_types.Reference.module_ :> Paths_types.Reference.any))
      | `ModuleType(p, s) ->
        Hashtbl.hash (44, hash_resolved (p : signature :> any), s)
      | `Type(p, s) ->
        Hashtbl.hash (45, hash_resolved (p : signature :> any), s)
      | `Constructor(p, s) ->
        Hashtbl.hash (46, hash_resolved (p : datatype :> any), s)
      | `Field(p, s) ->
        Hashtbl.hash (47, hash_resolved (p : parent :> any), s)
      | `Extension(p, s) ->
        Hashtbl.hash (48, hash_resolved (p : signature :> any), s)
      | `Exception(p, s) ->
        Hashtbl.hash (49, hash_resolved (p : signature :> any), s)
      | `Value(p, s) ->
        Hashtbl.hash (50, hash_resolved (p : signature :> any), s)
      | `Class(p, s) ->
        Hashtbl.hash (51, hash_resolved (p : signature :> any), s)
      | `ClassType(p, s) ->
        Hashtbl.hash (52, hash_resolved (p : signature :> any), s)
      | `Method(p, s) ->
        Hashtbl.hash (53, hash_resolved (p : class_signature :> any), s)
      | `InstanceVariable(p, s) ->
        Hashtbl.hash (54, hash_resolved (p : class_signature :> any), s)
      | `Label(p, s) ->
        Hashtbl.hash (55, hash_resolved (p : label_parent :> any), s)
      | `Hidden p ->
        Hashtbl.hash (1002, hash_resolved (p :> any))
  
  and hash_reference : Paths_types.Reference.any -> int =
    fun p ->
      let open Paths_types.Reference in
      match p with
      | `Resolved p -> hash_resolved p
      | `Root (s, k) -> Hashtbl.hash (56, s, k)
      | `Dot (p,s) -> Hashtbl.hash (57, hash_reference (p : label_parent :> any), s)
      | `Module (p,s) -> Hashtbl.hash (58, hash_reference (p : signature :> any), s)
      | `ModuleType (p,s) -> Hashtbl.hash (59, hash_reference (p : signature :> any), s)
      | `Type (p,s) -> Hashtbl.hash (60, hash_reference (p : signature :> any), s)
      | `Constructor (p,s) -> Hashtbl.hash (61, hash_reference (p : datatype :> any), s)
      | `Field (p,s) -> Hashtbl.hash (62, hash_reference (p : parent :> any), s)
      | `Extension (p,s) -> Hashtbl.hash (63, hash_reference (p : signature :> any), s)
      | `Exception (p,s) -> Hashtbl.hash (64, hash_reference (p : signature :> any), s)
      | `Value (p,s) -> Hashtbl.hash (65, hash_reference (p : signature :> any), s)
      | `Class (p,s) -> Hashtbl.hash (66, hash_reference (p : signature :> any), s)
      | `ClassType (p,s) -> Hashtbl.hash (67, hash_reference (p : signature :> any), s)
      | `Method (p,s) -> Hashtbl.hash (68, hash_reference (p : class_signature :> any), s)
      | `InstanceVariable (p,s) -> Hashtbl.hash (69, hash_reference (p : class_signature :> any), s)
      | `Label (p,s) -> Hashtbl.hash (70, hash_reference (p : label_parent :> any), s)

  let rec resolved_equal : Paths_types.Resolved_reference.any -> Paths_types.Resolved_reference.any -> bool =
    let open Paths_types.Resolved_reference in
    fun id1 id2 ->
      match id1, id2 with
      | `Identifier id1, `Identifier id2 ->
        Identifier.equal id1 id2
      | `SubstAlias (r1, m1), `SubstAlias (r2, m2) ->
        Path.Resolved.equal
          (r1 : Path.Resolved.Module.t :> Path.Resolved.t)
          (r2 : Path.Resolved.Module.t :> Path.Resolved.t)
        && resolved_equal (m1 : module_ :> any) (m2 : module_ :> any)
      | `Module(r1, s1), `Module(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Canonical(m1, r1), `Canonical(m2, r2) ->
        equal
          (r1 : Paths_types.Reference.module_ :> Paths_types.Reference.any)
          (r2 : Paths_types.Reference.module_ :> Paths_types.Reference.any)
        && resolved_equal (m1 : module_ :> any) (m2 : module_ :> any)
      | `ModuleType(r1, s1), `ModuleType(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Type(r1, s1), `Type(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Constructor(r1, s1), `Constructor(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : datatype :> any) (r2 : datatype :> any)
      | `Field(r1, s1), `Field(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : parent :> any) (r2 : parent :> any)
      | `Extension(r1, s1), `Extension(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Exception(r1, s1), `Exception(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Value(r1, s1), `Value(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Class(r1, s1), `Class(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `ClassType(r1, s1), `ClassType(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Method(r1, s1), `Method(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `InstanceVariable(r1, s1), `InstanceVariable(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `Label(r1, s1), `Label(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | _, _ -> false

  and equal : Paths_types.Reference.any -> Paths_types.Reference.any -> bool =
    let open Paths_types.Reference in
    fun r1 r2 ->
      match r1, r2 with
      | `Resolved r1, `Resolved r2 ->
        resolved_equal r1 r2
      | `Root (s1, k1), `Root (s2, k2) ->
        s1 = s2 && k1 = k2
      | `Dot(r1, s1), `Dot(r2, s2) ->
        s1 = s2 && equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | `Module(r1, s1), `Module(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `ModuleType(r1, s1), `ModuleType(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Type(r1, s1), `Type(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Constructor(r1, s1), `Constructor(r2, s2) ->
        s1 = s2 && equal (r1 : datatype :> any) (r2 : datatype :> any)
      | `Field(r1, s1), `Field(r2, s2) ->
        s1 = s2 && equal (r1 : parent :> any) (r2 : parent :> any)
      | `Extension(r1, s1), `Extension(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Exception(r1, s1), `Exception(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Class(r1, s1), `Class(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `ClassType(r1, s1), `ClassType(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Method(r1, s1), `Method(r2, s2) ->
        s1 = s2 && equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `InstanceVariable(r1, s1), `InstanceVariable(r2, s2) ->
        s1 = s2 && equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `Label(r1, s1), `Label(r2, s2) ->
        s1 = s2 && equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | _, _ -> false

  module Resolved = struct
    open Paths_types.Resolved_reference

  (*  let rec is_hidden : label_parent -> bool =
        function
        | `Identifier _ -> false
        | `Hidden _ -> true
        | `SubstAlias (sub, orig) ->
          Path.Resolved.Module.is_hidden sub && is_hidden (orig :> label_parent)
        | `Type (p, _)
        | `Class (p, _)
        | `ClassType (p, _)
        | `Module (p, _)
        | `ModuleType (p, _) -> is_hidden (p :> label_parent)
        | `Canonical (_, `Resolved _) -> false
        | `Canonical (p, _) -> is_hidden (p :> label_parent)*)

    let rec parent_signature_identifier : signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `Hidden s -> parent_signature_identifier (s :> signature)
      | `SubstAlias(sub, orig) -> 
        if Path.Resolved.Module.is_hidden sub
        then parent_signature_identifier (orig :> signature)
        else (Path.Resolved.Module.identifier sub :> Identifier.Signature.t)
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        parent_signature_identifier (r : module_ :> signature)
      | `Canonical (r, _) -> parent_signature_identifier (r : module_ :> signature)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)

    let parent_type_identifier : datatype -> Identifier.DataType.t =
      function
      | `Identifier id -> id
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)

    let parent_class_signature_identifier :
      class_signature -> Identifier.ClassSignature.t =
      function
      | `Identifier id -> id
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)

    let rec parent_identifier : parent -> Identifier.Parent.t =
      function
      | `Identifier id -> id
      | `Hidden m -> parent_identifier (m :> parent)
      | `SubstAlias(sub, _) ->
        let id = Path.Resolved.parent_module_identifier sub in
        (id : Identifier.Signature.t :> Identifier.Parent.t)
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        parent_identifier (r : module_ :> parent)
      | `Canonical (r, _) -> parent_identifier (r : module_ :> parent)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)

    let rec label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | `Hidden s -> label_parent_identifier (s :> label_parent)
      | `SubstAlias(sub, orig) -> 
        if Path.Resolved.Module.is_hidden sub
        then (parent_signature_identifier (orig :> signature) :> Identifier.LabelParent.t)
        else (Path.Resolved.Module.identifier sub :> Identifier.LabelParent.t)
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        label_parent_identifier (r : module_ :> label_parent)
      | `Canonical (r, _) -> label_parent_identifier (r : module_ :> label_parent)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)



    module Signature =
    struct
      type t = Paths_types.Resolved_reference.signature

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Signature.t = function
        | `Identifier id -> id
        | `Hidden s -> identifier (s :> t)
        | `SubstAlias(sub, orig) -> 
          if Path.Resolved.Module.is_hidden sub
          then parent_signature_identifier (orig :> signature)
          else (Path.Resolved.Module.identifier sub :> Identifier.Signature.t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> signature)
        | `Canonical(p, _) -> identifier (p : module_ :> signature)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)

    end

    module ClassSignature =
    struct
      type t = Paths_types.Resolved_reference.class_signature

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.ClassSignature.t = function
        | `Identifier id -> id
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)

    end


    module DataType =
    struct
      type t = Paths_types.Resolved_reference.datatype

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.DataType.t = function
        | `Identifier id -> id
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)
    end


    module Parent =
    struct
      type t = Paths_types.Resolved_reference.parent

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Parent.t = function
        | `Identifier id -> id
        | `Hidden p -> identifier (p :> t)
        | `SubstAlias(sub, orig) -> 
          if Path.Resolved.Module.is_hidden sub
          then (parent_signature_identifier (orig :> signature) :> Identifier.Parent.t)
          else (Path.Resolved.Module.identifier sub :> Identifier.Parent.t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)

    end


    module LabelParent =
    struct
      type t = Paths_types.Resolved_reference.label_parent

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.LabelParent.t = function
        | `Identifier id -> id
        | `Hidden p -> identifier (p :> t)
        | `SubstAlias(sub, orig) -> 
        if Path.Resolved.Module.is_hidden sub
        then (parent_signature_identifier (orig :> signature) :> Identifier.LabelParent.t)
        else (Path.Resolved.Module.identifier sub :> Identifier.LabelParent.t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)

    end

    module Module =
    struct
      type t = Paths_types.Resolved_reference.module_

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Module.t = function
        | `Identifier id -> id
        | `Hidden p -> identifier (p :> t)
        | `SubstAlias(sub, orig) -> 
        if Path.Resolved.Module.is_hidden sub
        then identifier orig
        else (Path.Resolved.Module.identifier sub)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)

    end

  module ModuleType =
    struct
      type t = Paths_types.Resolved_reference.module_type

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.ModuleType.t = function
        | `Identifier id -> id
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)

    end

    module Type =
    struct
      type t = Paths_types.Resolved_reference.type_

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.Path.Type.t = function
        | `Identifier id -> id
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)
        | `Class(s,n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s,n) -> `ClassType(parent_signature_identifier s, n)

    end

    module Constructor =
    struct
      type t = Paths_types.Resolved_reference.constructor

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_constructor = function
        | `Identifier id -> id
        | `Constructor(s, n) -> `Constructor(parent_type_identifier s, n)
        | `Extension(s, n) -> `Extension(parent_signature_identifier s, n)
        | `Exception(s, n) -> `Exception(parent_signature_identifier s, n)
    end

    module Field =
    struct
      type t = Paths_types.Resolved_reference.field

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.Field.t = function
        | `Identifier id -> id
        | `Field(p, n) -> `Field(parent_identifier p, n)

    end

    module Extension =
    struct
      type t = Paths_types.Resolved_reference.extension

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_extension  = function
        | `Identifier id -> id
        | `Extension(p,q) -> `Extension(parent_signature_identifier p, q)
        | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)

    end

    module Exception =
    struct
      type t = Paths_types.Resolved_reference.exception_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_exception  = function
        | `Identifier id -> id
        | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)
    end

    module Value =
    struct
      type t = Paths_types.Resolved_reference.value

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_value  = function
        | `Identifier id -> id
        | `Value(p,q) -> `Value(parent_signature_identifier p, q)

    end

    module Class =
    struct
      type t = Paths_types.Resolved_reference.class_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_class  = function
        | `Identifier id -> id
        | `Class(p,q) -> `Class(parent_signature_identifier p, q)

    end

    module ClassType =
    struct
      type t = Paths_types.Resolved_reference.class_type

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_class_type  = function
        | `Identifier id -> id
        | `Class(p,q) -> `Class(parent_signature_identifier p, q)
        | `ClassType(p,q) -> `ClassType(parent_signature_identifier p, q)

    end


    module Method =
    struct
      type t = Paths_types.Resolved_reference.method_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_method  = function
        | `Identifier id -> id
        | `Method(p,q) -> `Method(parent_class_signature_identifier p, q)

    end

    module InstanceVariable =
    struct
      type t = Paths_types.Resolved_reference.instance_variable

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_instance_variable  = function
        | `Identifier id -> id
        | `InstanceVariable(p,q) -> `InstanceVariable(parent_class_signature_identifier p, q)

    end

    module Label =
    struct
      type t = Paths_types.Resolved_reference.label

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_label  = function
        | `Identifier id -> id
        | `Label(p,q) -> `Label(label_parent_identifier p, q)

    end

    module Page =
    struct
      type t = Paths_types.Resolved_reference.page

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_page  = function
        | `Identifier id -> id

    end

    type t = Paths_types.Resolved_reference.any

    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `SubstAlias(sub, orig) -> 
        if Path.Resolved.Module.is_hidden sub
        then (parent_signature_identifier (orig :> signature) :> Identifier.t)
        else (Path.Resolved.Module.identifier sub :> Identifier.t)
      | `Module(s, n) -> `Module(parent_signature_identifier s, n)
      | `Canonical(_, `Resolved p) -> identifier (p :> t)
      | `Hidden p -> identifier (p :> t)
      | `Canonical(p, _) -> identifier (p :> t)
      | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
      | `Field(p, n) -> `Field(parent_identifier p, n)
      | `Type(s, n) -> `Type(parent_signature_identifier s, n)
      | `Constructor(s, n) -> `Constructor(parent_type_identifier s, n)
      | `Extension(p,q) -> `Extension(parent_signature_identifier p, q)
      | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)
      | `Value(p,q) -> `Value(parent_signature_identifier p, q)
      | `Class(p,q) -> `Class(parent_signature_identifier p, q)
      | `ClassType(p,q) -> `ClassType(parent_signature_identifier p, q)
      | `Method(p,q) -> `Method(parent_class_signature_identifier p, q)
      | `InstanceVariable(p,q) -> `InstanceVariable(parent_class_signature_identifier p, q)
      | `Label(p,q) -> `Label(label_parent_identifier p, q)


  end
  type t = Paths_types.Reference.any

  let equal : t -> t -> bool = equal

  let hash p = hash_reference p

  module Signature =
  struct
    type t = Paths_types.Reference.signature
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ClassSignature =
  struct
    type t = Paths_types.Reference.class_signature
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module DataType =
  struct
    type t = Paths_types.Reference.datatype
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Parent =
  struct
    type t = Paths_types.Reference.parent
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module LabelParent =
  struct
    type t = Paths_types.Reference.label_parent
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Module =
  struct
    type t = Paths_types.Reference.module_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ModuleType =
  struct
    type t = Paths_types.Reference.module_type
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Type =
  struct
    type t = Paths_types.Reference.type_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Constructor =
  struct
    type t = Paths_types.Reference.constructor
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Field =
  struct
    type t = Paths_types.Reference.field
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Extension =
  struct
    type t = Paths_types.Reference.extension
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Exception =
  struct
    type t = Paths_types.Reference.exception_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Value =
  struct
    type t = Paths_types.Reference.value
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Class =
  struct
    type t = Paths_types.Reference.class_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Reference.class_type
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Method =
  struct
    type t = Paths_types.Reference.method_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module InstanceVariable =
  struct
    type t = Paths_types.Reference.instance_variable
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Label =
  struct
    type t = Paths_types.Reference.label
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Page =
  struct
    type t = Paths_types.Reference.page
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

end
