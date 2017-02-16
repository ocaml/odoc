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

type sexp =
  | List of sexp list
  | Atom of string

let rec string_of_sexp = function
  | Atom s -> s
  | List lst ->
      let s = List.map string_of_sexp lst in
      Printf.sprintf "(%s)" (String.concat " " s)

let atom s = Atom (Printf.sprintf "%S" s)

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false else
    if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

module Kind = struct

  type any =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label ]

  type signature = [ `Module | `ModuleType ]

  type class_signature = [ `Class | `ClassType ]

  type datatype = [ `Type ]

  type parent = [ signature | class_signature | datatype ]

  type identifier = any

  type identifier_module = [ `Module ]
  type identifier_module_type = [ `ModuleType ]
  type identifier_type =  [ `Type ]
  type identifier_constructor = [ `Constructor ]
  type identifier_field = [ `Field ]
  type identifier_extension = [ `Extension ]
  type identifier_exception = [ `Exception ]
  type identifier_value = [ `Value ]
  type identifier_class = [ `Class ]
  type identifier_class_type = [ `ClassType ]
  type identifier_method = [ `Method ]
  type identifier_instance_variable = [ `InstanceVariable ]
  type identifier_label = [ `Label ]

  type path = [ `Module | `ModuleType | `Type | `Class | `ClassType ]

  type path_module = [ `Module ]
  type path_module_type = [ `ModuleType ]
  type path_type = [ `Type | `Class | `ClassType ]
  type path_class_type = [ `Class | `ClassType ]

  type fragment = [ `Module | `Type | `Class | `ClassType ]

  type fragment_module = [ `Module ]
  type fragment_type = [ `Type | `Class | `ClassType ]

  type reference = any

  type reference_module = [ `Module ]
  type reference_module_type = [ `ModuleType ]
  type reference_type = [ `Type | `Class | `ClassType ]
  type reference_constructor = [ `Constructor | `Extension | `Exception ]
  type reference_field = [ `Field ]
  type reference_extension = [ `Extension | `Exception ]
  type reference_exception = [ `Exception ]
  type reference_value = [ `Value ]
  type reference_class = [ `Class ]
  type reference_class_type = [ `Class | `ClassType ]
  type reference_method = [ `Method ]
  type reference_instance_variable = [ `InstanceVariable ]
  type reference_label = [ `Label ]

end

open Kind

module Reversed = struct
  type elt =
    | Root of string
    | Module of string
    | ModuleType of string
    | Argument of int * string

  type t = elt list

  let sexp_of_elt = function
    | Root s -> List [Atom "Root"; Atom s]
    | Module s -> List [Atom "Module"; Atom s]
    | ModuleType s -> List [Atom "ModuleType"; Atom s]
    | Argument (i, s) -> List [Atom "Argument"; Atom (string_of_int i); Atom s]

  let sexp_of_t l = List (List.map sexp_of_elt l)

  let rec remove_prefix prefix ~of_ =
    match prefix, of_ with
    | x1 :: xs1, x2 :: xs2 when x1 = x2 ->
      remove_prefix xs1 ~of_:xs2
    | _, _ -> of_
end

module Identifier = struct

  type kind = Kind.identifier

  type ('a, 'b) t =
    | Root : 'a * string -> ('a, [< kind > `Module]) t
    | Module : 'a signature * string -> ('a, [< kind > `Module]) t
    | Argument : 'a signature * int * string -> ('a, [< kind > `Module]) t
    | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
    | Type : 'a signature * string -> ('a, [< kind > `Type]) t
    | CoreType : string -> ('a, [< kind > `Type]) t
    | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
    | Field : 'a parent * string -> ('a, [< kind > `Field]) t
    | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
    | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
    | CoreException : string -> ('a, [< kind > `Exception]) t
    | Value : 'a signature * string -> ('a, [< kind > `Value]) t
    | Class : 'a signature * string -> ('a, [< kind > `Class]) t
    | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
    | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
    | InstanceVariable : 'a class_signature * string ->
                           ('a, [< kind > `InstanceVariable]) t
    | Label : 'a parent * string -> ('a, [< kind > `Label]) t

  and 'a any = ('a, Kind.any) t
  and 'a signature = ('a, Kind.signature) t
  and 'a class_signature = ('a, Kind.class_signature) t
  and 'a datatype = ('a, Kind.datatype) t
  and 'a parent = ('a, Kind.parent) t

  let rec sexp_of_t : type a b. (a -> sexp) -> (a,b) t -> sexp =
    fun sexp_of_a t ->
      let int i = Atom (string_of_int i) in
      match t with
      | Root (a, s) -> List [ Atom "Root"; List [sexp_of_a a; atom s] ]
      | Module (sg, s) ->
          List [ Atom "Module"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Argument (sg, i, s) ->
          List [ Atom "Argument"; List [sexp_of_t sexp_of_a sg; int i; atom s] ]
      | ModuleType (sg, s) ->
          List [ Atom "ModuleType"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Type (sg, s) ->
          List [ Atom "Type"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | CoreType s -> List [ Atom "CoreType"; atom s ]
      | Constructor (cs, s) ->
          List [ Atom "Constructor"; List [sexp_of_t sexp_of_a cs; atom s] ]
      | Field (f, s) ->
          List [ Atom "Field"; List [sexp_of_t sexp_of_a f; atom s] ]
      | Extension (sg, s) ->
          List [ Atom "Extension"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Exception (sg, s) ->
          List [ Atom "Exception"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | CoreException s -> List [ Atom "CoreException"; atom s ]
      | Value (sg, s) ->
          List [ Atom "Value"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Class (sg, s) ->
          List [ Atom "Class"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | ClassType (sg, s) ->
          List [ Atom "ClassType"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Method (sg, s) ->
          List [ Atom "Method"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | InstanceVariable (sg, s) ->
          List [ Atom "InstanceVariable"; List [sexp_of_t sexp_of_a sg; atom s] ]
      | Label (sg, s) ->
          List [ Atom "Label"; List [sexp_of_t sexp_of_a sg; atom s] ]

  type 'a module_ = ('a, identifier_module) t
  type 'a module_type = ('a, identifier_module_type) t
  type 'a type_ =  ('a, identifier_type) t
  type 'a constructor = ('a, identifier_constructor) t
  type 'a field = ('a, identifier_field) t
  type 'a extension = ('a, identifier_extension) t
  type 'a exception_ = ('a, identifier_exception) t
  type 'a value = ('a, identifier_value) t
  type 'a class_ = ('a, identifier_class) t
  type 'a class_type = ('a, identifier_class_type) t
  type 'a method_ = ('a, identifier_method) t
  type 'a instance_variable = ('a, identifier_instance_variable) t
  type 'a label = ('a, identifier_label) t

  type 'a path_module = ('a, Kind.path_module) t
  type 'a path_module_type = ('a, Kind.path_module_type) t
  type 'a path_type =  ('a, Kind.path_type) t
  type 'a path_class_type = ('a, Kind.path_class_type) t

  type 'a fragment_module = ('a, Kind.fragment_module) t
  type 'a fragment_type =  ('a, Kind.fragment_type) t

  type 'a reference_module = ('a, Kind.reference_module) t
  type 'a reference_module_type = ('a, Kind.reference_module_type) t
  type 'a reference_type =  ('a, Kind.reference_type) t
  type 'a reference_constructor = ('a, Kind.reference_constructor) t
  type 'a reference_field = ('a, Kind.reference_field) t
  type 'a reference_extension = ('a, Kind.reference_extension) t
  type 'a reference_exception = ('a, Kind.reference_exception) t
  type 'a reference_value = ('a, Kind.reference_value) t
  type 'a reference_class = ('a, Kind.reference_class) t
  type 'a reference_class_type = ('a, Kind.reference_class_type) t
  type 'a reference_method = ('a, Kind.reference_method) t
  type 'a reference_instance_variable = ('a, Kind.reference_instance_variable) t
  type 'a reference_label = ('a, Kind.reference_label) t

  let signature_of_module : 'a module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> x

  let signature_of_module_type : 'a module_type -> _  = function
    | ModuleType _ as x -> x

  let class_signature_of_class : 'a class_ -> _ = function
    | Class _ as x -> x

  let class_signature_of_class_type : 'a class_type -> _ = function
    | ClassType _ as x -> x

  let datatype_of_type : 'a type_ -> 'a datatype = function
    | x -> x

  let parent_of_signature : 'a signature -> 'a parent = function
    | Root _ | Module _ | Argument _ | ModuleType _ as x -> x

  let parent_of_class_signature : 'a class_signature -> 'a parent =
    function Class _ | ClassType _ as x -> x

  let parent_of_datatype : 'a datatype -> 'a parent =
    function Type _ | CoreType _ as x -> x

  let any : type k. ('a, k) t -> 'a any = function
    | Root _ as x -> x
    | Module _ as x -> x
    | Argument _ as x -> x
    | ModuleType _ as x -> x
    | Type _ as x -> x
    | CoreType _ as x -> x
    | Constructor _ as x -> x
    | Field _ as x -> x
    | Extension _ as x -> x
    | Exception _ as x -> x
    | CoreException _ as x -> x
    | Value _ as x -> x
    | Class _ as x -> x
    | ClassType _ as x -> x
    | Method _ as x -> x
    | InstanceVariable _ as x -> x
    | Label _ as x -> x

  let name : type k. ('a, k) t -> string = function
    | Root(_, name) -> name
    | Module(_, name) -> name
    | Argument(_, _, name) -> name
    | ModuleType(_, name) -> name
    | Type(_, name) -> name
    | CoreType name -> name
    | Constructor(_, name) -> name
    | Field(_, name) -> name
    | Extension(_, name) -> name
    | Exception(_, name) -> name
    | CoreException name -> name
    | Value(_, name) -> name
    | Class(_, name) -> name
    | ClassType(_, name) -> name
    | Method(_, name) -> name
    | InstanceVariable(_, name) -> name
    | Label(_, name) -> name

  let equal ~equal id1 id2 =
    let rec loop : type k. ('a -> 'a -> bool) ->
                            ('a, k) t -> ('a, k) t -> bool =
      fun equal id1 id2 ->
        match id1, id2 with
        | Root(r1, s1), Root(r2, s2) ->
            s1 = s2 && equal r1 r2
        | Module(id1, s1), Module(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Argument(id1, n1, s1), Argument(id2, n2, s2) ->
            n1 = n2 && s1 = s2 && loop equal id1 id2
        | ModuleType(id1, s1), ModuleType(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Type(id1, s1), Type(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | CoreType s1, CoreType s2 ->
            s1 = s2
        | Constructor(id1, s1), Constructor(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Field(id1, s1), Field(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Extension(id1, s1), Extension(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Exception(id1, s1), Exception(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | CoreException s1, CoreException s2 ->
            s1 = s2
        | Value(id1, s1), Value(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Class(id1, s1), Class(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | ClassType(id1, s1), ClassType(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Method(id1, s1), Method(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | InstanceVariable(id1, s1), InstanceVariable(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | Label(id1, s1), Label(id2, s2) ->
            s1 = s2 && loop equal id1 id2
        | _, _ -> false
    in
      loop equal id1 id2

  let hash ~hash id =
    let rec loop : type k. ('a -> int) -> ('a, k) t -> int =
      fun hash id ->
        match id with
        | Root(r, s) ->
            Hashtbl.hash (1, hash r, s)
        | Module(id, s) ->
            Hashtbl.hash (2, loop hash id, s)
        | Argument(id, n, s) ->
            Hashtbl.hash (3, loop hash id, n, s)
        | ModuleType(id, s) ->
            Hashtbl.hash (4, loop hash id, s)
        | Type(id, s) ->
            Hashtbl.hash (5, loop hash id, s)
        | CoreType s ->
            Hashtbl.hash (6, s)
        | Constructor(id, s) ->
            Hashtbl.hash (7, loop hash id, s)
        | Field(id, s) ->
            Hashtbl.hash (8, loop hash id, s)
        | Extension(id, s) ->
            Hashtbl.hash (9, loop hash id, s)
        | Exception(id, s) ->
            Hashtbl.hash (10, loop hash id, s)
        | CoreException s ->
            Hashtbl.hash (11, s)
        | Value(id, s) ->
            Hashtbl.hash (12, loop hash id, s)
        | Class(id, s) ->
            Hashtbl.hash (13, loop hash id, s)
        | ClassType(id, s) ->
            Hashtbl.hash (14, loop hash id, s)
        | Method(id, s) ->
            Hashtbl.hash (15, loop hash id, s)
        | InstanceVariable(id, s) ->
            Hashtbl.hash (16, loop hash id, s)
        | Label(id, s) ->
            Hashtbl.hash (17, loop hash id, s)
    in
      loop hash id

  let rec signature_root : 'a signature -> 'a = function
    | Root(r, _) -> r
    | Module(id, _) -> signature_root id
    | Argument(id, _, _) -> signature_root id
    | ModuleType(id, _) -> signature_root id

  let module_root : 'a module_ -> 'a = function
    | Root(r, _) -> r
    | Module(id, _) -> signature_root id
    | Argument(id, _, _) -> signature_root id

  let module_type_root : 'a module_type -> 'a = function
    | ModuleType(id, _) -> signature_root id

  let class_signature_root : 'a class_signature -> 'a = function
    | Class(id, _)
    | ClassType(id, _) -> signature_root id

  let to_reversed i =
    let rec loop acc : 'a signature -> Reversed.t = function
      | Root (_, s) -> Reversed.Root s :: acc
      | Module (i, s) -> loop (Reversed.Module s :: acc) i
      | ModuleType (i, s) -> loop (Reversed.ModuleType s :: acc) i
      | Argument (i, d, s) -> loop (Reversed.Argument (d, s) :: acc) i
    in
    loop [] i
end



module Path = struct

  (* Separate types module to avoid repeating type definitions *)
  module rec Types : sig

    module Resolved : sig

      type kind = Kind.path

      type ('a, 'b) t =
        | Identifier : ('a, 'b) Identifier.t -> ('a, [< kind] as 'b) t
        | Subst : 'a module_type * 'a module_ -> ('a, [< kind > `Module]) t
        | SubstAlias : 'a module_ * 'a module_ -> ('a, [< kind > `Module]) t
        | Hidden : 'a module_ -> ('a, [< kind > `Module ]) t
        | Module : 'a module_ * string -> ('a, [< kind > `Module]) t
        | Canonical : 'a module_ * 'a Types.Path.module_ -> ('a, [< kind > `Module]) t
        | Apply : 'a module_ * 'a Types.Path.module_ -> ('a, [< kind > `Module]) t
        | ModuleType : 'a module_ * string -> ('a, [< kind > `ModuleType]) t
        | Type : 'a module_ * string -> ('a, [< kind > `Type]) t
        | Class : 'a module_ * string -> ('a, [< kind > `Class]) t
        | ClassType : 'a module_ * string -> ('a, [< kind > `ClassType]) t

      and 'a any = ('a, kind) t

      and 'a module_ = ('a, path_module) t
      and 'a module_type = ('a, path_module_type) t
      and 'a type_ = ('a, path_type) t
      and 'a class_type = ('a, path_class_type) t

    end

    module Path : sig

      type kind = Kind.path

      type ('a, 'b) t =
      | Resolved : ('a, 'b) Types.Resolved.t -> ('a, 'b) t
      | Root : string -> ('a, [< kind >`Module]) t
      | Forward : string -> ('a, [< kind >`Module]) t
      | Dot : 'a module_ * string -> ('a, [< kind]) t
      | Apply : 'a module_ * 'a module_ -> ('a, [< kind >`Module]) t

      and 'a any = ('a, kind) t

      and 'a module_ = ('a, path_module) t
      and 'a module_type = ('a, path_module_type) t
      and 'a type_ = ('a, path_type) t
      and 'a class_type = ('a, path_class_type) t

    end

  end = Types

  let rec equal_resolved_path : type k. ('a -> 'a -> bool) ->
                                     ('a, k) Types.Resolved.t ->
                                       ('a, k) Types.Resolved.t -> bool =
    fun equal p1 p2 ->
      let open Types.Resolved in
        match p1, p2 with
        | Identifier id1, Identifier id2 ->
            Identifier.equal ~equal id1 id2
        | Subst(sub1, p1), Subst(sub2, p2) ->
            equal_resolved_path equal p1 p2
            && equal_resolved_path equal sub1 sub2
        | SubstAlias(sub1, p1), SubstAlias(sub2, p2) ->
            equal_resolved_path equal p1 p2
            && equal_resolved_path equal sub1 sub2
        | Module(p1, s1), Module(p2, s2) ->
            s1 = s2 && equal_resolved_path equal p1 p2
        | Apply(p1, arg1), Apply(p2, arg2) ->
            equal_path equal arg1 arg2
            && equal_resolved_path equal p1 p2
        | ModuleType(p1, s1), ModuleType(p2, s2) ->
            s1 = s2 && equal_resolved_path equal p1 p2
        | Type(p1, s1), Type(p2, s2) ->
            s1 = s2 && equal_resolved_path equal p1 p2
        | Class(p1, s1), Class(p2, s2) ->
            s1 = s2 && equal_resolved_path equal p1 p2
        | ClassType(p1, s1), ClassType(p2, s2) ->
            s1 = s2 && equal_resolved_path equal p1 p2
        | _, _ -> false

  and equal_path : type k. ('a -> 'a -> bool) ->
                             ('a, k) Types.Path.t ->
                               ('a, k) Types.Path.t -> bool =
    fun equal p1 p2 ->
      let open Types.Path in
        match p1, p2 with
        | Resolved p1, Resolved p2 ->
            equal_resolved_path equal p1 p2
        | Root s1, Root s2 ->
            s1 = s2
        | Dot(p1, s1), Dot(p2, s2) ->
            s1 = s2 && equal_path equal p1 p2
        | Apply(p1, arg1), Apply(p2, arg2) ->
            equal_path equal arg1 arg2 && equal_path equal p1 p2
        | _, _ -> false

  let rec hash_resolved_path : type k. ('a -> int) ->
                                    ('a, k) Types.Resolved.t -> int =
    fun hash p ->
      let open Types.Resolved in
        match p with
        | Identifier id ->
            Identifier.hash ~hash id
        | Subst(sub, p) ->
            Hashtbl.hash (18, hash_resolved_path hash sub,
                          hash_resolved_path hash p)
        | SubstAlias(sub, p) ->
            Hashtbl.hash (19, hash_resolved_path hash sub,
                          hash_resolved_path hash p)
        | Hidden p -> Hashtbl.hash (20, hash_resolved_path hash p)
        | Module(p, s) ->
            Hashtbl.hash (21, hash_resolved_path hash p, s)
        | Canonical(p, canonical) ->
          Hashtbl.hash (22, hash_resolved_path hash p, hash_path hash canonical)
        | Apply(p, arg) ->
            Hashtbl.hash (23, hash_resolved_path hash p, hash_path hash arg)
        | ModuleType(p, s) ->
            Hashtbl.hash (24, hash_resolved_path hash p, s)
        | Type(p, s) ->
            Hashtbl.hash (25, hash_resolved_path hash p, s)
        | Class(p, s) ->
            Hashtbl.hash (26, hash_resolved_path hash p, s)
        | ClassType(p, s) ->
            Hashtbl.hash (27, hash_resolved_path hash p, s)

  and hash_path : type k. ('a -> int) -> ('a, k) Types.Path.t -> int =
    fun hash p ->
      let open Types.Path in
        match p with
        | Resolved p -> hash_resolved_path hash p
        | Root s ->
            Hashtbl.hash (28, s)
        | Forward s ->
            Hashtbl.hash (29, s)
        | Dot(p, s) ->
            Hashtbl.hash (30, hash_path hash p, s)
        | Apply(p, arg) ->
            Hashtbl.hash (31, hash_path hash p, hash_path hash arg)

  let equal ~equal p1 p2 = equal_path equal p1 p2

  let hash ~hash p = hash_path hash p

  let rec sexp_of_path : type a b. (a -> sexp) -> (a, b) Types.Path.t -> sexp =
    fun sexp_of_a t ->
      let atom s = Atom (Printf.sprintf "%S" s) in
      let open Types.Path in
      match t with
      | Resolved r -> List [ Atom "Resolved"; sexp_of_resolved_path sexp_of_a r ]
      | Root s -> List [ Atom "Root"; atom s ]
      | Forward s -> List [ Atom "Forward"; atom s ]
      | Dot (md, s) -> List [ Atom "Dot" ; List [sexp_of_path sexp_of_a md; atom s]]
      | Apply (m1, m2) ->
        List [ Atom "Apply" ; List [ sexp_of_path sexp_of_a m1
                                   ; sexp_of_path sexp_of_a m2 ]]

  and sexp_of_resolved_path : type a b. (a -> sexp) -> (a, b) Types.Resolved.t -> sexp =
    fun sexp_of_a t ->
      let atom s = Atom (Printf.sprintf "%S" s) in
      let open Types.Resolved in
      match t with
      | Identifier id -> List [ Atom "Identifier"; Identifier.sexp_of_t
                                                     sexp_of_a id ]
      | Subst (sg, t) ->
        List [ Atom "Subst"; List [ sexp_of_resolved_path sexp_of_a sg
                                  ; sexp_of_resolved_path sexp_of_a t ]]
      | SubstAlias (sg, t) ->
        List [ Atom "SubstAlias"; List [ sexp_of_resolved_path sexp_of_a sg
                                       ; sexp_of_resolved_path sexp_of_a t ]]
      | Hidden p -> List [ Atom "Hidden" ; sexp_of_resolved_path sexp_of_a p ]
      | Module (md, s) ->
        List [ Atom "Module"; List [ sexp_of_resolved_path sexp_of_a md ; atom s ]]
      | Canonical (md, p) ->
        List [ Atom "Canonical"; List [ sexp_of_resolved_path sexp_of_a md
                                      ; sexp_of_path sexp_of_a p ]]
      | Apply (md, arg) ->
        List [ Atom "Apply"; List [ sexp_of_resolved_path sexp_of_a md
                                  ; sexp_of_path sexp_of_a arg ]]
      | ModuleType (md, s) ->
        List [ Atom "ModuleType"; List [ sexp_of_resolved_path sexp_of_a md ; atom s ]]
      | Type (md, s) ->
        List [ Atom "Type"; List [ sexp_of_resolved_path sexp_of_a md ; atom s ]]
      | Class (md, s) ->
        List [ Atom "Class"; List [ sexp_of_resolved_path sexp_of_a md ; atom s ]]
      | ClassType (md, s) ->
        List [ Atom "ClassType"; List [ sexp_of_resolved_path sexp_of_a md ; atom s ]]

  let rec is_resolved_hidden : type k. ('a, k) Types.Resolved.t -> bool =
    let open Types.Resolved in
    function
    | Identifier _ -> false
    | Hidden _ -> true
    | Subst(p1, p2) -> is_resolved_hidden p1 || is_resolved_hidden p2
    | SubstAlias(p1, p2) -> is_resolved_hidden p1 || is_resolved_hidden p2
    | Module (p, _) -> is_resolved_hidden p
    | Canonical (_, p) -> is_path_hidden p
    | Apply (p, _) -> is_resolved_hidden p
    | ModuleType (p, _) -> is_resolved_hidden p
    | Type (p, _) -> is_resolved_hidden p
    | Class (p, _) -> is_resolved_hidden p
    | ClassType (p, _) -> is_resolved_hidden p

  and is_path_hidden : type k. ('a, k) Types.Path.t -> bool =
    let open Types.Path in
    function
    | Resolved r -> is_resolved_hidden r
    | Root _ -> false
    | Forward _ -> false
    | Dot(p, _) -> is_path_hidden p
    | Apply(p1, p2) -> is_path_hidden p1 || is_path_hidden p2

  module Resolved = struct

    open Identifier

    include Types.Resolved

    let rec sexp_of_t : type a b. (a -> sexp) -> (a, b) t -> sexp =
      fun sexp_of_a t ->
        sexp_of_resolved_path sexp_of_a t

    let ident_module : 'a Identifier.module_ -> _ = function
      | Root _ | Module _ | Argument _ as x -> Identifier x

    let ident_module_type : 'a Identifier.module_type -> _ = function
      | ModuleType _ as x -> Identifier x

    let ident_type : 'a Identifier.type_ -> _ = function
      | Type _ | CoreType _ as x -> Identifier x

    let ident_class : 'a Identifier.class_ -> _ = function
      | Class _ as x -> Identifier x

    let ident_class_type : 'a Identifier.class_type -> _ = function
      | ClassType _ as x -> Identifier x

    let rec any : type k. ('a, k) t -> 'a any = function
      | Identifier (Root _) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _) as x -> x
      | Identifier (ModuleType _) as x -> x
      | Identifier (Type _) as x -> x
      | Identifier (CoreType _) as x -> x
      | Identifier (Class _) as x -> x
      | Identifier (ClassType _) as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Hidden _ as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | Apply _ as x -> x
      | ModuleType _ as x -> x
      | Type _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x

    let open_module : 'b. 'a module_ -> ('a, [< kind > `Module ] as 'b) t = function
      | Identifier (Root _ | Module _ | Argument _) | Subst _ | SubstAlias _
      | Hidden _ | Module _ | Canonical _ | Apply _ as x -> x

    let rec parent_module_type_identifier : 'a module_type -> 'a Identifier.signature = function
      | Identifier id -> Identifier.signature_of_module_type id
      | ModuleType(m, n) -> ModuleType(parent_module_identifier m, n)

    and parent_module_identifier : 'a module_ -> 'a Identifier.signature = function
      | Identifier id -> Identifier.signature_of_module id
      | Subst(sub, _) -> parent_module_type_identifier sub
      | SubstAlias(sub, _) -> parent_module_identifier sub
      | Hidden p -> parent_module_identifier p
      | Module(m, n) -> Module(parent_module_identifier m, n)
      | Canonical(_, Types.Path.Resolved p) -> parent_module_identifier p
      | Canonical(p, _) -> parent_module_identifier p
      | Apply(m, _) -> parent_module_identifier m

    let rec identifier : type k. ('a, k) t -> ('a, k) Identifier.t = function
      | Identifier id -> id
      | Subst(_, p) -> identifier (open_module p)
      | SubstAlias(_, p) -> identifier (open_module p)
      | Hidden p -> identifier (open_module p)
      | Module(m, n) -> Module(parent_module_identifier m, n)
      | Canonical(_, Types.Path.Resolved p) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | Canonical(p, _) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | Apply(m, _) -> begin
          match identifier m with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | ModuleType(m, n) -> ModuleType(parent_module_identifier m, n)
      | Type(m, n) -> Type(parent_module_identifier m, n)
      | Class(m, n) -> Class(parent_module_identifier m, n)
      | ClassType(m, n) -> ClassType(parent_module_identifier m, n)

    let equal ~equal p1 p2 = equal_resolved_path equal p1 p2

    let hash ~hash p = hash_resolved_path hash p

    type ('a, 'b) rebase_result =
      | Stop of ('a, 'b) t
      | Continue of ('a, 'b) Identifier.t * Reversed.t

    let rec rebase_module_path : Reversed.t -> 'a module_ -> ('a, Kind.path_module) rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed @@ signature_of_module id) in
          let new_base' = Reversed.remove_prefix rev ~of_:new_base in
          if new_base == new_base' then
            Stop t
          else
            Continue (id, new_base')
        | Subst (_, p)
        | SubstAlias (_, p)
        | Hidden p -> begin
            match rebase_module_path new_base p with
            | Stop p' when p == p' -> Stop t
            | otherwise -> otherwise
          end
        | Module (m, s) ->
          begin match rebase_module_path new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.Module(Identifier.signature_of_module id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
                Stop (Identifier id)
          end
        | Canonical (rp, Types.Path.Resolved p) ->
          (* We only care about printing at this point, so let's drop the lhs. *)
          rebase_module_path new_base p
        | Canonical (rp, p) ->
          begin match rebase_module_path new_base rp with
          | Stop rp' -> Stop (Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            Stop t
          end
        | Apply _ -> Stop t
        (* TODO: rewrite which side? *)

    let rec rebase : type k. Reversed.t -> ('a, k) t -> ('a, k) t =
      fun new_base t ->
        match t with
        | Identifier _ -> t
        | Subst _ -> t (* TODO: rewrite which side? *)
        | SubstAlias _ -> t (* TODO: rewrite which side? *)
        | Hidden p  -> begin
            match rebase_module_path new_base p with
            | Stop p' ->
              if p == p' then t else open_module p'
            | Continue (id, _) -> open_module (Identifier id)
          end
        | Module (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Module (signature_of_module id, s))
          | Stop mp' -> Module (mp', s)
          end
        | Canonical (p, Types.Path.Resolved rp) ->
          begin match rebase_module_path new_base rp with
          | Continue (id, _) -> ident_module id
          | Stop rp ->
            (* Easier to reexport a canonical than get the type for rp right... *)
            Canonical (p, Types.Path.Resolved rp)
          end
        | Canonical (rp, p) ->
          begin match rebase_module_path new_base rp with
          | Stop rp' -> Canonical (rp', p)
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            t
          end
        | Apply (mp, arg) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) -> Apply (Identifier id, arg)
          | Stop mp' -> Apply (mp', arg)
          end
        | ModuleType (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(ModuleType (signature_of_module id, s))
          | Stop mp' -> ModuleType (mp', s)
          end
        | Type (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Type (signature_of_module id, s))
          | Stop mp' -> Type (mp', s)
          end
        | Class (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Class (signature_of_module id, s))
          | Stop mp' -> Class (mp', s)
          end
        | ClassType (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(ClassType (signature_of_module id, s))
          | Stop mp' -> ClassType (mp', s)
          end

    let rebase id t =
      let rev = Identifier.to_reversed id in
      rebase rev t

    let rec signature_of_module : 'a module_ -> ('a, Kind.signature) t = function
      | Identifier (Root _) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _) as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | Apply _ as x -> x
      | Hidden _ as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x

    let rec equal_identifier :
      type k. equal:('a -> 'a -> bool) -> ('a, k) Identifier.t -> ('a, k) t -> bool =
      fun ~equal id p ->
        match id, p with
        | _, Identifier id' -> Identifier.equal ~equal id id'
        | Module (id, s1), Module (p, s2) when s1 = s2 ->
          equal_identifier ~equal id (signature_of_module p)
        | ModuleType (id, s1), ModuleType (p, s2) when s1 = s2 ->
          equal_identifier ~equal id (signature_of_module p)
        | _, _ ->
          false


    let is_hidden = is_resolved_hidden
  end

  open Identifier
  open Resolved

  include Types.Path

  let sexp_of_t : type a b. (a -> sexp) -> (a, b) t -> sexp =
    fun sexp_of_a t ->
      sexp_of_path sexp_of_a t

  let ident_module : 'a Identifier.module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> Resolved (Identifier x)

  let ident_module_type : 'a Identifier.module_type -> _ = function
    | ModuleType _ as x -> Resolved (Identifier x)

  let ident_type : 'a Identifier.type_ -> _ = function
    | Type _ | CoreType _ as x -> Resolved (Identifier x)

  let ident_class : 'a Identifier.class_ -> _ = function
    | Class _ as x -> Resolved (Identifier x)

  let ident_class_type : 'a Identifier.class_type -> _ = function
    | ClassType _ as x -> Resolved (Identifier x)

  let any : type k. ('a, k) t -> 'a any = function
    | Resolved (Identifier (Root _)) as x -> x
    | Resolved (Identifier (Module _)) as x -> x
    | Resolved (Identifier (Argument _)) as x -> x
    | Resolved (Identifier (ModuleType _)) as x -> x
    | Resolved (Identifier (Type _)) as x -> x
    | Resolved (Identifier (CoreType _)) as x -> x
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Hidden _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Canonical _) as x -> x
    | Resolved (Apply _) as x -> x
    | Resolved (ModuleType _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Resolved (Subst _) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Root _ as x -> x
    | Forward _ as x -> x
    | Dot _ as x -> x
    | Apply _ as x -> x

  let module_ p name =
    match p with
    | Resolved p -> Resolved (Module(p, name))
    | p -> Dot(p, name)

  let apply p arg =
    match p with
    | Resolved p -> Resolved (Apply(p, arg))
    | p -> Apply(p, arg)

  let module_type p name =
    match p with
    | Resolved p -> Resolved (ModuleType(p, name))
    | p -> Dot(p, name)

  let type_ p name =
    match p with
    | Resolved p -> Resolved (Type(p, name))
    | p -> Dot(p, name)

  let class_ p name =
    match p with
    | Resolved p -> Resolved (Class(p, name))
    | p -> Dot(p, name)

  let class_type_ p name =
    match p with
    | Resolved p -> Resolved (ClassType(p, name))
    | p -> Dot(p, name)

  let type_of_class_type : 'a class_type -> 'a type_ = function
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Dot _ as x -> x

  let is_hidden = is_path_hidden
end



module Fragment = struct

  module Resolved = struct

    type kind = Kind.fragment

    type sort = [ `Root | `Branch ]

    type ('a, 'b, 'c) raw =
      | Root : ('a, 'b, [< sort > `Root]) raw
      | Subst : 'a Path.Resolved.module_type * 'a module_ ->
          ('a, [< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | SubstAlias : 'a Path.Resolved.module_ * 'a module_ ->
          ('a, [< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | Module : 'a signature * string ->
          ('a, [< kind > `Module], [< sort > `Branch]) raw
      | Type : 'a signature * string ->
          ('a, [< kind > `Type], [< sort > `Branch]) raw
      | Class : 'a signature * string ->
          ('a, [< kind > `Class], [< sort > `Branch]) raw
      | ClassType : 'a signature * string ->
          ('a, [< kind > `ClassType], [< sort > `Branch]) raw

    and ('a, 'b) t = ('a, 'b, [`Branch]) raw

    and 'a any = ('a, kind) t
    and 'a signature = ('a, fragment_module, [`Root | `Branch]) raw
    and 'a module_ = ('a, fragment_module) t

    let rec sexp_of_t :
      type a c. ('b -> sexp) -> ('b, a, c) raw -> sexp =
      fun sexp_of_a raw ->
        match raw with
        | Root -> Atom "Root"
        | Subst (path, raw) ->
            List [
              Atom "Subst";
              List [ Path.Resolved.sexp_of_t sexp_of_a path
                  ; sexp_of_t sexp_of_a raw ]
            ]
        | SubstAlias (path, raw) ->
            List [
              Atom "SubstAlias";
              List [ Path.Resolved.sexp_of_t sexp_of_a path
                  ; sexp_of_t sexp_of_a raw ]
            ]
        | Module (raw, s) ->
            List [
              Atom "Module";
              List [ sexp_of_t sexp_of_a raw
                  ; atom s ]
            ]
        | Type (raw, s) ->
            List [
              Atom "Type";
              List [ sexp_of_t sexp_of_a raw
                  ; atom s ]
            ]
        | Class (raw, s) ->
            List [
              Atom "Class";
              List [ sexp_of_t sexp_of_a raw
                  ; atom s ]
            ]
        | ClassType (raw, s) ->
            List [
              Atom "ClassType";
              List [ sexp_of_t sexp_of_a raw
                  ; atom s ]
            ]

    type 'a type_ = ('a, fragment_type) t

    let rec signature_of_module : 'a module_ -> 'a signature = function
      | Subst _ | SubstAlias _ | Module _ as x -> x

    let rec any_sort : type b c. ('a, b, c) raw -> ('a, b, sort) raw =
      function
      | Root as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Module (_,_) as x -> x
      | Type (_,_) as x -> x
      | Class (_,_) as x -> x
      | ClassType (_,_) as x -> x

    let open_sort : 'a module_ -> ('a, Kind.fragment_module, [< sort > `Branch ]) raw =
      function
      | Module _ | Subst _ | SubstAlias _ as x -> x

    let open_module : 'a module_ -> ('a, [< kind > `Module ]) t =
      function
      | Module _ | Subst _ | SubstAlias _ as x -> x

    let rec any : type k. ('a, k) t -> 'a any = function
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Module _ as x -> x
      | Type _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x

    let rec parent_resolved_path root = function
      | Root -> root
      | Subst(sub, p) ->
          Path.Resolved.Subst(sub, parent_resolved_path root (open_sort p))
      | SubstAlias(sub, p) ->
          Path.Resolved.SubstAlias(sub, parent_resolved_path root (open_sort p))
      | Module(m, n) ->
          Path.Resolved.Module(parent_resolved_path root m, n)

    let rec resolved_path
        : type k. 'a Path.Resolved.module_ ->
               ('a, k) t -> ('a, k) Path.Resolved.t =
      fun root frag ->
        match frag with
        | Subst(sub, p) ->
            Path.Resolved.Subst(sub, resolved_path root p)
        | SubstAlias(sub, p) ->
            Path.Resolved.SubstAlias(sub, resolved_path root p)
        | Module(m, n) ->
            Path.Resolved.Module(parent_resolved_path root m, n)
        | Type( m, n) ->
            Path.Resolved.Type(parent_resolved_path root m, n)
        | Class( m, n) ->
            Path.Resolved.Class(parent_resolved_path root m, n)
        | ClassType( m, n) ->
            Path.Resolved.ClassType(parent_resolved_path root m, n)

    let rec parent_unresolved_path root = function
      | Root -> root
      | Subst(_, p) -> parent_unresolved_path root (open_sort p)
      | SubstAlias(_, p) -> parent_unresolved_path root (open_sort p)
      | Module(m, n) -> Path.Dot(parent_unresolved_path root m, n)

    let rec unresolved_path
        : type k. 'a Path.module_ -> ('a, k) t -> ('a, k) Path.t =
      fun root -> function
        | Subst(_, p) -> unresolved_path root (open_module p)
        | SubstAlias(_, p) -> unresolved_path root (open_module p)
        | Module(m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | Type( m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | Class( m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | ClassType( m, n) -> Path.Dot(parent_unresolved_path root m, n)

    let parent_path root frag =
      match root with
      | Path.Resolved root -> Path.Resolved (parent_resolved_path root frag)
      | _ -> parent_unresolved_path root frag

    let path (root : 'a Path.module_) frag =
      match root with
      | Path.Resolved root -> Path.Resolved (resolved_path root frag)
      | _ -> unresolved_path root frag

    let rec parent_identifier root = function
      | Root -> root
      | Subst(sub, _) -> Path.Resolved.parent_module_type_identifier sub
      | SubstAlias(sub, _) -> Path.Resolved.parent_module_identifier sub
      | Module(m, n) -> Identifier.Module(parent_identifier root m, n)

    let rec identifier :
      type k. 'a Identifier.signature -> ('a, k) t -> ('a, k) Identifier.t =
        fun root -> function
          | Subst(_, p) -> identifier root (open_module p)
          | SubstAlias(_, p) -> identifier root (open_module p)
          | Module(m, n) -> Identifier.Module(parent_identifier root m, n)
          | Type(m, n) -> Identifier.Type(parent_identifier root m, n)
          | Class(m, n) -> Identifier.Class(parent_identifier root m, n)
          | ClassType(m, n) ->
              Identifier.ClassType(parent_identifier root m, n)

    type ('a, 'b) base_name =
      | Base : ('a, [< sort > `Root]) base_name
      | Branch : string * 'a signature -> ('a, [< sort > `Branch]) base_name

    let rec split_parent
            : type s . ('a, fragment_module, s) raw -> ('a, s) base_name =
      function
        | Root -> Base
        | Subst(_, p) -> split_parent (open_sort p)
        | SubstAlias(_, p) -> split_parent (open_sort p)
        | Module(m, name) ->
            match split_parent m with
            | Base -> Branch(name, Root)
            | Branch(base, m) -> Branch(base, Module(m, name))

    let rec split : type k . ('a, k) t -> string * ('a, k) t option = function
      | Subst(_, p) -> split (open_module p)
      | SubstAlias(_, p) -> split (open_module p)
      | Module(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Module(m, name))
        end
      | Type(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Type(m, name))
        end
      | Class(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Class(m, name))
        end
      | ClassType(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (ClassType(m, name))
        end

    let equal ~equal p1 p2 =
      let rec loop : type k s. ('a -> 'a -> bool) ->
                              ('a, k, s) raw -> ('a, k, s) raw -> bool =
        fun equal p1 p2 ->
          match p1, p2 with
          | Root, Root -> true
          | Subst(sub1, p1), Subst(sub2, p2) ->
              Path.Resolved.equal ~equal sub1 sub2
              && loop equal p1 p2
          | SubstAlias(sub1, p1), SubstAlias(sub2, p2) ->
              Path.Resolved.equal ~equal sub1 sub2
              && loop equal p1 p2
          | Module(p1, s1), Module(p2, s2) ->
              s1 = s2 && loop equal p1 p2
          | Type(p1, s1), Type(p2, s2) ->
              s1 = s2 && loop equal p1 p2
          | Class(p1, s1), Class(p2, s2) ->
              s1 = s2 && loop equal p1 p2
          | ClassType(p1, s1), ClassType(p2, s2) ->
              s1 = s2 && loop equal p1 p2
          | _, _ -> false
      in
        loop equal p1 p2

    let hash ~hash p =
      let rec loop : type k s. ('a -> int) -> ('a, k, s) raw -> int =
        fun hash p ->
          match p with
          | Root -> Hashtbl.hash 32
          | Subst(sub, p) ->
              Hashtbl.hash (33, Path.Resolved.hash ~hash sub, loop hash p)
          | SubstAlias(sub, p) ->
              Hashtbl.hash (34, Path.Resolved.hash ~hash sub, loop hash p)
          | Module(p, s) ->
              Hashtbl.hash (35, loop hash p, s)
          | Type(p, s) ->
              Hashtbl.hash (36, loop hash p, s)
          | Class(p, s) ->
              Hashtbl.hash (37, loop hash p, s)
          | ClassType(p, s) ->
              Hashtbl.hash (38, loop hash p, s)
      in
        loop hash p

  end

  open Resolved

  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('a, 'b, 'c) raw =
    | Resolved : ('a, 'b, 'c) Resolved.raw -> ('a, 'b, 'c) raw
    | Dot : 'a signature * string -> ('a, [< kind], [< sort > `Branch]) raw

  and ('a, 'b) t = ('a, 'b, [`Branch]) raw

  and 'a any = ('a, kind) t
  and 'a signature = ('a, fragment_module, [`Root | `Branch]) raw

  let rec sexp_of_t :
    type a c. ('b -> sexp) -> ('b, a, c) raw -> sexp =
    fun sexp_of_a raw ->
      match raw with
      | Resolved r ->
          List [
            Atom "Resolved";
            Resolved.sexp_of_t sexp_of_a r;
          ]
      | Dot (raw, s) ->
          List [
            Atom "Dot";
            List [ sexp_of_t sexp_of_a raw ; atom s ];
          ]

  type 'a module_ = ('a, fragment_module) t
  type 'a type_ = ('a, fragment_type) t

  let signature_of_module : 'a module_ -> 'a signature = function
    | Resolved(Subst _ | SubstAlias _ | Module _) | Dot _ as x -> x

  let any_sort : type b c. ('a, b, c) raw -> ('a, b, sort) raw = function
    | Resolved r -> Resolved (any_sort r)
    | Dot _ as x -> x

  let any : type k. ('a, k) t -> 'a any = function
    | Resolved (Subst _) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Dot _ as x -> x

  let rec parent_path root = function
    | Resolved r -> Resolved.parent_path root r
    | Dot(m, n) -> Path.Dot(parent_path root m, n)

  let rec path : type k. 'a Path.module_ -> ('a, k) t -> ('a, k) Path.t =
   fun root -> function
    | Resolved r -> Resolved.path root r
    | Dot(m, s) -> Path.Dot(parent_path root m, s)

  type ('a, 'b) base_name =
    | Base : ('a, [< sort > `Root]) base_name
    | Branch : string * 'a signature -> ('a, [< sort > `Branch]) base_name

  let rec split_parent
          : type s . ('a, fragment_module, s) raw -> ('a, s) base_name =
    function
      | Resolved r -> begin
          match Resolved.split_parent r with
          | Base -> Base
          | Branch(base, m) -> Branch(base, Resolved m)
        end
      | Dot(m, name) -> begin
          match split_parent m with
          | Base -> Branch(name, Resolved Root)
          | Branch(base, m) -> Branch(base, Dot(m, name))
        end

  let split : type k . ('a, k) t -> string * ('a, k) t option = function
    | Resolved r ->
        let base, m = Resolved.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (Resolved m)
        in
          base, m
    | Dot(m, name) ->
        match split_parent m with
        | Base -> name, None
        | Branch(base, m) -> base, Some(Dot(m, name))

  let equal ~equal p1 p2 =
    let rec loop : type k s. ('a -> 'a -> bool) ->
                            ('a, k, s) raw -> ('a, k, s) raw -> bool =
      fun equal p1 p2 ->
        match p1, p2 with
        | Resolved p1, Resolved p2 ->
            Resolved.equal ~equal p1 p2
        | Dot(p1, s1), Dot(p2, s2) ->
            s1 = s2 && loop equal p1 p2
        | _, _ -> false
    in
      loop equal p1 p2

  let hash ~hash p =
    let rec loop : type k s. ('a -> int) -> ('a, k, s) raw -> int =
      fun hash p ->
        match p with
        | Resolved p -> Resolved.hash ~hash p
        | Dot(p, s) ->
            Hashtbl.hash (39, loop hash p, s)
    in
      loop hash p

end



module Reference = struct
  module rec Types : sig
    module Resolved : sig
      open Kind

      type kind = Kind.reference

      type ('a, 'b) t =
        | Identifier : ('a, 'b) Identifier.t -> ('a, 'b) t
        | SubstAlias : 'a Path.Resolved.module_ * 'a module_ -> ('a, [< kind > `Module ]) t
        | Module : 'a signature * string -> ('a, [< kind > `Module]) t
        | Canonical : 'a module_ * 'a Types.Reference.module_ -> ('a, [< kind > `Module]) t
        | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
        | Type : 'a signature * string -> ('a, [< kind > `Type]) t
        | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
        | Field : 'a parent * string -> ('a, [< kind > `Field]) t
        | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
        | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
        | Value : 'a signature * string -> ('a, [< kind > `Value]) t
        | Class : 'a signature * string -> ('a, [< kind > `Class]) t
        | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
        | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
        | InstanceVariable : 'a class_signature * string ->
          ('a, [< kind > `InstanceVariable]) t
        | Label : 'a parent * string -> ('a, [< kind > `Label]) t

      and 'a any = ('a, kind) t
      and 'a signature = ('a, Kind.signature) t
      and 'a class_signature = ('a, Kind.class_signature) t
      and 'a datatype = ('a, Kind.datatype) t
      and 'a parent = ('a, Kind.parent) t
      and 'a module_ = ('a, reference_module) t

      type 'a module_type = ('a, reference_module_type) t
      type 'a type_ = ('a, reference_type) t
      type 'a constructor = ('a, reference_constructor) t
      type 'a field = ('a, reference_field) t
      type 'a extension = ('a, reference_extension) t
      type 'a exception_ = ('a, reference_exception) t
      type 'a value = ('a, reference_value) t
      type 'a class_ = ('a, reference_class) t
      type 'a class_type = ('a, reference_class_type) t
      type 'a method_ = ('a, reference_method) t
      type 'a instance_variable = ('a, reference_instance_variable) t
      type 'a label = ('a, reference_label) t
    end

    module Reference : sig
      type kind = Kind.reference

      type ('a, 'b) t =
        | Resolved : ('a, 'b) Resolved.t -> ('a, 'b) t
        | Root : string -> ('a, [< kind]) t
        | Dot : 'a parent * string -> ('a, [< kind]) t

      and 'a any = ('a, kind) t
      and 'a signature = ('a, Kind.signature) t
      and 'a class_signature = ('a, Kind.class_signature) t
      and 'a datatype = ('a, Kind.datatype) t
      and 'a parent = ('a, Kind.parent) t

      type 'a module_ = ('a, reference_module) t
      type 'a module_type = ('a, reference_module_type) t
      type 'a type_ = ('a, reference_type) t
      type 'a constructor = ('a, reference_constructor) t
      type 'a field = ('a, reference_field) t
      type 'a extension = ('a, reference_extension) t
      type 'a exception_ = ('a, reference_exception) t
      type 'a value = ('a, reference_value) t
      type 'a class_ = ('a, reference_class) t
      type 'a class_type = ('a, reference_class_type) t
      type 'a method_ = ('a, reference_method) t
      type 'a instance_variable = ('a, reference_instance_variable) t
      type 'a label = ('a, reference_label) t
    end
  end = Types

  let rec sexp_of_resolved : type a b. (a -> sexp) -> (a, b) Types.Resolved.t -> sexp =
    fun sexp_of_a t ->
      let atom s = Atom (Printf.sprintf "%S" s) in
      let open Types.Resolved in
      match t with
      | Identifier id -> List [ Atom "Identifier"; Identifier.sexp_of_t
                                                     sexp_of_a id ]
      | SubstAlias (r1, r2) ->
        List [ Atom "SubstAlias"; List [ Path.Resolved.sexp_of_t sexp_of_a r1;
                                         sexp_of_resolved sexp_of_a r2] ]
      | Module (sg, s) ->
        List [ Atom "Module"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Canonical (t, rf) ->
        List [ Atom "Canonical"; List [ sexp_of_resolved sexp_of_a t
                                      ; sexp_of_t sexp_of_a rf ] ]
      | ModuleType (sg, s) ->
        List [ Atom "ModuleType"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Type (sg, s) ->
        List [ Atom "Type"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Constructor (cs, s) ->
        List [ Atom "Constructor"; List [sexp_of_resolved sexp_of_a cs; atom s] ]
      | Field (f, s) ->
        List [ Atom "Field"; List [sexp_of_resolved sexp_of_a f; atom s] ]
      | Extension (sg, s) ->
        List [ Atom "Extension"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Exception (sg, s) ->
        List [ Atom "Exception"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Value (sg, s) ->
        List [ Atom "Value"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Class (sg, s) ->
        List [ Atom "Class"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | ClassType (sg, s) ->
        List [ Atom "ClassType"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Method (sg, s) ->
        List [ Atom "Method"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | InstanceVariable (sg, s) ->
        List [ Atom "InstanceVariable"; List [sexp_of_resolved sexp_of_a sg; atom s] ]
      | Label (sg, s) ->
        List [ Atom "Label"; List [sexp_of_resolved sexp_of_a sg; atom s] ]

  and sexp_of_t : type a b. (a -> sexp) -> (a, b) Types.Reference.t -> sexp =
    fun sexp_of_a t ->
      let atom s = Atom (Printf.sprintf "%S" s) in
      let open Types.Reference in
      match t with
      | Resolved r -> List [ Atom "Resolved"; sexp_of_resolved sexp_of_a r ]
      | Root s -> List [ Atom "Root"; atom s ]
      | Dot (md, s) -> List [ Atom "Dot" ; List [sexp_of_t sexp_of_a md; atom s]]

  let rec hash_resolved : type k. ('a -> int) -> ('a, k) Types.Resolved.t -> int =
    fun hash p ->
      let open Types.Resolved in
      match p with
      | Identifier id ->
        Identifier.hash ~hash id
      | SubstAlias (r1, r2) ->
        Hashtbl.hash (40, Path.Resolved.hash ~hash r1, hash_resolved hash r2)
      | Module(p, s) ->
        Hashtbl.hash (41, hash_resolved hash p, s)
      | Canonical (rp, p) ->
        Hashtbl.hash (42, hash_resolved hash rp, hash_reference hash p)
      | ModuleType(p, s) ->
        Hashtbl.hash (43, hash_resolved hash p, s)
      | Type(p, s) ->
        Hashtbl.hash (44, hash_resolved hash p, s)
      | Constructor(p, s) ->
        Hashtbl.hash (45, hash_resolved hash p, s)
      | Field(p, s) ->
        Hashtbl.hash (46, hash_resolved hash p, s)
      | Extension(p, s) ->
        Hashtbl.hash (47, hash_resolved hash p, s)
      | Exception(p, s) ->
        Hashtbl.hash (48, hash_resolved hash p, s)
      | Value(p, s) ->
        Hashtbl.hash (49, hash_resolved hash p, s)
      | Class(p, s) ->
        Hashtbl.hash (50, hash_resolved hash p, s)
      | ClassType(p, s) ->
        Hashtbl.hash (51, hash_resolved hash p, s)
      | Method(p, s) ->
        Hashtbl.hash (52, hash_resolved hash p, s)
      | InstanceVariable(p, s) ->
        Hashtbl.hash (53, hash_resolved hash p, s)
      | Label(p, s) ->
        Hashtbl.hash (54, hash_resolved hash p, s)

  and hash_reference : type k. ('a -> int) -> ('a, k) Types.Reference.t -> int =
    fun hash p ->
      let open Types.Reference in
      match p with
      | Resolved p -> hash_resolved hash p
      | Root s -> Hashtbl.hash (55, s)
      | Dot(p, s) -> Hashtbl.hash (56, hash_reference hash p, s)

  module Resolved = struct
    open Identifier
    open Kind

    include Types.Resolved

    let sexp_of_t sexp_of_a t = sexp_of_resolved sexp_of_a t

    let ident_module : 'a Identifier.module_ -> _ = function
      | Root _ | Module _ | Argument _ as x -> Identifier x

    let ident_module_type : 'a Identifier.module_type -> _ = function
      | ModuleType _ as x -> Identifier x

    let ident_type : 'a Identifier.type_ -> _ = function
      | Type _ | CoreType _ as x -> Identifier x

    let ident_constructor : 'a Identifier.constructor -> _ = function
      | Constructor _ as x -> Identifier x

    let ident_field : 'a Identifier.field -> _ = function
      | Field _ as x -> Identifier x

    let ident_extension : 'a Identifier.extension -> _ = function
      | Extension _ as x -> Identifier x

    let ident_exception : 'a Identifier.exception_ -> _ = function
      | Exception _ | CoreException _ as x -> Identifier x

    let ident_value : 'a Identifier.value -> _ = function
      | Value _ as x -> Identifier x

    let ident_class : 'a Identifier.class_ -> _ = function
      | Class _ as x -> Identifier x

    let ident_class_type : 'a Identifier.class_type -> _ = function
      | ClassType _ as x -> Identifier x

    let ident_method : 'a Identifier.method_ -> _ = function
      | Method _ as x -> Identifier x

    let ident_instance_variable : 'a Identifier.instance_variable -> _ =
      function InstanceVariable _ as x -> Identifier x

    let ident_label : 'a Identifier.label -> _ = function
      | Label _ as x -> Identifier x

    let signature_of_module : 'a module_ -> _ = function
      | Identifier (Root _ | Module _ | Argument _)
      | SubstAlias _
      | Module _
      | Canonical _ as x -> x

    let signature_of_module_type : 'a module_type -> _ = function
      | Identifier (ModuleType _) | ModuleType _ as x -> x

    let class_signature_of_class : 'a class_ -> _ = function
      | Identifier (Class _) | Class _ as x -> x

    let class_signature_of_class_type : 'a class_type -> _ = function
      | Identifier (Class _ | ClassType _) | Class _ | ClassType _ as x -> x

    let parent_of_signature : 'a signature -> _ = function
      | Identifier (Root _ | Module _ | Argument _ | ModuleType _)
      | SubstAlias _ | Module _ | ModuleType _ | Canonical _ as x -> x

    let parent_of_class_signature : 'a class_signature -> _ =
      function
      | Identifier (Class _ | ClassType _) | Class _ | ClassType _ as x -> x

    let parent_of_datatype : 'a datatype -> _ = function
      | Identifier (Type _ |CoreType _) | Type _ as x -> x

    let any : type k. ('a, k) t -> 'a any = function
      | Identifier (Root _ ) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _ ) as x -> x
      | Identifier (ModuleType _) as x -> x
      | Identifier (Type _) as x -> x
      | Identifier (CoreType _) as x -> x
      | Identifier (Constructor _) as x -> x
      | Identifier (Field _) as x -> x
      | Identifier (Extension _) as x -> x
      | Identifier (Exception _) as x -> x
      | Identifier (CoreException _) as x -> x
      | Identifier (Value _) as x -> x
      | Identifier (Class _) as x -> x
      | Identifier (ClassType _) as x -> x
      | Identifier (Method _) as x -> x
      | Identifier (InstanceVariable _) as x -> x
      | Identifier (Label _) as x -> x
      | SubstAlias _ as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | ModuleType _ as x -> x
      | Type _ as x -> x
      | Constructor _ as x -> x
      | Field _ as x -> x
      | Extension _ as x -> x
      | Exception _ as x -> x
      | Value _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x
      | Method _ as x -> x
      | InstanceVariable _ as x -> x
      | Label _ as x -> x

    let open_module : 'b. 'a module_ -> ('a, [< kind > `Module ] as 'b) t =
      function
      | Identifier (Root _ | Module _ | Argument _) | SubstAlias _
      | Module _ | Canonical _ as x -> x

    let rec parent_signature_identifier : 'a signature -> 'a Identifier.signature =
      function
      | Identifier id -> id
      | SubstAlias(sub, _) -> Path.Resolved.parent_module_identifier sub
      | Module(m, n) -> Module(parent_signature_identifier m, n)
      | Canonical(_, Types.Reference.Resolved r) ->
        parent_signature_identifier (open_module r)
      | Canonical (r, _) -> parent_signature_identifier (open_module r)
      | ModuleType(m, s) -> ModuleType(parent_signature_identifier m, s)

    let parent_type_identifier : 'a datatype -> 'a Identifier.datatype =
      function
      | Identifier id -> id
      | Type(sg, s) -> Type(parent_signature_identifier sg, s)

    let parent_class_signature_identifier :
      'a class_signature -> 'a Identifier.class_signature =
      function
      | Identifier id -> id
      | Class(sg, s) -> Class(parent_signature_identifier sg, s)
      | ClassType(sg, s) -> ClassType(parent_signature_identifier sg, s)

    let rec parent_identifier : 'a parent -> 'a Identifier.parent =
      function
      | Identifier id -> id
      | SubstAlias(sub, _) ->
        Identifier.parent_of_signature
          (Path.Resolved.parent_module_identifier sub)
      | Module(m, n) -> Module(parent_signature_identifier m, n)
      | Canonical(_, Types.Reference.Resolved r) ->
        parent_identifier (open_module r)
      | Canonical (r, _) -> parent_identifier (open_module r)
      | ModuleType(m, s) -> ModuleType(parent_signature_identifier m, s)
      | Type(sg, s) -> Type(parent_signature_identifier sg, s)
      | Class(sg, s) -> Class(parent_signature_identifier sg, s)
      | ClassType(sg, s) -> ClassType(parent_signature_identifier sg, s)

    let rec identifier: type k. ('a, k) t -> ('a, k) Identifier.t = function
       | Identifier id -> id
       | SubstAlias(_, p) -> identifier (open_module p)
       | Module(s, n) -> Module(parent_signature_identifier s, n)
       | Canonical(_, Types.Reference.Resolved p) -> begin
           match identifier p with
           | Root _ | Module _ | Argument _ as x -> x
         end
       | Canonical(p, _) -> begin
           match identifier p with
           | Root _ | Module _ | Argument _ as x -> x
         end
       | ModuleType(s, n) -> ModuleType(parent_signature_identifier s, n)
       | Type(s, n) -> Type(parent_signature_identifier s, n)
       | Constructor(s, n) -> Constructor(parent_type_identifier s, n)
       | Field(s, n) -> Field(parent_identifier s, n)
       | Extension(s, n) -> Extension(parent_signature_identifier s, n)
       | Exception(s, n) -> Exception(parent_signature_identifier s, n)
       | Value(s, n) -> Value(parent_signature_identifier s, n)
       | Class(s, n) -> Class(parent_signature_identifier s, n)
       | ClassType(s, n) -> ClassType(parent_signature_identifier s, n)
       | Method(s, n) -> Method(parent_class_signature_identifier s, n)
       | InstanceVariable(s, n) ->
         InstanceVariable(parent_class_signature_identifier s, n)
       | Label(s, n) -> Label(parent_identifier s, n)

    let equal ~equal r1 r2 =
      let rec loop : type k. ('a -> 'a -> bool) ->
                              ('a, k) t -> ('a, k) t -> bool =
        fun equal id1 id2 ->
          match id1, id2 with
          | Identifier id1, Identifier id2 ->
              Identifier.equal ~equal id1 id2
          | Module(r1, s1), Module(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | ModuleType(r1, s1), ModuleType(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Type(r1, s1), Type(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Constructor(r1, s1), Constructor(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Field(r1, s1), Field(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Extension(r1, s1), Extension(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Exception(r1, s1), Exception(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Value(r1, s1), Value(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Class(r1, s1), Class(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | ClassType(r1, s1), ClassType(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Method(r1, s1), Method(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | InstanceVariable(r1, s1), InstanceVariable(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | Label(r1, s1), Label(r2, s2) ->
              s1 = s2 && loop equal r1 r2
          | _, _ -> false
      in
        loop equal r1 r2

    let hash ~hash p = hash_resolved hash p

    type ('a, 'b) rebase_result =
      | Stop of ('a, 'b) t
      | Continue of ('a, 'b) Identifier.t * Reversed.t

    let x_ _ = Atom ""

    let rec rebase_module_reference :
      Reversed.t -> 'a module_ -> ('a, Kind.reference_module) rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed @@ signature_of_module id) in
          let new_base = Reversed.remove_prefix rev ~of_:new_base in
          Continue (id, new_base)
        | SubstAlias _ -> Stop t (* FIXME? *)
        | Module (m, s) ->
          begin match rebase_signature_reference new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.Module(id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
              Stop (Identifier id)
          end
        | Canonical (rp, Types.Reference.Resolved p) ->
          (* We only care about printing at this point, so let's drop the lhs. *)
          rebase_module_reference new_base (signature_of_module p)
        | Canonical (rp, p) ->
          begin match rebase_module_reference new_base (signature_of_module rp) with
          | Stop rp' -> Stop (Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            Stop t
          end

    and rebase_signature_reference :
      Reversed.t -> 'a signature -> ('a, Kind.signature) rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed id) in
          let new_base = Reversed.remove_prefix rev ~of_:new_base in
          Continue (id, new_base)
        | ModuleType (m, s) ->
          begin match rebase_signature_reference new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.ModuleType(id, s) in
            match new_base with
            | Reversed.ModuleType s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
              Stop (Identifier id)
          end
        | Module _ | Canonical _ as x ->
          begin match rebase_module_reference new_base x with
          | Stop rp -> Stop (signature_of_module rp)
          | Continue (id, rev) ->
            Continue (Identifier.signature_of_module id, rev)
          end
        | SubstAlias _ -> Stop t (* FIXME? *)

    let rec rebase : type k. Reversed.t -> ('a, k) t -> ('a, k) t =
      fun new_base t ->
        match t with
        | Identifier _ -> t
        | SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | Module (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Module(id, s))
          | Stop mp' -> Module (mp', s)
          end
        | Canonical (p, Types.Reference.Resolved rp) ->
          begin match rebase_module_reference new_base (signature_of_module rp) with
          | Continue (id, _) -> ident_module id
          | Stop rp ->
            (* Easier to reexport a canonical than get the type for rp right... *)
            Canonical (p, Types.Reference.Resolved rp)
          end
        | Canonical (rp, p) ->
          begin match rebase_module_reference new_base rp with
          | Stop rp' -> Canonical (rp', p)
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            t
          end
        | ModuleType (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.ModuleType (id, s))
          | Stop mp' -> ModuleType (mp', s)
          end
        | Type (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Type (id, s))
          | Stop mp' -> Type (mp', s)
          end
        | Constructor (parent, s) ->
          Constructor(rebase new_base parent, s)
        | Field (parent, s) ->
          Field(rebase new_base parent, s)
        | Extension (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Extension (id, s))
          | Stop mp' -> Extension (mp', s)
          end
        | Exception (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Exception (id, s))
          | Stop mp' -> Exception (mp', s)
          end
        | Value (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Value (id, s))
          | Stop mp' -> Value (mp', s)
          end
        | Class (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Class (id, s))
          | Stop mp' -> Class (mp', s)
          end
        | ClassType (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.ClassType (id, s))
          | Stop mp' -> ClassType (mp', s)
          end
        | Method (mp, s) ->
            Method (rebase new_base mp, s)
        | InstanceVariable (mp, s) ->
            InstanceVariable (rebase new_base mp, s)
        | Label (mp, s) ->
            Label (rebase new_base mp, s)

    let rebase id t =
      let rev = Identifier.to_reversed id in
      rebase rev t
  end

  open Identifier
  open Resolved
  open Kind

  include Types.Reference

  let ident_module : 'a Identifier.module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> Resolved (Identifier x)

  let ident_module_type : 'a Identifier.module_type -> _ = function
    | ModuleType _ as x -> Resolved (Identifier x)

  let ident_type : 'a Identifier.type_ -> _ = function
    | Type _ | CoreType _ as x -> Resolved (Identifier x)

  let ident_constructor : 'a Identifier.constructor -> _ = function
    | Constructor _ as x -> Resolved (Identifier x)

  let ident_field : 'a Identifier.field -> _ = function
    | Field _ as x -> Resolved (Identifier x)

  let ident_extension : 'a Identifier.extension -> _ = function
    | Extension _ as x -> Resolved (Identifier x)

  let ident_exception : 'a Identifier.exception_ -> _ = function
    | Exception _ | CoreException _ as x -> Resolved (Identifier x)

  let ident_value : 'a Identifier.value -> _ = function
    | Value _ as x -> Resolved (Identifier x)

  let ident_class : 'a Identifier.class_ -> _ = function
    | Class _ as x -> Resolved (Identifier x)

  let ident_class_type : 'a Identifier.class_type -> _ = function
    | ClassType _ as x -> Resolved (Identifier x)

  let ident_method : 'a Identifier.method_ -> _ = function
    | Method _ as x -> Resolved (Identifier x)

  let ident_instance_variable : 'a Identifier.instance_variable -> _ =
    function InstanceVariable _ as x -> Resolved (Identifier x)

  let ident_label : 'a Identifier.label -> _ = function
    | Label _ as x -> Resolved (Identifier x)

  let signature_of_module : 'a module_ -> _ = function
    | Resolved (Identifier (Root _ | Module _ | Argument _)
               | SubstAlias _ | Module _ | Canonical _)
    | Root _ | Dot _ as x -> x

  let signature_of_module_type : 'a module_type -> _ = function
    | Resolved (Identifier (ModuleType _) | ModuleType _)
    | Root _ | Dot _ as x -> x

  let class_signature_of_class : 'a class_ -> _ = function
    | Resolved (Identifier (Class _) | Class _)
    | Root _ | Dot _ as x -> x

  let class_signature_of_class_type : 'a class_type -> _ = function
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _)
    | Root _ | Dot _ as x -> x

  let parent_of_signature : 'a signature -> 'a parent = function
    | Resolved (Identifier (Root _ | Module _ | Argument _ | ModuleType _)
               | SubstAlias _ | Module _ | ModuleType _ | Canonical _)
    | Root _ | Dot _ as x -> x

  let parent_of_class_signature : 'a class_signature -> 'a parent = function
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _)
    | Root _ | Dot _ as x -> x

  let parent_of_datatype : 'a datatype -> 'a parent = function
    | Resolved (Identifier (Type _ | CoreType _) | Type _)
    | Root _ | Dot _ as x -> x

  let any : type k. ('a, k) t -> 'a any = function
    | Resolved (Identifier (Root _)) as x -> x
    | Resolved (Identifier (Module _)) as x -> x
    | Resolved (Identifier (Argument _)) as x -> x
    | Resolved (Identifier (ModuleType _)) as x -> x
    | Resolved (Identifier (Type _)) as x -> x
    | Resolved (Identifier (CoreType _)) as x -> x
    | Resolved (Identifier (Constructor _)) as x -> x
    | Resolved (Identifier (Field _)) as x -> x
    | Resolved (Identifier (Extension _)) as x -> x
    | Resolved (Identifier (Exception _)) as x -> x
    | Resolved (Identifier (CoreException _)) as x -> x
    | Resolved (Identifier (Value _)) as x -> x
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Identifier (Method _)) as x -> x
    | Resolved (Identifier (InstanceVariable _)) as x -> x
    | Resolved (Identifier (Label _)) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Canonical _) as x -> x
    | Resolved (ModuleType _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Constructor _) as x -> x
    | Resolved (Field _) as x -> x
    | Resolved (Extension _) as x -> x
    | Resolved (Exception _) as x -> x
    | Resolved (Value _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Resolved (Method _) as x -> x
    | Resolved (InstanceVariable _) as x -> x
    | Resolved (Label _) as x -> x
    | Root _ as x -> x
    | Dot _ as x -> x

  let module_ p name =
    match p with
    | Resolved p -> Resolved (Module(p, name))
    | p -> Dot(parent_of_signature p, name)

  let module_type p name =
    match p with
    | Resolved p -> Resolved (ModuleType(p, name))
    | p -> Dot(parent_of_signature p, name)

  let type_ p name =
    match p with
    | Resolved p -> Resolved (Type(p, name))
    | p -> Dot(parent_of_signature p, name)

  let constructor p arg =
    match p with
    | Resolved p -> Resolved (Constructor(p, arg))
    | p -> Dot(parent_of_datatype p, arg)

  let field p arg =
    match p with
    | Resolved p -> Resolved (Field(p, arg))
    | p -> Dot(p, arg)

  let extension p arg =
    match p with
    | Resolved p -> Resolved (Extension(p, arg))
    | p -> Dot(parent_of_signature p, arg)

  let exception_ p arg =
    match p with
    | Resolved p -> Resolved (Exception(p, arg))
    | p -> Dot(parent_of_signature p, arg)

  let value p arg =
    match p with
    | Resolved p -> Resolved (Value(p, arg))
    | p -> Dot(parent_of_signature p, arg)

  let class_ p name =
    match p with
    | Resolved p -> Resolved (Class(p, name))
    | p -> Dot(parent_of_signature p, name)

  let class_type p name =
    match p with
    | Resolved p -> Resolved (ClassType(p, name))
    | p -> Dot(parent_of_signature p, name)

  let method_ p arg =
    match p with
    | Resolved p -> Resolved (Method(p, arg))
    | p -> Dot(parent_of_class_signature p, arg)

  let instance_variable p arg =
    match p with
    | Resolved p -> Resolved (InstanceVariable(p, arg))
    | p -> Dot(parent_of_class_signature p, arg)

  let label p arg =
    match p with
    | Resolved p -> Resolved (Label(p, arg))
    | p -> Dot(p, arg)


  let equal ~equal r1 r2 =
    let rec loop : type k. ('a -> 'a -> bool) ->
                            ('a, k) t -> ('a, k) t -> bool =
      fun equal r1 r2 ->
        match r1, r2 with
        | Resolved r1, Resolved r2 ->
            Resolved.equal ~equal r1 r2
        | Root s1, Root s2 ->
            s1 = s2
        | Dot(r1, s1), Dot(r2, s2) ->
            s1 = s2 && loop equal r1 r2
        | _, _ -> false
    in
      loop equal r1 r2

  let hash ~hash p = hash_reference hash p

end
