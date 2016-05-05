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

open DocOckPaths
open DocOckTypes
open DocOckComponents
open DocOckComponentTbl

type 'a lookup_result = 'a DocOckComponentTbl.lookup_result =
  | Forward_reference
  | Found of 'a
  | Not_found

type 'a parent_module_path =
  | Resolved of 'a Path.Resolved.module_ * 'a Sig.t
  | Unresolved of 'a Path.module_

type 'a parent_module_type_path =
  | Resolved of 'a Path.Resolved.module_type * 'a Sig.t
  | Unresolved of 'a Path.module_type

let rec find_with_path_substs
        : 'b 'c. ('a Sig.t -> 'b) -> ('a Path.Resolved.module_ -> 'b -> 'c) ->
                   ('a Path.Resolved.module_ -> 'c) -> 'a t -> 'a Unit.t ->
                     'a Path.Resolved.module_ -> 'a Sig.t -> 'c =
  fun find resolved unresolved tbl u pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      try
        match Sig.find_parent_subst parent with
        | Parent.Subst subp -> begin
            match resolve_parent_module_type_path tbl u subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
                find_with_path_substs
                  find resolved unresolved tbl u
                  (Path.Resolved.Subst(subpr, pr)) parent
          end
        | Parent.SubstAlias subp -> begin
            match resolve_parent_module_path tbl u subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
                find_with_path_substs
                  find resolved unresolved tbl u
                  (Path.Resolved.SubstAlias(subpr, pr)) parent
          end
      with Not_found -> unresolved pr

and resolve_parent_module_path tbl u p : 'a parent_module_path =
  let open Path.Resolved in
  let open Path in
    match p with
    | Root s -> begin
        match base tbl u s with
        | Not_found -> Unresolved p
        | Forward_reference ->
            (* TODO: fail? *)
            Unresolved p
        | Found r ->
            let p = Identifier (Identifier.Root(r, s)) in
              Resolved(p, resolved_module_path tbl u p)
      end
    | Resolved r -> Resolved(r, resolved_module_path tbl u r)
    | Dot(pr, name) -> begin
        match resolve_parent_module_path tbl u pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : 'a Parent.module_) =
              (Resolved(Module(pr, name), md) : 'a parent_module_path)
            in
            let unresolved pr =
              (Unresolved(Dot(Resolved pr, name)) : 'a parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_module name)
                resolved unresolved tbl u pr parent
      end
    | Apply(pr, arg) -> begin
        let arg = resolve_module_path tbl u arg in
        match resolve_parent_module_path tbl u pr with
        | Unresolved pr -> Unresolved(Apply(pr, arg))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : 'a Parent.module_) =
              (Resolved(Apply(pr, arg), md) : 'a parent_module_path)
            in
            let unresolved pr =
              (Unresolved(Apply(Resolved pr, arg)) : 'a parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_apply (module_path tbl u) arg)
                resolved unresolved tbl u pr parent
      end

and resolve_parent_module_type_path tbl u p : 'a parent_module_type_path =
  let open Path.Resolved in
  let open Path in
    match p with
    | Resolved r -> Resolved(r, resolved_module_type_path tbl u r)
    | Dot(pr, name) -> begin
        match resolve_parent_module_path tbl u pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.ModuleType md : 'a Parent.module_type) =
              (Resolved(ModuleType(pr, name), md) : 'a parent_module_type_path)
            in
            let unresolved pr =
              Unresolved(Dot(Resolved pr, name))
            in
              find_with_path_substs
                (Sig.find_parent_module_type name)
                resolved unresolved tbl u pr parent
      end

and resolve_module_path tbl u =
  let open Path.Resolved in
  let open Path in function
  | Root s as p -> begin
      match base tbl u s with
      | Not_found -> p
      | Forward_reference ->
          (* TODO: fail? *)
          p
      | Found r -> Resolved (Identifier (Identifier.Root(r, s)))
    end
  | Resolved r as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_module_path tbl u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (Element.Module : Element.signature_module) =
            Resolved (Module(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_module_element name)
              resolved unresolved tbl u p parent
    end
  | Apply(p, arg) -> begin
      let arg = resolve_module_path tbl u arg in
        match resolve_parent_module_path tbl u p with
        | Unresolved p -> Apply(p, arg)
        | Resolved(p, parent) ->
          let resolved p (Element.Module : Element.signature_module) =
            Resolved (Apply(p, arg))
          in
          let unresolved p =
            Apply(Resolved p, arg)
          in
            find_with_path_substs
              Sig.find_apply_element
              resolved unresolved tbl u p parent
    end

and resolve_module_type_path tbl u =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : 'a Path.module_type) as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_module_path tbl u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (Element.ModuleType : Element.signature_module_type) =
            Resolved (ModuleType(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_module_type_element name)
              resolved unresolved tbl u p parent
    end

and resolve_type_path tbl u =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : 'a Path.type_) as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_module_path tbl u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_type -> _ = function
            | Element.Type -> Resolved (Type(p, name))
            | Element.Class -> Resolved (Class(p, name))
            | Element.ClassType -> Resolved (ClassType(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_type_element name)
              resolved unresolved tbl u p parent
    end

and resolve_class_type_path tbl u =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : 'a Path.class_type) as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_module_path tbl u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_class_type -> _ = function
            | Element.Class -> Resolved (Class(p, name))
            | Element.ClassType -> Resolved (ClassType(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_class_type_element name)
              resolved unresolved tbl u p parent
    end

type 'a parent_fragment =
  | Resolved of 'a Fragment.Resolved.signature * 'a Sig.t
  | Unresolved of 'a Fragment.signature

let rec find_with_fragment_substs
        : 'b 'c. ('a Sig.t -> 'b) ->
                   ('a Fragment.Resolved.signature -> 'b -> 'c) ->
                     ('a Fragment.Resolved.signature -> 'c) ->
                       'a t -> 'a Unit.t ->
                         'a Fragment.Resolved.signature -> 'a Sig.t -> 'c =
  fun find resolved unresolved tbl u pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      try
        match Sig.find_parent_subst parent with
        | Parent.Subst subp -> begin
            match resolve_parent_module_type_path tbl u subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
                find_with_fragment_substs
                  find resolved unresolved tbl u
                  (Fragment.Resolved.Subst(subpr, pr)) parent
          end
        | Parent.SubstAlias subp -> begin
            match resolve_parent_module_path tbl u subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
                find_with_fragment_substs
                  find resolved unresolved tbl u
                  (Fragment.Resolved.SubstAlias(subpr, pr)) parent
          end
      with Not_found -> unresolved pr

let rec resolve_parent_fragment tbl base u p
        : 'a parent_fragment =
  let open Fragment.Resolved in
  let open Fragment in
    match p with
    | (Resolved r : 'a signature) ->
          Resolved(r, resolved_signature_fragment base r)
    | Dot(pr, name) -> begin
        match resolve_parent_fragment tbl base u pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : 'a Parent.module_) =
              (Resolved(Module(pr, name), md) : 'a parent_fragment)
            in
            let unresolved pr =
              (Unresolved(Dot(Resolved pr, name)) : 'a parent_fragment)
            in
              find_with_fragment_substs
                (Sig.find_parent_module name)
                resolved unresolved tbl u pr parent
      end

and resolve_module_fragment tbl base u =
  let open Fragment.Resolved in
  let open Fragment in function
  | Resolved r as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_fragment tbl base u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (Element.Module : Element.signature_module) =
            Resolved (Module(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_fragment_substs
              (Sig.find_module_element name)
              resolved unresolved tbl u p parent
    end

and resolve_type_fragment tbl base u =
  let open Fragment.Resolved in
  let open Fragment in function
  | (Resolved r : 'a Fragment.type_) as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_fragment tbl base u p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_type -> _ = function
            | Element.Type -> Resolved (Type(p, name))
            | Element.Class -> Resolved (Class(p, name))
            | Element.ClassType -> Resolved (ClassType(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_fragment_substs
              (Sig.find_type_element name)
              resolved unresolved tbl u p parent
    end

type ('a, 'b) parent_reference =
  | ResolvedSig : 'a Reference.Resolved.signature * 'a Sig.t ->
                  ('a, [> `Module | `ModuleType]) parent_reference
  | ResolvedDatatype : 'a Reference.Resolved.datatype * 'a Datatype.t ->
                   ('a, [> `Type]) parent_reference
  | ResolvedClassSig : 'a Reference.Resolved.class_signature * 'a ClassSig.t ->
                   ('a, [> `Class | `ClassType]) parent_reference
  | Unresolved of 'a Reference.parent

type 'a parent_kind =
  | PParent : Kind.parent parent_kind
  | PSig : Kind.signature parent_kind
  | PDatatype : Kind.datatype parent_kind
  | PClassSig : Kind.class_signature parent_kind
  | PSigOrType : [Kind.signature | Kind.datatype] parent_kind

let find_parent_reference (type k) (kind : k parent_kind) r name parent
    : (_, k) parent_reference =
  let open Reference.Resolved in
    match kind with
    | PParent -> begin
        match Sig.find_parent name parent with
        | Parent.Module md -> ResolvedSig(Module(r, name), md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
        | Parent.Class cls -> ResolvedClassSig(Class(r, name), cls)
        | Parent.ClassType cls -> ResolvedClassSig(ClassType(r, name), cls)
      end
    | PSig -> begin
        match Sig.find_parent_signature name parent with
        | Parent.Module md -> ResolvedSig(Module(r, name), md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
      end
    | PDatatype -> begin
        match Sig.find_parent_datatype name parent with
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
      end
    | PClassSig -> begin
        match Sig.find_parent_class_signature name parent with
        | Parent.Class cls -> ResolvedClassSig(Class(r, name), cls)
        | Parent.ClassType cls -> ResolvedClassSig(ClassType(r, name), cls)
      end
    | PSigOrType -> begin
        match Sig.find_parent_sig_or_type name parent with
        | Parent.Module md -> ResolvedSig(Module(r, name), md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
      end

let rec resolve_parent_reference :
  type k . k parent_kind -> 'a t -> 'a Unit.t ->
       'a Reference.parent -> ('a, k) parent_reference =
    fun kind tbl u r ->
      let open Identifier in
      let open Reference.Resolved in
      let open Reference in
        match r with
        | Root s -> begin
            match base tbl u s with
            | Not_found -> Unresolved r
            | Forward_reference ->
                (* TODO: fail? *)
                Unresolved r
            | Found base ->
                let root = Identifier (Identifier.Root(base, s)) in
                  match kind with
                  | PParent ->
                      ResolvedSig(root, resolved_signature_reference tbl root)
                  | PSig ->
                      ResolvedSig(root, resolved_signature_reference tbl root)
                  | PSigOrType ->
                      ResolvedSig(root, resolved_signature_reference tbl root)
                  | _ -> Unresolved r
          end
        | Resolved
            (Identifier (Root _ | Module _ | Argument _ | ModuleType _)
             | Module _ | ModuleType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedSig(rr, resolved_signature_reference tbl rr)
            | PSig ->
                ResolvedSig(rr, resolved_signature_reference tbl rr)
            | PSigOrType ->
                ResolvedSig(rr, resolved_signature_reference tbl rr)
            | _ -> Unresolved r
          end
        | Resolved
            (Identifier (Class _ | ClassType _)
            | Class _ | ClassType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedClassSig(rr, resolved_class_signature_reference tbl rr)
            | PClassSig ->
                ResolvedClassSig(rr, resolved_class_signature_reference tbl rr)
            | _ -> Unresolved r
          end
        | Resolved (Identifier (Type _ | CoreType _) | Type _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedDatatype(rr, resolved_datatype_reference tbl rr)
            | PDatatype ->
                ResolvedDatatype(rr, resolved_datatype_reference tbl rr)
            | PSigOrType ->
                ResolvedDatatype(rr, resolved_datatype_reference tbl rr)
            | _ -> Unresolved r
          end
        | Dot(r, name) -> begin
            match resolve_parent_reference PSig tbl u r with
            | Unresolved r -> Unresolved(Dot(r, name))
            | ResolvedSig(r, parent) ->
                try
                  find_parent_reference kind r name parent
                with Not_found ->
                  let r = Resolved.parent_of_signature r in
                    Unresolved(Dot(Resolved r, name))
          end

and resolve_module_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root s -> begin
        match base tbl u s with
        | Not_found -> r
        | Forward_reference ->
            (* TODO: fail? *)
            r
        | Found base -> Resolved (Identifier (Identifier.Root(base, s)))
      end
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              let Element.Module =
                Sig.find_module_element name parent
              in
                Resolved (Module(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_module_type_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              let Element.ModuleType =
                Sig.find_module_type_element name parent
              in
                Resolved (ModuleType(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_type_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              match Sig.find_type_element name parent with
              | Element.Type -> Resolved (Type(r, name))
              | Element.Class -> Resolved (Class(r, name))
              | Element.ClassType -> Resolved (ClassType(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_constructor_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSigOrType tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) -> begin
            try
              match Sig.find_constructor_element name parent with
              | Element.Constructor type_name ->
                  Resolved (Constructor(Type(r, type_name), name))
              | Element.Extension -> Resolved (Extension(r, name))
              | Element.Exception -> Resolved (Exception(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
          end
        | ResolvedDatatype(r, parent) -> begin
            try
              let Element.Constructor _ =
                Datatype.find_constructor_element name parent
              in
                Resolved (Constructor(r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved r, name)
          end
      end

and resolve_field_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSigOrType tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) -> begin
            try
              let Element.Field type_name =
                Sig.find_field_element name parent
              in
                Resolved (Field(Type(r, type_name), name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
          end
        | ResolvedDatatype(r, parent) -> begin
            try
              let Element.Field _ =
                Datatype.find_field_element name parent
              in
                Resolved (Field(r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved r, name)
          end
      end

and resolve_extension_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              match Sig.find_extension_element name parent with
              | Element.Extension -> Resolved (Extension(r, name))
              | Element.Exception -> Resolved (Exception(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_exception_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              let Element.Exception =
                Sig.find_exception_element name parent
              in
                Resolved (Exception(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_value_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              let Element.Value =
                Sig.find_value_element name parent
              in
                Resolved (Value(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_class_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              let Element.Class =
                Sig.find_class_element name parent
              in
                Resolved (Class(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_class_type_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
            try
              match Sig.find_class_type_element name parent with
              | Element.ClassType -> Resolved (ClassType(r, name))
              | Element.Class -> Resolved (Class(r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
      end

and resolve_method_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PClassSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.Method =
                ClassSig.find_method_element name parent
              in
                Resolved (Method(r, name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved r, name)
      end

and resolve_instance_variable_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PClassSig tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.InstanceVariable =
                ClassSig.find_instance_variable_element name parent
              in
                Resolved (InstanceVariable(r, name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved r, name)
      end

and resolve_label_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PParent tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) -> begin
            try
              match Sig.find_label_element name parent with
              | Element.Label (Some type_name) ->
                  Resolved (Label(Type(r, type_name), name))
              | Element.Label None ->
                  Resolved (Label(Resolved.parent_of_signature r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
          end
        | ResolvedDatatype(r, parent) -> begin
            let r = Resolved.parent_of_datatype r in
              try
                let Element.Label _ =
                  Datatype.find_label_element name parent
                in
                  Resolved (Label(r, name))
              with Not_found ->
                  Dot(Resolved r, name)
          end
        | ResolvedClassSig(r, parent) -> begin
            let r = Resolved.parent_of_class_signature r in
              try
                let Element.Label _ =
                  ClassSig.find_label_element name parent
                in
                  Resolved (Label(r, name))
              with Not_found ->
                  Dot(Resolved r, name)
          end
      end

and resolve_element_reference tbl u r =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved _ -> r
    | Dot(r, name) -> begin
        match resolve_parent_reference PParent tbl u r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) -> begin
            try
              match Sig.find_element name parent with
              | Element.Module -> Resolved (Module(r, name))
              | Element.ModuleType -> Resolved (ModuleType(r, name))
              | Element.Type -> Resolved (Type(r, name))
              | Element.Constructor type_name ->
                  Resolved (Constructor(Type(r, type_name) , name))
              | Element.Field type_name ->
                  Resolved (Field(Type(r, type_name) , name))
              | Element.Extension -> Resolved (Extension(r, name))
              | Element.Exception -> Resolved (Exception(r, name))
              | Element.Value -> Resolved (Value(r, name))
              | Element.Class -> Resolved (Class(r, name))
              | Element.ClassType -> Resolved (ClassType(r, name))
              | Element.Label (Some type_name) ->
                  Resolved (Label(Type(r, type_name), name))
              | Element.Label None ->
                  Resolved (Label(Resolved.parent_of_signature r, name))
            with Not_found ->
              let r = Resolved.parent_of_signature r in
                Dot(Resolved r, name)
          end
        | ResolvedDatatype(r, parent) -> begin
            try
              match Datatype.find_element name parent with
              | Element.Constructor _ -> Resolved (Constructor(r , name))
              | Element.Field _ -> Resolved (Field(r , name))
              | Element.Label _ ->
                  Resolved (Label(Resolved.parent_of_datatype r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved r, name)
          end
        | ResolvedClassSig(r, parent) -> begin
            try
              match ClassSig.find_element name parent with
              | Element.Method -> Resolved (Method(r, name))
              | Element.InstanceVariable -> Resolved (InstanceVariable(r, name))
              | Element.Label _ ->
                  Resolved (Label(Resolved.parent_of_class_signature r, name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved r, name)
          end
      end

let unwrap opt =
    match opt with
    | Some x -> x
    | None -> assert false

class ['a] resolver ?equal ?hash lookup fetch = object (self)
  val tbl = create ?equal ?hash lookup fetch
  val unit = None

  inherit ['a] DocOckMaps.types as super
  method root x = x

  method identifier_module x = x
  method identifier_module_type x = x
  method identifier_type x = x
  method identifier_constructor x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_value x = x
  method identifier_class x = x
  method identifier_class_type x = x
  method identifier_method x = x
  method identifier_instance_variable x = x
  method identifier_label x = x
  method identifier_signature x = x
  method identifier x = x

  method path_module x = resolve_module_path tbl (unwrap unit) x
  method path_module_type x = resolve_module_type_path tbl (unwrap unit) x
  method path_type x = resolve_type_path tbl (unwrap unit) x
  method path_class_type x = resolve_class_type_path tbl (unwrap unit) x

  method module_ md =
    let open Module in
    let {id; doc; type_} = md in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let sig_id = Identifier.signature_of_module id' in
    let type' = self#module_decl_with_id sig_id type_ in
      if id != id' || doc != doc' || type_ != type' then
        {id = id'; doc = doc'; type_ = type'}
      else md

  method module_type mty =
    let open ModuleType in
    let {id; doc; expr} = mty in
    let id' = self#identifier_module_type id in
    let doc' = self#documentation doc in
    let expr' =
      match expr with
      | None -> expr
      | Some body ->
          let sig_id = Identifier.signature_of_module_type id' in
          let body' = self#module_type_expr_with_id sig_id body in
          if body != body' then Some body'
          else expr
    in
      if id != id' || doc != doc' || expr != expr' then
        {id = id'; doc = doc'; expr = expr'}
      else mty

  method include_ incl =
    let open Include in
    let {parent; doc; decl} = incl in
    let parent' = self#identifier_signature parent in
    let doc' = self#documentation doc in
    let decl' = self#module_decl_with_id parent decl in
      if parent != parent' || doc != doc' || decl != decl' then
        {parent = parent'; doc = doc'; decl = decl'}
      else incl

  method module_type_functor_arg arg =
    match arg with
    | None -> arg
    | Some(id, expr) ->
        let id' = self#identifier_module id in
        let sig_id = Identifier.signature_of_module id' in
        let expr' = self#module_type_expr_with_id sig_id expr in
          if id != id' || expr != expr' then Some(id', expr')
          else arg

  method private module_type_expr_with_id id expr =
    let open ModuleType in
    let unit = unwrap unit in
      match expr with
      | With(body, substs) ->
          let body = self#module_type_expr_with_id id body in
          let base = module_type_expr_with tbl unit id body in
          let substs =
            List.map
              (function
                | ModuleEq(frag, eq) ->
                    let frag = resolve_module_fragment tbl base unit frag in
                    let eq = self#module_equation eq in
                      ModuleEq(frag, eq)
                | TypeEq(frag, eq) ->
                    let frag = resolve_type_fragment tbl base unit frag in
                    let eq = self#type_decl_equation eq in
                      TypeEq(frag, eq)
                | ModuleSubst(frag, p) ->
                    let frag = resolve_module_fragment tbl base unit frag in
                    let p = self#path_module p in
                      ModuleSubst(frag, p)
                | TypeSubst(frag, params, p) ->
                    let frag = resolve_type_fragment tbl base unit frag in
                    let params = List.map self#type_decl_param_name params in
                    let p = self#path_type p in
                      TypeSubst(frag, params, p))
              substs
          in
            With(body, substs)
      | Functor(arg, res) ->
          let arg' = self#module_type_functor_arg arg in
          let res' = self#module_type_expr_with_id id res in
            if res != res' || arg != arg' then Functor(arg', res')
            else expr
      | TypeOf decl ->
          let decl' = self#module_decl_with_id id decl in
            if decl != decl' then TypeOf decl'
            else expr
      | Path _ | Signature _ -> self#module_type_expr expr

  method private module_decl_with_id id decl =
    let open Module in
      match decl with
      | ModuleType expr ->
          let expr' = self#module_type_expr_with_id id expr in
            if expr != expr' then ModuleType expr'
            else decl
      | Alias _ -> self#module_decl decl

  method type_expr_package pkg =
    let open TypeExpr.Package in
    let unit = unwrap unit in
    let path = resolve_module_type_path tbl unit pkg.path in
    let base = module_type_path_with tbl unit path in
    let substitutions =
      List.map
        (fun (frag, eq) ->
           let frag = resolve_type_fragment tbl base unit frag in
           let eq = self#type_expr eq in
             (frag, eq))
        pkg.substitutions
    in
      {path; substitutions}

  method fragment_type x = x
  method fragment_module x = x

  method reference_module x =
    resolve_module_reference tbl (unwrap unit) x
  method reference_module_type x =
    resolve_module_type_reference tbl (unwrap unit) x
  method reference_type x =
    resolve_type_reference tbl (unwrap unit) x
  method reference_constructor x =
    resolve_constructor_reference tbl (unwrap unit) x
  method reference_field x =
    resolve_field_reference tbl (unwrap unit) x
  method reference_extension x =
    resolve_extension_reference tbl (unwrap unit) x
  method reference_exception x =
    resolve_exception_reference tbl (unwrap unit) x
  method reference_value x =
    resolve_value_reference tbl (unwrap unit) x
  method reference_class x =
    resolve_class_reference tbl (unwrap unit) x
  method reference_class_type x =
    resolve_class_type_reference tbl (unwrap unit) x
  method reference_method x =
    resolve_method_reference tbl (unwrap unit) x
  method reference_instance_variable x =
    resolve_instance_variable_reference tbl (unwrap unit) x
  method reference_label x =
    resolve_label_reference tbl (unwrap unit) x
  method reference_any x =
    resolve_element_reference tbl (unwrap unit) x

  method resolve unit =
    let this = {< unit = Some unit >} in
      this#unit unit

end

let build_resolver ?equal ?hash lookup fetch =
  new resolver ?equal ?hash lookup fetch

let resolve r u = r#resolve u
