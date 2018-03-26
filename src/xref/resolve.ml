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

open Model
open Paths
open Components
module CTbl = Component_table

let lpop = Reference.label_parent_of_parent
let rlpop = Reference.Resolved.label_parent_of_parent

type parent_module_path =
  | Resolved of Path.Resolved.module_ * Sig.t
  | Unresolved of Path.module_

type parent_module_type_path =
  | Resolved of Path.Resolved.module_type * Sig.t
  | Unresolved of Path.module_type

let rec add_subst_alias :
  Path.Resolved.module_ ->
  subp:Path.Resolved.module_ ->
  Path.Resolved.module_ = fun orig ~subp ->
  let open Path.Resolved in
  if equal orig subp then orig else
  match orig with
  | Canonical (_, Path.Resolved rp) ->
    add_subst_alias rp ~subp
  | Canonical (rp, p) ->
    let rp' = add_subst_alias rp ~subp in
    if rp == rp' then orig else Canonical (rp', p)
  | Module (p1, s1) -> begin
      match subp with
      | Module (subp, s2) when s1 = s2 ->
        let p1' = add_subst_alias p1 ~subp in
        if p1 == p1' then orig else Module (p1', s1)
      | _ ->
        SubstAlias(subp, orig)
    end
  | SubstAlias (subst, printing) ->
    if equal subst subp then orig else
      SubstAlias (subp, printing)
  (* Not trying to be smart in these cases, let's just subst... *)
  | _ ->
    SubstAlias(subp, orig)

let rec find_with_path_substs
  : 'b 'c. (Sig.t -> 'b) -> (Path.Resolved.module_ -> 'b -> 'c)
    -> (Path.Resolved.module_ -> 'c) -> Identifier.signature option
    -> CTbl.t -> Path.Resolved.module_ -> Sig.t
    -> 'c =
  fun find resolved unresolved ident tbl pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      try
        match Sig.find_parent_subst parent with
        | Parent.Subst subp -> begin
            match resolve_parent_module_type_path ident tbl subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
              let pr' =
                let open Path.Resolved in
                match pr with
                | Canonical (_, Path.Resolved _) ->
                  Path.Resolved.Subst(subpr, pr)
                | Canonical (p1, p2) ->
                  Path.Resolved.Canonical(Subst(subpr, p1), p2)
                | _ ->
                  Path.Resolved.Subst(subpr, pr)
              in
                find_with_path_substs
                  find resolved unresolved ident tbl
                  pr' parent
          end
        | Parent.SubstAlias subp -> begin
            match resolve_parent_module_path ident tbl subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
              let pr' = add_subst_alias pr ~subp:subpr in
              find_with_path_substs
                find resolved unresolved ident tbl
                pr' parent
          end
      with Not_found -> unresolved pr

and resolve_canonical_path :
  type k. Identifier.signature option -> _ -> k Path.Resolved.t ->
  k Path.Resolved.t =
  fun ident tbl p ->
    let open Path.Resolved in
    let open Path in
    match p with
    | Canonical(orig, cano) ->
      let orig' = resolve_resolved_module_path ident tbl orig in
      let cano' = resolve_module_path ident tbl cano in
      if orig == orig' && cano == cano' then p
      else (
        let cano' =
          match ident, cano' with
          | Some ident, Resolved rp ->
            let rp' = rebase ident rp in
            if rp != rp' then Resolved rp' else cano'
          | _ -> cano'
        in
        let c = Canonical(orig, cano') in
        match ident, cano' with
        | Some ident, Path.Resolved (Identifier id)
          when Identifier.equal ident
                 (Identifier.signature_of_module id) ->
          Hidden c
        | _, _ -> c
      )
    | _ -> p

and resolve_parent_module_path ident tbl p : parent_module_path =
  let open Path.Resolved in
  let open Path in
    match p with
    | Root s -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> Unresolved p
        | CTbl.Forward_reference ->
            (* We can't have a forward ref as parent. *)
            Unresolved p
        | CTbl.Found { root ; hidden } ->
            let p = Identifier (Identifier.Root(root, s)) in
            let p = if hidden then Hidden p else p in
              Resolved(p, CTbl.resolved_module_path tbl p)
      end
    | Forward s -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> Unresolved p
        | CTbl.Forward_reference ->
            (* We can't have a forward ref as parent. *)
            Unresolved p
        | CTbl.Found { root ; hidden } ->
            let p = Identifier (Identifier.Root(root, s)) in
            let p = if hidden then Hidden p else p in
              Resolved(p, CTbl.resolved_module_path tbl p)
      end
    | Resolved r -> Resolved(r, CTbl.resolved_module_path tbl r)
    | Dot(pr, name) -> begin
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : Parent.module_) =
              let pr = Module (pr, name) in
              let pr = if Sig.get_hidden md then Hidden pr else pr in
              let pr =
                match Sig.get_canonical md with
                | None -> pr
                | Some (p, _) ->
                  resolve_canonical_path ident tbl (Canonical(pr, p))
              in
              (Resolved(pr, md) : parent_module_path)
            in
            let unresolved pr =
              (Unresolved(Dot(Resolved pr, name)) : parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_module name)
                resolved unresolved ident tbl pr parent
      end
    | Apply(pr, arg) -> begin
        let arg = resolve_module_path ident tbl arg in
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(Apply(pr, arg))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : Parent.module_) =
              let pr = Resolved.Apply(pr, arg) in
              let pr = if Sig.get_hidden md then Hidden pr else pr in
              let pr =
                match Sig.get_canonical md with
                | None -> pr
                | Some (p, _) ->
                  resolve_canonical_path ident tbl (Canonical(pr, p))
              in
              (Resolved(pr, md) : parent_module_path)
            in
            let unresolved pr =
              (Unresolved(Apply(Resolved pr, arg)) : parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_apply (CTbl.module_path tbl) arg)
                resolved unresolved ident tbl pr parent
      end

and resolve_parent_module_type_path ident tbl p : parent_module_type_path =
  let open Path.Resolved in
  let open Path in
    match p with
    | Resolved r -> Resolved(r, CTbl.resolved_module_type_path tbl r)
    | Dot(pr, name) -> begin
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.ModuleType md : Parent.module_type) =
              (Resolved(ModuleType(pr, name), md) : parent_module_type_path)
            in
            let unresolved pr =
              Unresolved(Dot(Resolved pr, name))
            in
              find_with_path_substs
                (Sig.find_parent_module_type name)
                resolved unresolved ident tbl pr parent
      end

and resolve_module_path ident tbl =
  let open Path.Resolved in
  let open Path in function
  | Root s as p -> begin
      match CTbl.base tbl s with
      | CTbl.Not_found -> p
      | CTbl.Forward_reference -> Forward s
      | CTbl.Found { root ; hidden } ->
        let p = Identifier (Identifier.Root(root, s)) in
        Resolved (if hidden then Hidden p else p)
    end
  | Forward s as p -> begin
      match CTbl.base tbl s with
      | CTbl.Not_found -> p
      | CTbl.Forward_reference -> Forward s
      | CTbl.Found { root ; hidden } ->
        let p = Identifier (Identifier.Root(root, s)) in
        Resolved (if hidden then Hidden p else p)
    end
  | Resolved r as p ->
    let r' = resolve_resolved_module_path ident tbl r in
    if r != r' then Resolved r' else p
  | Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (elem : Element.signature_module) =
            let Element.Module {canonical; hidden} = elem in
            let pr = Resolved.Module(p, name) in
            let pr = if hidden then Resolved.Hidden pr else pr in
            let pr =
              match canonical with
              | None -> pr
              | Some (p, _) -> Canonical(pr, p)
            in
            Resolved pr
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_module_element name)
              resolved unresolved ident tbl p parent
    end
  | Apply(p, arg) -> begin
      let arg = resolve_module_path ident tbl arg in
        match resolve_parent_module_path ident tbl p with
        | Unresolved p -> Apply(p, arg)
        | Resolved(p, parent) ->
          let resolved p (elem : Element.signature_module) =
            let Element.Module {canonical; hidden} = elem in
            let pr = Resolved.Apply (p, arg) in
            let pr = if hidden then Resolved.Hidden pr else pr in
            let pr =
              match canonical with
              | None -> pr
              | Some (p, _) -> Canonical(pr, p)
            in
            Resolved pr
          in
          let unresolved p =
            Apply(Resolved p, arg)
          in
            find_with_path_substs
              Sig.find_apply_element
              resolved unresolved ident tbl p parent
    end

and resolve_resolved_module_path :
  type a. _ -> _ -> a Path.Resolved.t -> a Path.Resolved.t =
  fun ident tbl p ->
  let open Path.Resolved in
  match p with
  | Identifier _ -> p
  | Subst(sub, orig) ->
    let sub' = resolve_resolved_module_path ident tbl sub in
    let orig' = resolve_resolved_module_path ident tbl orig in
    if sub != sub' || orig != orig' then Subst(sub', orig')
    else p
  | SubstAlias(sub, orig) ->
    let sub'  = resolve_resolved_module_path ident tbl sub in
    let orig' = resolve_resolved_module_path ident tbl orig in
    if sub != sub' || orig != orig' then SubstAlias(sub', orig')
    else p
  | Hidden hp ->
    let hp'  = resolve_resolved_module_path ident tbl hp in
    if hp != hp' then Hidden hp'
    else p
  | Module(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then
      Module(parent', name)
    else p
  | Canonical(_, _) ->
    resolve_canonical_path ident tbl p
  | Apply(fn, arg) ->
    let fn' = resolve_resolved_module_path ident tbl fn in
    if fn != fn' then Apply(fn', arg)
    else p
  | ModuleType(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then
      ModuleType(parent', name)
    else p
  | Type(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then Type(parent', name)
    else p
  | Class(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then Class(parent', name)
    else p
  | ClassType(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then
      ClassType(parent', name)
    else p

and resolve_module_type_path ident tbl =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : Path.module_type) as p ->
    let r' = resolve_resolved_module_path ident tbl r in
    if r != r' then Resolved r' else p
  | Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
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
              resolved unresolved ident tbl p parent
    end

and resolve_type_path ident tbl =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : Path.type_) as p ->
    let r' = resolve_resolved_module_path ident tbl r in
    if r != r' then Resolved r' else p
  | Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
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
              resolved unresolved ident tbl p parent
    end

and resolve_class_type_path ident tbl =
  let open Path.Resolved in
  let open Path in function
  | (Resolved r : Path.class_type) as p ->
    let r' = resolve_resolved_module_path ident tbl r in
    if r != r' then Resolved r' else p
  | Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
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
              resolved unresolved ident tbl p parent
    end

type parent_fragment =
  | Resolved of Fragment.Resolved.signature * Sig.t
  | Unresolved of Fragment.signature

let rec find_with_fragment_substs
        : 'b 'c. (Sig.t -> 'b) ->
                   (Fragment.Resolved.signature -> 'b -> 'c) ->
                     (Fragment.Resolved.signature -> 'c) ->
                       _ -> CTbl.t ->
                         Fragment.Resolved.signature -> Sig.t -> 'c =
  fun find resolved unresolved ident tbl pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      let open Fragment.Resolved in
      match pr with
      | Subst _ | SubstAlias _ | Module _ as pr -> begin
          try
            match Sig.find_parent_subst parent with
            | Parent.Subst subp -> begin
                match resolve_parent_module_type_path ident tbl subp with
                | Unresolved _ -> unresolved pr
                | Resolved(subpr, parent) ->
                  find_with_fragment_substs
                    find resolved unresolved ident tbl
                    (Fragment.Resolved.Subst(subpr, pr)) parent
              end
            | Parent.SubstAlias subp -> begin
                match resolve_parent_module_path ident tbl subp with
                | Unresolved _ -> unresolved pr
                | Resolved(subpr, parent) ->
                  find_with_fragment_substs
                    find resolved unresolved ident tbl
                    (Fragment.Resolved.SubstAlias(subpr, pr)) parent
              end
          with Not_found -> unresolved pr
        end
      | _ -> unresolved pr (* we can find substs only for modules. *)

let rec resolve_parent_fragment ident tbl base p : parent_fragment =
  let open Fragment.Resolved in
  let open Fragment in
    match p with
    | (Resolved r : signature) ->
          Resolved(r, CTbl.resolved_signature_fragment base r)
    | Dot(pr, name) -> begin
        match resolve_parent_fragment ident tbl base pr with
        | Unresolved pr -> Unresolved(Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (Parent.Module md : Parent.module_) =
              (Resolved(Module(pr, name), md) : parent_fragment)
            in
            let unresolved pr =
              (Unresolved(Dot(Resolved pr, name)) : parent_fragment)
            in
              find_with_fragment_substs
                (Sig.find_parent_module name)
                resolved unresolved ident tbl pr parent
      end

and resolve_module_fragment ident tbl base =
  let open Fragment.Resolved in
  let open Fragment in function
  | Resolved _ as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_fragment ident tbl base p with
      | Unresolved p -> Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (Element.Module _ : Element.signature_module) =
            (* Handle canonical path here? *)
            Resolved (Module(p, name))
          in
          let unresolved p =
            Dot(Resolved p, name)
          in
            find_with_fragment_substs
              (Sig.find_module_element name)
              resolved unresolved ident tbl p parent
    end

and resolve_type_fragment ident tbl base =
  let open Fragment.Resolved in
  let open Fragment in function
  | (Resolved _ : Fragment.type_) as p -> p
  | Dot(p, name) -> begin
      match resolve_parent_fragment ident tbl base p with
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
              resolved unresolved ident tbl p parent
    end

type 'kind parent_reference =
  | ResolvedSig : Reference.Resolved.signature * Sig.t ->
                  [> `Module | `ModuleType] parent_reference
  | ResolvedDatatype : Reference.Resolved.datatype * Datatype.t ->
                   [> `Type] parent_reference
  | ResolvedClassSig : Reference.Resolved.class_signature * ClassSig.t ->
                   [> `Class | `ClassType] parent_reference
  | ResolvedPage : Reference.Resolved.page * Page.t ->
                  [> `Page ] parent_reference
  | Unresolved of Reference.label_parent

type 'kind parent_kind =
  | PParent : Kind.parent parent_kind
  | PSig : Kind.signature parent_kind
  | PClassSig : Kind.class_signature parent_kind
  | PSigOrType : [Kind.signature | Kind.datatype] parent_kind
  | PLabelParent : [Kind.page | Kind.parent] parent_kind

let find_parent_reference (type k) (kind : k parent_kind) r name parent
    : k parent_reference =
  let open Reference.Resolved in
    match kind with
    | PParent -> begin
        match Sig.find_parent name parent with
        | Parent.Module md ->
          let pr = Module (r, name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
        | Parent.Class cls -> ResolvedClassSig(Class(r, name), cls)
        | Parent.ClassType cls -> ResolvedClassSig(ClassType(r, name), cls)
      end
    | PSig -> begin
        match Sig.find_parent_signature name parent with
        | Parent.Module md ->
          let pr = Module (r, name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
      end
    | PClassSig -> begin
        match Sig.find_parent_class_signature name parent with
        | Parent.Class cls -> ResolvedClassSig(Class(r, name), cls)
        | Parent.ClassType cls -> ResolvedClassSig(ClassType(r, name), cls)
      end
    | PSigOrType -> begin
        match Sig.find_parent_sig_or_type name parent with
        | Parent.Module md ->
          let pr = Module (r, name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
      end
    | PLabelParent ->  begin
        match Sig.find_parent name parent with
        | Parent.Module md ->
          let pr = Module (r, name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | Parent.ModuleType md -> ResolvedSig(ModuleType(r, name), md)
        | Parent.Datatype t -> ResolvedDatatype(Type(r, name), t)
        | Parent.Class cls -> ResolvedClassSig(Class(r, name), cls)
        | Parent.ClassType cls -> ResolvedClassSig(ClassType(r, name), cls)
      end

let rec find_with_reference_substs
   : 'b 'c. (Sig.t -> 'b)
  -> (Reference.Resolved.signature -> 'b -> 'c)
  -> (Reference.Resolved.signature -> 'c)
  -> Identifier.signature option -> CTbl.t
  -> Reference.Resolved.signature -> Sig.t
  -> 'c
  = fun find resolved unresolved ident tbl pr parent ->
    try resolved pr (find parent)
    with Not_found ->
      let open Reference.Resolved in
      match pr with
      | Identifier (Identifier.Root _ | Identifier.Module _ | Identifier.Argument _)
      | SubstAlias _ | Module _ | Canonical _ as pr -> begin
          match Sig.find_parent_subst parent with
          | Parent.SubstAlias subp -> begin
              match resolve_parent_module_path ident tbl subp with
              | Unresolved _ -> unresolved pr
              | Resolved(subpr, parent) ->
                find_with_reference_substs find resolved unresolved ident tbl
                  (SubstAlias(subpr, pr)) parent
            end
          | _ -> unresolved pr
          | exception Not_found -> unresolved pr
        end
      | _ -> unresolved pr (* we can find substs only for modules *)

let rec resolve_parent_reference :
  type k . k parent_kind -> Identifier.signature option -> CTbl.t ->
       Reference.parent -> k parent_reference =
    fun kind ident tbl r ->
      let open Identifier in
      let open Reference.Resolved in
      let open Reference in
        match r with
        | Root (s, _) -> begin
            match CTbl.base tbl s with
            | CTbl.Not_found -> Unresolved (lpop r)
            | CTbl.Forward_reference ->
                (* TODO: fail? *)
                Unresolved (lpop r)
            | CTbl.Found {root;_} ->
                let root = Identifier (Identifier.Root(root, s)) in
                  match kind with
                  | PParent ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PSig ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PSigOrType ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PLabelParent ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | _ -> Unresolved (lpop r)
          end
        | Resolved
            (Identifier (Root _ | Module _ | Argument _ | ModuleType _)
             | SubstAlias _ | Module _ | Canonical _ | ModuleType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PSig ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PSigOrType ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PLabelParent ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | Resolved
            (Identifier (Class _ | ClassType _)
            | Class _ | ClassType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | PClassSig ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | PLabelParent ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | Resolved (Identifier (Type _ | CoreType _) | Type _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | PSigOrType ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | PLabelParent ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | Dot(pr, name) -> begin
            match resolve_label_parent_reference PParent ident tbl pr with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  Dot(Resolved (rlpop (Resolved.parent_of_signature pr)), name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
            | _ ->
              Unresolved (lpop r)
          end
        | Module(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (Reference.parent_of_signature pr)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  Module(Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
          end
        | ModuleType(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (Reference.parent_of_signature pr)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  ModuleType(Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
          end
        | Type(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (Reference.parent_of_signature pr)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  Type(Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
          end
        | Class(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (Reference.parent_of_signature pr)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  Class(Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
          end
        | ClassType(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (Reference.parent_of_signature pr)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  ClassType(Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
          end

and resolve_label_parent_reference :
  type k . k parent_kind -> Identifier.signature option -> CTbl.t ->
       Reference.label_parent -> k parent_reference =
  fun kind ident tbl r ->
    let handle_root s : k parent_reference =
      match CTbl.page_base tbl s with
      | None -> Unresolved r
      | Some root ->
        let root = Reference.Resolved.Identifier (Identifier.Page(root, s)) in
        match kind with
        | PLabelParent ->
          ResolvedPage(root, CTbl.resolved_page_reference tbl root)
        | _ -> Unresolved r
    in
    let open Reference.Resolved in
    let open Reference in
      match r with
      | Root (s, TPage) -> handle_root s
      | Root (s, TUnknown) as r -> begin
          match resolve_parent_reference kind ident tbl r with
          | Unresolved _ -> handle_root s
          | otherwise -> otherwise
        end
      | Resolved (Identifier Identifier.Page _ as root) -> begin
          match kind with
          | PLabelParent ->
            ResolvedPage(root, CTbl.resolved_page_reference tbl root)
          | _ -> Unresolved r
        end
      | Root (_, (TModule | TModuleType | TType | TClass | TClassType))
      | Resolved (Identifier ( Identifier.Root _ | Identifier.Module _
                             | Identifier.Argument _ | Identifier.ModuleType _
                             | Identifier.Type _ | Identifier.CoreType _
                             | Identifier.Class _ | Identifier.ClassType _)
                 | SubstAlias _ | Module _ | Canonical _
                 | ModuleType _ | Type _ | Class _ | ClassType _)
      | Dot _ | Module _ | ModuleType _ | Type _ | Class _ | ClassType _ as r ->
        resolve_parent_reference kind ident tbl r

and resolve_resolved_reference :
  type k. _ -> _ -> k Reference.Resolved.t -> k Reference.Resolved.t =
  fun ident tbl r ->
    let open Reference.Resolved in
        match r with
        | Identifier _ -> r
        | SubstAlias(sub, orig) ->
          let sub' = resolve_resolved_module_path ident tbl sub in
          let orig' = resolve_resolved_reference ident tbl orig in
          if sub != sub' || orig != orig' then
            SubstAlias(sub', orig')
          else r
        | Module(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Module(parent', name)
          else r
        | Canonical(orig, cano) ->
          let orig' = resolve_resolved_reference ident tbl orig in
          let cano' = resolve_module_reference ident tbl cano in
          if orig != orig' || cano != cano' then
            let cano' =
              match ident, cano' with
              | Some ident, Reference.Resolved rp ->
                  let rp' = Reference.Resolved.rebase ident rp in
                  if rp != rp' then Reference.Resolved rp' else cano'
              | _, _ -> cano'
            in
            Canonical(orig', cano')
          else r
        | ModuleType(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            ModuleType(parent', name)
          else r
        | Type(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Type(parent', name)
          else r
        | Constructor(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Constructor(parent', name)
          else r
        | Field(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Field(parent', name)
          else r
        | Extension(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Extension(parent', name)
          else r
        | Exception(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Exception(parent', name)
          else r
        | Value(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Value(parent', name)
          else r
        | Class(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Class(parent', name)
          else r
        | ClassType(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            ClassType(parent', name)
          else r
        | Method(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Method(parent', name)
          else r
        | InstanceVariable(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            InstanceVariable(parent', name)
          else r
        | Label(parent, name) ->
          let parent' = resolve_resolved_reference ident tbl parent in
          if parent != parent' then
            Label(parent', name)
          else r

and resolve_module_reference ident tbl (r : Reference.module_)
  : Reference.module_ =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root (s, _) -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> r
        | CTbl.Forward_reference -> r (* TODO *)
        | CTbl.Found {root; _} -> Resolved (Identifier (Identifier.Root(root, s)))
      end
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_module) =
            let Element.Module {canonical; hidden = _} = elem in
            let rr : Reference.Resolved.module_ = Module(r, name) in
            let rr : Reference.Resolved.module_ =
              match canonical with
              | None -> rr
              | Some (_, r) -> Canonical(rr, r)
            in
              Resolved rr
          in
          let unresolved r =
            let r = rlpop (Resolved.parent_of_signature r) in
              Dot(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_module_element name)
              resolved unresolved ident tbl r parent
      end
    | Module(r, name) -> begin
        match
          resolve_parent_reference PSig ident tbl
            (Reference.parent_of_signature r)
        with
        | Unresolved _ -> Module(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_module) =
            let Element.Module {canonical; hidden = _} = elem in
            let rr : Reference.Resolved.module_ = Module(r, name) in
            let rr : Reference.Resolved.module_ =
              match canonical with
              | None -> rr
              | Some (_, r) -> Canonical(rr, r)
            in
              Resolved rr
          in
          let unresolved r = Module(Resolved r, name) in
            find_with_reference_substs (Sig.find_module_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_module_type_reference ident tbl (r : Reference.module_type)
  : Reference.module_type =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.ModuleType : Element.signature_module_type) =
            Resolved(ModuleType(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_module_type_element name)
              resolved unresolved ident tbl r parent
      end
    | ModuleType(r, name) -> begin
        match
          resolve_parent_reference PSig ident tbl
            (Reference.parent_of_signature r)
        with
        | Unresolved _ -> ModuleType(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.ModuleType : Element.signature_module_type) =
            Resolved(ModuleType(r, name))
          in
          let unresolved r = ModuleType(Resolved r, name) in
            find_with_reference_substs (Sig.find_module_type_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_type_reference ident tbl (r : Reference.type_)
  : Reference.type_ =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> Resolved (Type(r, name))
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> Resolved (ClassType(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end
    | Type(r, name) as typ -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> typ
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> Resolved (Type(r, name))
            | Element.Class -> typ
            | Element.ClassType -> typ
          in
          let unresolved r = Type(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end
    | Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> cls
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> cls
          in
          let unresolved r = Class(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end
    | ClassType(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> cls
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> Resolved (ClassType(r, name))
          in
          let unresolved r = ClassType(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_constructor_reference ident tbl (r : Reference.constructor)
  : Reference.constructor =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSigOrType ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor type_name ->
              Resolved (Constructor(Type(r, type_name), name))
            | Element.Extension -> Resolved (Extension(r, name))
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let Element.Constructor _ =
                Datatype.find_constructor_element name parent
              in
                Resolved (Constructor(r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved (rlpop r), name)
          end
      end
    | Constructor(r, name) -> begin
        match
          resolve_parent_reference PSigOrType ident tbl
            (parent_of_datatype r)
        with
        | Unresolved _ -> Constructor(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor type_name ->
              Resolved (Constructor(Type(r, type_name), name))
            | Element.Extension -> Resolved (Extension(r, name))
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let Element.Constructor _ =
                Datatype.find_constructor_element name parent
              in
                Resolved (Constructor(r, name))
            with Not_found ->
                Constructor(Resolved r, name)
          end
      end
    | Extension(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (parent_of_signature r)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor _ -> ext
            | Element.Extension -> Resolved (Extension(r, name))
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
              Extension(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
      end
    | Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (parent_of_signature r)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor _ -> ext
            | Element.Extension -> ext
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
              Exception(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_field_reference ident tbl (r : Reference.field)
  : Reference.field =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSigOrType ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Field type_name : Element.signature_field) =
            Resolved (Field(Type(r, type_name), name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_field_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let Element.Field _ =
                Datatype.find_field_element name parent
              in
                Resolved (Field(Reference.Resolved.parent_of_datatype r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved (rlpop r), name)
          end
      end
    | Field(r, name) -> begin
        match resolve_parent_reference PSigOrType ident tbl r with
        | Unresolved _ -> Field(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Field type_name : Element.signature_field) =
            Resolved (Field(Type(r, type_name), name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Field(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_field_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let Element.Field _ =
                Datatype.find_field_element name parent
              in
                Resolved (Field(Reference.Resolved.parent_of_datatype r, name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Field(Resolved r, name)
          end
      end

and resolve_extension_reference ident tbl (r : Reference.extension)
  : Reference.extension =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr as r ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_extension) =
            match elem with
            | Element.Extension -> Resolved (Extension(r, name))
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_extension_element name)
              resolved unresolved ident tbl r parent
      end
    | Extension(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (parent_of_signature r)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor _ -> ext
            | Element.Extension -> Resolved (Extension(r, name))
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
              Extension(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
      end
    | Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (parent_of_signature r)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor _ -> ext
            | Element.Extension -> ext
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
              Exception(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_exception_reference ident tbl (r : Reference.exception_)
  : Reference.exception_ =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Exception : Element.signature_exception) =
            Resolved (Exception(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_exception_element name)
              resolved unresolved ident tbl r parent
      end
    | Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (parent_of_signature r)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | Element.Constructor _ -> ext
            | Element.Extension -> ext
            | Element.Exception -> Resolved (Exception(r, name))
          in
          let unresolved r =
              Exception(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_value_reference ident tbl (r : Reference.value)
  : Reference.value =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Value : Element.signature_value) =
            Resolved (Value(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_value_element name)
              resolved unresolved ident tbl r parent
      end
    | Value(r, name) -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> Value(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Value : Element.signature_value) =
            Resolved (Value(r, name))
          in
          let unresolved r =
            Value(Resolved r, name)
          in
            find_with_reference_substs (Sig.find_value_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_class_reference ident tbl (r : Reference.class_)
  : Reference.class_ =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (Element.Class : Element.signature_class) =
            Resolved (Class(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_class_element name)
              resolved unresolved ident tbl r parent
      end
    | Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> cls
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> cls
          in
          let unresolved r = Class(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_class_type_reference ident tbl (r : Reference.class_type)
  : Reference.class_type =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r : Element.signature_class_type -> _ = function
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> Resolved (ClassType(r, name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_class_type_element name)
              resolved unresolved ident tbl r parent
      end
    | Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> cls
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> cls
          in
          let unresolved r = Class(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end
    | ClassType(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (parent_of_signature r)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | Element.Type -> cls
            | Element.Class -> Resolved (Class(r, name))
            | Element.ClassType -> Resolved (ClassType(r, name))
          in
          let unresolved r = ClassType(Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
      end

and resolve_method_reference ident tbl (r : Reference.method_)
  : Reference.method_ =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PClassSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.Method =
                ClassSig.find_method_element name parent
              in
                Resolved (Method(r, name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved (rlpop r), name)
      end
    | Method(r, name) -> begin
        match
          resolve_parent_reference PClassSig ident tbl
            (parent_of_class_signature r)
        with
        | Unresolved _ -> Method(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.Method =
                ClassSig.find_method_element name parent
              in
                Resolved (Method(r, name))
            with Not_found ->
              Method(Resolved r, name)
      end

and resolve_instance_variable_reference ident tbl
      (r : Reference.instance_variable) : Reference.instance_variable =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PClassSig ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.InstanceVariable =
                ClassSig.find_instance_variable_element name parent
              in
                Resolved (InstanceVariable(r, name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved (rlpop r), name)
      end
    | InstanceVariable(r, name) -> begin
        match
          resolve_parent_reference PClassSig ident tbl
            (parent_of_class_signature r)
        with
        | Unresolved _ -> InstanceVariable(r, name)
        | ResolvedClassSig(r, parent) ->
            try
              let Element.InstanceVariable =
                ClassSig.find_instance_variable_element name parent
              in
                Resolved (InstanceVariable(r, name))
            with Not_found ->
              InstanceVariable(Resolved r, name)
      end

and resolve_label_reference ident tbl (r : Reference.label)
  : Reference.label =
  let open Reference.Resolved in
  let open Reference in
    match r with
    | Root _ -> r
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_label) =
            match elem with
            | Element.Label (Some type_name) ->
                Resolved (Label(Type(r, type_name), name))
            | Element.Label None ->
                Resolved (Label(rlpop (Resolved.parent_of_signature r), name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_label_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            let r = Resolved.parent_of_datatype r in
              try
                let Element.Label _ =
                  Datatype.find_label_element name parent
                in
                  Resolved (Label(rlpop r, name))
              with Not_found ->
                  Dot(Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            let r = Resolved.parent_of_class_signature r in
              try
                let Element.Label _ =
                  ClassSig.find_label_element name parent
                in
                  Resolved (Label((rlpop r), name))
              with Not_found ->
                  Dot(Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
            match Page.find_label_element name page with
            | exception Not_found ->
              Dot(Resolved (Resolved.label_parent_of_page r), name)
            | Element.Label None ->
              Resolved (Label (Resolved.label_parent_of_page r, name))
            | Element.Label (Some _) -> assert false
          end
      end
    | Label(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> Label(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_label) =
            match elem with
            | Element.Label (Some type_name) ->
                Resolved (Label(Type(r, type_name), name))
            | Element.Label None ->
                Resolved (Label(rlpop (Resolved.parent_of_signature r), name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Label(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_label_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            let r = Resolved.parent_of_datatype r in
              try
                let Element.Label _ =
                  Datatype.find_label_element name parent
                in
                  Resolved (Label(rlpop r, name))
              with Not_found ->
                  Label(Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            let r = Resolved.parent_of_class_signature r in
              try
                let Element.Label _ =
                  ClassSig.find_label_element name parent
                in
                  Resolved (Label(rlpop r, name))
              with Not_found ->
                  Label(Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
              match Page.find_label_element name page with
              | exception Not_found ->
                Label(Resolved (Resolved.label_parent_of_page r), name)
              | Element.Label None ->
                Resolved (Label (Resolved.label_parent_of_page r, name))
              | Element.Label (Some _) -> assert false
          end
      end

and resolve_element_reference ident tbl (r : Reference.any)
  : Reference.any =
  let open Reference.Resolved in
  let open Reference in
    let find_root_page s =
        match CTbl.page_base tbl s with
        | None -> r
        | Some root -> Resolved (Identifier (Identifier.Page(root, s)))
    in
    match r with
    | Root (s, TPage) -> find_root_page s
    | Root (s, TUnknown) -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> find_root_page s
        | CTbl.Forward_reference ->
          r (* not looking for a page since [s] "exists". *)
        | CTbl.Found {root;_} -> Resolved (Identifier (Identifier.Root(root, s)))
      end
    | Root (s, _) -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> r
        | CTbl.Forward_reference -> r
        | CTbl.Found {root;_} -> Resolved (Identifier (Identifier.Root(root, s)))
      end
    | Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then Resolved rr' else r
    | Dot(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature) =
            match elem with
            | Element.Module {canonical; hidden = _} ->
              let rr = Resolved.Module(r, name) in
              let rr =
                match canonical with
                | None -> rr
                | Some (_, r) -> Resolved.Canonical(rr, r)
              in
              Resolved rr
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
                Resolved (Label(rlpop (Resolved.parent_of_signature r), name))
          in
          let unresolved r =
            let r = Resolved.parent_of_signature r in
              Dot(Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            try
              match Datatype.find_element name parent with
              | Element.Constructor _ -> Resolved (Constructor(r , name))
              | Element.Field _ ->
                  Resolved (Field(Reference.Resolved.parent_of_datatype r , name))
              | Element.Label _ ->
                  Resolved (Label(rlpop (Resolved.parent_of_datatype r), name))
            with Not_found ->
              let r = Resolved.parent_of_datatype r in
                Dot(Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            try
              match ClassSig.find_element name parent with
              | Element.Method -> Resolved (Method(r, name))
              | Element.InstanceVariable -> Resolved (InstanceVariable(r, name))
              | Element.Label _ ->
                  Resolved (Label(rlpop (Resolved.parent_of_class_signature r), name))
            with Not_found ->
              let r = Resolved.parent_of_class_signature r in
                Dot(Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
              match Page.find_label_element name page with
              | exception Not_found ->
                Dot(Resolved (Resolved.label_parent_of_page r), name)
              | Element.Label None ->
                Resolved (Label (Resolved.label_parent_of_page r, name))
              | Element.Label (Some _) -> assert false
          end
      end
    | Module _ as r -> any (resolve_module_reference ident tbl r)
    | ModuleType _ as r -> any (resolve_module_type_reference ident tbl r)
    | Type _ as r -> any (resolve_type_reference ident tbl r)
    | Constructor _ as r -> any (resolve_constructor_reference ident tbl r)
    | Field _ as r -> any (resolve_field_reference ident tbl r)
    | Extension _ as r -> any (resolve_extension_reference ident tbl r)
    | Exception _ as r -> any (resolve_exception_reference ident tbl r)
    | Value _ as r -> any (resolve_value_reference ident tbl r)
    | Class _ as r -> any (resolve_class_reference ident tbl r)
    | ClassType _ as r -> any (resolve_class_type_reference ident tbl r)
    | Method _ as r -> any (resolve_method_reference ident tbl r)
    | InstanceVariable _ as r -> any (resolve_instance_variable_reference ident tbl r)
    | Label _ as r -> any (resolve_label_reference ident tbl r)

let splice_section_title tbl path elements =
  let open Reference in
  let title_of_parent =
    let open Resolved in
    let open Identifier in
    fun name parent_ref ->
      match parent_ref with
      | (Identifier (Root _ | Module _ | Argument _ | ModuleType _)
        | SubstAlias _ | Module _ | Canonical _ | ModuleType _ as rr) ->
          Some (Sig.find_section_title name
                  (CTbl.resolved_signature_reference tbl rr))
      | Identifier Page _ as rr->
        Some (Page.find_section_title name
                (CTbl.resolved_page_reference tbl rr))
      | _ -> None
  in
  let find_section_title :
    Resolved.label -> Model.Comment.link_content option =
    function
    | Resolved.Identifier Identifier.Label (parent, str) ->
      let parent_ref = Resolved.Identifier parent in
      title_of_parent str parent_ref
    | Resolved.Label (parent_ref, str) ->
      title_of_parent str parent_ref
  in
  match (path, elements) with
  | (r, []) ->
    begin match r with
    | Resolved (Resolved.Label _
                       |Resolved.Identifier (Identifier.Label _) as rr) ->
      begin match find_section_title rr with
      | None -> (path, elements)
      | Some txt -> (r, txt)
      end
    | _ -> (path, elements)
    end
  | otherwise -> otherwise

class resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  object (self)
    val tbl =
      CTbl.create ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page
    val where_am_i : Identifier.signature option = None

    inherit Maps.types as super
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
    method identifier_page x = x
    method identifier_signature x = x
    method identifier x = x

    method path_module x = resolve_module_path where_am_i tbl x
    method path_module_type x = resolve_module_type_path where_am_i tbl x
    method path_type x = resolve_type_path where_am_i tbl x
    method path_class_type x = resolve_class_type_path where_am_i tbl x

    method! module_ md =
      let open Lang.Module in
      let {id; doc; type_; expansion; canonical;display_type;hidden} = md in
      let id' = self#identifier_module id in
      let sig_id = Identifier.signature_of_module id' in
      let self = {< where_am_i = Some sig_id >} in
      let doc' = self#documentation doc in
      let type' = self#module_decl_with_id sig_id type_ in
      let expansion' = Maps.option_map self#module_expansion expansion in
      let canonical' =
        Maps.option_map
          (Maps.pair_map self#path_module self#reference_module)
          canonical
      in
      let display_type' =
        Maps.option_map (self#module_decl_with_id sig_id) display_type
      in
      let hidden' = self#module_hidden hidden in
        if id != id' || doc != doc' || type_ != type'
          || expansion != expansion' || canonical != canonical'
          || display_type != display_type'
        then
          {id = id'; doc = doc'; type_ = type';
          expansion = expansion'; canonical = canonical';
          display_type = display_type'; hidden = hidden'}
        else md

    method! module_type mty =
      let open Lang.ModuleType in
      let {id; doc; expr; expansion} = mty in
      let id' = self#identifier_module_type id in
      let sig_id = Identifier.signature_of_module_type id' in
      let self = {< where_am_i = Some sig_id >} in
      let doc' = self#documentation doc in
      let expr' =
        match expr with
        | None -> expr
        | Some body ->
            let body' = self#module_type_expr_with_id sig_id body in
            if body != body' then Some body'
            else expr
      in
      let expansion' = Maps.option_map self#module_expansion expansion in
        if id != id' || doc != doc' || expr != expr' || expansion != expansion' then
          {id = id'; doc = doc'; expr = expr'; expansion = expansion'}
        else mty

    method! include_ incl =
      let open Lang.Include in
      let {parent; doc; decl; expansion} = incl in
      let parent' = self#identifier_signature parent in
      let doc' = self#documentation doc in
      let decl' = self#module_decl_with_id parent decl in
      let expansion' = self#include_expansion expansion in
        if parent != parent' || doc != doc' || decl != decl' || expansion != expansion' then
          {parent = parent'; doc = doc'; decl = decl'; expansion = expansion'}
        else incl

    method! module_type_functor_arg arg =
      let open Lang.FunctorArgument in
      match arg with
      | None -> arg
      | Some{ id; expr; expansion } ->
          let id' = self#identifier_module id in
          let sig_id = Identifier.signature_of_module id' in
          let expr' = self#module_type_expr_with_id sig_id expr in
          let expansion' =
            Maps.option_map self#module_expansion expansion
          in
            if id != id' || expr != expr' || expansion != expansion' then
              Some {id = id'; expr = expr'; expansion = expansion'}
            else arg

    method module_type_expr_with_id id expr =
      let open Lang.ModuleType in
        match expr with
        | With(body, substs) ->
          let body = self#module_type_expr_with_id id body in
          let base = CTbl.module_type_expr_with tbl id body in
          let substs =
            List.map (function
              | ModuleEq(frag, eq) ->
                let frag =
                  resolve_module_fragment where_am_i tbl base frag
                in
                let eq = self#module_equation eq in
                ModuleEq(frag, eq)
              | TypeEq(frag, eq) ->
                let frag =
                  resolve_type_fragment where_am_i tbl base frag
                in
                let eq = self#type_decl_equation eq in
                TypeEq(frag, eq)
              | ModuleSubst(frag, p) ->
                let frag =
                  resolve_module_fragment where_am_i tbl base frag
                in
                let p = self#path_module p in
                ModuleSubst(frag, p)
              | TypeSubst(frag, eq) ->
                let frag =
                  resolve_type_fragment where_am_i tbl base frag
                in
                let eq = self#type_decl_equation eq in
                TypeSubst(frag, eq)
            ) substs
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

    method module_decl_with_id id decl =
      let open Lang.Module in
        match decl with
        | ModuleType expr ->
            let expr' = self#module_type_expr_with_id id expr in
              if expr != expr' then ModuleType expr'
              else decl
        | Alias _ -> self#module_decl decl

    method! type_expr_package pkg =
      let open Lang.TypeExpr.Package in
      let path = resolve_module_type_path where_am_i tbl pkg.path in
      let base = CTbl.module_type_path_with tbl path in
      let substitutions =
        List.map
          (fun (frag, eq) ->
            let frag = resolve_type_fragment where_am_i tbl base frag in
            let eq = self#type_expr eq in
              (frag, eq))
          pkg.substitutions
      in
        {path; substitutions}

    method fragment_type x = x
    method fragment_module x = x

    method reference_module x =
      resolve_module_reference where_am_i tbl x
    method reference_module_type x =
      resolve_module_type_reference where_am_i tbl
        x
    method reference_type x =
      resolve_type_reference where_am_i tbl x
    method reference_constructor x =
      resolve_constructor_reference where_am_i tbl
        x
    method reference_field x =
      resolve_field_reference where_am_i tbl x
    method reference_extension x =
      resolve_extension_reference where_am_i tbl
        x
    method reference_exception x =
      resolve_exception_reference where_am_i tbl
        x
    method reference_value x =
      resolve_value_reference where_am_i tbl x
    method reference_class x =
      resolve_class_reference where_am_i tbl x
    method reference_class_type x =
      resolve_class_type_reference where_am_i tbl
        x
    method reference_method x =
      resolve_method_reference where_am_i tbl x
    method reference_instance_variable x =
      resolve_instance_variable_reference where_am_i tbl
        x
    method reference_label x =
      resolve_label_reference where_am_i tbl x
    method reference_any x =
      resolve_element_reference where_am_i tbl x

    method! documentation_reference r =
      let (path, elements) = super#documentation_reference r in
      splice_section_title tbl path elements

    method! unit_import import =
      let open Lang.Compilation_unit.Import in
        match import with
        | Resolved _ -> import
        | Unresolved(name, _) ->
            match CTbl.base tbl name with
            | CTbl.Found {root; _} -> Resolved root
            | _ -> import

    method resolve_unit unit =
      let this =
        {< where_am_i =
          Some (Identifier.signature_of_module unit.Lang.Compilation_unit.id) >}
      in
        this#unit unit

    method resolve_page page =
      let this = {< where_am_i = None >} in
        this#page page
end

let build_resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  new resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page

let resolve r u = r#resolve_unit u

let resolve_page r p = r#resolve_page p
