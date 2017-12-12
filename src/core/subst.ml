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


class type t = object
  method root : Root.t -> Root.t
  inherit Maps.paths
  method offset_identifier_signature :
    Identifier.signature * int -> Identifier.signature * int
  inherit Maps.types
end

let signature s sg =
  s#signature sg

let class_signature s csig =
  s#class_signature csig

let datatype s decl =
  s#type_decl_representation decl

let module_ s md =
  s#module_ md

let module_type s mty =
  s#module_type mty

let type_decl s decl =
  s#type_decl decl

let constructor s cstr =
  s#type_decl_constructor cstr

let field s field =
  s#type_decl_field field

let extension s ext =
  s#extension ext

let exception_ s exn =
  s#exception_ exn

let value s v =
  s#value v

let class_ s cl =
  s#class_ cl

let class_type s cty =
  s#class_type cty

let method_ s meth =
  s#method_ meth

let instance_variable s inst =
  s#instance_variable inst

let comment s com =
  s#documentation_comment com

let documentation s doc =
  s#documentation doc

let identifier_signature s id =
  s#identifier_signature id

let offset_identifier_signature s idoff =
  s#offset_identifier_signature idoff

(* TODO either expose more maps or expose argument map directly *)
let identifier_module s id =
  s#identifier_module id

let module_type_expr s expr =
  s#module_type_expr expr

let module_expansion s expr =
  s#module_expansion expr

class rename_signature ~equal:_ (x : Identifier.signature)
        (y : Identifier.signature) offset : t = object

  inherit Maps.paths as super

  method root x = x

  method! identifier_signature id =
    if Identifier.equal id x then y
    else super#identifier_signature id

  method! identifier (type k) (id : k Identifier.t)
         : k Identifier.t =
    match id with
    | Identifier.Argument(parent, pos, name) ->
        if Identifier.equal parent x then
          Identifier.Argument(y, pos + offset, name)
        else super#identifier id
    | id -> super#identifier id

  method offset_identifier_signature (id, offset') =
    if Identifier.equal id x then (y, offset + offset')
    else (super#identifier_signature id, offset')

  inherit Maps.types

end

let rename_signature ~equal x y offset =
  new rename_signature ~equal x y offset

class rename_class_signature ~equal:_
           (x : Identifier.class_signature)
           (y : Identifier.class_signature) : t = object (self)

  inherit Maps.paths as super

  method root x = x

  method! identifier_class_signature id =
    if Identifier.equal id x then y
    else super#identifier_class_signature id

  inherit Maps.types

  method offset_identifier_signature (id, offset) =
    (self#identifier_signature id, offset)

end

let rename_class_signature ~equal x y =
  new rename_class_signature ~equal x y

class rename_datatype ~equal:_ (x : Identifier.datatype)
        (y : Identifier.datatype) : t = object (self)

  inherit Maps.paths as super

  method root x = x

  method! identifier_datatype id =
    if Identifier.equal id x then y
    else super#identifier_datatype id

  inherit Maps.types

  method offset_identifier_signature (id, offset) =
    (self#identifier_signature id, offset)

end

let rename_datatype ~equal x y =
  new rename_datatype ~equal x y

(*let module_id_path (type k) (Witness : k is_path_kind)
                   (id : ('a, k) Identifier.t) name =
  let open Path.Resolved in
    (Module(Identifier id, name))*)

class prefix ~equal:_ ~canonical id : t = object (self)

  inherit Maps.paths as super

  method root x = x

  (* OCaml can't type-check this method yet, so we use magic*)
  method! path_resolved : type k. k Path.Resolved.t -> k Path.Resolved.t =
    fun p ->
      let matches id' =
        Identifier.equal (Identifier.signature_of_module id) id'
      in
      let open Path.Resolved in
      let replacement =
        match canonical with
        | None -> Identifier id
        | Some(path, _) -> Canonical(Identifier id, path)
      in
        match p with
        | Identifier (Identifier.Module(parent, name)) ->
            if matches parent then Obj.magic (Module(replacement, name))
            else super#path_resolved p
        | Identifier (Identifier.ModuleType(parent, name)) ->
            if matches parent then Obj.magic (ModuleType(replacement, name))
            else super#path_resolved p
        | Identifier (Identifier.Type(parent, name)) ->
            if matches parent then Obj.magic (Type(replacement, name))
            else super#path_resolved p
        | Identifier (Identifier.Class(parent, name)) ->
            if matches parent then Obj.magic (Class(replacement, name))
            else super#path_resolved p
        | Identifier (Identifier.ClassType(parent, name)) ->
            if matches parent then Obj.magic (ClassType(replacement, name))
            else super#path_resolved p
        | _ -> super#path_resolved p

  method! reference_resolved : type k. k Reference.Resolved.t ->
                              k Reference.Resolved.t =
    fun r ->
      let sid = Identifier.signature_of_module id in
      let matches id' =
        Identifier.equal sid id'
      in
      let open Reference.Resolved in
      let replacement =
        match canonical with
        | None -> Identifier id
        | Some(_, reference) -> Canonical(Identifier id, reference)
      in
      let sreplacement = signature_of_module replacement in
      let preplacement = parent_of_signature sreplacement in
      let lreplacement = label_parent_of_parent preplacement in
      match r with
      | Identifier (Identifier.Module(parent, name)) ->
          if matches parent then Module(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.ModuleType(parent, name)) ->
          if matches parent then ModuleType(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Type(parent, name)) ->
          if matches parent then Type(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Extension(parent, name)) ->
          if matches parent then Extension(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Exception(parent, name)) ->
          if matches parent then Exception(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Value(parent, name)) ->
          if matches parent then Value(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Class(parent, name)) ->
          if matches parent then Class(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.ClassType(parent, name)) ->
          if matches parent then ClassType(sreplacement, name)
          else super#reference_resolved r
      | Identifier (Identifier.Label(parent, name)) -> begin
          match parent with
          | Identifier.Root _ | Identifier.Argument _
          | Identifier.Module _ | Identifier.ModuleType _ as parent ->
                if matches parent then Label(lreplacement, name)
                else super#reference_resolved r
          | _ -> super#reference_resolved r
        end
      | _ -> super#reference_resolved r

  inherit Maps.types

  method offset_identifier_signature (id, offset) =
    (self#identifier_signature id, offset)

end

let prefix ~equal ~canonical id =
  new prefix ~equal ~canonical id

class strengthen path : t = object

  inherit Maps.types

  method root x = x

  method! documentation_comment x = x

  method! module_ md =
    if Path.Resolved.is_hidden path then md
    else begin
      let open Lang.Module in
      match md.type_ with
      | Alias p when not (Path.is_hidden p) -> md
      | _ ->
          let name = Identifier.name md.id in
          let path = Path.Resolved(Path.Resolved.Module(path, name)) in
          let type_ = Alias path in
          let expansion = None in
          { md with type_; expansion }
    end

  method! module_type x = x

  method! type_decl x = x

  method! extension x = x

  method! exception_ x = x

  method! value x = x

  method! external_ x = x

  method! class_ x = x

  method! class_type x = x

  method! include_ x = x

  inherit Maps.paths

  method offset_identifier_signature x = x

  method! module_type_expr x = x

end

let strengthen path =
  new strengthen path

let make_lookup ~equal:_ ~hash:_
                (items : (Identifier.module_ * Identifier.module_) list) =
  let module Hash = struct
    type t = Identifier.module_
    let equal = Identifier.equal
    let hash = Identifier.hash
  end in
  let module Tbl = Hashtbl.Make(Hash) in
  let tbl = Tbl.create 13 in
  List.iter (fun (id1, id2) -> Tbl.add tbl id1 id2) items;
    fun id ->
        match Tbl.find tbl id with
        | id -> Some id
        | exception Not_found -> None

class pack ~equal ~hash
           (items : (Identifier.module_
                     * Identifier.module_) list) : t = object (self)

  val lookup = make_lookup ~equal ~hash items

  method root x = x

  inherit Maps.paths as super

  method! identifier : type k. k Identifier.t -> k Identifier.t =
    fun id ->
      let open Identifier in
        match id with
        | Root _ as id -> begin
            match lookup id with
            | Some (Root _ | Module _ | Argument _ as id) -> id
            | None -> super#identifier id
          end
        | Module _ as id -> begin
            match lookup id with
            | Some (Root _ | Module _ | Argument _ as id) -> id
            | None -> super#identifier id
          end
        | Argument _ as id -> begin
            match lookup id with
            | Some (Root _ | Module _ | Argument _ as id) -> id
            | None -> super#identifier id
          end
        | _ -> super#identifier id

  inherit Maps.types

  method offset_identifier_signature (id, offset) =
    (self#identifier_signature id, offset)

end

let pack ~equal ~hash items =
  new pack ~equal ~hash items
