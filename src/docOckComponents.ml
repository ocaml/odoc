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

let ident_name id =
  match Identifier.name id with
  | None -> assert false
  | Some name -> name

(* Sets and maps for components *)

module SSet = Set.Make(String)

module SMap = struct

  include Map.Make(String)

  let filter_item name pred map =
    try
      let v = find name map in
        if pred v then map
        else remove name map
    with Not_found -> map

  let map_item name f map =
    try
      let v = find name map in
        add name (f v) map
    with Not_found -> map

end

module LMap = struct

  type 'a t = 'a list SMap.t

  let empty = SMap.empty

  let add name item map =
    try
      let items = SMap.find name map in
        SMap.add name (item :: items) map
    with Not_found ->
      SMap.add name [item] map

  let find name pred map =
    let items = SMap.find name map in
      List.find pred items

  let find_name name map =
    let items = SMap.find name map in
      match items with
      | [] -> raise Not_found
      | x :: _ -> x

  let map_find name pred map =
    let rec loop pred = function
      | [] -> raise Not_found
      | x :: l ->
          match pred x with
          | Some x -> x
          | None -> loop pred l
    in
    let items = SMap.find name map in
      loop pred items

  let fold f map acc =
    SMap.fold
      (fun name -> List.fold_right (f name))
      map acc

  let filter_item name pred map =
    try
      let items = SMap.find name map in
      let items = List.filter pred items in
        match items with
        | [] -> SMap.remove name map
        | _ -> SMap.add name items map
    with Not_found -> map

  let map_item name f map =
    try
      let items = SMap.find name map in
      let items = List.map f items in
        SMap.add name items map
    with Not_found -> map

end

(* Read labels from documentation *)

let rec text_element_labels acc =
  let open Documentation in function
  | Title(_, Some id, txt) ->
      let name = ident_name id in
        text_labels (name :: acc) txt
  | Raw _ | Code _ | PreCode _ | Verbatim _
  | Newline | Target _ | Special _ | Reference(_, None) -> acc
  | Style(_, txt) | Title(_, None, txt) | Reference(_, Some txt) ->
      text_labels acc txt
  | List txts | Enum txts ->
      List.fold_left text_labels acc txts

and text_labels acc txt = List.fold_left text_element_labels acc txt

let tag_labels acc =
  let open Documentation in function
  | Author _ | Version _ | Since _ -> acc
  | See(_, txt) | Before(_, txt) | Deprecated txt
  | Param(_, txt) | Raise(_, txt) | Return txt | Tag(_, txt) ->
      text_labels acc txt

let tags_labels acc tags = List.fold_left tag_labels acc tags

let documentation_labels acc doc =
  let open Documentation in
  let acc = tags_labels acc doc.tags in
    text_labels acc doc.text

let comment_labels acc comment =
  let open Documentation in
  match comment with
  | Documentation doc -> documentation_labels acc doc
  | Stop -> acc

module rec Sig : sig

  type 'a t

  val find_parent_module : string -> 'a t -> 'a Parent.module_

  val find_parent_apply : ('a Path.module_ -> 'a t) -> 'a Path.module_ ->
        'a t -> 'a Parent.module_

  val find_parent_module_type : string -> 'a t -> 'a Parent.module_type

  val find_parent_signature : string -> 'a t -> 'a Parent.signature

  val find_parent_class_signature : string -> 'a t -> 'a Parent.class_signature

  val find_parent_datatype : string -> 'a t -> 'a Parent.datatype

  val find_parent_sig_or_type : string -> 'a t -> 'a Parent.sig_or_type

  val find_parent_subst : 'a t -> 'a Parent.subst

  val find_parent : string -> 'a t -> 'a Parent.any

  val find_module_element : string -> 'a t -> Element.signature_module

  val find_apply_element : 'a t -> Element.signature_module

  val find_module_type_element : string -> 'a t -> Element.signature_module_type

  val find_type_element : string -> 'a t -> Element.signature_type

  val find_constructor_element : string -> 'a t -> Element.signature_constructor

  val find_field_element : string -> 'a t -> Element.signature_field

  val find_extension_element : string -> 'a t -> Element.signature_extension

  val find_exception_element : string -> 'a t -> Element.signature_exception

  val find_value_element : string -> 'a t -> Element.signature_value

  val find_class_element : string -> 'a t -> Element.signature_class

  val find_class_type_element : string -> 'a t -> Element.signature_class_type

  val find_label_element : string -> 'a t -> Element.signature_label

  val find_element : string -> 'a t -> Element.signature

  val lookup_module : string -> 'a t -> 'a t

  val lookup_argument : int -> 'a t -> 'a t

  val lookup_apply : ('a Path.module_ -> 'a t) -> 'a Path.module_ ->
        'a t -> 'a t

  val lookup_module_type  : string -> 'a t -> 'a t

  val lookup_class_type : string -> 'a t -> 'a ClassSig.t

  val lookup_datatype : string -> 'a t -> 'a Datatype.t

  type 'a signature

  val empty : 'a signature

  val add_module : string -> 'a t -> 'a signature -> 'a signature

  val add_module_type : string -> 'a t -> 'a signature -> 'a signature

  val add_datatype : string -> 'a Datatype.t -> 'a signature -> 'a signature

  val add_class : string -> 'a ClassSig.t -> 'a signature -> 'a signature

  val add_class_type : string -> 'a ClassSig.t -> 'a signature -> 'a signature

  val add_element : string -> Element.signature -> 'a signature -> 'a signature

  val add_documentation : 'a Documentation.t -> 'a signature -> 'a signature

  val add_comment : 'a Documentation.comment -> 'a signature -> 'a signature

  val include_ : 'a t -> 'a signature -> 'a signature

  val path : ('a Path.module_type -> 'a t) -> 'a Path.module_type -> 'a t

  val alias : ('a Path.module_ -> 'a t) -> 'a Path.module_ -> 'a t

  val signature : ('b -> 'a signature) -> 'b -> 'a t

  val functor_ : 'a Identifier.module_ -> 'a t -> 'a t -> 'a t

  val generative : 'a t -> 'a t

  val abstract : 'a t

  val unresolved : 'a t

  val with_module : 'a Fragment.module_ -> 'a t -> 'a t -> 'a t

  val with_module_subst : 'a Fragment.module_ -> 'a t -> 'a t

  val with_type_subst : 'a Fragment.type_ -> 'a t -> 'a t

end = struct

  type 'a term =
    | Path of 'a Path.module_type * bool
    | Alias of 'a Path.module_ * bool
    | WithModule of 'a expr * 'a Fragment.module_ * 'a t
    | WithModuleSubst of 'a expr * 'a Fragment.module_
    | WithTypeSubst of 'a expr * 'a Fragment.type_

  and 'a expr =
    { term : 'a term;
      expansion : 'a t Lazy.t; }

  and 'a functor_ =
    { id : 'a Identifier.module_;
      arg : 'a t;
      res : 'a t;
      cache : ('a Path.module_, 'a t) Hashtbl.t; }

  and 'a signature =
    { modules: 'a t SMap.t;
      module_types: 'a t SMap.t;
      class_signatures: 'a ClassSig.t SMap.t;
      types: Element.signature_type SMap.t;
      parents: 'a Parent.any LMap.t;
      elements: Element.signature LMap.t; }

  and 'a t =
    | Expr of 'a expr
    | Sig of 'a signature Lazy.t
    | Functor of 'a functor_
    | Generative of 'a t
    | Abstract
    | Unresolved

  let rec lift_find f x = function
    | Expr expr -> begin
        match expr.term with
        | Path(_, true) | Alias(_, true) -> raise Not_found
        | _ -> lift_find f x (Lazy.force expr.expansion)
      end
    | Sig sg -> f x (Lazy.force sg)
    | Functor fn -> lift_find f x fn.res
    | Generative t -> lift_find f x t
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_parent_module name t =
    let find name sg =
      Parent.Module (SMap.find name sg.modules)
    in
      lift_find find name t

  let find_parent_module_type name t =
    let find name sg =
      Parent.ModuleType (SMap.find name sg.module_types)
    in
      lift_find find name t

  let find_parent_signature name t =
    let find name sg =
      LMap.map_find name
        (function
          | Parent.Module _ as x -> Some x
          | Parent.ModuleType _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_class_signature name t =
    let find name sg =
      LMap.map_find name
        (function
          | Parent.Class _ as x -> Some x
          | Parent.ClassType _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_datatype name t =
    let find name sg =
      LMap.map_find name
        (function
          | Parent.Datatype _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_sig_or_type name t =
    let find name sg =
      LMap.map_find name
        (function
          | Parent.Module _ as x -> Some x
          | Parent.ModuleType _ as x -> Some x
          | Parent.Datatype _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let rec find_parent_subst = function
    | Expr expr -> begin
        match expr.term with
        | Path(p, true) -> Parent.Subst p
        | Alias(p, true) -> Parent.SubstAlias p
        | _ -> find_parent_subst (Lazy.force expr.expansion)
      end
    | Sig sg -> raise Not_found
    | Functor fn -> find_parent_subst fn.res
    | Generative t -> find_parent_subst t
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_parent name t =
    let find name sg = LMap.find_name name sg.parents in
      lift_find find name t

  let find_module_element name t =
    let find name sg =
      if SMap.mem name sg.modules then Element.Module
      else raise Not_found
    in
      lift_find find name t

  let rec find_apply_element = function
    | Expr expr -> begin
        match expr.term with
        | Path(_, true) | Alias(_, true) -> raise Not_found
        | _ -> find_apply_element (Lazy.force expr.expansion)
      end
    | Sig sg -> raise Not_found
    | Functor fn -> Element.Module
    | Generative t -> raise Not_found
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_module_type_element name t =
    let find name sg =
      if SMap.mem name sg.module_types then Element.ModuleType
      else raise Not_found
    in
      lift_find find name t

  let find_type_element name t =
    let find name sg = SMap.find name sg.types in
      lift_find find name t

  let find_constructor_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Constructor _ as x -> Some x
          | Element.Extension as x -> Some x
          | Element.Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_field_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Field _ as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_extension_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Extension as x -> Some x
          | Element.Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_exception_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_value_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Value as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_class_element name t =
    let find name sg =
      match SMap.find name sg.types with
      | Element.Class as x -> x
      | _ -> raise Not_found
    in
      lift_find find name t

  let find_class_type_element name t =
    let find name sg =
      match SMap.find name sg.types with
      | Element.Class as x -> x
      | Element.ClassType as x -> x
      | _ -> raise Not_found
    in
      lift_find find name t

  let find_label_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | Element.Label _ as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_element name t =
    let find name sg = LMap.find_name name sg.elements in
      lift_find find name t

  let rec lookup_module name = function
    | Expr expr -> lookup_module name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).modules
        with Not_found -> Unresolved
      end
    | Functor fn -> lookup_module name fn.res
    | Generative t -> lookup_module name t
    | Abstract -> Unresolved
    | Unresolved -> Unresolved

  let rec lookup_argument pos = function
    | Expr expr -> lookup_argument pos (Lazy.force expr.expansion)
    | Sig sg -> Unresolved
    | Functor fn ->
        if pos = 1 then fn.arg
        else lookup_argument (pos - 1) fn.res
    | Generative t ->
        if pos = 1 then Unresolved
        else lookup_argument (pos - 1) t
    | Abstract -> Unresolved
    | Unresolved -> Unresolved

  let rec lookup_module_type name = function
    | Expr expr -> lookup_module_type name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).module_types
        with Not_found -> Unresolved
      end
    | Functor fn -> lookup_module_type name fn.res
    | Generative t -> lookup_module_type name t
    | Abstract -> Unresolved
    | Unresolved -> Unresolved

  let rec lookup_class_type name = function
    | Expr expr -> lookup_class_type name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).class_signatures
        with Not_found -> ClassSig.unresolved
      end
    | Functor fn -> lookup_class_type name fn.res
    | Generative t -> lookup_class_type name t
    | Abstract -> ClassSig.unresolved
    | Unresolved -> ClassSig.unresolved

  let rec lookup_datatype name = function
    | Expr expr -> lookup_datatype name (Lazy.force expr.expansion)
    | Sig sg -> begin
          try
            LMap.map_find name
              (function
                | Parent.Datatype t -> Some t
                | _ -> None)
              (Lazy.force sg).parents
          with Not_found -> Datatype.unresolved
      end
    | Functor fn -> lookup_datatype name fn.res
    | Generative t -> lookup_datatype name t
    | Abstract -> Datatype.unresolved
    | Unresolved -> Datatype.unresolved

  let empty =
    { modules = SMap.empty;
      module_types = SMap.empty;
      class_signatures = SMap.empty;
      types = SMap.empty;
      parents = SMap.empty;
      elements = SMap.empty; }

  let add_module name md sg =
    let modules = SMap.add name md sg.modules in
    let parents = LMap.add name (Parent.Module md) sg.parents in
    let elements = LMap.add name Element.Module sg.elements in
      {sg with modules; parents; elements}

  let add_module_type name mty sg =
    let module_types = SMap.add name mty sg.module_types in
    let parents = LMap.add name (Parent.ModuleType mty) sg.parents in
    let elements = LMap.add name Element.ModuleType sg.elements in
      {sg with module_types; parents; elements}

  let add_datatype name decl sg =
    let types = SMap.add name Element.Type sg.types in
    let parents = LMap.add name (Parent.Datatype decl) sg.parents in
    let elements =
      let add_element name (elem : Element.datatype) acc =
        let open Element in
        let (Constructor _ | Field _ | Label _ as elem) = elem in
          LMap.add name elem acc
      in
        LMap.fold add_element (Datatype.elements decl) sg.elements
    in
    let elements = LMap.add name Element.Type elements in
      {sg with types; parents; elements}

  let add_class name cl sg =
    let types = SMap.add name Element.Class sg.types in
    let class_signatures = SMap.add name cl sg.class_signatures in
    let parents = LMap.add name (Parent.Class cl) sg.parents in
    let elements = LMap.add name Element.Class sg.elements in
      {sg with types; class_signatures; parents; elements}

  let add_class_type name clty sg =
    let types = SMap.add name Element.ClassType sg.types in
    let class_signatures = SMap.add name clty sg.class_signatures in
    let parents = LMap.add name (Parent.ClassType clty) sg.parents in
    let elements = LMap.add name Element.ClassType sg.elements in
      {sg with types; class_signatures; parents; elements}

  let add_element name element sg =
    let elements = LMap.add name element sg.elements in
      {sg with elements}

  let add_documentation doc sg =
    let labels = documentation_labels [] doc in
    let add_label sg label = add_element label (Element.Label None) sg in
      List.fold_left add_label sg labels

  let add_comment comment sg =
    let labels = comment_labels [] comment in
    let add_label sg label = add_element label (Element.Label None) sg in
      List.fold_left add_label sg labels

  let rec include_ t sg =
      match t with
    | Expr expr -> include_ (Lazy.force expr.expansion) sg
    | Sig incl ->
        let incl = Lazy.force incl in
        let modules =
          SMap.fold SMap.add incl.modules sg.modules
        in
        let module_types =
          SMap.fold SMap.add incl.module_types sg.module_types
        in
        let class_signatures =
          SMap.fold SMap.add incl.class_signatures sg.class_signatures
        in
        let types =
          SMap.fold SMap.add incl.types sg.types
        in
        let parents =
          LMap.fold LMap.add incl.parents sg.parents
        in
        let elements =
          LMap.fold LMap.add incl.elements sg.elements
        in
          {modules; module_types; class_signatures;
           types; parents; elements}
    | Functor _ | Generative _ | Abstract | Unresolved -> sg

  let path lookup p =
    let term = Path(p, false) in
    let expansion = lazy (lookup p) in
      Expr {term; expansion}

  let alias lookup p =
    let term = Alias(p, false) in
    let expansion = lazy (lookup p) in
      Expr {term; expansion}

  let signature f x = Sig (lazy (f x))

  let functor_ id arg res =
    let cache = Hashtbl.create 3 in
      Functor {id; arg; res; cache}

  let generative t = Generative t

  let abstract = Abstract

  let unresolved = Unresolved

  let replace_module name t sg =
    let modules = SMap.map_item name (fun _ -> t) sg.modules in
    let parents =
      LMap.map_item name
        (function
          | Parent.Module _ -> Parent.Module t
          | item -> item)
        sg.parents
    in
      {sg with modules; parents}

  let map_module name f sg =
    let modules = SMap.map_item name f sg.modules in
    let parents =
      LMap.map_item name
        (function
          | Parent.Module t -> Parent.Module (f t)
          | item -> item)
        sg.parents
    in
      {sg with modules; parents}

  let remove_module name sg =
    let modules = SMap.remove name sg.modules in
    let parents =
      LMap.filter_item name
        (function
          | Parent.Module _ -> true
          | _ -> false)
        sg.parents
    in
    let elements = LMap.filter_item name ((<>) Element.Module) sg.elements in
      {sg with modules; parents; elements}

  let remove_datatype name sg =
    let types = SMap.filter_item name ((<>) Element.Type) sg.types in
    let parents =
      LMap.filter_item name
        (function
          | Parent.Datatype _ -> true
          | _ -> false)
        sg.parents
    in
    let elements = LMap.filter_item name ((<>) Element.Type) sg.elements in
      {sg with types; parents; elements}

  let rec with_module frag eq = function
    | Expr expr ->
        let term = WithModule(expr, frag, eq) in
        let expansion =
          lazy (with_module frag eq (Lazy.force expr.expansion))
        in
          Expr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.split frag in
                match frag with
                | None -> replace_module name eq sg
                | Some frag -> map_module name (with_module frag eq) sg )
        in
          Sig sg
    | Functor _ | Generative _ | Abstract | Unresolved as t -> t

  let rec with_module_subst frag = function
    | Expr expr ->
        let term = WithModuleSubst(expr, frag) in
        let expansion =
          lazy (with_module_subst frag (Lazy.force expr.expansion))
        in
          Expr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.split frag in
                match frag with
                | None -> remove_module name sg
                | Some frag -> map_module name (with_module_subst frag) sg )
        in
          Sig sg
    | Functor _ | Generative _ | Abstract | Unresolved as t -> t

  let rec with_type_subst frag = function
    | Expr expr ->
        let term = WithTypeSubst(expr, frag) in
        let expansion =
          lazy (with_type_subst frag (Lazy.force expr.expansion))
        in
          Expr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.split frag in
                match frag with
                | None -> remove_datatype name sg
                | Some frag -> map_module name (with_type_subst frag) sg )
        in
          Sig sg
    | Functor _ | Generative _ | Abstract | Unresolved as t -> t

  let module_type_substitution path expansion body =
    match body with
    | Abstract ->
        let term = Path(path, true) in
          Expr {term; expansion}
    | _ -> body

  let rec module_substitution path expansion body =
    match body with
    | Expr _ -> body
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let modules =
                SMap.mapi
                  (fun name body ->
                     let path = Path.module_ path name in
                     let expansion =
                       lazy (lookup_module name (Lazy.force expansion))
                     in
                       module_substitution path expansion body)
               sg.modules
              in
              let module_types =
                SMap.mapi
                  (fun name body ->
                     let path = Path.module_type path name in
                     let expansion =
                       lazy (lookup_module_type name (Lazy.force expansion))
                     in
                       module_type_substitution path expansion body)
                  sg.module_types
              in
                {sg with modules; module_types} )
        in
          Sig sg
    | Functor fn ->
        let res = module_substitution path expansion fn.res in
        let cache = Hashtbl.create 3 in
          Functor {fn with res; cache}
    | Generative body ->
        let body = module_substitution path expansion body in
          Generative body
    | Abstract ->
        let term = Alias(path, true) in
          Expr {term; expansion}
    | Unresolved -> body

  let rec reduce_signature_ident id path =
    let open Identifier in function
      | Root _ -> None
      | Module(p, name) -> begin
          match reduce_signature_ident id path p with
          | Some p -> Some (Path.module_ p name)
          | None -> None
        end
      | Argument _ as id' -> if id = id' then Some path else None
      | ModuleType _ -> None

  and reduce_module_ident id path =
    let open Identifier in function
      | (Root _ : 'a module_) -> None
      | Module(p, name) -> begin
          match reduce_signature_ident id path p with
          | Some p -> Some (Path.module_ p name)
          | None -> None
        end
      | Argument _ as id' -> if id = id' then Some path else None

  and reduce_resolved_module_path in_arg id path =
    let open Path.Resolved in function
    | Identifier id' ->
        if in_arg then reduce_module_ident id path id' else None
    | Subst(_, p) ->
        reduce_resolved_module_path in_arg id path p
    | SubstAlias(_, p) ->
        reduce_resolved_module_path in_arg id path p
    | Module(p, name) -> begin
        match reduce_resolved_module_path in_arg id path p with
        | Some p -> Some (Path.module_ p name)
        | None -> None
      end
    | Apply(p, arg) -> begin
        let rp = reduce_resolved_module_path in_arg id path p in
        let rarg = reduce_module_path true id path arg in
          match rp, rarg with
          | None, None -> None
          | None, Some arg -> Some(Path.Resolved(Apply(p, arg)))
          | Some p, None -> Some(Path.apply p arg)
          | Some p, Some arg -> Some(Path.apply p arg)
      end

  and reduce_resolved_module_type_path id path =
    let open Path.Resolved in function
    | (Identifier _ : 'a module_type) -> None
    | ModuleType(p, name) -> begin
        match reduce_resolved_module_path false id path p with
        | Some p -> Some (Path.module_type p name)
        | None -> None
      end

  and reduce_module_path in_arg id path =
    let open Path in function
    | Resolved r -> reduce_resolved_module_path in_arg id path r
    | Root _ -> None
    | Dot(p, name) -> begin
        match reduce_module_path in_arg id path p with
        | Some p -> Some (Dot(p, name))
        | None -> None
      end
    | Apply(p, arg) -> begin
        let rp = reduce_module_path in_arg id path p in
        let rarg = reduce_module_path true id path arg in
          match rp, rarg with
          | None, None -> None
          | None, Some arg -> Some(Apply(p, arg))
          | Some p, None -> Some(Apply(p, arg))
          | Some p, Some arg -> Some(Apply(p, arg))
      end

  and reduce_module_type_path id path =
    let open Path in function
    | Resolved r -> reduce_resolved_module_type_path id path r
    | Dot(p, name) -> begin
        match reduce_module_path false id path p with
        | Some p -> Some (Dot(p, name))
        | None -> None
      end

  let rec subst_signature_ident id lookup path =
    let open Identifier in function
      | Root _ -> None
      | Module(p, name) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_ p name in
              let t = lazy (lookup_module name (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end
      | Argument _ as id' ->
          if id = id' then Some (path, lazy (lookup path))
          else None
      | ModuleType _ -> None

  and subst_module_ident id lookup path id' =
    let open Identifier in
    if id = id' then Some (path, lazy (lookup path))
    else match id' with
      | (Root _ : 'a Identifier.module_) -> None
      | Module(p, name) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_ p name in
              let t = lazy (lookup_module name (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end
      | Argument _ -> None

  and subst_module_type_ident id lookup (path : 'a Path.module_) id' =
    let open Identifier in
      match id' with
      | (ModuleType(p, name) : 'a Identifier.module_type) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_type p name in
              let t = lazy (lookup_module_type name (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end

  and subst_resolved_module_path id lookup path =
    let open Path.Resolved in function
    | Identifier id' -> subst_module_ident id lookup path id'
    | Subst(_, p) -> subst_resolved_module_path id lookup path p
    | SubstAlias(sub, p) -> subst_resolved_module_path id lookup path sub
    | Module(p, name) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.module_ p name in
            let t = lazy (lookup_module name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end
    | Apply(p, arg) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.apply p arg in
            let t = lazy (lookup_apply lookup arg (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_resolved_module_type_path id lookup (path : 'a Path.module_) =
    let open Path.Resolved in function
    | Identifier id' -> subst_module_type_ident id lookup path id'
    | ModuleType(p, name) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.module_type p name in
            let t = lazy (lookup_module_type name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_module_path id lookup path =
    let open Path in function
    | Resolved r -> subst_resolved_module_path id lookup path r
    | Root _ -> None
    | Dot(p, name) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = Dot(p, name) in
            let t = lazy (lookup_module name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end
    | Apply(p, arg) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = Apply(p, arg) in
            let t = lazy (lookup_apply lookup arg (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_module_type_path id lookup (path : 'a Path.module_) =
    let open Path in function
    | Resolved r -> subst_resolved_module_type_path id lookup path r
    | Dot(p, name) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = Dot(p, name) in
            let t = lazy (lookup_module_type name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_expr id lookup path expr =
    match expr.term with
    | Path(p, sub) -> begin
        let p =
          match reduce_module_type_path id path p with
          | None -> p
          | Some p -> p
        in
          match subst_module_type_path id lookup path p with
          | None ->
              let term = Path(p, sub) in
              let expansion =
                lazy (subst id lookup path (Lazy.force expr.expansion))
              in
                {term; expansion}
          | Some (p, t) ->
              let term = Path(p, sub) in
              let expansion =
                lazy (module_type_substitution p t
                        (subst id lookup path (Lazy.force expr.expansion)) )
              in
                {term; expansion}
      end
    | Alias(p, sub) -> begin
        let p =
          match reduce_module_path false id path p with
          | None -> p
          | Some p -> p
        in
          match subst_module_path id lookup path p with
          | None ->
              let term = Alias(p, sub) in
              let expansion =
                lazy (subst id lookup path (Lazy.force expr.expansion))
              in
                {term; expansion}
          | Some (p, t) ->
              let term = Alias(p, sub) in
              let expansion =
                lazy (module_substitution p t
                        (subst id lookup path (Lazy.force expr.expansion)) )
              in
                {term; expansion}
      end
    | WithModule(body, frag, eq) ->
        let body = subst_expr id lookup path body in
        let eq = subst id lookup path eq in
        let term = WithModule(body, frag, eq) in
        let expansion =
          lazy (with_module frag eq (Lazy.force body.expansion))
        in
          {term; expansion}
    | WithModuleSubst(body, frag) ->
        let body = subst_expr id lookup path body in
        let term = WithModuleSubst(body, frag) in
        let expansion =
          lazy (with_module_subst frag (Lazy.force body.expansion))
        in
          {term; expansion}
    | WithTypeSubst(body, frag) ->
        let body = subst_expr id lookup path body in
        let term = WithTypeSubst(body, frag) in
        let expansion =
          lazy (with_type_subst frag (Lazy.force body.expansion))
        in
          {term; expansion}

  and subst id lookup path = function
    | Expr expr -> Expr (subst_expr id lookup path expr)
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let modules =
                SMap.map (subst id lookup path) sg.modules
              in
              let module_types =
                SMap.map (subst id lookup path) sg.module_types
              in
                {sg with modules; module_types} )
        in
          Sig sg
    | Functor fn ->
        let arg = subst id lookup path fn.arg in
        let res = subst id lookup path fn.res in
        let cache = Hashtbl.create 3 in
          Functor {id = fn.id; arg; res; cache}
    | Generative t -> Generative (subst id lookup path t)
    | Abstract -> Abstract
    | Unresolved -> Unresolved

  and lookup_apply lookup arg = function
    | Expr expr -> lookup_apply lookup arg (Lazy.force expr.expansion)
    | Sig sg -> Unresolved
    | Functor fn -> begin
        try
          Hashtbl.find fn.cache arg
        with Not_found ->
          let res = subst fn.id lookup arg fn.res in
            Hashtbl.add fn.cache arg res;
            res
      end
    | Generative t -> Unresolved
    | Abstract -> Unresolved
    | Unresolved -> Unresolved

  let rec find_parent_apply lookup arg = function
    | Expr expr -> begin
        match expr.term with
        | Path(_, true) | Alias(_, true) -> raise Not_found
        | _ -> find_parent_apply lookup arg (Lazy.force expr.expansion)
      end
    | Sig sg -> raise Not_found
    | Functor fn -> begin
        try
          Parent.Module (Hashtbl.find fn.cache arg)
        with Not_found ->
          let res = subst fn.id lookup arg fn.res in
            Hashtbl.add fn.cache arg res;
            Parent.Module res
      end
    | Generative t -> raise Not_found
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

end

and Datatype : sig

  type +'a t

  val find_constructor_element : string -> 'a t -> Element.datatype_constructor

  val find_field_element : string -> 'a t -> Element.datatype_field

  val find_label_element : string -> 'a t -> Element.datatype_label

  val find_element : string -> 'a t -> Element.datatype

  val add_documentation : 'a Documentation.t -> 'a t -> 'a t

  val abstract : 'a t

  val variant : string -> string list -> 'a t

  val record : string -> string list -> 'a t

  val extensible : 'a t

  val unresolved : 'a t

  val elements : 'a t -> [ `Constructor | `Field | `Label] Element.t LMap.t

end = struct

  type 'a variant =
    { type_name: string;
      constructors: SSet.t;
      labels: SSet.t; }

  type 'a record =
    { type_name: string;
      fields: SSet.t;
      labels: SSet.t; }

  type 'a t =
    | Variant of 'a variant
    | Record of 'a record
    | Unresolved

  let find_constructor_element name = function
    | Variant v ->
        if SSet.mem name v.constructors then Element.Constructor v.type_name
        else raise Not_found
    | _ -> raise Not_found

  let find_field_element name = function
    | Record r ->
        if SSet.mem name r.fields then Element.Field r.type_name
        else raise Not_found
    | _ -> raise Not_found

  let find_label_element name = function
    | Variant v ->
        if SSet.mem name v.labels then Element.Label (Some v.type_name)
        else raise Not_found
    | Record r ->
        if SSet.mem name r.labels then Element.Label (Some r.type_name)
        else raise Not_found
    | _ -> raise Not_found

  let find_element name = function
    | Variant v ->
        if SSet.mem name v.constructors then Element.Constructor v.type_name
        else if SSet.mem name v.labels then Element.Label (Some v.type_name)
        else raise Not_found
    | Record r ->
        if SSet.mem name r.fields then Element.Field r.type_name
        else if SSet.mem name r.labels then Element.Label (Some r.type_name)
        else raise Not_found
    | _ -> raise Not_found

  let add_documentation doc = function
    | Variant v ->
        let lbls = documentation_labels [] doc in
        let labels = List.fold_right SSet.add lbls v.labels in
          Variant {v with labels}
    | Record r ->
        let lbls = documentation_labels [] doc in
        let labels = List.fold_right SSet.add lbls r.labels in
          Record {r with labels}
    | Unresolved -> Unresolved

  let abstract = Unresolved

  let variant type_name constructors =
    let open TypeDecl.Constructor in
    let constructors =
        List.fold_right SSet.add constructors SSet.empty
    in
    let labels = SSet.empty in
    let variant = {type_name; constructors; labels} in
      Variant variant

  let record type_name fields =
    let open TypeDecl.Field in
    let fields =
        List.fold_right SSet.add fields SSet.empty
    in
    let labels = SSet.empty in
    let record = {type_name; fields; labels} in
      Record record

  let extensible = Unresolved

  let unresolved = Unresolved

  let elements = function
    | Variant v ->
        let elements =
          SSet.fold
            (fun name acc ->
               LMap.add name (Element.Constructor v.type_name) acc)
              v.constructors LMap.empty
        in
        let elements =
          SSet.fold
            (fun name acc ->
               LMap.add name (Element.Label (Some v.type_name)) acc)
            v.labels elements
        in
          elements
      | Record r ->
          let elements =
            SSet.fold
              (fun name acc ->
                 LMap.add name (Element.Field r.type_name) acc)
              r.fields LMap.empty
          in
          let elements =
            SSet.fold
              (fun name acc ->
                 LMap.add name (Element.Label (Some r.type_name)) acc)
              r.labels elements
          in
            elements
      | Unresolved -> LMap.empty

end

and ClassSig : sig

  type 'a t

  val find_method_element : string -> 'a t -> Element.class_signature_method

  val find_instance_variable_element : string -> 'a t ->
        Element.class_signature_instance_variable

  val find_label_element : string -> 'a t -> Element.class_signature_label

  val find_element : string -> 'a t -> Element.class_signature

  type 'a signature

  val empty : 'a signature

  val add_element : string -> Element.class_signature ->
    'a signature -> 'a signature

  val add_documentation : 'a Documentation.t -> 'a signature -> 'a signature

  val add_comment : 'a Documentation.comment -> 'a signature -> 'a signature

  val inherit_ : 'a t -> 'a signature -> 'a signature

  val constr : ('a Path.class_type -> 'a t) -> 'a Path.class_type -> 'a t

  val signature : ('b -> 'a signature) -> 'b -> 'a t

  val unresolved : 'a t

end = struct

  type 'a signature = Element.class_signature LMap.t

  type 'a desc =
    | Sig of 'a signature
    | Unresolved

  type 'a t = 'a desc Lazy.t

  open Element

  let find_method_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | Method as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_instance_variable_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | InstanceVariable as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_label_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | Label _ as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.find_name name csig
      | Unresolved -> raise Not_found

  let empty = LMap.empty

  let add_element name element csig = LMap.add name element csig

  let add_documentation doc csig =
    let labels = documentation_labels [] doc in
    let add_label csig label = add_element label (Element.Label None) csig in
      List.fold_left add_label csig labels

  let add_comment comment sg =
    let labels = comment_labels [] comment in
    let add_label sg label = add_element label (Element.Label None) sg in
      List.fold_left add_label sg labels

  let inherit_ t csig =
    let desc = Lazy.force t in
      match desc with
      | Sig inhr -> LMap.fold LMap.add inhr csig
      | Unresolved -> csig

  let constr f x = lazy (Lazy.force (f x))

  let signature f x = lazy (Sig (f x))

  let unresolved = lazy Unresolved

end

and Parent : sig

  type kind = Kind.parent

  type ('a, 'b) t =
    | Module : 'a Sig.t -> ('a, [< kind > `Module]) t
    | ModuleType : 'a Sig.t -> ('a, [< kind > `ModuleType]) t
    | Datatype : 'a Datatype.t -> ('a, [< kind > `Type]) t
    | Class : 'a ClassSig.t -> ('a, [< kind > `Class]) t
    | ClassType : 'a ClassSig.t -> ('a, [< kind > `ClassType]) t

  type 'a signature = ('a, [`Module | `ModuleType]) t

  type 'a class_signature = ('a, [`Class |` ClassType]) t

  type 'a datatype = ('a, [`Type]) t

  type 'a module_ = ('a, [`Module]) t

  type 'a module_type = ('a, [`ModuleType]) t

  type 'a sig_or_type = ('a, [`Module | `ModuleType | `Type]) t

  type 'a any = ('a, kind) t

  type 'a subst =
    | Subst of 'a Path.module_type
    | SubstAlias of 'a Path.module_

end = Parent

and Element : sig

  type kind =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label ]

  type 'a t =
    | Module : [< kind > `Module] t
    | ModuleType : [< kind > `ModuleType] t
    | Type : [< kind > `Type] t
    | Constructor : string -> [< kind > `Constructor] t
    | Field : string -> [< kind > `Field] t
    | Extension : [< kind > `Extension] t
    | Exception : [< kind > `Exception] t
    | Value : [< kind > `Value] t
    | Class : [< kind > `Class] t
    | ClassType : [< kind > `ClassType] t
    | Method : [< kind > `Method] t
    | InstanceVariable : [< kind > `InstanceVariable] t
    | Label : string option -> [< kind > `Label] t

  type signature_module = [`Module] t

  type signature_module_type = [`ModuleType] t

  type signature_type = [`Type | `Class | `ClassType] t

  type signature_constructor = [`Constructor | `Extension | `Exception] t

  type signature_field = [`Field] t

  type signature_extension = [`Extension | `Exception] t

  type signature_exception = [`Exception] t

  type signature_value = [`Value] t

  type signature_class = [`Class] t

  type signature_class_type = [`Class | `ClassType] t

  type signature_label = [`Label] t

  type datatype_constructor = [`Constructor] t

  type datatype_field = [`Field] t

  type datatype_label = [`Label] t

  type class_signature_method = [`Method] t

  type class_signature_instance_variable = [`InstanceVariable] t

  type class_signature_label = [`Label] t

  type signature =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType | `Label ] t

  type class_signature = [ `Method | `InstanceVariable | `Label ] t

  type datatype = [ `Constructor | `Field | `Label ] t

end = Element
