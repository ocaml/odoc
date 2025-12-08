module Maps = Odoc_model.Paths.Identifier.Maps

module ModuleMap = Map.Make (struct
  type t = Ident.module_

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module TypeMap = Map.Make (struct
  type t = Ident.type_

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module ModuleTypeMap = Map.Make (struct
  type t = Ident.module_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module ValueMap = Map.Make (struct
  type t = Ident.value

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module IdentMap = Map.Make (struct
  type t = Ident.any

  let compare = Ident.compare
end)

module Delayed = struct
  let eager = ref false

  type 'a t = { mutable v : 'a option; mutable get : (unit -> 'a) option }

  let get : 'a t -> 'a =
   fun x ->
    match (x.v, x.get) with
    | Some x, _ -> x
    | None, Some get ->
        let v = get () in
        x.v <- Some v;
        x.get <- None;
        v
    | _, _ -> failwith "bad delayed"

  let put : (unit -> 'a) -> 'a t =
   fun f ->
    if !eager then { v = Some (f ()); get = None }
    else { v = None; get = Some f }

  let put_val : 'a -> 'a t = fun v -> { v = Some v; get = None }
end

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

module rec Module : sig
  type decl =
    | Alias of Cpath.module_ * ModuleType.simple_expansion option
    | ModuleType of ModuleType.expr

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    type_ : decl;
    canonical : Odoc_model.Paths.Path.Module.t option;
    hidden : bool;
  }
end =
  Module

and ModuleSubstitution : sig
  type t = { doc : CComment.docs; manifest : Cpath.module_ }
end =
  ModuleSubstitution

and ModuleTypeSubstitution : sig
  type t = { doc : CComment.docs; manifest : ModuleType.expr }
end =
  ModuleTypeSubstitution

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : CComment.docs;
      }
    end

    type element = Type of TypeExpr.t | Constructor of Constructor.t

    type t = { kind : kind; elements : element list }
  end

  module Object : sig
    type method_ = { name : string; type_ : TypeExpr.t }

    type field = Method of method_ | Inherit of TypeExpr.t

    type t = { fields : field list; open_ : bool }
  end

  module Package : sig
    type substitution = Cfrag.type_ * TypeExpr.t

    type t = { path : Cpath.module_type; substitutions : substitution list }
  end

  type label = Odoc_model.Lang.TypeExpr.label

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of (string option * t) list
    | Unboxed_tuple of (string option * t) list
    | Constr of Cpath.type_ * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Cpath.class_type * t list
    | Poly of string list * t
    | Quote of t
    | Splice of t
    | Package of TypeExpr.Package.t
end =
  TypeExpr

and Extension : sig
  module Constructor : sig
    type t = {
      name : string;
      source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
      doc : CComment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    type_path : Cpath.type_;
    doc : CComment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end =
  Extension

and Exception : sig
  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end =
  Exception

and FunctorParameter : sig
  type parameter = { id : Ident.module_; expr : ModuleType.expr }

  type t = Named of parameter | Unit
end =
  FunctorParameter

and ModuleType : sig
  type substitution =
    | ModuleEq of Cfrag.module_ * Module.decl
    | ModuleSubst of Cfrag.module_ * Cpath.module_
    | ModuleTypeEq of Cfrag.module_type * ModuleType.expr
    | ModuleTypeSubst of Cfrag.module_type * ModuleType.expr
    | TypeEq of Cfrag.type_ * TypeDecl.Equation.t
    | TypeSubst of Cfrag.type_ * TypeDecl.Equation.t

  type type_of_desc =
    | ModPath of Cpath.module_
    | StructInclude of Cpath.module_

  type simple_expansion =
    | Signature of Signature.t
    | Functor of FunctorParameter.t * simple_expansion

  type typeof_t = {
    t_desc : type_of_desc;
    t_original_path : Cpath.module_;
    t_expansion : simple_expansion option;
  }

  module U : sig
    type expr =
      | Path of Cpath.module_type
      | Signature of Signature.t
      | With of substitution list * expr
      | TypeOf of type_of_desc * Cpath.module_
      | Strengthen of expr * Cpath.module_ * bool
  end

  type path_t = {
    p_expansion : simple_expansion option;
    p_path : Cpath.module_type;
  }

  type with_t = {
    w_substitutions : substitution list;
    w_expansion : simple_expansion option;
    w_expr : U.expr;
  }

  type strengthen_t = {
    s_expansion : simple_expansion option;
    s_expr : U.expr;
    s_path : Cpath.module_;
    s_aliasable : bool
  }

  type expr =
    | Path of path_t
    | Signature of Signature.t
    | With of with_t
    | Functor of FunctorParameter.t * expr
    | TypeOf of typeof_t
    | Strengthen of strengthen_t

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    canonical : Odoc_model.Paths.Path.ModuleType.t option;
    expr : expr option;
  }
end =
  ModuleType

and TypeDecl : sig
  module Field : sig
    type t = {
      name : string;
      doc : CComment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module UnboxedField : sig
    type t = {
      name : string;
      doc : CComment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      name : string;
      doc : CComment.docs;
      args : argument;
      res : TypeExpr.t option;
    }
  end

  module Representation : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Record_unboxed_product of UnboxedField.t list
      | Extensible
  end

  type param = Odoc_model.Lang.TypeDecl.param

  module Equation : sig
    type t = {
      params : param list;
      private_ : bool;
      manifest : TypeExpr.t option;
      constraints : (TypeExpr.t * TypeExpr.t) list;
    }
  end

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    canonical : Odoc_model.Paths.Path.Type.t option;
    equation : Equation.t;
    representation : Representation.t option;
  }
end =
  TypeDecl

and Value : sig
  type value = Odoc_model.Lang.Value.value

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    type_ : TypeExpr.t;
    value : value;
  }
end =
  Value

and Signature : sig
  type recursive = Odoc_model.Lang.Signature.recursive

  type item =
    | Module of Ident.module_ * recursive * Module.t Delayed.t
    | ModuleSubstitution of Ident.module_ * ModuleSubstitution.t
    | ModuleType of Ident.module_type * ModuleType.t Delayed.t
    | ModuleTypeSubstitution of Ident.module_type * ModuleTypeSubstitution.t
    | Type of Ident.type_ * recursive * TypeDecl.t Delayed.t
    | TypeSubstitution of Ident.type_ * TypeDecl.t
    | Exception of Ident.exception_ * Exception.t
    | TypExt of Extension.t
    | Value of Ident.value * Value.t Delayed.t
    | Class of Ident.type_ * recursive * Class.t
    | ClassType of Ident.type_ * recursive * ClassType.t
    | Include of Include.t
    | Open of Open.t
    | Comment of CComment.docs_or_stop

  (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
  type removed_item =
    | RModule of Odoc_model.Names.ModuleName.t * Cpath.module_
    | RType of Odoc_model.Names.TypeName.t * TypeExpr.t * TypeDecl.Equation.t
        (** [RType (_, texpr, eq)], [eq.manifest = Some texpr] *)
    | RModuleType of Odoc_model.Names.ModuleTypeName.t * ModuleType.expr

  type t = {
    items : item list;
    compiled : bool;
    removed : removed_item list;
    doc : CComment.docs;
  }
end =
  Signature

and Open : sig
  type t = { expansion : Signature.t; doc : CComment.docs }
end =
  Open

and Include : sig
  type decl = Alias of Cpath.module_ | ModuleType of ModuleType.U.expr

  type t = {
    parent : Odoc_model.Paths.Identifier.Signature.t;
    strengthened : Cpath.module_ option;
    doc : CComment.docs;
    status : [ `Default | `Inline | `Closed | `Open ];
    shadowed : Odoc_model.Lang.Include.shadowed;
    expansion_ : Signature.t;
    decl : decl;
    loc : Odoc_model.Location_.span;
  }
end =
  Include

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    type_ : decl;
    expansion : ClassSignature.t option;
  }
end =
  Class

and ClassType : sig
  type expr =
    | Constr of Cpath.class_type * TypeExpr.t list
    | Signature of ClassSignature.t

  type t = {
    source_loc : Odoc_model.Paths.Identifier.SourceLocation.t option;
    source_loc_jane : Odoc_model.Lang.Source_loc_jane.t option;
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end =
  ClassType

and ClassSignature : sig
  module Constraint : sig
    type t = { left : TypeExpr.t; right : TypeExpr.t; doc : CComment.docs }
  end

  module Inherit : sig
    type t = { expr : ClassType.expr; doc : CComment.docs }
  end

  type item =
    | Method of Ident.method_ * Method.t
    | InstanceVariable of Ident.instance_variable * InstanceVariable.t
    | Constraint of Constraint.t
    | Inherit of Inherit.t
    | Comment of CComment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list; doc : CComment.docs }
end =
  ClassSignature

and Method : sig
  type t = {
    doc : CComment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  Method

and InstanceVariable : sig
  type t = {
    doc : CComment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  InstanceVariable

and Substitution : sig
  type subst_module =
    [ `Prefixed of Cpath.module_ * Cpath.Resolved.module_
    | `Substituted
    | `Renamed of Ident.module_ ]

  type subst_module_type =
    [ `Prefixed of Cpath.module_type * Cpath.Resolved.module_type
    | `Renamed of Ident.module_type ]

  type subst_type =
    [ `Prefixed of Cpath.type_ * Cpath.Resolved.type_ | `Renamed of Ident.type_ ]

  type subst_class_type =
    [ `Prefixed of Cpath.class_type * Cpath.Resolved.class_type
    | `Renamed of Ident.type_ ]

  type t = {
    module_ : subst_module ModuleMap.t;
    module_type : subst_module_type ModuleTypeMap.t;
    type_ : subst_type TypeMap.t;
    class_type : subst_class_type TypeMap.t;
    type_replacement : (TypeExpr.t * TypeDecl.Equation.t) TypeMap.t;
    module_type_replacement : ModuleType.expr ModuleTypeMap.t;
    path_invalidating_modules : Ident.module_ list;
    unresolve_opaque_paths : bool;
  }
end =
  Substitution

and CComment : sig
  type block_element =
    [ Odoc_model.Comment.nestable_block_element
    | `Heading of Label.t
    | `Tag of Odoc_model.Comment.tag
    | `Media of
      Odoc_model.Comment.media_href * Odoc_model.Comment.media * string ]

  type docs = {
    elements : block_element Odoc_model.Comment.with_location list;
    warnings_tag : string option;
  }

  type docs_or_stop = [ `Docs of docs | `Stop ]
end =
  CComment

and Label : sig
  type t = {
    attrs : Odoc_model.Comment.heading_attrs;
    label : Ident.label;
    text : Odoc_model.Comment.paragraph;
    location : Odoc_model.Location_.span;
  }
end =
  Label

module Element = struct
  open Odoc_model.Paths

  type module_ = [ `Module of Identifier.Path.Module.t * Module.t Delayed.t ]

  type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]

  type datatype = [ `Type of Identifier.Type.t * TypeDecl.t ]

  type value = [ `Value of Identifier.Value.t * Value.t ]

  type label = [ `Label of Identifier.Label.t * Label.t ]

  type class_ = [ `Class of Identifier.Class.t * Class.t ]

  type class_type = [ `ClassType of Identifier.ClassType.t * ClassType.t ]

  type type_ = [ datatype | class_ | class_type ]

  type signature = [ module_ | module_type ]

  type constructor =
    [ `Constructor of Identifier.Constructor.t * TypeDecl.Constructor.t ]

  type exception_ = [ `Exception of Identifier.Exception.t * Exception.t ]

  type extension =
    [ `Extension of
      Identifier.Extension.t * Extension.Constructor.t * Extension.t ]

  type extension_decl =
    [ `ExtensionDecl of Identifier.Extension.t * Extension.Constructor.t ]

  type field = [ `Field of Identifier.Field.t * TypeDecl.Field.t ]

  type unboxed_field =
    [ `UnboxedField of Identifier.UnboxedField.t * TypeDecl.UnboxedField.t ]

  (* No component for pages yet *)
  type page = [ `Page of Identifier.Page.t * Odoc_model.Lang.Page.t ]

  type label_parent = [ signature | type_ | page ]

  type fragment_type_parent = [ signature | datatype ]

  type any =
    [ signature
    | value
    | datatype
    | label
    | class_
    | class_type
    | constructor
    | exception_
    | extension
    | extension_decl
    | field
    | unboxed_field
    | page ]

  let identifier : [< any ] -> Odoc_model.Paths.Identifier.t =
    let open Odoc_model.Paths.Identifier in
    function
    | `Module (id, _) -> (id :> t)
    | `ModuleType (id, _) -> (id :> t)
    | `Type (id, _) -> (id :> t)
    | `ClassType (id, _) -> (id :> t)
    | `Class (id, _) -> (id :> t)
    | `Value (id, _) -> (id :> t)
    | `Label (id, _) -> (id :> t)
    | `Constructor (id, _) -> (id :> t)
    | `Exception (id, _) -> (id :> t)
    | `Field (id, _) -> (id :> t)
    | `UnboxedField (id, _) -> (id :> t)
    | `Extension (id, _, _) -> (id :> t)
    | `ExtensionDecl (id, _) -> (id :> t)
    | `Page (id, _) -> (id :> t)
end

module Fmt = struct
  type config = {
    short_paths : bool;
    show_canonical : bool;
    show_removed : bool;
    show_expansions : bool;
    show_include_expansions : bool;
  }

  let default =
    {
      short_paths = false;
      show_canonical = true;
      show_removed = true;
      show_expansions = true;
      show_include_expansions = true;
    }

  type id = Odoc_model.Paths.Identifier.t
  type path = Odoc_model.Paths.Path.t
  type rpath = Odoc_model.Paths.Path.Resolved.t
  open Odoc_model.Names
  open Odoc_model.Paths

  let fpf = Format.fprintf

  let fpp_opt (c : config) fmt pp_a ppf = function
    | Some t -> fpf ppf fmt (pp_a c) t
    | None -> ()

  let fpp_list fmt_sep fmt_outer pp_a ppf t =
    let pp_sep ppf () = fpf ppf fmt_sep in
    match t with
    | [] -> ()
    | t -> fpf ppf fmt_outer (Format.pp_print_list ~pp_sep pp_a) t

  (* Three helper functions to help with paths. Generally paths
     have constructors of the form [`Hidden(p1)] or
     [`Alias(p1,p2)]. When printing these paths, if we're printing a
     short path we often want to just ignore the constructor and print
     one of the inner paths, [p1] or [p2]. These functions do that. If
     [short_paths] is set in the config, we skip to one of the inner
     paths - in [wrap] there's no choice, but in [wrap2] we pick [p1]
     and in [wrap2r] we pick [p2]. If [short_paths] is not set, we
     print a string representing the constructor, and one or both paths
     with brackets. *)
  let wrap : type a.
      config ->
      string ->
      (config -> Format.formatter -> a -> unit) ->
      Format.formatter ->
      a ->
      unit =
   fun c txt fn ppf x ->
    if c.short_paths then Format.fprintf ppf "%a" (fn c) x
    else Format.fprintf ppf "%s(%a)" txt (fn c) x

  let wrap2 : type a b.
      config ->
      string ->
      (config -> Format.formatter -> a -> unit) ->
      (config -> Format.formatter -> b -> unit) ->
      Format.formatter ->
      a ->
      b ->
      unit =
   fun c txt fn1 fn2 ppf x y ->
    if c.short_paths then Format.fprintf ppf "%a" (fn1 c) x
    else Format.fprintf ppf "%s(%a,%a)" txt (fn1 c) x (fn2 c) y

  let wrap2r : type a b.
      config ->
      string ->
      (config -> Format.formatter -> a -> unit) ->
      (config -> Format.formatter -> b -> unit) ->
      Format.formatter ->
      a ->
      b ->
      unit =
   fun c txt fn1 fn2 ppf x y ->
    if c.short_paths then Format.fprintf ppf "%a" (fn2 c) y
    else Format.fprintf ppf "%s(%a,%a)" txt (fn1 c) x (fn2 c) y

  let str : config -> Format.formatter -> string -> unit =
   fun _ ppf s -> Format.fprintf ppf "%s" s

  let bool : config -> Format.formatter -> bool -> unit =
   fun _ ppf b -> Format.fprintf ppf "%b" b

  let ident_fmt : config -> Format.formatter -> [< Ident.any ] -> unit =
   fun c ppf i ->
    if c.short_paths then Ident.short_fmt ppf i else Ident.fmt ppf i

  let rec model_identifier c ppf (p : id) =
    match p.iv with
    | `Root (_, unit_name) ->
        wrap c "root" (fun _ -> ModuleName.fmt) ppf unit_name
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (parent :> id)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (parent :> id)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (parent :> id)
          (TypeName.to_string name)
    | `Parameter (parent, name) ->
        Format.fprintf ppf "(param %a %s)" (model_identifier c)
          (parent :> id)
          (ModuleName.to_string name)
    | `Result parent ->
        if c.short_paths then model_identifier c ppf (parent :> id)
        else Format.fprintf ppf "%a.result" (model_identifier c) (parent :> id)
    | `Constructor (ty, x) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (ty :> id)
          (ConstructorName.to_string x)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (parent :> id)
          (ValueName.to_string name)
    | `Class (sg, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (sg :> id)
          (TypeName.to_string name)
    | `ClassType (sg, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (sg :> id)
          (TypeName.to_string name)
    | `InstanceVariable (sg, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (sg :> id)
          (InstanceVariableName.to_string name)
    | `Method (sg, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (sg :> id)
          (MethodName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (parent :> id)
          (LabelName.to_string name)
    | `Field (ty, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (ty :> id)
          (FieldName.to_string name)
    | `UnboxedField (ty, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (ty :> id)
          (UnboxedFieldName.to_string name)
    | `Exception (p, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (p :> id)
          (ExceptionName.to_string name)
    | `Extension (p, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (p :> id)
          (ExtensionName.to_string name)
    | `ExtensionDecl (p, _, name) ->
        Format.fprintf ppf "%a.%s" (model_identifier c)
          (p :> id)
          (ExtensionName.to_string name)
    | `Page (_, name) | `LeafPage (_, name) ->
        Format.fprintf ppf "%s" (PageName.to_string name)
    | `SourcePage (p, name) ->
        Format.fprintf ppf "%a/%s" (model_identifier c) (p :> id) name
    | `SourceLocation (p, def) ->
        Format.fprintf ppf "%a#%s" (model_identifier c)
          (p :> id)
          (DefName.to_string def)
    | `SourceLocationInternal (p, def) ->
        Format.fprintf ppf "%a#%s" (model_identifier c)
          (p :> id)
          (LocalName.to_string def)
    | `SourceLocationMod p ->
        Format.fprintf ppf "%a#" (model_identifier c) (p :> id)
    | `AssetFile (p, name) ->
        Format.fprintf ppf "%a/%s" (model_identifier c)
          (p :> id)
          (AssetName.to_string name)

  let rec signature : config -> Format.formatter -> Signature.t -> unit =
   fun c ppf sg ->
    let open Signature in
    let ident_fmt = if c.short_paths then Ident.short_fmt else Ident.fmt in
    let sig_item ppf = function
      | Module (id, _, m) ->
          Format.fprintf ppf "@[<hov 2>module %a %a@]" ident_fmt id (module_ c)
            (Delayed.get m)
      | ModuleSubstitution (id, m) ->
          Format.fprintf ppf "@[<v 2>module %a := %a@]" ident_fmt id
            (module_path c) m.ModuleSubstitution.manifest
      | ModuleType (id, mt) ->
          Format.fprintf ppf "@[<hov 2>module type %a %a@]" ident_fmt id
            (module_type c) (Delayed.get mt)
      | ModuleTypeSubstitution (id, mts) ->
          Format.fprintf ppf "@[<v 2>module type %a := %a@]" ident_fmt id
            (module_type_expr c) mts.ModuleTypeSubstitution.manifest
      | Type (id, _, t) ->
          Format.fprintf ppf "@[<v 2>type %a%a@]" ident_fmt id (type_decl c)
            (Delayed.get t)
      | TypeSubstitution (id, t) ->
          Format.fprintf ppf "@[<v 2>type %a :=%a@]" ident_fmt id (type_decl c)
            t
      | Exception (id, e) ->
          Format.fprintf ppf "@[<v 2>exception %a %a@]" ident_fmt id
            (exception_ c) e
      | TypExt e ->
          Format.fprintf ppf "@[<v 2>type_extension %a@]" (extension c) e
      | Value (id, v) ->
          Format.fprintf ppf "@[<v 2>val %a %a@]" ident_fmt id (value c)
            (Delayed.get v)
      | Class (id, _, cls) ->
          Format.fprintf ppf "@[<v 2>class %a %a@]" ident_fmt id (class_ c) cls
      | ClassType (id, _, cty) ->
          Format.fprintf ppf "@[<v 2>class type %a %a@]" ident_fmt id
            (class_type c) cty
      | Include i -> Format.fprintf ppf "@[<hov 2>include %a@]" (include_ c) i
      | Open o -> Format.fprintf ppf "open [ %a ]" (signature c) o.expansion
      | Comment _c -> ()
    in
    let rec inner ppf = function
      | [ x ] -> sig_item ppf x
      | x :: xs -> Format.fprintf ppf "%a@ %a" sig_item x inner xs
      | [] -> ()
    in
    let removed_fmt ppf removed =
      match (c.show_removed, removed) with
      | false, _ | _, [] -> ()
      | true, items ->
          Format.fprintf ppf "@ (removed=%a)" (removed_item_list c) items
    in
    Format.fprintf ppf "%a%a" inner sg.items removed_fmt sg.removed

  and option : type a.
      config ->
      (config -> Format.formatter -> a -> unit) ->
      Format.formatter ->
      a option ->
      unit =
   fun c pp ppf x ->
    match x with
    | Some x -> Format.fprintf ppf "Some(%a)" (pp c) x
    | None -> Format.fprintf ppf "None"

  and class_signature c ppf sg =
    let open ClassSignature in
    Format.fprintf ppf "@[<v>self=%a@," (option c type_expr) sg.self;
    List.iter
      (function
        | Method (id, m) ->
            Format.fprintf ppf "@[<v 2>method %a : %a@]@," Ident.fmt id
              (method_ c) m
        | InstanceVariable (id, i) ->
            Format.fprintf ppf "@[<v 2>instance variable %a : %a@]@," Ident.fmt
              id (instance_variable c) i
        | Constraint cst ->
            Format.fprintf ppf "@[<v 2>constraint %a = %a@]@," (type_expr c)
              cst.Constraint.left (type_expr c) cst.right
        | Inherit i ->
            Format.fprintf ppf "@[<v 2>inherit %a" (class_type_expr c)
              i.Inherit.expr
        | Comment _ -> ())
      sg.items

  and method_ c ppf m =
    let open Method in
    Format.fprintf ppf "%s%s%a"
      (if m.private_ then "private " else "")
      (if m.virtual_ then "virtual " else "")
      (type_expr c) m.type_

  and instance_variable c ppf i =
    let open InstanceVariable in
    Format.fprintf ppf "%s%s%a"
      (if i.mutable_ then "mutable " else "")
      (if i.virtual_ then "virtual " else "")
      (type_expr c) i.type_

  and list c pp ppf ls =
    match ls with
    | x :: y :: rest ->
        Format.fprintf ppf "%a, %a" (pp c) x (list c pp) (y :: rest)
    | [ x ] -> Format.fprintf ppf "%a" (pp c) x
    | [] -> ()

  and class_type_expr c ppf cty =
    let open ClassType in
    match cty with
    | Constr (p, ts) ->
        Format.fprintf ppf "constr(%a,%a)" (class_type_path c) p
          (list c type_expr) ts
    | Signature sg -> Format.fprintf ppf "(%a)" (class_signature c) sg

  and removed_item c ppf r =
    let open Signature in
    match r with
    | RModule (id, path) ->
        Format.fprintf ppf "module %a (%a)" ModuleName.fmt id (module_path c)
          path
    | RType (id, texpr, eq) ->
        Format.fprintf ppf "type %a %a = (%a)" type_params eq.params
          TypeName.fmt id (type_expr c) texpr
    | RModuleType (id, mty) ->
        Format.fprintf ppf "module type %a = %a" ModuleTypeName.fmt id
          (module_type_expr c) mty

  and removed_item_list c ppf r =
    match r with
    | [] -> ()
    | [ x ] -> Format.fprintf ppf "%a" (removed_item c) x
    | x :: ys ->
        Format.fprintf ppf "%a;%a" (removed_item c) x (removed_item_list c) ys

  and class_decl c ppf cls =
    let open Class in
    match cls with
    | ClassType cty -> Format.fprintf ppf "%a" (class_type_expr c) cty
    | Arrow (lbl, ty, decl) ->
        Format.fprintf ppf "%a%a -> %a" type_expr_label lbl (type_expr c) ty
          (class_decl c) decl

  and class_ c ppf cls = Format.fprintf ppf "%a" (class_decl c) cls.type_

  and class_type _c ppf _ = Format.fprintf ppf "<todo>"

  and include_ c ppf i =
    Format.fprintf ppf "%a@ %a" (include_decl c) i.decl
      (simple_expansion c true)
      (ModuleType.Signature i.expansion_ : ModuleType.simple_expansion)

  and include_decl c ppf =
    let open Include in
    function
    | Alias p -> Format.fprintf ppf "%a" (module_path c) p
    | ModuleType mt -> Format.fprintf ppf "%a" (u_module_type_expr c) mt

  and value c ppf v =
    let open Value in
    Format.fprintf ppf ": %a" (type_expr c) v.type_

  and module_decl c ppf d =
    let open Module in
    match d with
    | Alias (p, Some e) ->
        Format.fprintf ppf "=@ %a@ %a" (module_path c) p
          (simple_expansion c false) e
    | Alias (p, None) -> Format.fprintf ppf "=@ %a" (module_path c) p
    | ModuleType mt ->
        Format.fprintf ppf ": %a%a" (module_type_expr c) mt
          (module_type_expansion c) mt

  and module_ c ppf m =
    let fmt_canonical ppf popt =
      if c.show_canonical then
        Format.fprintf ppf "@ (canonical=%a)" (option c model_path) popt
      else ()
    in
    Format.fprintf ppf "%a%a" (module_decl c) m.type_ fmt_canonical
      (m.canonical :> path option)

  and simple_expansion c is_include ppf (m : ModuleType.simple_expansion) =
    if c.show_expansions || (is_include && c.show_include_expansions) then
      match m with
      | ModuleType.Signature sg ->
          Format.fprintf ppf "@[<hv 2>(sig :@ %a@;<1 -1>end@])" (signature c) sg
      | Functor (arg, sg) ->
          Format.fprintf ppf "(functor: (%a) -> %a)" (functor_parameter c) arg
            (simple_expansion c is_include)
            sg
    else ()

  and module_type c ppf mt =
    match mt.expr with
    | Some x ->
        Format.fprintf ppf "= %a%a" (module_type_expr c) x
          (module_type_expansion c) x
    | None -> ()

  and module_type_type_of_desc c ppf t =
    match t with
    | ModuleType.ModPath p ->
        Format.fprintf ppf "module type of %a" (module_path c) p
    | StructInclude p ->
        Format.fprintf ppf "module type of struct include %a end"
          (module_path c) p

  and u_module_type_expr c ppf mt =
    let open ModuleType.U in
    match mt with
    | Path p -> module_type_path c ppf p
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" (signature c) sg
    | With (subs, e) ->
        Format.fprintf ppf "%a with [%a]" (u_module_type_expr c) e
          (substitution_list c) subs
    | TypeOf (t_desc, _) -> module_type_type_of_desc c ppf t_desc
    | Strengthen (e, p, _) ->
        Format.fprintf ppf "%a with %a" (u_module_type_expr c) e (module_path c) p

  and module_type_expr c ppf mt =
    let open ModuleType in
    match mt with
    | Path { p_path; _ } -> module_type_path c ppf p_path
    | Signature sg ->
        Format.fprintf ppf "@,@[<hv 2>sig@ %a@;<1 -2>end@]" (signature c) sg
    | With { w_substitutions = subs; w_expr; _ } ->
        Format.fprintf ppf "%a with @[<hov 2>%a@]" (u_module_type_expr c) w_expr
          (substitution_list c) subs
    | Functor (arg, res) ->
        Format.fprintf ppf "(%a) -> %a" (functor_parameter c) arg
          (module_type_expr c) res
    | TypeOf { t_desc = ModPath p; _ } ->
        Format.fprintf ppf "module type of %a" (module_path c) p
    | TypeOf { t_desc = StructInclude p; _ } ->
        Format.fprintf ppf "module type of struct include %a end"
          (module_path c) p
    | Strengthen { s_expr; s_path; _ } ->
        Format.fprintf ppf "%a with %a" (u_module_type_expr c) s_expr
          (module_path c) s_path

  and module_type_expansion c ppf mt =
    let open ModuleType in
    match mt with
    | Signature _ -> ()
    | Path { p_expansion = Some e; _ }
    | With { w_expansion = Some e; _ }
    | TypeOf { t_expansion = Some e; _ } ->
        Format.fprintf ppf "@ %a" (simple_expansion c false) e
    | _ -> ()

  and functor_parameter c ppf x =
    let open FunctorParameter in
    match x with
    | Unit -> ()
    | Named x -> Format.fprintf ppf "%a" (functor_parameter_parameter c) x

  and functor_parameter_parameter c ppf x =
    Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorParameter.id
      (module_type_expr c) x.FunctorParameter.expr

  and type_decl c ppf t =
    let open TypeDecl in
    match t.representation with
    | Some repr ->
        Format.fprintf ppf "%a = %a"
          (fpp_opt c " : %a" type_expr)
          t.equation.Equation.manifest (type_decl_repr c) repr
    | None -> (fpp_opt c " = %a" type_expr) ppf t.equation.Equation.manifest

  and type_decl_repr c ppf =
    let open TypeDecl.Representation in
    function
    | Variant cs -> fpp_list " | " "%a" (type_decl_constructor c) ppf cs
    | Record fs -> type_decl_fields c ppf fs
    | Record_unboxed_product fs -> type_decl_unboxed_fields c ppf fs
    | Extensible -> Format.fprintf ppf ".."

  and type_decl_constructor c ppf t =
    let open TypeDecl.Constructor in
    match t.res with
    | Some res ->
        fpf ppf "%s : %a -> %a" t.name
          (type_decl_constructor_arg c)
          t.args (type_expr c) res
    | None -> fpf ppf "%s of %a" t.name (type_decl_constructor_arg c) t.args

  and type_decl_constructor_arg c ppf =
    let open TypeDecl.Constructor in
    function
    | Tuple ts -> type_constructor_params c ppf ts
    | Record fs -> type_decl_fields c ppf fs

  and type_decl_field c ppf t =
    let open TypeDecl.Field in
    let mutable_ = if t.mutable_ then "mutable " else "" in
    fpf ppf "%s%s : %a" mutable_ t.name (type_expr c) t.type_

  and type_decl_unboxed_field c ppf t =
    let open TypeDecl.UnboxedField in
    let mutable_ = if t.mutable_ then "mutable " else "" in
    fpf ppf "%s%s : %a" mutable_ t.name (type_expr c) t.type_

  and type_decl_fields c ppf fs =
    fpf ppf "{ %a }" (fpp_list "; " "%a" (type_decl_field c)) fs

  and type_decl_unboxed_fields c ppf fs =
    fpf ppf "#{ %a }" (fpp_list "; " "%a" (type_decl_unboxed_field c)) fs

  and type_constructor_params c ppf ts =
    fpp_list " * " "%a" (type_expr c) ppf ts

  and type_param ppf t =
    let desc =
      match t.Odoc_model.Lang.TypeDecl.desc with Any -> "_" | Var n -> n
    and variance =
      match t.variance with
      | Some Pos -> "+"
      | Some Neg -> "-"
      | Some Bivariant -> "+-"
      | None -> ""
    and injectivity = if t.injectivity then "!" else "" in
    Format.fprintf ppf "%s%s%s" variance injectivity desc

  and type_params ppf ts =
    let pp_sep ppf () = Format.fprintf ppf ", " in
    Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep type_param) ts

  and type_equation c ppf t =
    match t.TypeDecl.Equation.manifest with
    | None -> ()
    | Some m -> Format.fprintf ppf " = %a" (type_expr c) m

  and exception_ _c _ppf _e = ()

  and extension c ppf e =
    Format.fprintf ppf "%a" (type_path c) e.Extension.type_path

  and substitution c ppf t =
    let open ModuleType in
    match t with
    | ModuleEq (frag, decl) ->
        Format.fprintf ppf "%a %a" (module_fragment c) frag (module_decl c) decl
    | ModuleSubst (frag, mpath) ->
        Format.fprintf ppf "%a := %a" (module_fragment c) frag (module_path c)
          mpath
    | ModuleTypeEq (frag, mty) ->
        Format.fprintf ppf "%a = %a" (module_type_fragment c) frag
          (module_type_expr c) mty
    | ModuleTypeSubst (frag, mty) ->
        Format.fprintf ppf "%a := %a" (module_type_fragment c) frag
          (module_type_expr c) mty
    | TypeEq (frag, decl) ->
        Format.fprintf ppf "%a%a" (type_fragment c) frag (type_equation c) decl
    | TypeSubst (frag, decl) ->
        Format.fprintf ppf "%a%a" (type_fragment c) frag (type_equation c) decl

  and substitution_list c ppf l =
    match l with
    | [ sub ] -> Format.fprintf ppf "%a" (substitution c) sub
    | sub :: subs ->
        Format.fprintf ppf "%a; %a" (substitution c) sub (substitution_list c)
          subs
    | [] -> ()

  and type_expr_label ppf l =
    match l with
    | Some (Odoc_model.Lang.TypeExpr.Label l) -> Format.fprintf ppf "%s:" l
    | Some (RawOptional o) -> Format.fprintf ppf "?(%s):" o
    | Some (Optional o) -> Format.fprintf ppf "?%s:" o
    | None -> ()

  and type_expr_list c ppf l =
    match l with
    | [ t ] -> Format.fprintf ppf "%a" (type_expr c) t
    | t :: ts ->
        Format.fprintf ppf "%a * %a" (type_expr c) t (type_expr_list c) ts
    | [] -> ()

  and type_labeled_tuple c ppf l =
    match l with
    | [ t ] -> with_label c ppf t
    | t :: ts ->
        Format.fprintf ppf "%a * %a" (with_label c) t (type_labeled_tuple c) ts
    | [] -> ()

  and with_label c ppf (l, ty) =
    match l with
    | None -> type_expr c ppf ty
    | Some lbl -> Format.fprintf ppf "%s:%a" lbl (type_expr c) ty

  and type_object _c ppf _o = Format.fprintf ppf "(object)"

  and type_class c ppf (x, ys) =
    Format.fprintf ppf "(class %a %a)" (class_type_path c) x (type_expr_list c)
      ys

  and type_package _c ppf _p = Format.fprintf ppf "(package)"

  and type_expr_polymorphic_variant c ppf p =
    let open TypeExpr.Polymorphic_variant in
    let pp_element ppf = function
      | Type t -> type_expr c ppf t
      | Constructor cstr ->
          fpf ppf "`%s%a" cstr.Constructor.name
            (fpp_list " * " " of %a" (type_expr c))
            cstr.arguments
    in
    let pp_elements = fpp_list " | " "%a" pp_element in
    match p.kind with
    | Fixed -> fpf ppf "[ %a ]" pp_elements p.elements
    | Closed xs ->
        fpf ppf "[ %a > %a ]" pp_elements p.elements
          (fpp_list " " "%a" Format.pp_print_string)
          xs
    | Open -> fpf ppf "[> %a ]" pp_elements p.elements

  and type_expr c ppf e =
    let open TypeExpr in
    match e with
    | Var x -> Format.fprintf ppf "%s" x
    | Any -> Format.fprintf ppf "_"
    | Alias (x, y) -> Format.fprintf ppf "(alias %a %s)" (type_expr c) x y
    | Arrow (l, t1, t2) ->
        Format.fprintf ppf "%a(%a) -> %a" type_expr_label l (type_expr c) t1
          (type_expr c) t2
    | Tuple ts -> Format.fprintf ppf "(%a)" (type_labeled_tuple c) ts
    | Unboxed_tuple ts -> Format.fprintf ppf "#(%a)" (type_labeled_tuple c) ts
    | Constr (p, args) -> (
        match args with
        | [] -> Format.fprintf ppf "%a" (type_path c) p
        | _ ->
            Format.fprintf ppf "[%a] %a" (type_expr_list c) args (type_path c) p
        )
    | Polymorphic_variant poly ->
        Format.fprintf ppf "(poly_var %a)"
          (type_expr_polymorphic_variant c)
          poly
    | Object x -> type_object c ppf x
    | Class (x, y) -> type_class c ppf (x, y)
    | Poly (_ss, _t) -> Format.fprintf ppf "(poly)"
    | Quote t -> Format.fprintf ppf "(quote %a)" (type_expr c) t
    | Splice t -> Format.fprintf ppf "(splice %a)" (type_expr c) t
    | Package x -> type_package c ppf x

  and resolved_module_path :
      config -> Format.formatter -> Cpath.Resolved.module_ -> unit =
   fun c ppf p ->
    match p with
    | `Local ident -> ident_fmt c ppf ident
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" (resolved_module_path c) p1
          (resolved_module_path c) p2
    | `Gpath p -> Format.fprintf ppf "%a" (model_resolved_path c) (p :> rpath)
    | `Substituted p -> wrap c "substituted" resolved_module_path ppf p
    | `Module (p, m) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (ModuleName.to_string m)
    | `Alias (p1, p2, _) ->
        wrap2r c "alias" resolved_module_path module_path ppf p1 p2
    | `Subst (p1, p2) ->
        wrap2r c "subst" resolved_module_type_path resolved_module_path ppf p1
          p2
    | `Hidden p1 -> wrap c "hidden" resolved_module_path ppf p1
    | `Canonical (p1, p2) ->
        wrap2 c "canonical" resolved_module_path model_path ppf p1 (p2 :> path)
    | `OpaqueModule m -> wrap c "opaquemodule" resolved_module_path ppf m

  and module_path : config -> Format.formatter -> Cpath.module_ -> unit =
   fun c ppf p ->
    match p with
    | `Resolved p -> wrap c "resolved" resolved_module_path ppf p
    | `Dot (p, n) ->
        Format.fprintf ppf "%a.%a" (module_path c) p ModuleName.fmt n
    | `Module (p, n) ->
        Format.fprintf ppf "%a.%a" (resolved_parent_path c) p ModuleName.fmt n
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" (module_path c) p1 (module_path c) p2
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b
    | `Local (id, b) -> wrap2 c "local" ident_fmt bool ppf id b
    | `Substituted p -> wrap c "substituted" module_path ppf p
    | `Forward s -> wrap c "forward" str ppf s
    | `Root r -> wrap c "unresolvedroot" str ppf (ModuleName.to_string r)

  and resolved_module_type_path :
      config -> Format.formatter -> Cpath.Resolved.module_type -> unit =
   fun c ppf p ->
    match p with
    | `Local id -> ident_fmt c ppf id
    | `Gpath p -> model_resolved_path c ppf (p :> rpath)
    | `Substituted x -> wrap c "substituted" resolved_module_type_path ppf x
    | `ModuleType (p, m) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (ModuleTypeName.to_string m)
    | `CanonicalModuleType (m1, m2) ->
        wrap2 c "canonicalt" resolved_module_type_path model_path ppf m1
          (m2 :> path)
    | `OpaqueModuleType m ->
        wrap c "opaquemoduletype" resolved_module_type_path ppf m
    | `AliasModuleType (mt1, mt2) ->
        wrap2 c "aliasmoduletype" resolved_module_type_path
          resolved_module_type_path ppf mt1 mt2
    | `SubstT (mt1, mt2) ->
        wrap2 c "substt" resolved_module_type_path resolved_module_type_path ppf
          mt1 mt2

  and module_type_path : config -> Format.formatter -> Cpath.module_type -> unit
      =
   fun c ppf m ->
    match m with
    | `Resolved p -> wrap c "r" resolved_module_type_path ppf p
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b
    | `Local (id, b) -> wrap2 c "local" ident_fmt bool ppf id b
    | `Substituted s -> wrap c "substituted" module_type_path ppf s
    | `DotMT (m, s) ->
        Format.fprintf ppf "%a.%a" (module_path c) m ModuleTypeName.fmt s
    | `ModuleType (m, n) ->
        Format.fprintf ppf "%a.%a" (resolved_parent_path c) m ModuleTypeName.fmt
          n

  and resolved_type_path :
      config -> Format.formatter -> Cpath.Resolved.type_ -> unit =
   fun c ppf p ->
    match p with
    | `CoreType n -> Format.fprintf ppf "%s" (TypeName.to_string n)
    | `Local id -> ident_fmt c ppf id
    | `Gpath p -> model_resolved_path c ppf (p :> rpath)
    | `Substituted x -> wrap c "substituted" resolved_type_path ppf x
    | `CanonicalType (t1, t2) ->
        wrap2 c "canonicaltype" resolved_type_path model_path ppf t1
          (t2 :> path)
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `Type (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)

  and resolved_value_path :
      config -> Format.formatter -> Cpath.Resolved.value -> unit =
   fun c ppf p ->
    match p with
    | `Value (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (ValueName.to_string t)
    | `Gpath p -> Format.fprintf ppf "%a" (model_resolved_path c) (p :> rpath)

  and resolved_parent_path :
      config -> Format.formatter -> Cpath.Resolved.parent -> unit =
   fun c ppf p ->
    match p with
    | `Module m -> resolved_module_path c ppf m
    | `ModuleType m ->
        if c.short_paths then resolved_module_type_path c ppf m
        else Format.fprintf ppf ">>%a<<" (resolved_module_type_path c) m
    | `FragmentRoot -> Format.fprintf ppf "FragmentRoot"

  and type_path : config -> Format.formatter -> Cpath.type_ -> unit =
   fun c ppf p ->
    match p with
    | `Resolved r -> wrap c "resolved" resolved_type_path ppf r
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b
    | `Local (id, b) -> wrap2 c "local" ident_fmt bool ppf id b
    | `Substituted s -> wrap c "substituted" type_path ppf s
    | `DotT (m, s) ->
        Format.fprintf ppf "%a.%a" (module_path c) m TypeName.fmt s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `Type (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)

  and value_path : config -> Format.formatter -> Cpath.value -> unit =
   fun c ppf p ->
    match p with
    | `Resolved r -> wrap c "resolved" resolved_value_path ppf r
    | `DotV (m, s) ->
        Format.fprintf ppf "%a.%a" (module_path c) m ValueName.fmt s
    | `Value (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (ValueName.to_string t)
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b

  and resolved_class_type_path :
      config -> Format.formatter -> Cpath.Resolved.class_type -> unit =
   fun c ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Gpath p -> Format.fprintf ppf "%a" (model_resolved_path c) (p :> rpath)
    | `Substituted s -> wrap c "substituted" resolved_class_type_path ppf s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)

  and class_type_path : config -> Format.formatter -> Cpath.class_type -> unit =
   fun c ppf p ->
    match p with
    | `Resolved r -> Format.fprintf ppf "%a" (resolved_class_type_path c) r
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b
    | `Local (id, b) -> wrap2 c "local" ident_fmt bool ppf id b
    | `Substituted s -> wrap c "substituted" class_type_path ppf s
    | `DotT (m, s) ->
        Format.fprintf ppf "%a.%a" (module_path c) m TypeName.fmt s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" (resolved_parent_path c) p
          (TypeName.to_string t)

  and model_path : config -> Format.formatter -> path -> unit =
   fun c ppf (p : path) ->
    let dot p s =
      Format.fprintf ppf "%a.%s" (model_path c)
        (p : Odoc_model.Paths.Path.Module.t :> path)
        s
    in

    match p with
    | `Resolved rp -> wrap c "resolved" model_resolved_path ppf rp
    | `Identifier (id, b) ->
        wrap2 c "identifier" model_identifier bool ppf (id :> id) b
    | `Root s -> wrap c "root" str ppf (ModuleName.to_string s)
    | `Forward s -> wrap c "forward" str ppf s
    | `Dot (p, s) -> dot p (ModuleName.to_string s)
    | `DotMT (p, s) -> dot p (ModuleTypeName.to_string s)
    | `DotT (p, s) -> dot p (TypeName.to_string s)
    | `DotV (p, s) -> dot p (ValueName.to_string s)
    | `Apply (func, arg) ->
        Format.fprintf ppf "%a(%a)" (model_path c)
          (func :> path)
          (model_path c)
          (arg :> path)
    | `Substituted m ->
        wrap c "substituted" model_path ppf (m :> Odoc_model.Paths.Path.t)
    | `SubstitutedMT m ->
        wrap c "substitutedmt" model_path ppf (m :> Odoc_model.Paths.Path.t)
    | `SubstitutedT m ->
        wrap c "substitutedt" model_path ppf (m :> Odoc_model.Paths.Path.t)
    | `SubstitutedCT m ->
        wrap c "substitutedct" model_path ppf (m :> Odoc_model.Paths.Path.t)

  and model_resolved_path (c : config) ppf (p : rpath) =
    let open Odoc_model.Paths.Path.Resolved in
    match p with
    | `CoreType x -> Format.fprintf ppf "%s" (TypeName.to_string x)
    | `Identifier id -> Format.fprintf ppf "%a" (model_identifier c) (id :> id)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (TypeName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (ValueName.to_string name)
    | `Alias (dest, src) ->
        wrap2r c "alias" model_resolved_path model_path ppf
          (dest :> t)
          (src :> path)
    | `AliasModuleType (path, realpath) ->
        wrap2r c "aliasmoduletype" model_resolved_path model_resolved_path ppf
          (path :> t)
          (realpath :> t)
    | `Subst (modty, m) ->
        wrap2 c "subst" model_resolved_path model_resolved_path ppf
          (modty :> t)
          (m :> t)
    | `SubstT (t1, t2) ->
        wrap2 c "substt" model_resolved_path model_resolved_path ppf
          (t1 :> t)
          (t2 :> t)
    | `CanonicalModuleType (t1, t2) ->
        wrap2 c "canonicalmoduletype" model_resolved_path model_path ppf
          (t1 :> t)
          (t2 :> path)
    | `CanonicalType (t1, t2) ->
        wrap2 c "canonicaltype" model_resolved_path model_path ppf
          (t1 :> t)
          (t2 :> path)
    | `Apply (funct, arg) ->
        Format.fprintf ppf "%a(%a)" (model_resolved_path c)
          (funct :> t)
          (model_resolved_path c)
          (arg :> t)
    | `Canonical (p1, p2) ->
        wrap2 c "canonical" model_resolved_path model_path ppf
          (p1 :> t)
          (p2 :> path)
    | `Hidden p -> wrap c "hidden" model_resolved_path ppf (p :> t)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (TypeName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_resolved_path c)
          (parent :> t)
          (TypeName.to_string name)
    | `OpaqueModule m -> wrap c "opaquemodule" model_resolved_path ppf (m :> t)
    | `OpaqueModuleType m ->
        wrap c "opaquemoduletype" model_resolved_path ppf (m :> t)
    | `Substituted m -> wrap c "substituted" model_resolved_path ppf (m :> t)
    | `SubstitutedMT m ->
        wrap c "substitutedmt" model_resolved_path ppf (m :> t)
    | `SubstitutedT m -> wrap c "substitutedt" model_resolved_path ppf (m :> t)
    | `SubstitutedCT m ->
        wrap c "substitutedct" model_resolved_path ppf (m :> t)

  and model_fragment c ppf (f : Odoc_model.Paths.Fragment.t) =
    match f with
    | `Resolved rf -> model_resolved_fragment c ppf rf
    | `Dot (sg, d) ->
        Format.fprintf ppf "*%a.%s" (model_fragment c)
          (sg :> Odoc_model.Paths.Fragment.t)
          d
    | `Root -> ()

  and model_resolved_fragment c ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
    let open Odoc_model.Paths.Fragment.Resolved in
    match f with
    | `Root (`ModuleType p) ->
        Format.fprintf ppf "root(%a)" (model_resolved_path c) (p :> rpath)
    | `Root (`Module p) ->
        Format.fprintf ppf "root(%a)" (model_resolved_path c) (p :> rpath)
    | `Module (`Root _, m) when c.short_paths ->
        Format.fprintf ppf "%s" (ModuleName.to_string m)
    | `Module (sg, m) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_fragment c)
          (sg :> t)
          (ModuleName.to_string m)
    | `Module_type (`Root _, m) when c.short_paths ->
        Format.fprintf ppf "%s" (ModuleTypeName.to_string m)
    | `Module_type (sg, mty) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_fragment c)
          (sg :> t)
          (ModuleTypeName.to_string mty)
    | `Type (`Root _, t) when c.short_paths ->
        Format.fprintf ppf "%s" (TypeName.to_string t)
    | `Type (sg, t) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_fragment c)
          (sg :> t)
          (TypeName.to_string t)
    | `Subst (path, m) ->
        Format.fprintf ppf "(%a subst -> %a)" (model_resolved_path c)
          (path :> rpath)
          (model_resolved_fragment c)
          (m :> t)
    | `Alias (_, _) -> Format.fprintf ppf "UNIMPLEMENTED subst alias!?"
    | `Class (sg, cls) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_fragment c)
          (sg :> t)
          (TypeName.to_string cls)
    | `ClassType (sg, cls) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_fragment c)
          (sg :> t)
          (TypeName.to_string cls)
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)"
          (model_resolved_fragment c)
          (m :> Odoc_model.Paths.Fragment.Resolved.t)

  and resolved_root_fragment c ppf (f : Cfrag.root) =
    match f with
    | `ModuleType p ->
        Format.fprintf ppf "root(%a)" (resolved_module_type_path c) p
    | `Module p -> Format.fprintf ppf "root(%a)" (resolved_module_path c) p

  and resolved_signature_fragment c ppf (f : Cfrag.resolved_signature) =
    match f with
    | `Root r -> Format.fprintf ppf "%a" (resolved_root_fragment c) r
    | (`Subst _ | `Alias _ | `Module _) as x -> resolved_module_fragment c ppf x
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" (resolved_module_fragment c) m

  and resolved_module_fragment c ppf (f : Cfrag.resolved_module) =
    match f with
    | `Subst (s, f) ->
        wrap2r c "subst" resolved_module_type_path resolved_module_fragment ppf
          s f
    | `Alias (m, f) ->
        wrap2r c "alias" resolved_module_path resolved_module_fragment ppf m f
    | `Module (`Root _, n) when c.short_paths ->
        Format.fprintf ppf "%s" (ModuleName.to_string n)
    | `Module (p, n) ->
        Format.fprintf ppf "%a.%s"
          (resolved_signature_fragment c)
          p (ModuleName.to_string n)
    | `OpaqueModule m -> wrap c "opaquemodule" resolved_module_fragment ppf m

  and resolved_module_type_fragment c ppf (f : Cfrag.resolved_module_type) =
    match f with
    | `ModuleType (`Root _, n) when c.short_paths ->
        Format.fprintf ppf "%s" (ModuleTypeName.to_string n)
    | `ModuleType (p, n) ->
        Format.fprintf ppf "%a.%s"
          (resolved_signature_fragment c)
          p
          (ModuleTypeName.to_string n)

  and resolved_type_fragment c ppf (f : Cfrag.resolved_type) =
    match f with
    | `Type (`Root _, n) when c.short_paths ->
        Format.fprintf ppf "%s" (TypeName.to_string n)
    | `Class (`Root _, n) when c.short_paths ->
        Format.fprintf ppf "%s" (TypeName.to_string n)
    | `ClassType (`Root _, n) when c.short_paths ->
        Format.fprintf ppf "%s" (TypeName.to_string n)
    | `Type (s, n) ->
        Format.fprintf ppf "%a.%s"
          (resolved_signature_fragment c)
          s (TypeName.to_string n)
    | `Class (s, n) ->
        Format.fprintf ppf "%a.%s"
          (resolved_signature_fragment c)
          s (TypeName.to_string n)
    | `ClassType (s, n) ->
        Format.fprintf ppf "%a.%s"
          (resolved_signature_fragment c)
          s (TypeName.to_string n)

  and signature_fragment c ppf (f : Cfrag.signature) =
    match f with
    | `Resolved r ->
        Format.fprintf ppf "r(%a)" (resolved_signature_fragment c) r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" (signature_fragment c) s n
    | `Root -> Format.fprintf ppf "root"

  and module_fragment c ppf (f : Cfrag.module_) =
    match f with
    | `Resolved r -> wrap c "resolved" resolved_module_fragment ppf r
    | `Dot (`Root, n) when c.short_paths -> Format.fprintf ppf "%s" n
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" (signature_fragment c) s n

  and module_type_fragment c ppf (f : Cfrag.module_type) =
    match f with
    | `Resolved r -> wrap c "resolved" resolved_module_type_fragment ppf r
    | `Dot (`Root, n) when c.short_paths -> Format.fprintf ppf "%s" n
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" (signature_fragment c) s n

  and type_fragment c ppf (f : Cfrag.type_) =
    match f with
    | `Resolved r -> wrap c "resolved" resolved_type_fragment ppf r
    | `Dot (`Root, n) when c.short_paths -> Format.fprintf ppf "%s" n
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" (signature_fragment c) s n

  and model_resolved_reference c ppf (r : Odoc_model.Paths.Reference.Resolved.t)
      =
    let open Odoc_model.Paths.Reference.Resolved in
    match r with
    | `Identifier id -> Format.fprintf ppf "%a" (model_identifier c) id
    | `Hidden p ->
        Format.fprintf ppf "hidden(%a)" (model_resolved_reference c) (p :> t)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ConstructorName.to_string name)
    | `PolyConstructor (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (FieldName.to_string name)
    | `UnboxedField (parent, name) ->
        Format.fprintf ppf "%a.#%s"
          (model_resolved_reference c)
          (parent :> t)
          (UnboxedFieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ExtensionName.to_string name)
    | `ExtensionDecl (parent, name, _) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (InstanceVariableName.to_string name)
    | `Alias (x, y) ->
        Format.fprintf ppf "alias(%a,%a)" (model_resolved_path c)
          (x :> rpath)
          (model_resolved_reference c)
          (y :> Odoc_model.Paths.Reference.Resolved.t)
    | `AliasModuleType (x, y) ->
        Format.fprintf ppf "aliasmoduletype(%a,%a)" (model_resolved_path c)
          (x :> rpath)
          (model_resolved_reference c)
          (y :> Odoc_model.Paths.Reference.Resolved.t)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s"
          (model_resolved_reference c)
          (parent :> t)
          (LabelName.to_string name)

  and model_reference_hierarchy _c ppf
      ((tag, components) : Reference.Hierarchy.t) =
    (match tag with
    | `TRelativePath -> fpf ppf "./"
    | `TAbsolutePath -> fpf ppf "/"
    | `TCurrentPackage -> fpf ppf "//");
    let pp_sep ppf () = fpf ppf "/" in
    Format.pp_print_list ~pp_sep Format.pp_print_string ppf components

  and model_reference c ppf (r : Reference.t) =
    let open Reference in
    match r with
    | `Resolved r' -> Format.fprintf ppf "r(%a)" (model_resolved_reference c) r'
    | `Root (name, _) -> Format.fprintf ppf "unresolvedroot(%s)" name
    | `Dot (parent, str) ->
        Format.fprintf ppf "%a.%s" (model_reference c) (parent :> t) str
    | `Page_path p -> model_reference_hierarchy c ppf p
    | `Asset_path p -> model_reference_hierarchy c ppf p
    | `Module_path p -> model_reference_hierarchy c ppf p
    | `Any_path p -> model_reference_hierarchy c ppf p
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (FieldName.to_string name)
    | `UnboxedField (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (UnboxedFieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ExtensionName.to_string name)
    | `ExtensionDecl (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (TypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (InstanceVariableName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" (model_reference c)
          (parent :> t)
          (LabelName.to_string name)
end

module LocalIdents = struct
  open Odoc_model
  (** The purpose of this module is to extract identifiers that could be
      referenced in Paths - that is, modules, module types, types, classes and
      class types. That way we can assign them an Ident.t ahead of time and be
      self-consistent. Because we don't need _all_ of the identifiers we don't
      traverse the entire structure. Additionally, we stop at (class_)signature
      boundaries since identifiers within these won't be referenced except
      within them, so we only do that on demand. *)

  type t = {
    modules : Paths.Identifier.Module.t list;
    module_types : Paths.Identifier.ModuleType.t list;
    types : Paths.Identifier.Type.t list;
    classes : Paths.Identifier.Class.t list;
    class_types : Paths.Identifier.ClassType.t list;
  }

  let empty =
    {
      modules = [];
      module_types = [];
      types = [];
      classes = [];
      class_types = [];
    }

  open Lang

  let rec signature_items s ids =
    let open Signature in
    List.fold_left
      (fun ids c ->
        match c with
        | Module (_, { Module.id; _ }) ->
            { ids with modules = id :: ids.modules }
        | ModuleType m ->
            { ids with module_types = m.ModuleType.id :: ids.module_types }
        | ModuleSubstitution { ModuleSubstitution.id; _ } ->
            { ids with modules = id :: ids.modules }
        | ModuleTypeSubstitution { ModuleTypeSubstitution.id; _ } ->
            { ids with module_types = id :: ids.module_types }
        | Type (_, t) -> { ids with types = t.TypeDecl.id :: ids.types }
        | TypeSubstitution t -> { ids with types = t.TypeDecl.id :: ids.types }
        | Class (_, c) -> { ids with classes = c.Class.id :: ids.classes }
        | ClassType (_, c) ->
            { ids with class_types = c.ClassType.id :: ids.class_types }
        | TypExt _ | Exception _ | Value _ | Comment _ -> ids
        | Include i -> signature i.Include.expansion.content ids
        | Open o -> signature o.Open.expansion ids)
      ids s

  and signature s ids = signature_items s.items ids
end

module Of_Lang = struct
  open Odoc_model

  type map = {
    modules : Ident.module_ Paths.Identifier.Maps.Module.t;
    module_types : Ident.module_type Paths.Identifier.Maps.ModuleType.t;
    functor_parameters : Ident.module_ Paths.Identifier.Maps.FunctorParameter.t;
    types : Ident.type_ Paths.Identifier.Maps.Type.t;
    path_types : Ident.type_ Paths.Identifier.Maps.Path.Type.t;
    path_class_types : Ident.type_ Paths.Identifier.Maps.Path.ClassType.t;
    classes : Ident.type_ Paths.Identifier.Maps.Class.t;
    class_types : Ident.type_ Paths.Identifier.Maps.ClassType.t;
  }

  let empty () =
    let open Paths.Identifier.Maps in
    {
      modules = Module.empty;
      module_types = ModuleType.empty;
      functor_parameters = FunctorParameter.empty;
      types = Type.empty;
      path_types = Path.Type.empty;
      path_class_types = Path.ClassType.empty;
      classes = Class.empty;
      class_types = ClassType.empty;
    }

  let map_of_idents ids map =
    let open Paths.Identifier in
    (* New types go into [types_new] and [path_types_new]
       New classes go into [classes_new] and [path_class_types_new]
       New class_types go into [class_types_new], [path_types_new] and [path_class_types_new] *)
    let types_new, path_types_new =
      List.fold_left
        (fun (types, path_types) i ->
          let id = Ident.Of_Identifier.type_ i in
          ( Maps.Type.add i id types,
            Maps.Path.Type.add (i :> Path.Type.t) id path_types ))
        (map.types, map.path_types)
        ids.LocalIdents.types
    in
    let classes_new, path_class_types_new =
      List.fold_left
        (fun (classes, path_class_types) i ->
          let id = Ident.Of_Identifier.class_ i in
          ( Maps.Class.add i id classes,
            Maps.Path.ClassType.add (i :> Path.ClassType.t) id path_class_types
          ))
        (map.classes, map.path_class_types)
        ids.LocalIdents.classes
    in
    let class_types_new, path_types_new, path_class_types_new =
      List.fold_left
        (fun (class_types, path_types, path_class_types) i ->
          let id = Ident.Of_Identifier.class_type i in
          ( Maps.ClassType.add i id class_types,
            Maps.Path.Type.add (i :> Path.Type.t) id path_types,
            Maps.Path.ClassType.add (i :> Path.ClassType.t) id path_class_types
          ))
        (map.class_types, path_types_new, path_class_types_new)
        ids.LocalIdents.class_types
    in
    let modules_new =
      List.fold_left
        (fun acc i ->
          Maps.Module.add (i :> Module.t) (Ident.Of_Identifier.module_ i) acc)
        map.modules ids.LocalIdents.modules
    in
    let module_types_new =
      List.fold_left
        (fun acc i ->
          Maps.ModuleType.add i (Ident.Of_Identifier.module_type i) acc)
        map.module_types ids.LocalIdents.module_types
    in
    let modules = modules_new in
    let module_types = module_types_new in
    let functor_parameters = map.functor_parameters in
    let types = types_new in
    let classes = classes_new in
    let class_types = class_types_new in
    let path_types = path_types_new in
    let path_class_types = path_class_types_new in
    {
      modules;
      module_types;
      functor_parameters;
      types;
      classes;
      class_types;
      path_types;
      path_class_types;
    }

  let option conv ident_map x =
    match x with None -> None | Some x' -> Some (conv ident_map x')

  let identifier lookup map i =
    match lookup i map with
    | x -> `Local x
    | exception Not_found -> `Identifier i

  let find_any_module i ident_map =
    match i with
    | { Odoc_model.Paths.Identifier.iv = `Root _ | `Module _; _ } as id ->
        Maps.Module.find id ident_map.modules
    | {
        Odoc_model.Paths.Identifier.iv = #Paths.Identifier.FunctorParameter.t_pv;
        _;
      } as id ->
        Maps.FunctorParameter.find id ident_map.functor_parameters
    | _ -> raise Not_found

  let rec resolved_module_path :
      _ -> Odoc_model.Paths.Path.Resolved.Module.t -> Cpath.Resolved.module_ =
   fun ident_map p ->
    let recurse = resolved_module_path ident_map in
    match p with
    | `Identifier i -> (
        match identifier find_any_module ident_map i with
        | `Local l -> `Local l
        | `Identifier _ -> `Gpath p)
    | `Module (p, name) -> `Module (`Module (recurse p), name)
    | `Apply (p1, p2) -> `Apply (recurse p1, recurse p2)
    | `Alias (p1, p2) -> `Alias (recurse p1, module_path ident_map p2, None)
    | `Subst (p1, p2) ->
        `Subst (resolved_module_type_path ident_map p1, recurse p2)
    | `Canonical (p1, p2) -> `Canonical (recurse p1, p2)
    | `Hidden p1 -> `Hidden (recurse p1)
    | `OpaqueModule m -> `OpaqueModule (recurse m)
    | `Substituted m -> `Substituted (recurse m)

  and resolved_module_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ModuleType.t ->
      Cpath.Resolved.module_type =
   fun ident_map p ->
    match p with
    | `Identifier i -> (
        match identifier Maps.ModuleType.find ident_map.module_types i with
        | `Local l -> `Local l
        | `Identifier _ -> `Gpath p)
    | `ModuleType (p, name) ->
        `ModuleType (`Module (resolved_module_path ident_map p), name)
    | `CanonicalModuleType (p1, p2) ->
        `CanonicalModuleType (resolved_module_type_path ident_map p1, p2)
    | `OpaqueModuleType m ->
        `OpaqueModuleType (resolved_module_type_path ident_map m)
    | `AliasModuleType (m1, m2) ->
        `AliasModuleType
          ( resolved_module_type_path ident_map m1,
            resolved_module_type_path ident_map m2 )
    | `SubstT (p1, p2) ->
        `SubstT
          ( resolved_module_type_path ident_map p1,
            resolved_module_type_path ident_map p2 )
    | `SubstitutedMT m -> `Substituted (resolved_module_type_path ident_map m)

  and resolved_type_path :
      _ -> Odoc_model.Paths.Path.Resolved.Type.t -> Cpath.Resolved.type_ =
   fun ident_map p ->
    match p with
    | `CoreType _ as c -> c
    | `Identifier i -> (
        match identifier Maps.Path.Type.find ident_map.path_types i with
        | `Local l -> `Local l
        | `Identifier _ -> `Gpath p)
    | `CanonicalType (p1, p2) ->
        `CanonicalType (resolved_type_path ident_map p1, p2)
    | `Type (p, name) -> `Type (`Module (resolved_module_path ident_map p), name)
    | `Class (p, name) ->
        `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) ->
        `ClassType (`Module (resolved_module_path ident_map p), name)
    | `SubstitutedT m -> `Substituted (resolved_type_path ident_map m)
    | `SubstitutedCT m ->
        `Substituted
          (resolved_class_type_path ident_map m :> Cpath.Resolved.type_)

  and resolved_value_path :
      _ -> Odoc_model.Paths.Path.Resolved.Value.t -> Cpath.Resolved.value =
   fun ident_map p ->
    match p with
    | `Value (p, name) ->
        `Value (`Module (resolved_module_path ident_map p), name)
    | `Identifier _ -> `Gpath p

  and resolved_class_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ClassType.t ->
      Cpath.Resolved.class_type =
   fun ident_map p ->
    match p with
    | `Identifier i -> (
        match
          identifier Maps.Path.ClassType.find ident_map.path_class_types i
        with
        | `Local l -> `Local l
        | `Identifier _ -> `Gpath p)
    | `Class (p, name) ->
        `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) ->
        `ClassType (`Module (resolved_module_path ident_map p), name)
    | `SubstitutedCT c -> `Substituted (resolved_class_type_path ident_map c)

  and module_path : _ -> Odoc_model.Paths.Path.Module.t -> Cpath.module_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_path ident_map r)
    | `Substituted m -> `Substituted (module_path ident_map m)
    | `Identifier (i, b) -> (
        match identifier find_any_module ident_map i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)
    | `Apply (p1, p2) ->
        `Apply (module_path ident_map p1, module_path ident_map p2)
    | `Forward str -> `Forward str
    | `Root str -> `Root str

  and module_type_path :
      _ -> Odoc_model.Paths.Path.ModuleType.t -> Cpath.module_type =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_type_path ident_map r)
    | `SubstitutedMT m -> `Substituted (module_type_path ident_map m)
    | `Identifier (i, b) -> (
        match identifier Maps.ModuleType.find ident_map.module_types i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `DotMT (path', x) -> `DotMT (module_path ident_map path', x)

  and type_path : _ -> Odoc_model.Paths.Path.Type.t -> Cpath.type_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_type_path ident_map r)
    | `SubstitutedT t -> `Substituted (type_path ident_map t)
    | `Identifier (i, b) -> (
        match identifier Maps.Path.Type.find ident_map.path_types i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `DotT (path', x) -> `DotT (module_path ident_map path', x)

  and value_path : _ -> Odoc_model.Paths.Path.Value.t -> Cpath.value =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_value_path ident_map r)
    | `DotV (path', x) -> `DotV (module_path ident_map path', x)
    | `Identifier (i, b) -> `Identifier (i, b)

  and class_type_path :
      _ -> Odoc_model.Paths.Path.ClassType.t -> Cpath.class_type =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_class_type_path ident_map r)
    | `SubstitutedCT c -> `Substituted (class_type_path ident_map c)
    | `Identifier (i, b) -> (
        match
          identifier Maps.Path.ClassType.find ident_map.path_class_types i
        with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `DotT (path', x) -> `DotT (module_path ident_map path', x)

  let rec resolved_signature_fragment :
      map ->
      Odoc_model.Paths.Fragment.Resolved.Signature.t ->
      Cfrag.resolved_signature =
   fun ident_map ty ->
    match ty with
    | `Root (`ModuleType path) ->
        `Root (`ModuleType (resolved_module_type_path ident_map path))
    | `Root (`Module path) ->
        `Root (`Module (resolved_module_path ident_map path))
    | (`Alias _ | `Subst _ | `Module _ | `OpaqueModule _) as x ->
        (resolved_module_fragment ident_map x :> Cfrag.resolved_signature)

  and resolved_module_fragment :
      _ -> Odoc_model.Paths.Fragment.Resolved.Module.t -> Cfrag.resolved_module
      =
   fun ident_map ty ->
    match ty with
    | `Subst (p, m) ->
        `Subst
          ( resolved_module_type_path ident_map p,
            resolved_module_fragment ident_map m )
    | `Alias (p, m) ->
        `Alias
          ( resolved_module_path ident_map p,
            resolved_module_fragment ident_map m )
    | `Module (p, m) -> `Module (resolved_signature_fragment ident_map p, m)
    | `OpaqueModule m -> `OpaqueModule (resolved_module_fragment ident_map m)

  and resolved_module_type_fragment :
      _ ->
      Odoc_model.Paths.Fragment.Resolved.ModuleType.t ->
      Cfrag.resolved_module_type =
   fun ident_map ty ->
    match ty with
    | `Module_type (p, m) ->
        `ModuleType (resolved_signature_fragment ident_map p, m)

  and resolved_type_fragment :
      _ -> Odoc_model.Paths.Fragment.Resolved.Type.t -> Cfrag.resolved_type =
   fun ident_map ty ->
    match ty with
    | `Type (p, n) -> `Type (resolved_signature_fragment ident_map p, n)
    | `Class (p, n) -> `Class (resolved_signature_fragment ident_map p, n)
    | `ClassType (p, n) ->
        `ClassType (resolved_signature_fragment ident_map p, n)

  let rec signature_fragment :
      _ -> Odoc_model.Paths.Fragment.Signature.t -> Cfrag.signature =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_signature_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)
    | `Root -> `Root

  let module_fragment : _ -> Odoc_model.Paths.Fragment.Module.t -> Cfrag.module_
      =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_module_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let module_type_fragment :
      _ -> Odoc_model.Paths.Fragment.ModuleType.t -> Cfrag.module_type =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_module_type_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let type_fragment : _ -> Odoc_model.Paths.Fragment.Type.t -> Cfrag.type_ =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_type_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let rec type_decl ident_map ty =
    let open Odoc_model.Lang.TypeDecl in
    {
      TypeDecl.source_loc = ty.source_loc;
      source_loc_jane = ty.source_loc_jane;
      doc = docs ident_map ty.doc;
      canonical = ty.canonical;
      equation = type_equation ident_map ty.equation;
      representation =
        Opt.map (type_decl_representation ident_map) ty.representation;
    }

  and type_decl_representation ident_map r =
    let open Odoc_model.Lang.TypeDecl.Representation in
    match r with
    | Variant cs ->
        TypeDecl.Representation.Variant
          (List.map (type_decl_constructor ident_map) cs)
    | Record fs -> Record (List.map (type_decl_field ident_map) fs)
    | Record_unboxed_product fs ->
      Record_unboxed_product (List.map (type_decl_unboxed_field ident_map) fs)
    | Extensible -> Extensible

  and type_decl_constructor ident_map t =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    let args = type_decl_constructor_argument ident_map t.args in
    let res = Opt.map (type_expression ident_map) t.res in
    {
      TypeDecl.Constructor.name = Paths.Identifier.name t.id;
      doc = docs ident_map t.doc;
      args;
      res;
    }

  and type_decl_constructor_argument ident_map a =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    match a with
    | Tuple ts ->
        TypeDecl.Constructor.Tuple (List.map (type_expression ident_map) ts)
    | Record fs -> Record (List.map (type_decl_field ident_map) fs)

  and type_decl_field ident_map f =
    let open Odoc_model.Lang.TypeDecl.Field in
    let type_ = type_expression ident_map f.type_ in
    {
      TypeDecl.Field.name = Paths.Identifier.name f.id;
      doc = docs ident_map f.doc;
      mutable_ = f.mutable_;
      type_;
    }

  and type_decl_unboxed_field ident_map f =
    let type_ = type_expression ident_map f.type_ in
    {
      TypeDecl.UnboxedField.name = Paths.Identifier.name f.id;
      doc = docs ident_map f.doc;
      mutable_ = f.mutable_;
      type_;
    }

  and type_equation ident_map teq =
    let open Odoc_model.Lang.TypeDecl.Equation in
    {
      TypeDecl.Equation.params = teq.params;
      private_ = teq.private_;
      manifest = option type_expression ident_map teq.manifest;
      constraints =
        List.map
          (fun (x, y) ->
            (type_expression ident_map x, type_expression ident_map y))
          teq.constraints;
    }

  and type_expr_polyvar ident_map v =
    let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
    let map_element = function
      | Type expr ->
          TypeExpr.Polymorphic_variant.Type (type_expression ident_map expr)
      | Constructor c ->
          Constructor
            TypeExpr.Polymorphic_variant.Constructor.
              {
                name = c.name;
                constant = c.constant;
                arguments = List.map (type_expression ident_map) c.arguments;
                doc = docs ident_map c.doc;
              }
    in
    {
      TypeExpr.Polymorphic_variant.kind = v.kind;
      elements = List.map map_element v.elements;
    }

  and type_object ident_map o =
    let open Odoc_model.Lang.TypeExpr.Object in
    let map_field = function
      | Method m ->
          TypeExpr.(
            Object.Method
              {
                Object.name = m.name;
                type_ = type_expression ident_map m.type_;
              })
      | Inherit i -> Inherit (type_expression ident_map i)
    in
    { TypeExpr.Object.open_ = o.open_; fields = List.map map_field o.fields }

  and type_package ident_map pkg =
    let open Odoc_model.Lang.TypeExpr.Package in
    {
      TypeExpr.Package.path = module_type_path ident_map pkg.path;
      substitutions =
        List.map
          (fun (x, y) ->
            let f = type_fragment ident_map x in
            (f, type_expression ident_map y))
          pkg.substitutions;
    }

  and type_expression ident_map expr =
    let open Odoc_model.Lang.TypeExpr in
    match expr with
    | Var s -> TypeExpr.Var s
    | Any -> Any
    | Constr (p, xs) ->
        Constr (type_path ident_map p, List.map (type_expression ident_map) xs)
    | Arrow (lbl, t1, t2) ->
        Arrow (lbl, type_expression ident_map t1, type_expression ident_map t2)
    | Tuple ts ->
        Tuple
          (List.map (fun (lbl, ty) -> (lbl, type_expression ident_map ty)) ts)
    | Unboxed_tuple ts ->
        Unboxed_tuple (List.map (fun (l, t) -> l, type_expression ident_map t) ts)
    | Polymorphic_variant v ->
        Polymorphic_variant (type_expr_polyvar ident_map v)
    | Poly (s, ts) -> Poly (s, type_expression ident_map ts)
    | Alias (t, s) -> Alias (type_expression ident_map t, s)
    | Class (p, ts) ->
        Class
          (class_type_path ident_map p, List.map (type_expression ident_map) ts)
    | Object o -> Object (type_object ident_map o)
    | Quote t -> Quote (type_expression ident_map t)
    | Splice t -> Splice (type_expression ident_map t)
    | Package p -> Package (type_package ident_map p)

  and module_decl ident_map m =
    match m with
    | Lang.Module.Alias (p, e) ->
        Module.Alias
          (module_path ident_map p, option simple_expansion ident_map e)
    | Lang.Module.ModuleType s ->
        Module.ModuleType (module_type_expr ident_map s)

  and include_decl ident_map m =
    match m with
    | Odoc_model.Lang.Include.Alias p -> Include.Alias (module_path ident_map p)
    | ModuleType s -> ModuleType (u_module_type_expr ident_map s)

  and simple_expansion ident_map
      (f : Odoc_model.Lang.ModuleType.simple_expansion) :
      ModuleType.simple_expansion =
    let open Odoc_model.Lang.ModuleType in
    let open Odoc_model.Lang.FunctorParameter in
    match f with
    | Signature t -> Signature (signature ident_map t)
    | Functor (arg, sg) -> (
        match arg with
        | Named arg ->
            let identifier = arg.Odoc_model.Lang.FunctorParameter.id in
            let id = Ident.Of_Identifier.functor_parameter identifier in
            let ident_map' =
              {
                ident_map with
                functor_parameters =
                  Maps.FunctorParameter.add identifier id
                    ident_map.functor_parameters;
              }
            in
            let arg' = functor_parameter ident_map' id arg in
            Functor (FunctorParameter.Named arg', simple_expansion ident_map' sg)
        | Unit -> Functor (FunctorParameter.Unit, simple_expansion ident_map sg)
        )

  and module_ ident_map m =
    let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
    let canonical = m.Odoc_model.Lang.Module.canonical in
    {
      Module.source_loc = m.source_loc;
      source_loc_jane = m.source_loc_jane;
      doc = docs ident_map m.doc;
      type_;
      canonical;
      hidden = m.hidden;
    }

  and with_module_type_substitution ident_map m =
    let open Odoc_model.Lang.ModuleType in
    match m with
    | ModuleEq (frag, decl) ->
        ModuleType.ModuleEq
          (module_fragment ident_map frag, module_decl ident_map decl)
    | ModuleSubst (frag, p) ->
        ModuleType.ModuleSubst
          (module_fragment ident_map frag, module_path ident_map p)
    | ModuleTypeEq (frag, mty) ->
        ModuleType.ModuleTypeEq
          (module_type_fragment ident_map frag, module_type_expr ident_map mty)
    | ModuleTypeSubst (frag, mty) ->
        ModuleType.ModuleTypeSubst
          (module_type_fragment ident_map frag, module_type_expr ident_map mty)
    | TypeEq (frag, eqn) ->
        ModuleType.TypeEq
          (type_fragment ident_map frag, type_equation ident_map eqn)
    | TypeSubst (frag, eqn) ->
        ModuleType.TypeSubst
          (type_fragment ident_map frag, type_equation ident_map eqn)

  and functor_parameter ident_map id a =
    let expr' =
      module_type_expr ident_map a.Odoc_model.Lang.FunctorParameter.expr
    in
    { FunctorParameter.id; expr = expr' }

  and extension ident_map e =
    let open Odoc_model.Lang.Extension in
    let type_path = type_path ident_map e.type_path in
    let constructors =
      List.map (extension_constructor ident_map) e.constructors
    in
    {
      Extension.type_path;
      doc = docs ident_map e.doc;
      type_params = e.type_params;
      private_ = e.private_;
      constructors;
    }

  and extension_constructor ident_map c =
    let open Odoc_model.Lang.Extension.Constructor in
    let args = type_decl_constructor_argument ident_map c.args in
    let res = Opt.map (type_expression ident_map) c.res in
    {
      Extension.Constructor.name = Paths.Identifier.name c.id;
      source_loc = c.source_loc;
      doc = docs ident_map c.doc;
      args;
      res;
    }

  and exception_ ident_map e =
    let open Odoc_model.Lang.Exception in
    let args = type_decl_constructor_argument ident_map e.args in
    let res = Opt.map (type_expression ident_map) e.res in
    {
      Exception.source_loc = e.source_loc;
      source_loc_jane = e.source_loc_jane;
      doc = docs ident_map e.doc;
      args;
      res;
    }

  and u_module_type_expr ident_map m =
    let open Odoc_model in
    match m with
    | Lang.ModuleType.U.Signature s ->
        let s = signature ident_map s in
        ModuleType.U.Signature s
    | Path p ->
        let p' = module_type_path ident_map p in
        Path p'
    | With (w, e) ->
        let w' = List.map (with_module_type_substitution ident_map) w in
        With (w', u_module_type_expr ident_map e)
    | TypeOf (t_desc, t_original_path) ->
        let t_desc =
          match t_desc with
          | ModPath p -> ModuleType.ModPath (module_path ident_map p)
          | StructInclude p -> StructInclude (module_path ident_map p)
        in
        (* see comment in module_type_expr below *)
        let t_original_path = module_path (empty ()) t_original_path in
        TypeOf (t_desc, t_original_path)
    | Strengthen (e, p, a) ->
        let e = u_module_type_expr ident_map e in
        let p = module_path ident_map p in
        Strengthen (e, p, a)

  and module_type_expr ident_map m =
    let open Odoc_model in
    let open Paths in
    match m with
    | Lang.ModuleType.Signature s ->
        let s = signature ident_map s in
        ModuleType.Signature s
    | Lang.ModuleType.Path p ->
        let p' =
          ModuleType.
            {
              p_path = module_type_path ident_map p.p_path;
              p_expansion = option simple_expansion ident_map p.p_expansion;
            }
        in
        ModuleType.Path p'
    | Lang.ModuleType.With w ->
        let w' =
          ModuleType.
            {
              w_substitutions =
                List.map
                  (with_module_type_substitution ident_map)
                  w.w_substitutions;
              w_expansion = option simple_expansion ident_map w.w_expansion;
              w_expr = u_module_type_expr ident_map w.w_expr;
            }
        in
        ModuleType.With w'
    | Lang.ModuleType.Functor (Named arg, expr) ->
        let identifier = arg.Lang.FunctorParameter.id in
        let id = Ident.Of_Identifier.functor_parameter identifier in
        let ident_map' =
          {
            ident_map with
            functor_parameters =
              Identifier.Maps.FunctorParameter.add identifier id
                ident_map.functor_parameters;
          }
        in
        let arg' = functor_parameter ident_map' id arg in
        let expr' = module_type_expr ident_map' expr in
        ModuleType.Functor (Named arg', expr')
    | Lang.ModuleType.Functor (Unit, expr) ->
        let expr' = module_type_expr ident_map expr in
        ModuleType.Functor (Unit, expr')
    | Lang.ModuleType.TypeOf { t_desc; t_original_path; t_expansion } ->
        let t_desc =
          match t_desc with
          | ModPath p -> ModuleType.ModPath (module_path ident_map p)
          | StructInclude p -> StructInclude (module_path ident_map p)
        in
        let t_expansion = option simple_expansion ident_map t_expansion in
        (* Nb, we _never_ want to relativize this path, because this should always be
           the _original_ path. That's why we're passing in (empty()) rather than
           ident_map. We don't leave it as a Lang path because we'll occasionally
           _create_ a `TypeOf` expression as part of fragmap *)
        let t_original_path = module_path (empty ()) t_original_path in
        ModuleType.(TypeOf { t_desc; t_original_path; t_expansion })
    | Lang.ModuleType.Strengthen s ->
        let s' =
          ModuleType.
            { s_expr = u_module_type_expr ident_map s.s_expr;
              s_path = module_path ident_map s.s_path;
              s_aliasable = s.s_aliasable;
              s_expansion = option simple_expansion ident_map s.s_expansion
            }
        in
        ModuleType.Strengthen s'

  and module_type ident_map m =
    let expr =
      Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr
    in
    {
      ModuleType.source_loc = m.source_loc;
      source_loc_jane = m.source_loc_jane;
      doc = docs ident_map m.doc;
      canonical = m.canonical;
      expr;
    }

  and value ident_map v =
    let type_ = type_expression ident_map v.Lang.Value.type_ in
    {
      Value.type_;
      doc = docs ident_map v.doc;
      value = v.value;
      source_loc = v.source_loc;
      source_loc_jane = v.source_loc_jane
    }

  and include_ ident_map i =
    let open Odoc_model.Lang.Include in
    let decl = include_decl ident_map i.decl in
    {
      Include.parent = i.parent;
      doc = docs ident_map i.doc;
      shadowed = i.expansion.shadowed;
      expansion_ = apply_sig_map ident_map i.expansion.content;
      status = i.status;
      strengthened = option module_path ident_map i.strengthened;
      decl;
      loc = i.loc;
    }

  and class_ ident_map c =
    let open Odoc_model.Lang.Class in
    let expansion = Opt.map (class_signature ident_map) c.expansion in
    {
      Class.source_loc = c.source_loc;
      source_loc_jane = c.source_loc_jane;
      doc = docs ident_map c.doc;
      virtual_ = c.virtual_;
      params = c.params;
      type_ = class_decl ident_map c.type_;
      expansion;
    }

  and class_decl ident_map c =
    let open Odoc_model.Lang.Class in
    match c with
    | ClassType e -> Class.ClassType (class_type_expr ident_map e)
    | Arrow (lbl, e, d) ->
        Arrow (lbl, type_expression ident_map e, class_decl ident_map d)

  and class_type_expr ident_map e =
    let open Odoc_model.Lang.ClassType in
    match e with
    | Constr (p, ts) ->
        ClassType.Constr
          (class_type_path ident_map p, List.map (type_expression ident_map) ts)
    | Signature s -> Signature (class_signature ident_map s)

  and class_type ident_map t =
    let open Odoc_model.Lang.ClassType in
    let expansion = Opt.map (class_signature ident_map) t.expansion in
    {
      ClassType.source_loc = t.source_loc;
      source_loc_jane = t.source_loc_jane;
      doc = docs ident_map t.doc;
      virtual_ = t.virtual_;
      params = t.params;
      expr = class_type_expr ident_map t.expr;
      expansion;
    }

  and class_signature ident_map sg =
    let open Odoc_model.Lang.ClassSignature in
    let items =
      List.map
        (function
          | Method m ->
              let id = Ident.Of_Identifier.method_ m.id in
              let m' = method_ ident_map m in
              ClassSignature.Method (id, m')
          | InstanceVariable i ->
              let id = Ident.Of_Identifier.instance_variable i.id in
              let i' = instance_variable ident_map i in
              ClassSignature.InstanceVariable (id, i')
          | Constraint cst -> Constraint (class_constraint ident_map cst)
          | Inherit e -> Inherit (inherit_ ident_map e)
          | Comment c -> Comment (docs_or_stop ident_map c))
        sg.items
    in
    {
      ClassSignature.self = Opt.map (type_expression ident_map) sg.self;
      items;
      doc = docs ident_map sg.doc;
    }

  and method_ ident_map m =
    let open Odoc_model.Lang.Method in
    {
      Method.doc = docs ident_map m.doc;
      private_ = m.private_;
      virtual_ = m.virtual_;
      type_ = type_expression ident_map m.type_;
    }

  and instance_variable ident_map i =
    {
      InstanceVariable.doc = docs ident_map i.doc;
      mutable_ = i.mutable_;
      virtual_ = i.virtual_;
      type_ = type_expression ident_map i.type_;
    }

  and class_constraint ident_map cst =
    {
      ClassSignature.Constraint.doc = docs ident_map cst.doc;
      left = type_expression ident_map cst.left;
      right = type_expression ident_map cst.right;
    }

  and inherit_ ident_map ih =
    {
      ClassSignature.Inherit.doc = docs ident_map ih.doc;
      expr = class_type_expr ident_map ih.expr;
    }

  and module_substitution ident_map (t : Odoc_model.Lang.ModuleSubstitution.t) =
    {
      ModuleSubstitution.doc = docs ident_map t.doc;
      manifest = module_path ident_map t.manifest;
    }

  and module_type_substitution ident_map
      (t : Odoc_model.Lang.ModuleTypeSubstitution.t) =
    {
      ModuleTypeSubstitution.doc = docs ident_map t.doc;
      manifest = module_type_expr ident_map t.manifest;
    }

  and module_of_module_substitution ident_map
      (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let manifest = module_path ident_map t.manifest in
    {
      Module.source_loc = None;
      source_loc_jane = None;
      doc = docs ident_map t.doc;
      type_ = Alias (manifest, None);
      canonical = None;
      hidden = false;
    }

  and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
   fun ident_map items ->
    (* First we construct a list of brand new [Ident.t]s
                for each item in the signature *)
    let ident_map =
      map_of_idents (LocalIdents.signature items LocalIdents.empty) ident_map
    in
    (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
    apply_sig_map ident_map items

  and open_ ident_map o =
    Open.
      {
        expansion = apply_sig_map ident_map o.Odoc_model.Lang.Open.expansion;
        doc = docs ident_map o.Odoc_model.Lang.Open.doc;
      }

  and removed_item ident_map r =
    let open Odoc_model.Lang.Signature in
    match r with
    | RModule (id, p) -> Signature.RModule (id, module_path ident_map p)
    | RType (id, texpr, eqn) ->
        RType (id, type_expression ident_map texpr, type_equation ident_map eqn)
    | RModuleType (id, m) -> RModuleType (id, module_type_expr ident_map m)

  and apply_sig_map ident_map sg =
    let items =
      List.rev_map
        (let open Odoc_model.Lang.Signature in
         let open Odoc_model.Paths in
         function
         | Type (r, t) ->
             let id = Identifier.Maps.Type.find t.id ident_map.types in
             let t' = Delayed.put (fun () -> type_decl ident_map t) in
             Signature.Type (id, r, t')
         | TypeSubstitution t ->
             let id = Identifier.Maps.Type.find t.id ident_map.types in
             let t' = type_decl ident_map t in
             Signature.TypeSubstitution (id, t')
         | Module (r, m) ->
             let id =
               Identifier.Maps.Module.find
                 (m.id :> Identifier.Module.t)
                 ident_map.modules
             in
             let m' = Delayed.put (fun () -> module_ ident_map m) in
             Signature.Module (id, r, m')
         | ModuleSubstitution m ->
             let id = Identifier.Maps.Module.find m.id ident_map.modules in
             let m' = module_substitution ident_map m in
             Signature.ModuleSubstitution (id, m')
         | ModuleTypeSubstitution m ->
             let id =
               Identifier.Maps.ModuleType.find m.id ident_map.module_types
             in
             let m' = module_type_substitution ident_map m in
             Signature.ModuleTypeSubstitution (id, m')
         | ModuleType m ->
             let id =
               Identifier.Maps.ModuleType.find m.id ident_map.module_types
             in
             let m' = Delayed.put (fun () -> module_type ident_map m) in
             Signature.ModuleType (id, m')
         | Value v ->
             let id = Ident.Of_Identifier.value v.id in
             let v' = Delayed.put (fun () -> value ident_map v) in
             Signature.Value (id, v')
         | Comment c -> Comment (docs_or_stop ident_map c)
         | TypExt e -> TypExt (extension ident_map e)
         | Exception e ->
             let id = Ident.Of_Identifier.exception_ e.id in
             Exception (id, exception_ ident_map e)
         | Class (r, c) ->
             let id = Identifier.Maps.Class.find c.id ident_map.classes in
             Class (id, r, class_ ident_map c)
         | ClassType (r, c) ->
             let id =
               Identifier.Maps.ClassType.find c.id ident_map.class_types
             in
             ClassType (id, r, class_type ident_map c)
         | Open o -> Open (open_ ident_map o)
         | Include i -> Include (include_ ident_map i))
        sg.items
      |> List.rev
    in
    let removed = List.map (removed_item ident_map) sg.removed in
    { items; removed; compiled = sg.compiled; doc = docs ident_map sg.doc }

  and block_element _ b :
      CComment.block_element Odoc_model.Comment.with_location =
    match b with
    | { Odoc_model.Location_.value = `Heading (attrs, label, text); location }
      ->
        let label = Ident.Of_Identifier.label label in
        Odoc_model.Location_.same b
          (`Heading { Label.attrs; label; text; location })
    | { value = `Tag _ | `Media _; _ } as t -> t
    | { value = #Odoc_model.Comment.nestable_block_element; _ } as n -> n

  and docs ident_map d =
    {
      elements = List.map (block_element ident_map) d.elements;
      warnings_tag = d.warnings_tag;
    }

  and docs_or_stop ident_map = function
    | `Docs d -> `Docs (docs ident_map d)
    | `Stop -> `Stop
end

let module_of_functor_argument (arg : FunctorParameter.parameter) =
  {
    Module.source_loc = None;
    source_loc_jane = None;
    doc = { elements = []; warnings_tag = None };
    type_ = ModuleType arg.expr;
    canonical = None;
    hidden = false;
  }

(** This is equivalent to {!Lang.extract_signature_doc}. *)
let extract_signature_doc (s : Signature.t) =
  match (s.doc, s.items) with
  | { elements = []; _ }, Include { expansion_; status = `Inline; _ } :: _ ->
      expansion_.doc
  | doc, _ -> doc
