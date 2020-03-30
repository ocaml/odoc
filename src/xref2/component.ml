(* The following types are identical in structure to those in
    {!Lang}, which may well be too complex for our use, we'll
    see as we expand on this *)

module ModuleMap = Map.Make (struct
  type t = Ident.module_

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module ModuleTypeMap = Map.Make (struct
  type t = Ident.module_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module TypeMap = Map.Make (struct
  type t = Ident.path_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module ClassTypeMap = Map.Make (struct
  type t = Ident.path_class_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module IdentMap = Map.Make (struct
  type t = Ident.any

  let compare = Ident.compare
end)

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

module rec Module : sig
  type expansion =
    | AlreadyASig
    | Signature of Signature.t
    | Functor of FunctorParameter.t list * Signature.t

  type decl = Alias of Cpath.module_ | ModuleType of ModuleType.expr

  type t = {
    doc : Odoc_model.Comment.docs;
    type_ : decl;
    canonical : (Cpath.module_ * Odoc_model.Paths.Reference.Module.t) option;
    hidden : bool;
    display_type : decl option;
    expansion : expansion option;
  }
end =
  Module

and ModuleSubstitution : sig
  type t = { doc : Odoc_model.Comment.docs; manifest : Cpath.module_ }
end =
  ModuleSubstitution

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : Odoc_model.Comment.docs;
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
    | Tuple of t list
    | Constr of Cpath.type_ * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Cpath.class_type * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t
end =
  TypeExpr

and Extension : sig
  module Constructor : sig
    type t = {
      name : string;
      doc : Odoc_model.Comment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    type_path : Cpath.type_;
    doc : Odoc_model.Comment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end =
  Extension

and Exception : sig
  type t = {
    doc : Odoc_model.Comment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end =
  Exception

and FunctorParameter : sig
  type parameter = {
    id : Ident.module_;
    expr : ModuleType.expr;
    expansion : Module.expansion option;
  }
  type t =
    | Named of parameter
    | Unit
end =
  FunctorParameter

and ModuleType : sig
  type substitution =
    | ModuleEq of Cfrag.module_ * Module.decl
    | ModuleSubst of Cfrag.module_ * Cpath.module_
    | TypeEq of Cfrag.type_ * TypeDecl.Equation.t
    | TypeSubst of Cfrag.type_ * TypeDecl.Equation.t

  type expr =
    | Path of Cpath.module_type
    | Signature of Signature.t
    | With of expr * substitution list
    | Functor of FunctorParameter.t * expr
    | TypeOf of Module.decl

  type t = {
    doc : Odoc_model.Comment.docs;
    expr : expr option;
    expansion : Module.expansion option;
  }
end =
  ModuleType

and TypeDecl : sig
  module Field : sig
    type t = {
      name : string;
      doc : Odoc_model.Comment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      name : string;
      doc : Odoc_model.Comment.docs;
      args : argument;
      res : TypeExpr.t option;
    }
  end

  module Representation : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
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
    doc : Odoc_model.Comment.docs;
    equation : Equation.t;
    representation : Representation.t option;
  }
end =
  TypeDecl

and Value : sig
  type t = { doc : Odoc_model.Comment.docs; type_ : TypeExpr.t }
end =
  Value

and Signature : sig
  type recursive = Odoc_model.Lang.Signature.recursive

  type item =
    | Module of Ident.module_ * recursive * Module.t Substitution.delayed
    | ModuleSubstitution of Ident.module_ * ModuleSubstitution.t
    | ModuleType of Ident.module_type * ModuleType.t Substitution.delayed
    | Type of Ident.type_ * recursive * TypeDecl.t Substitution.delayed
    | TypeSubstitution of Ident.type_ * TypeDecl.t
    | Exception of Ident.exception_ * Exception.t
    | TypExt of Extension.t
    | Value of Ident.value * Value.t
    | External of Ident.value * External.t
    | Class of Ident.class_ * recursive * Class.t
    | ClassType of Ident.class_type * recursive * ClassType.t
    | Include of Include.t
    | Comment of Odoc_model.Comment.docs_or_stop

  (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
  type removed_item =
    | RModule of Ident.module_ * Cpath.Resolved.module_
    | RType of Ident.type_ * TypeExpr.t

  type t = { items : item list; removed : removed_item list }
end =
  Signature

and Include : sig
  type t = {
    parent : Odoc_model.Paths.Identifier.Signature.t;
    doc : Odoc_model.Comment.docs;
    expansion_ : Signature.t;
    decl : Module.decl;
  }
end =
  Include

and External : sig
  type t = { doc : Odoc_model.Comment.docs; type_ : TypeExpr.t; primitives : string list }
end =
  External

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    doc : Odoc_model.Comment.docs;
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
    doc : Odoc_model.Comment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end =
  ClassType

and ClassSignature : sig
  type item =
    | Method of Ident.method_ * Method.t
    | InstanceVariable of Ident.instance_variable * InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of Odoc_model.Comment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list }
end =
  ClassSignature

and Method : sig
  type t = {
    doc : Odoc_model.Comment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  Method

and InstanceVariable : sig
  type t = {
    doc : Odoc_model.Comment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  InstanceVariable

and Substitution : sig
  type t = {
    module_ : Cpath.Resolved.module_ ModuleMap.t;
    module_type : Cpath.Resolved.module_type ModuleTypeMap.t;
    type_ : Cpath.Resolved.type_ TypeMap.t;
    class_type : Cpath.Resolved.class_type ClassTypeMap.t;
    type_replacement : TypeExpr.t TypeMap.t;
    (* Reference maps *)
  }

  type 'a delayed = DelayedSubst of t * 'a | NoSubst of 'a
end =
  Substitution

module Element = struct
  open Odoc_model.Paths

  type module_ = [ `Module of Identifier.Module.t * Module.t ]

  type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]

  type type_ = [ `Type of Identifier.Type.t * TypeDecl.t ]

  type value = [ `Value of Identifier.Value.t * Value.t ]

  type label = [ `Label of Identifier.Label.t ]

  type class_ = [ `Class of Identifier.Class.t * Class.t ]

  type class_type = [ `ClassType of Identifier.ClassType.t * ClassType.t ]

  type datatype = [ type_ | class_ | class_type ]

  type signature = [ module_ | module_type ]

  type external_ = [ `External of Identifier.Value.t * External.t ]

  type any =
    [ signature | value | type_ | label | class_ | class_type | external_ ]
end

module Fmt = struct
  open Odoc_model.Names

  let option formatter fmt x =
    match x with Some x' -> Format.fprintf fmt "%a" formatter x' | None -> ()

  let string_of fmt c =
    let b = Buffer.create 100 in
    Format.fprintf (Format.formatter_of_buffer b) "%a%!" fmt c;
    Buffer.contents b

  let subst_delayed f ppf = function
    | Substitution.DelayedSubst (_, v) ->
      Format.fprintf ppf "@[DelayedSubst (<subst>, %a)@]" f v
    | NoSubst v -> f ppf v

  let rec signature ppf sg =
    let open Signature in
    Format.fprintf ppf "@[<v>";
    List.iter
      (function
        | Module (id, _, m) ->
            Format.fprintf ppf "@[<v 2>module %a %a@]@," Ident.fmt id
              (subst_delayed module_) m
        | ModuleSubstitution (id, m) ->
            Format.fprintf ppf "@[<v 2>module %a := %a@]@," Ident.fmt id
              module_path m.ModuleSubstitution.manifest
        | ModuleType (id, mt) ->
            Format.fprintf ppf "@[<v 2>module type %a %a@]@," Ident.fmt id
              (subst_delayed module_type) mt
        | Type (id, _, t) ->
            Format.fprintf ppf "@[<v 2>type %a %a@]@," Ident.fmt id
              (subst_delayed type_decl) t
        | TypeSubstitution (id, t) ->
            Format.fprintf ppf "@[<v 2>type %a := %a@]@," Ident.fmt id type_decl
              t
        | Exception (id, e) ->
            Format.fprintf ppf "@[<v 2>exception %a %a@]@," Ident.fmt id
              exception_ e
        | TypExt e ->
            Format.fprintf ppf "@[<v 2>type_extension %a@]@," extension e
        | Value (id, v) ->
            Format.fprintf ppf "@[<v 2>val %a %a@]@," Ident.fmt id value v
        | External (id, e) ->
            Format.fprintf ppf "@[<v 2>external %a %a@]@," Ident.fmt id
              external_ e
        | Class (id, _, c) ->
            Format.fprintf ppf "@[<v 2>class %a %a@]@," Ident.fmt id class_ c
        | ClassType (id, _, c) ->
            Format.fprintf ppf "@[<v 2>class type %a %a@]@," Ident.fmt id
              class_type c
        | Include i -> Format.fprintf ppf "@[<v 2>include %a@]@," include_ i
        | Comment _c -> ())
      sg.items;
    Format.fprintf ppf "@] (removed=[%a])" removed_item_list sg.removed

  and removed_item ppf r =
    let open Signature in
    match r with
    | RModule (id, path) -> Format.fprintf ppf "module %a (%a)" Ident.fmt id resolved_module_path path
    | RType (id, texpr) -> Format.fprintf ppf "type %a (%a)" Ident.fmt id type_expr texpr 

  and removed_item_list ppf r =
    match r with
    | [] -> ()
    | [x] -> Format.fprintf ppf "%a" removed_item x
    | x::ys -> Format.fprintf ppf "%a;%a" removed_item x removed_item_list ys

  and external_ ppf _ = Format.fprintf ppf "<todo>"

  and class_ ppf _c = Format.fprintf ppf "<todo>"

  and class_type ppf _c = Format.fprintf ppf "<todo>"

  and include_ ppf i =
    Format.fprintf ppf "%a (sig = %a)" module_decl i.decl signature i.expansion_

  and value ppf v =
    let open Value in
    Format.fprintf ppf ": %a" type_expr v.type_

  and module_decl ppf d =
    let open Module in
    match d with
    | Alias p -> Format.fprintf ppf "= %a" module_path p
    | ModuleType mt -> Format.fprintf ppf ": %a" module_type_expr mt

  and module_ ppf m = Format.fprintf ppf "%a" module_decl m.type_

  and module_type ppf mt =
    match mt.expr with
    | Some x -> Format.fprintf ppf "= %a" module_type_expr x
    | None -> ()

  and module_type_expr ppf mt =
    let open ModuleType in
    match mt with
    | Path p -> module_type_path ppf p
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
    | With (expr, subs) ->
        Format.fprintf ppf "%a with [%a]" module_type_expr expr
          substitution_list subs
    | Functor (arg, res) ->
        Format.fprintf ppf "(%a) -> %a" functor_parameter arg
          module_type_expr res
    | TypeOf decl -> Format.fprintf ppf "module type of %a" module_decl decl

  and functor_parameter ppf x =
    let open FunctorParameter in
    match x with
    | Unit -> ()
    | Named x -> Format.fprintf ppf "%a" functor_parameter_parameter x

  and functor_parameter_parameter ppf x =
    Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorParameter.id module_type_expr
      x.FunctorParameter.expr

  and type_decl ppf t =
    match TypeDecl.(t.equation.Equation.manifest) with
    | Some x -> Format.fprintf ppf "= %a" type_expr x
    | None -> ()

  and type_equation ppf t =
    match t.TypeDecl.Equation.manifest with
    | None -> ()
    | Some m -> Format.fprintf ppf " = %a" type_expr m

  and type_equation2 ppf t =
    match t.TypeDecl.Equation.manifest with
    | None -> ()
    | Some m -> Format.fprintf ppf " = %a" type_expr m

  and exception_ _ppf _e = ()

  and extension ppf e = Format.fprintf ppf "%a" type_path e.Extension.type_path

  and substitution ppf t =
    let open ModuleType in
    match t with
    | ModuleEq (frag, decl) ->
        Format.fprintf ppf "%a = %a" module_fragment frag
          module_decl decl
    | ModuleSubst (frag, mpath) ->
        Format.fprintf ppf "%a := %a" module_fragment frag
          module_path mpath
    | TypeEq (frag, decl) ->
        Format.fprintf ppf "%a%a" type_fragment frag 
        type_equation decl
    | TypeSubst (frag, decl) ->
        Format.fprintf ppf "%a%a" type_fragment frag
          type_equation2 decl

  and substitution_list ppf l =
    match l with
    | [ sub ] -> Format.fprintf ppf "%a" substitution sub
    | sub :: subs ->
        Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
    | [] -> ()

  and type_expr_list ppf l =
    match l with
    | [ t ] -> Format.fprintf ppf "%a" type_expr t
    | t :: ts -> Format.fprintf ppf "%a * %a" type_expr t type_expr_list ts
    | [] -> ()

  and type_object ppf _o = Format.fprintf ppf "(object)"

  and type_class ppf (x, ys) =
    Format.fprintf ppf "(class %a %a)" class_type_path x type_expr_list ys

  and type_package ppf _p = Format.fprintf ppf "(package)"

  and type_expr_polymorphic_variant ppf p =
    let kind ppf k =
      let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
      match k with
      | Fixed -> Format.fprintf ppf "Fixed"
      | Closed xs -> Format.fprintf ppf "Closed [%s]" (String.concat ";" xs)
      | Open -> Format.fprintf ppf "Open"
    in
    let open TypeExpr.Polymorphic_variant in
    let constructor ppf c =
      Format.fprintf ppf "name=%s" c.Constructor.name
    in
    let element ppf k =
      match k with
      | Type t -> Format.fprintf ppf "Type (%a)" type_expr t
      | Constructor c -> Format.fprintf ppf "Constructor (%a)" constructor c
    in
    let rec elements ppf k =
      match k with
      | [] -> ()
      | [x] -> Format.fprintf ppf "%a" element x
      | x::xs -> Format.fprintf ppf "%a; %a" element x elements xs
    in
    Format.fprintf ppf "{ kind=%a; elements=[%a] }"
      kind p.kind elements p.elements
  and type_expr ppf e =
    let open TypeExpr in
    match e with
    | Var x -> Format.fprintf ppf "%s" x
    | Any -> Format.fprintf ppf "_"
    | Alias (x, y) -> Format.fprintf ppf "(alias %a %s)" type_expr x y
    | Arrow (_l, t1, t2) ->
        Format.fprintf ppf "%a -> %a" type_expr t1 type_expr t2
    | Tuple ts -> Format.fprintf ppf "(%a)" type_expr_list ts
    | Constr (p, _args) -> type_path ppf p
    | Polymorphic_variant poly ->
      Format.fprintf ppf "(poly_var %a)" type_expr_polymorphic_variant poly
    | Object x -> type_object ppf x
    | Class (x, y) -> type_class ppf (x, y)
    | Poly (_ss, _t) -> Format.fprintf ppf "(poly)"
    | Package x -> type_package ppf x

  and resolved_module_path : Format.formatter -> Cpath.Resolved.module_ -> unit =
   fun ppf p ->
    match p with
    | `Local ident -> Format.fprintf ppf "local(%a)" Ident.fmt ident
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" resolved_module_path p1 module_path p2
    | `Identifier p ->
        Format.fprintf ppf "identifier(%a)" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
    | `Substituted p ->
        Format.fprintf ppf "substituted(%a)" resolved_module_path p
    | `Module (p, m) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ModuleName.to_string m)
    | `Alias (p1, p2) ->
        Format.fprintf ppf "alias(%a,%a)" resolved_module_path p1
          resolved_module_path p2
    | `Subst (p1, p2) ->
        Format.fprintf ppf "subst(%a,%a)" resolved_module_type_path p1
          resolved_module_path p2
    | `SubstAlias (p1, p2) ->
        Format.fprintf ppf "substalias(%a,%a)" resolved_module_path p1
          resolved_module_path p2
    | `Hidden p1 -> Format.fprintf ppf "hidden(%a)" resolved_module_path p1
    | `Canonical (p1, p2) ->
        Format.fprintf ppf "canonical(%a,%a)" resolved_module_path p1
          module_path p2

  and module_path : Format.formatter -> Cpath.module_ -> unit =
   fun ppf p ->
    match p with
    | `Resolved p -> Format.fprintf ppf "resolved(%a)" resolved_module_path p
    | `Dot (p, str) -> Format.fprintf ppf "%a.%s" module_path p str
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" module_path p1 module_path p2
    | `Substituted p -> Format.fprintf ppf "substituted(%a)" module_path p
    | `Forward s -> Format.fprintf ppf "forward(%s)" s
    | `Root r -> Format.fprintf ppf "unresolvedroot(%s)" r

  and resolved_module_type_path :
      Format.formatter -> Cpath.Resolved.module_type -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "identifier(%a)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted x ->
        Format.fprintf ppf "substituted(%a)" resolved_module_type_path x
    | `ModuleType (p, m) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (ModuleTypeName.to_string m)
    | `SubstT (m1, m2) ->
        Format.fprintf ppf "substt(%a,%a)" resolved_module_type_path m1 resolved_module_type_path m2
    
    
  and module_type_path : Format.formatter -> Cpath.module_type -> unit =
   fun ppf m ->
    match m with
    | `Resolved p -> Format.fprintf ppf "resolved(%a)" resolved_module_type_path p
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" module_type_path s
    | `Dot (m, s) ->
        Format.fprintf ppf "%a.%s" module_path m s

  and resolved_type_path : Format.formatter -> Cpath.Resolved.type_ -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "identifier(%a)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted x ->
        Format.fprintf ppf "substituted(%a)" resolved_type_path x
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)
    | `Type (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.TypeName.to_string t)

  and resolved_parent_path : Format.formatter -> Cpath.Resolved.parent -> unit =
    fun ppf p ->
      match p with
      | `Module m -> resolved_module_path ppf m
      | `ModuleType m -> Format.fprintf ppf ">>%a<<" resolved_module_type_path m
      | `FragmentRoot -> Format.fprintf ppf "FragmentRoot"

  and type_path : Format.formatter -> Cpath.type_ -> unit =
   fun ppf p ->
    match p with
    | `Resolved r -> Format.fprintf ppf "resolved(%a)" resolved_type_path r
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" type_path s
    | `Dot (m, s) -> Format.fprintf ppf "%a.%s" module_path m s

  and resolved_class_type_path :
      Format.formatter -> Cpath.Resolved.class_type -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "%a" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted s ->
        Format.fprintf ppf "substituted(%a)" resolved_class_type_path s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)

  and class_type_path : Format.formatter -> Cpath.class_type -> unit =
   fun ppf p ->
    match p with
    | `Resolved r -> Format.fprintf ppf "%a" resolved_class_type_path r
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" class_type_path s
    | `Dot (m, s) -> Format.fprintf ppf "%a.%s" module_path m s

  and model_path : Format.formatter -> Odoc_model.Paths.Path.t -> unit =
   fun ppf (p : Odoc_model.Paths.Path.t) ->
    match p with
    | `Resolved rp -> Format.fprintf ppf "resolved(%a)" model_resolved_path rp
    | `Root s -> Format.fprintf ppf "root(%s)" s
    | `Forward s -> Format.fprintf ppf "forward(%s)" s
    | `Dot (parent, s) ->
        Format.fprintf ppf "%a.%s" model_path
          (parent :> Odoc_model.Paths.Path.t)
          s
    | `Apply (func, arg) ->
        Format.fprintf ppf "%a(%a)" model_path
          (func :> Odoc_model.Paths.Path.t)
          model_path
          (arg :> Odoc_model.Paths.Path.t)

  and model_resolved_path ppf (p : Odoc_model.Paths.Path.Resolved.t) =
    let open Odoc_model.Paths.Path.Resolved in
    match p with
    | `Identifier id ->
        Format.fprintf ppf "identifier(%a)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.TypeName.to_string name)
    | `Alias (path, realpath) ->
        Format.fprintf ppf "alias(%a,%a)" model_resolved_path
          (path :> t)
          model_resolved_path
          (realpath :> t)
    | `Subst (modty, m) ->
        Format.fprintf ppf "subst(%a,%a)" model_resolved_path
          (modty :> t)
          model_resolved_path
          (m :> t)
    | `SubstT (t1, t2) ->
        Format.fprintf ppf "substt(%a,%a)" model_resolved_path
          (t1 :> t)
          model_resolved_path
          (t2 :> t)
    | `Apply (funct, arg) ->
        Format.fprintf ppf "%a(%a)" model_resolved_path
          (funct :> t)
          model_path
          (arg :> Odoc_model.Paths.Path.t)
    | `Canonical (p1, p2) ->
        Format.fprintf ppf "canonical(%a,%a)" model_resolved_path
          (p1 :> t)
          model_path
          (p2 :> Odoc_model.Paths.Path.t)
    | `Hidden p -> Format.fprintf ppf "hidden(%a)" model_resolved_path (p :> t)
    | `SubstAlias (_, _) ->
        Format.fprintf ppf "UNIMPLEMENTED substalias in model_resolved_path"
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ClassTypeName.to_string name)

  and model_identifier ppf (p : Odoc_model.Paths.Identifier.t) =
    match p with
    | `Root (_, unit_name) ->
        Format.fprintf ppf "(root %s)" (Odoc_model.Names.UnitName.to_string unit_name)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.TypeName.to_string name)
    | `Parameter (parent, name) ->
        Format.fprintf ppf "(param %a %s)" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ParameterName.to_string name)
    | `Result parent ->
        Format.fprintf ppf "%a.result" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
    | `CoreType name ->
        Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
    | `Constructor (ty, x) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (ty :> Odoc_model.Paths.Identifier.t)
          (ConstructorName.to_string x)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (ValueName.to_string name)
    | `CoreException name -> Format.fprintf ppf "%s" (ExceptionName.to_string name)
    | `Class (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (ClassName.to_string name)
    | `ClassType (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (ClassTypeName.to_string name)
    | `InstanceVariable (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (InstanceVariableName.to_string name)
    | `Method (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (MethodName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (LabelName.to_string name)
    | `Field (ty, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (ty :> Odoc_model.Paths.Identifier.t)
          (FieldName.to_string name)
    | `Exception (p, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
          (ExceptionName.to_string name)
    | `Extension (p, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
          (ExtensionName.to_string name)
    | `Page (_, name) -> Format.fprintf ppf "%s" (PageName.to_string name)

  and model_fragment ppf (f : Odoc_model.Paths.Fragment.t) =
    match f with
    | `Resolved rf -> model_resolved_fragment ppf rf
    | `Dot (sg, d) ->
        Format.fprintf ppf "*%a.%s" model_fragment
          (sg :> Odoc_model.Paths.Fragment.t)
          d
    | `Root -> ()

  and model_resolved_fragment ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
    let open Odoc_model.Paths.Fragment.Resolved in
    match f with
    | `Root (`ModuleType p) -> Format.fprintf ppf "root(%a)" model_resolved_path (p :> Odoc_model.Paths.Path.Resolved.t)
    | `Root (`Module p) -> Format.fprintf ppf "root(%a)" model_resolved_path (p :> Odoc_model.Paths.Path.Resolved.t)
    | `Module (sg, m) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ModuleName.to_string m)
    | `Type (sg, t) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.TypeName.to_string t)
    | `Subst (path, m) ->
        Format.fprintf ppf "(%a subst -> %a)" model_resolved_path
          (path :> Odoc_model.Paths.Path.Resolved.t)
          model_resolved_fragment
          (m :> t)
    | `SubstAlias (_, _) -> Format.fprintf ppf "UNIMPLEMENTED subst alias!?"
    | `Class (sg, c) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ClassName.to_string c)
    | `ClassType (sg, c) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ClassTypeName.to_string c)

  and resolved_signature_fragment ppf (f : Cfrag.resolved_signature) =
    match f with
    | `Root (`ModuleType p) -> Format.fprintf ppf "root(%a)" resolved_module_type_path p
    | `Root (`Module p) -> Format.fprintf ppf "root(%a)" resolved_module_path p
    | `Subst _ | `SubstAlias _ | `Module _ as x -> resolved_module_fragment ppf x
  
  and resolved_module_fragment ppf (f : Cfrag.resolved_module) =
    match f with
    | `Subst (s, f) -> Format.fprintf ppf "subst(%a,%a)" resolved_module_type_path s resolved_module_fragment f
    | `SubstAlias (m, f) -> Format.fprintf ppf "substalias(%a,%a)" resolved_module_path m resolved_module_fragment f
    | `Module (p, n) -> Format.fprintf ppf "%a.%s" resolved_signature_fragment p (ModuleName.to_string n)

  and resolved_type_fragment ppf (f : Cfrag.resolved_type) =
    match f with
    | `Type (s, n) -> Format.fprintf ppf "%a.%s" resolved_signature_fragment s (TypeName.to_string n)
    | `Class (s, n) ->Format.fprintf ppf "%a.%s" resolved_signature_fragment s (ClassName.to_string n)
    | `ClassType (s, n) ->Format.fprintf ppf "%a.%s" resolved_signature_fragment s (ClassTypeName.to_string n)
  
  and signature_fragment ppf (f : Cfrag.signature) =
    match f with
    | `Resolved r -> Format.fprintf ppf "resolved(%a)" resolved_signature_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n
    | `Root -> Format.fprintf ppf "root"

    and module_fragment ppf (f : Cfrag.module_) =
    match f with
    | `Resolved r -> Format.fprintf ppf "resolved(%a)" resolved_module_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n

    and type_fragment ppf (f : Cfrag.type_) =
    match f with
    | `Resolved r -> Format.fprintf ppf "resolved(%a)" resolved_type_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n

  and model_resolved_reference ppf (r : Odoc_model.Paths.Reference.Resolved.t) =
    let open Odoc_model.Paths.Reference.Resolved in
    match r with
    | `Identifier id -> Format.fprintf ppf "identifier(%a)" model_identifier id
    | `Hidden p -> Format.fprintf ppf "hidden(%a)" model_resolved_reference (p :> t)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (FieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (ClassTypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (InstanceVariableName.to_string name)
    | `SubstAlias (x, y) -> Format.fprintf ppf "substalias(%a,%a)" model_resolved_path (x :> Odoc_model.Paths.Path.Resolved.t) model_resolved_reference (y :> Odoc_model.Paths.Reference.Resolved.t);
    | `Canonical (x, y) -> Format.fprintf ppf "canonical(%a,%a)" model_resolved_reference (x :> t) model_reference (y :> Odoc_model.Paths.Reference.t)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference (parent :> t) (LabelName.to_string name)

  and model_reference ppf (r : Odoc_model.Paths.Reference.t) =
    let open Odoc_model.Paths.Reference in
    match r with
    | `Resolved r' ->
        Format.fprintf ppf "resolved(%a)" model_resolved_reference r'
    | `Root (name, _) -> Format.fprintf ppf "unresolvedroot(%s)" (UnitName.to_string name)
    | `Dot (parent, str) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) str
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (FieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (ClassTypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (InstanceVariableName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) (LabelName.to_string name)
end

module LocalIdents = struct
  open Odoc_model

  type t = {
    modules : Paths.Identifier.Module.t list;
    module_types : Paths.Identifier.ModuleType.t list;
    fields : Paths.Identifier.Field.t list;
    constructors : Paths.Identifier.Constructor.t list;
    types : Paths.Identifier.Type.t list;
    extensions : Paths.Identifier.Extension.t list;
    exceptions : Paths.Identifier.Exception.t list;
    values : Paths.Identifier.Value.t list;
    classes : Paths.Identifier.Class.t list;
    class_types : Paths.Identifier.ClassType.t list;
    methods : Paths.Identifier.Method.t list;
    instance_variables : Paths.Identifier.InstanceVariable.t list;
    labels : Paths.Identifier.Label.t list;
  }

  let empty =
    {
      modules = [];
      module_types = [];
      fields = [];
      constructors = [];
      types = [];
      extensions = [];
      exceptions = [];
      values = [];
      classes = [];
      class_types = [];
      methods = [];
      instance_variables = [];
      labels = [];
    }

  open Lang

  let opt conv opt ids = match opt with Some x -> conv x ids | None -> ids

  let rec module_ m ids = docs m.Module.doc ids

  and module_substitution m ids = docs m.ModuleSubstitution.doc ids

  and module_type m ids = docs m.ModuleType.doc ids

  and type_decl t ids =
    let ids = opt type_decl_representation t.TypeDecl.representation ids in
    docs t.TypeDecl.doc ids

  and type_decl_representation r ids =
    match r with
    | TypeDecl.Representation.Extensible -> ids
    | Record fs -> List.fold_right type_decl_field fs ids
    | Variant cs -> List.fold_right type_decl_constructor cs ids

  and type_decl_field f ids =
    let ids = docs f.TypeDecl.Field.doc ids in
    { ids with fields = f.TypeDecl.Field.id :: ids.fields }

  and type_decl_constructor c ids =
    let ids = docs c.TypeDecl.Constructor.doc ids in
    { ids with constructors = c.TypeDecl.Constructor.id :: ids.constructors }

  and extension e ids =
    let ids = docs e.Extension.doc ids in
    List.fold_right extension_constructor e.Extension.constructors ids

  and extension_constructor c ids =
    let ids = docs c.Extension.Constructor.doc ids in
    { ids with extensions = c.Extension.Constructor.id :: ids.extensions }

  and exception_ e ids = docs e.Exception.doc ids

  and value_ v ids = docs v.Value.doc ids

  and external_ e ids = docs e.External.doc ids

  and class_ c ids = docs c.Class.doc ids

  and class_type c ids = docs c.ClassType.doc ids

  and method_ m ids =
    let ids = docs m.Method.doc ids in
    { ids with methods = m.Method.id :: ids.methods }

  and instance_variable i ids =
    let ids = docs i.InstanceVariable.doc ids in
    {
      ids with
      instance_variables = i.InstanceVariable.id :: ids.instance_variables;
    }

  and block_element d ids =
    match d with
    | `Heading (_, id, _) -> { ids with labels = id :: ids.labels }
    | _ -> ids

  and docs d ids =
    List.fold_right (fun bel_loc -> block_element bel_loc.Location_.value) d ids

  and docs_or_stop (d : Comment.docs_or_stop) ids =
    match d with `Docs d' -> docs d' ids | `Stop -> ids

  and class_signature s ids =
    List.fold_right
      (fun c ids ->
        match c with
        | ClassSignature.Method m -> method_ m ids
        | InstanceVariable i -> instance_variable i ids
        | Constraint _ -> ids
        | Inherit _ -> ids
        | Comment d -> docs_or_stop d ids)
      s ids

  and signature s ids =
    let open Signature in
    List.fold_right
      (fun c ids ->
        match c with
        | Module (_, m) ->
            let ids = module_ m ids in
            { ids with modules = m.Module.id :: ids.modules }
        | ModuleType m ->
            let ids = module_type m ids in
            { ids with module_types = m.ModuleType.id :: ids.module_types }
        | ModuleSubstitution m ->
            let ids = module_substitution m ids in
            { ids with modules = m.ModuleSubstitution.id :: ids.modules }
        | Type (_, t) ->
            let ids = type_decl t ids in
            { ids with types = t.TypeDecl.id :: ids.types }
        | TypeSubstitution t ->
            let ids = type_decl t ids in
            { ids with types = t.TypeDecl.id :: ids.types }
        | TypExt ext -> extension ext ids
        | Exception e ->
            let ids = exception_ e ids in
            { ids with exceptions = e.Exception.id :: ids.exceptions }
        | Value v ->
            let ids = value_ v ids in
            { ids with values = v.Value.id :: ids.values }
        | External e ->
            let ids = external_ e ids in
            { ids with values = e.External.id :: ids.values }
        | Class (_, c) ->
            let ids = class_ c ids in
            { ids with classes = c.Class.id :: ids.classes }
        | ClassType (_, c) ->
            let ids = class_type c ids in
            { ids with class_types = c.ClassType.id :: ids.class_types }
        | Include i ->
            let ids = docs i.Include.doc ids in
            signature i.Include.expansion.content ids
        | Comment c -> docs_or_stop c ids)
      s ids
end

let core_type_ids =
  let open Odoc_model.Predefined in
  List.map
    (fun id -> (id, Ident.Of_Identifier.type_ id))
    [
      bool_identifier;
      int_identifier;
      char_identifier;
      bytes_identifier;
      string_identifier;
      float_identifier;
      unit_identifier;
      exn_identifier;
      array_identifier;
      list_identifier;
      option_identifier;
      int32_identifier;
      int64_identifier;
      nativeint_identifier;
      lazy_t_identifier;
      extension_constructor_identifier;
      floatarray_identifier;
    ]

let core_constructors =
  let open Odoc_model.Predefined in
  List.map
    (fun id -> (id, Ident.Of_Identifier.constructor id))
    [
      false_identifier;
      true_identifier;
      void_identifier;
      nil_identifier;
      cons_identifier;
      none_identifier;
      some_identifier;
    ]

let core_exceptions =
  let open Odoc_model.Predefined in
  List.map
    (fun id -> (id, Ident.Of_Identifier.exception_ id))
    [
      match_failure_identifier;
      assert_failure_identifier;
      invalid_argument_identifier;
      failure_identifier;
      not_found_identifier;
      out_of_memory_identifier;
      stack_overflow_identifier;
      sys_error_identifier;
      end_of_file_identifier;
      division_by_zero_identifier;
      sys_blocked_io_identifier;
      undefined_recursive_module_identifier;
    ]

module Of_Lang = struct
  open Odoc_model

  type map = {
    modules : (Paths.Identifier.Module.t * Ident.module_) list;
    module_types : (Paths.Identifier.ModuleType.t * Ident.module_type) list;
    fields : (Paths.Identifier.Field.t * Ident.field) list;
    constructors : (Paths.Identifier.Constructor.t * Ident.constructor) list;
    types : (Paths.Identifier.Type.t * Ident.type_) list;
    path_types : (Paths_types.Identifier.path_type * Ident.path_type) list;
    path_class_types :
      (Paths_types.Identifier.path_class_type * Ident.path_class_type) list;
    extensions : (Paths.Identifier.Extension.t * Ident.extension) list;
    exceptions : (Paths.Identifier.Exception.t * Ident.exception_) list;
    values : (Paths.Identifier.Value.t * Ident.value) list;
    classes : (Paths.Identifier.Class.t * Ident.class_) list;
    class_types : (Paths.Identifier.ClassType.t * Ident.class_type) list;
    methods : (Paths.Identifier.Method.t * Ident.method_) list;
    instance_variables :
      (Paths.Identifier.InstanceVariable.t * Ident.instance_variable) list;
    signatures : (Paths.Identifier.Signature.t * Ident.signature) list;
    label_parents : (Paths.Identifier.LabelParent.t * Ident.label_parent) list;
    labels : (Paths.Identifier.Label.t * Ident.label) list;
    parents : (Paths.Identifier.Parent.t * Ident.parent) list;
    any : (Paths.Identifier.t * Ident.any) list;
  }

  let empty =
    {
      modules = [];
      module_types = [];
      fields = [];
      constructors = core_constructors;
      types = core_type_ids;
      path_types = [];
      path_class_types = [];
      extensions = [];
      exceptions = core_exceptions;
      values = [];
      classes = [];
      class_types = [];
      methods = [];
      instance_variables = [];
      signatures = [];
      label_parents = [];
      labels = [];
      parents = [];
      any = [];
    }

  let map_of_idents ids map =
    let open Paths_types.Identifier in
    let types_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.type_ i) :: acc)
        [] ids.LocalIdents.types
    in
    let classes_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.class_ i) :: acc)
        [] ids.LocalIdents.classes
    in
    let class_types_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.class_type i) :: acc)
        [] ids.LocalIdents.class_types
    in
    let modules_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.module_ i) :: acc)
        [] ids.LocalIdents.modules
    in
    let module_types_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.module_type i) :: acc)
        [] ids.LocalIdents.module_types
    in
    let fields_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.field i) :: acc)
        [] ids.LocalIdents.fields
    in
    let constructors_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.constructor i) :: acc)
        [] ids.LocalIdents.constructors
    in
    let extensions_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.extension i) :: acc)
        [] ids.LocalIdents.extensions
    in
    let exceptions_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.exception_ i) :: acc)
        [] ids.LocalIdents.exceptions
    in
    let values_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.value i) :: acc)
        [] ids.LocalIdents.values
    in
    let methods_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.method_ i) :: acc)
        [] ids.LocalIdents.methods
    in
    let instance_variables_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.instance_variable i) :: acc)
        [] ids.LocalIdents.instance_variables
    in
    let path_class_types_new =
      (classes_new :> (path_class_type * Ident.path_class_type) list)
      @ (class_types_new :> (path_class_type * Ident.path_class_type) list)
    in
    let path_types_new =
      (path_class_types_new :> (path_type * Ident.path_type) list)
      @ (types_new :> (path_type * Ident.path_type) list)
    in
    let signatures_new =
      (modules_new :> (signature * Ident.signature) list)
      @ (module_types_new :> (signature * Ident.signature) list)
    in
    let parents_new =
      (path_types_new :> (parent * Ident.parent) list)
      @ (signatures_new :> (parent * Ident.parent) list)
    in
    let label_parents_new =
      (parents_new :> (label_parent * Ident.label_parent) list)
    in
    let labels_new =
      List.fold_left
        (fun acc i -> (i, Ident.Of_Identifier.label i) :: acc)
        [] ids.LocalIdents.labels
    in
    let any_new =
      (label_parents_new :> (any * Ident.any) list)
      @ (fields_new :> (any * Ident.any) list)
      @ (constructors_new :> (any * Ident.any) list)
      @ (extensions_new :> (any * Ident.any) list)
      @ (exceptions_new :> (any * Ident.any) list)
      @ (values_new :> (any * Ident.any) list)
      @ (methods_new :> (any * Ident.any) list)
      @ (instance_variables_new :> (any * Ident.any) list)
      @ (labels_new :> (any * Ident.any) list)
    in
    let modules = modules_new @ map.modules in
    let module_types = module_types_new @ map.module_types in
    let fields = fields_new @ map.fields in
    let constructors = constructors_new @ map.constructors in
    let types = types_new @ map.types in
    let extensions = extensions_new @ map.extensions in
    let exceptions = exceptions_new @ map.exceptions in
    let values = values_new @ map.values in
    let classes = classes_new @ map.classes in
    let class_types = class_types_new @ map.class_types in
    let methods = methods_new @ map.methods in
    let instance_variables = instance_variables_new @ map.instance_variables in
    let path_types = path_types_new @ map.path_types in
    let path_class_types = path_class_types_new @ map.path_class_types in
    let signatures = signatures_new @ map.signatures in
    let parents = parents_new @ map.parents in
    let label_parents = label_parents_new @ map.label_parents in
    let any = any_new @ map.any in
    let labels = labels_new @ map.labels in
    {
      modules;
      module_types;
      fields;
      constructors;
      types;
      extensions;
      exceptions;
      values;
      classes;
      class_types;
      methods;
      instance_variables;
      path_types;
      path_class_types;
      signatures;
      parents;
      label_parents;
      labels;
      any;
    }

  let ident_of_identifier ident_map identifier =
    try Some (List.assoc identifier ident_map) with _ -> None

  let option conv ident_map x =
    match x with None -> None | Some x' -> Some (conv ident_map x')

  let optvalue o ~default = match o with None -> default | Some x -> x

  let optmap o f = match o with Some x -> Some (f x) | None -> None

  let identifier map i =
    optvalue
      (optmap (ident_of_identifier map i) (fun x -> `Local x))
      ~default:(`Identifier i)

  let rec resolved_module_path :
      _ -> Odoc_model.Paths.Path.Resolved.Module.t -> Cpath.Resolved.module_ =
   fun ident_map p ->
    let recurse p = resolved_module_path ident_map p in
    match p with
    | `Identifier i -> identifier ident_map.modules i
    | `Module (p, name) -> `Module (`Module (recurse p), name)
    | `Apply (p1, p2) -> `Apply (recurse p1, module_path ident_map p2)
    | `Alias (p1, p2) -> `Alias (recurse p1, recurse p2)
    | `Subst (p1, p2) ->
        `Subst (resolved_module_type_path ident_map p1, recurse p2)
    | `SubstAlias (p1, p2) -> `SubstAlias (recurse p1, recurse p2)
    | `Canonical (p1, p2) -> `Canonical (recurse p1, module_path ident_map p2)
    | `Hidden p1 -> `Hidden (recurse p1)

  and resolved_module_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ModuleType.t ->
      Cpath.Resolved.module_type =
   fun ident_map p ->
    match p with
    | `Identifier i -> identifier ident_map.module_types i
    | `ModuleType (p, name) ->
        `ModuleType (`Module (resolved_module_path ident_map p), name)
    | `SubstT (p1, p2) ->
        `SubstT (resolved_module_type_path ident_map p1, resolved_module_type_path ident_map p2)

  and resolved_type_path :
      _ -> Odoc_model.Paths.Path.Resolved.Type.t -> Cpath.Resolved.type_ =
   fun ident_map p ->
    match p with
    | `Identifier i -> identifier ident_map.path_types i
    | `Type (p, name) -> `Type (`Module (resolved_module_path ident_map p), name)
    | `Class (p, name) -> `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) -> `ClassType (`Module (resolved_module_path ident_map p), name)

  and resolved_class_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ClassType.t ->
      Cpath.Resolved.class_type =
   fun ident_map p ->
    match p with
    | `Identifier i -> identifier ident_map.path_class_types i
    | `Class (p, name) -> `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) -> `ClassType (`Module (resolved_module_path ident_map p), name)

  and module_path : _ -> Odoc_model.Paths.Path.Module.t -> Cpath.module_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_path ident_map r)
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
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  and type_path : _ -> Odoc_model.Paths.Path.Type.t -> Cpath.type_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_type_path ident_map r)
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  and class_type_path :
      _ -> Odoc_model.Paths.Path.ClassType.t -> Cpath.class_type =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_class_type_path ident_map r)
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  let rec resolved_signature_fragment : map -> Odoc_model.Paths.Fragment.Resolved.Signature.t -> Cfrag.resolved_signature =
    fun ident_map ty ->
      match ty with
      | `Root (`ModuleType path) -> `Root (`ModuleType (resolved_module_type_path ident_map path))
      | `Root (`Module path) -> `Root (`Module (resolved_module_path ident_map path))
      | `SubstAlias _ | `Subst _ | `Module _ as x -> (resolved_module_fragment ident_map x :> Cfrag.resolved_signature)

  and resolved_module_fragment : _ -> Odoc_model.Paths.Fragment.Resolved.Module.t -> Cfrag.resolved_module =
    fun ident_map ty ->
      match ty with
      | `Subst (p, m) -> `Subst (resolved_module_type_path ident_map p, resolved_module_fragment ident_map m)
      | `SubstAlias (p, m) -> `SubstAlias (resolved_module_path ident_map p, resolved_module_fragment ident_map m)
      | `Module (p, m) -> `Module (resolved_signature_fragment ident_map p, m)
  
  and resolved_type_fragment : _ -> Odoc_model.Paths.Fragment.Resolved.Type.t -> Cfrag.resolved_type =
    fun ident_map ty ->
      match ty with
      | `Type (p, n) -> `Type (resolved_signature_fragment ident_map p, n)
      | `Class (p, n) -> `Class (resolved_signature_fragment ident_map p, n)
      | `ClassType (p, n) -> `ClassType (resolved_signature_fragment ident_map p, n)

  let rec signature_fragment : _ -> Odoc_model.Paths.Fragment.Signature.t -> Cfrag.signature =
    fun ident_map ty ->
      match ty with
      | `Resolved r -> `Resolved (resolved_signature_fragment ident_map r)
      | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)
      | `Root -> `Root
  
  let module_fragment : _ -> Odoc_model.Paths.Fragment.Module.t -> Cfrag.module_ =
    fun ident_map ty ->
      match ty with
      | `Resolved r -> `Resolved (resolved_module_fragment ident_map r)
      | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let type_fragment : _ -> Odoc_model.Paths.Fragment.Type.t -> Cfrag.type_ =
    fun ident_map ty ->
      match ty with
      | `Resolved r -> `Resolved (resolved_type_fragment ident_map r)
      | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)


  let rec type_decl ident_map ty =
    let open Odoc_model.Lang.TypeDecl in
    {
      TypeDecl.doc = ty.doc;
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
    | Extensible -> Extensible

  and type_decl_constructor ident_map t =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    let args = type_decl_constructor_argument ident_map t.args in
    let res = Opt.map (type_expression ident_map) t.res in
    {
      TypeDecl.Constructor.name = Paths.Identifier.name t.id;
      doc = t.doc;
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
      doc = f.doc;
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
                doc = c.doc;
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
    | Constr (p, xs) -> Constr (type_path ident_map p, List.map (type_expression ident_map) xs)
    | Arrow (lbl, t1, t2) ->
        Arrow (lbl, type_expression ident_map t1, type_expression ident_map t2)
    | Tuple ts -> Tuple (List.map (type_expression ident_map) ts)
    | Polymorphic_variant v ->
        Polymorphic_variant (type_expr_polyvar ident_map v)
    | Poly (s, ts) -> Poly (s, type_expression ident_map ts)
    | Alias (t, s) -> Alias (type_expression ident_map t, s)
    | Class (p, ts) ->
        Class
          (class_type_path ident_map p, List.map (type_expression ident_map) ts)
    | Object o -> Object (type_object ident_map o)
    | Package p -> Package (type_package ident_map p)

  and module_decl ident_map m =
    match m with
    | Odoc_model.Lang.Module.Alias p -> Module.Alias (module_path ident_map p)
    | Odoc_model.Lang.Module.ModuleType s ->
        Module.ModuleType (module_type_expr ident_map s)

  and canonical ident_map
      (canonical :
        (Odoc_model.Paths.Path.Module.t * Odoc_model.Paths.Reference.Module.t)
        option) =
    match canonical with
    | Some (p, r) -> Some (module_path ident_map p, r)
    | None -> None

  and module_expansion ident_map f =
    let open Odoc_model.Lang.Module in
    let open Odoc_model.Lang.FunctorParameter in
    match f with
    | AlreadyASig -> Module.AlreadyASig
    | Signature t -> Signature (signature ident_map t)
    | Functor (args, sg) ->
        let rev_args, ident_map =
          List.fold_left
            (fun (args, ident_map) arg ->
              match arg with
              | Named arg ->
                  let identifier = arg.Odoc_model.Lang.FunctorParameter.id in
                  let id = Ident.Of_Identifier.module_ identifier in
                  let ident_map' =
                    {
                      ident_map with
                      modules = (identifier, id) :: ident_map.modules;
                    }
                  in
                  let arg' = functor_parameter ident_map' id arg in
                  (FunctorParameter.Named arg' :: args, ident_map')
              | Unit -> (FunctorParameter.Unit :: args, ident_map))
            ([], ident_map) args
        in
        Functor (List.rev rev_args, signature ident_map sg)

  and module_ ident_map m =
    let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
    let canonical = canonical ident_map m.Odoc_model.Lang.Module.canonical in
    let display_type =
      Opt.map (module_decl ident_map) m.Odoc_model.Lang.Module.display_type
    in
    let expansion = Opt.map (module_expansion ident_map) m.expansion in
    {
      Module.doc = m.doc;
      type_;
      canonical;
      hidden = m.hidden;
      display_type;
      expansion;
    }

  and module_type_substitution ident_map m =
    let open Odoc_model.Lang.ModuleType in
    match m with
    | ModuleEq (frag, decl) ->
        ModuleType.ModuleEq (module_fragment ident_map frag, module_decl ident_map decl)
    | ModuleSubst (frag, p) ->
        ModuleType.ModuleSubst (module_fragment ident_map frag, module_path ident_map p)
    | TypeEq (frag, eqn) -> ModuleType.TypeEq (type_fragment ident_map frag, type_equation ident_map eqn)
    | TypeSubst (frag, eqn) ->
        ModuleType.TypeSubst (type_fragment ident_map frag, type_equation ident_map eqn)

  and functor_parameter ident_map id a =
    let expr' =
      module_type_expr ident_map a.Odoc_model.Lang.FunctorParameter.expr
    in
    let expansion = Opt.map (module_expansion ident_map) a.expansion in
    { FunctorParameter.id; expr = expr'; expansion }

  and extension ident_map e =
    let open Odoc_model.Lang.Extension in
    let type_path = type_path ident_map e.type_path in
    let constructors =
      List.map (extension_constructor ident_map) e.constructors
    in
    {
      Extension.type_path;
      doc = e.doc;
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
      doc = c.doc;
      args;
      res;
    }

  and exception_ ident_map e =
    let open Odoc_model.Lang.Exception in
    let args = type_decl_constructor_argument ident_map e.args in
    let res = Opt.map (type_expression ident_map) e.res in
    { Exception.doc = e.doc; args; res }

  and module_type_expr ident_map m =
    match m with
    | Odoc_model.Lang.ModuleType.Signature s ->
        let s = signature ident_map s in
        ModuleType.Signature s
    | Odoc_model.Lang.ModuleType.Path p ->
        let p' = module_type_path ident_map p in
        ModuleType.Path p'
    | Odoc_model.Lang.ModuleType.With (e, subs) ->
        ModuleType.With
          ( module_type_expr ident_map e,
            List.map (module_type_substitution ident_map) subs )
    | Odoc_model.Lang.ModuleType.Functor (Named arg, expr) ->
        let identifier = arg.Odoc_model.Lang.FunctorParameter.id in
        let id = Ident.Of_Identifier.module_ identifier in
        let ident_map' =
          { ident_map with modules = (identifier, id) :: ident_map.modules }
        in
        let arg' = functor_parameter ident_map' id arg in
        let expr' = module_type_expr ident_map' expr in
        ModuleType.Functor (Named arg', expr')
    | Odoc_model.Lang.ModuleType.Functor (Unit, expr) ->
        let expr' = module_type_expr ident_map expr in
        ModuleType.Functor (Unit, expr')
    | Odoc_model.Lang.ModuleType.TypeOf decl ->
        let decl' = module_decl ident_map decl in
        ModuleType.TypeOf decl'

  and module_type ident_map m =
    let expr =
      Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr
    in
    let expansion = Opt.map (module_expansion ident_map) m.expansion in
    { ModuleType.doc = m.doc; expr; expansion }

  and value ident_map v =
    let type_ = type_expression ident_map v.Odoc_model.Lang.Value.type_ in
    { Value.type_; doc = v.doc }

  and external_ ident_map e =
    let open Odoc_model.Lang.External in
    let type_ = type_expression ident_map e.type_ in
    { External.doc = e.doc; type_; primitives = e.primitives }

  and include_ ident_map i =
    let open Odoc_model.Lang.Include in
    let decl = module_decl ident_map i.decl in
    {
      Include.parent = i.parent;
      doc = i.doc;
      expansion_ = apply_sig_map ident_map i.expansion.content;
      decl;
    }

  and class_ ident_map c =
    let open Odoc_model.Lang.Class in
    let expansion = Opt.map (class_signature ident_map) c.expansion in
    {
      Class.doc = c.doc;
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
      ClassType.doc = t.doc;
      virtual_ = t.virtual_;
      params = t.params;
      expr = class_type_expr ident_map t.expr;
      expansion;
    }

  and class_signature ident_map sg =
    let open Odoc_model.Lang.ClassSignature in
    (* First we construct a list of brand new [Ident.t]s
                for each item in the signature *)
    let ident_map =
      map_of_idents
        (LocalIdents.class_signature sg.items LocalIdents.empty)
        ident_map
    in
    (* Now we construct the Components for each item,
                            converting all paths containing Identifiers pointing at
                            our elements to local paths *)
    apply_class_sig_map ident_map sg

  and apply_class_sig_map ident_map sg =
    let open Odoc_model.Lang.ClassSignature in
    let items =
      List.map
        (function
          | Method m ->
              let id = List.assoc m.id ident_map.methods in
              let m' = method_ ident_map m in
              ClassSignature.Method (id, m')
          | InstanceVariable i ->
              let id = List.assoc i.id ident_map.instance_variables in
              let i' = instance_variable ident_map i in
              ClassSignature.InstanceVariable (id, i')
          | Constraint (t1, t2) ->
              Constraint
                (type_expression ident_map t1, type_expression ident_map t2)
          | Inherit e -> Inherit (class_type_expr ident_map e)
          | Comment c -> Comment c)
        sg.items
    in
    { ClassSignature.self = Opt.map (type_expression ident_map) sg.self; items }

  and method_ ident_map m =
    let open Odoc_model.Lang.Method in
    {
      Method.doc = m.doc;
      private_ = m.private_;
      virtual_ = m.virtual_;
      type_ = type_expression ident_map m.type_;
    }

  and instance_variable ident_map i =
    {
      InstanceVariable.doc = i.doc;
      mutable_ = i.mutable_;
      virtual_ = i.virtual_;
      type_ = type_expression ident_map i.type_;
    }

  and module_substitution ident_map (t : Odoc_model.Lang.ModuleSubstitution.t) =
    {
      ModuleSubstitution.doc = t.doc;
      manifest = module_path ident_map t.manifest;
    }

  and module_of_module_substitution ident_map
      (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let manifest = module_path ident_map t.manifest in
    let canonical =
      Some
        (manifest, `Root (Odoc_model.Names.UnitName.of_string "dummy", `TModule))
    in
    {
      Module.doc = t.doc;
      type_ = Alias manifest;
      canonical;
      hidden = true;
      display_type = None;
      expansion = None;
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

  and apply_sig_map ident_map items =
    let items =
      List.map
        (let open Odoc_model.Lang.Signature in
        function
        | Type (r, t) ->
            let id = List.assoc t.id ident_map.types in
            let t' = Substitution.NoSubst (type_decl ident_map t) in
            Signature.Type (id, r, t')
        | TypeSubstitution t ->
            let id = List.assoc t.id ident_map.types in
            let t' = type_decl ident_map t in
            Signature.TypeSubstitution (id, t')
        | Module (r, m) ->
            let id = List.assoc m.id ident_map.modules in
            let m' = Substitution.NoSubst (module_ ident_map m) in
            Signature.Module (id, r, m')
        | ModuleSubstitution m ->
            let id = List.assoc m.id ident_map.modules in
            let m' = module_substitution ident_map m in
            Signature.ModuleSubstitution (id, m')
        | ModuleType m ->
            let id = List.assoc m.id ident_map.module_types in
            let m' = Substitution.NoSubst (module_type ident_map m) in
            Signature.ModuleType (id, m')
        | Value v ->
            let id = List.assoc v.id ident_map.values in
            let v' = value ident_map v in
            Signature.Value (id, v')
        | Comment c -> Comment c
        | TypExt e -> TypExt (extension ident_map e)
        | Exception e ->
            let id = List.assoc e.id ident_map.exceptions in
            Exception (id, exception_ ident_map e)
        | External e ->
            let id = List.assoc e.id ident_map.values in
            External (id, external_ ident_map e)
        | Class (r, c) ->
            let id = List.assoc c.id ident_map.classes in
            Class (id, r, class_ ident_map c)
        | ClassType (r, c) ->
            let id = List.assoc c.id ident_map.class_types in
            ClassType (id, r, class_type ident_map c)
        | Include i -> Include (include_ ident_map i))
        items
    in
    { items; removed = [] }
end

let module_of_functor_argument (arg : FunctorParameter.parameter) =
  {
    Module.doc = [];
    display_type = None;
    type_ = ModuleType arg.expr;
    canonical = None;
    hidden = false;
    expansion = None;
  }
