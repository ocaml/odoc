open Odoc_model.Names
open Component

type module_ = [ `FModule of ModuleName.t * Module.t ]

type module_type = [ `FModuleType of ModuleTypeName.t * ModuleType.t ]

type datatype = [ `FType of TypeName.t * TypeDecl.t ]

type class_ =
  [ `FClass of ClassName.t * Class.t
  | `FClassType of ClassTypeName.t * ClassType.t ]

type value = [ `FValue of ValueName.t * Value.t ]

type label = [ `FLabel of Label.t ]

type exception_ = [ `FExn of ExceptionName.t * Exception.t ]

type extension = [ `FExt of Extension.t * Extension.Constructor.t ]

type substitution =
  [ `FModule_subst of ModuleSubstitution.t
  | `FType_subst of TypeDecl.t
  | `FModuleType_subst of ModuleTypeSubstitution.t ]

type signature = [ module_ | module_type ]

type type_ = [ datatype | class_ ]

type label_parent = [ signature | type_ ]

type constructor = [ `FConstructor of TypeDecl.Constructor.t ]

type field = [ `FField of TypeDecl.Field.t ]

type any_in_type = [ constructor | field ]

type any_in_type_in_sig =
  [ `In_type of Odoc_model.Names.TypeName.t * TypeDecl.t * any_in_type ]

type any_in_sig =
  [ label_parent
  | value
  | label
  | exception_
  | extension
  | substitution
  | any_in_type_in_sig ]

type instance_variable =
  [ `FInstance_variable of InstanceVariableName.t * InstanceVariable.t ]

type method_ = [ `FMethod of MethodName.t * Method.t ]

type any_in_class_sig = [ instance_variable | method_ ]

module N = Ident.Name

let rec find_map f = function
  | hd :: tl -> ( match f hd with Some _ as x -> x | None -> find_map f tl)
  | [] -> None

let find_in_sig sg f =
  let rec inner f = function
    | Signature.Include i :: tl -> (
        match inner f i.Include.expansion_.items with
        | Some _ as x -> x
        | None -> inner f tl)
    | hd :: tl -> ( match f hd with Some _ as x -> x | None -> inner f tl)
    | [] -> None
  in
  inner f sg.Signature.items

let filter_in_sig sg f =
  let rec inner f = function
    | Signature.Include i :: tl ->
        inner f i.Include.expansion_.items @ inner f tl
    | hd :: tl -> (
        match f hd with Some x -> x :: inner f tl | None -> inner f tl)
    | [] -> []
  in
  inner f sg.Signature.items

(** Returns the last element of a list. Used to implement [_unambiguous]
    functions. *)
let rec disambiguate = function
  | [ x ] -> Some x
  | [] -> None
  | _ :: tl -> disambiguate tl

let module_in_sig sg name =
  find_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`FModule (N.typed_module id, Delayed.get m))
    | _ -> None)

let module_type_in_sig sg name =
  find_in_sig sg (function
    | Signature.ModuleType (id, mt) when N.module_type id = name ->
        Some (`FModuleType (N.typed_module_type id, Delayed.get mt))
    | _ -> None)

let type_in_sig sg name =
  find_in_sig sg (function
    | Signature.Type (id, _, m) when N.type_ id = name ->
        Some (`FType (N.type' id, Delayed.get m))
    | Class (id, _, c) when N.class_ id = name ->
        Some (`FClass (N.class' id, c))
    | ClassType (id, _, c) when N.class_type id = name ->
        Some (`FClassType (N.class_type' id, c))
    | _ -> None)

let datatype_in_sig sg name =
  find_in_sig sg (function
    | Signature.Type (id, _, m) when N.type_ id = name ->
        Some (`FType (N.type' id, Delayed.get m))
    | _ -> None)

type removed_type =
  [ `FType_removed of TypeName.t * TypeExpr.t * TypeDecl.Equation.t ]

type careful_module = [ module_ | `FModule_removed of Cpath.Resolved.module_ ]

type careful_module_type =
  [ module_type | `FModuleType_removed of ModuleType.expr ]

type careful_type = [ type_ | removed_type ]

type careful_datatype = [ datatype | removed_type ]

type careful_class = [ class_ | removed_type ]

let careful_module_in_sig sg name =
  let removed_module = function
    | Signature.RModule (id, p) when N.module_ id = name ->
        Some (`FModule_removed p)
    | _ -> None
  in
  match module_in_sig sg name with
  | Some _ as x -> x
  | None -> find_map removed_module sg.Signature.removed

let careful_module_type_in_sig sg name =
  let removed_module_type = function
    | Signature.RModuleType (id, p) when N.module_type id = name ->
        Some (`FModuleType_removed p)
    | _ -> None
  in
  match module_type_in_sig sg name with
  | Some _ as x -> x
  | None -> find_map removed_module_type sg.Signature.removed

let removed_type_in_sig sg name =
  let removed_type = function
    | Signature.RType (id, p, eq) when N.type_ id = name ->
        Some (`FType_removed (N.type' id, p, eq))
    | _ -> None
  in
  find_map removed_type sg.Signature.removed

let careful_type_in_sig sg name =
  match type_in_sig sg name with
  | Some _ as x -> x
  | None -> removed_type_in_sig sg name

let careful_datatype_in_sig sg name =
  match datatype_in_sig sg name with
  | Some _ as x -> x
  | None -> removed_type_in_sig sg name

let class_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Class (id, _, c) when N.class_ id = name ->
        Some (`FClass (N.class' id, c))
    | Signature.ClassType (id, _, c) when N.class_type id = name ->
        Some (`FClassType (N.class_type' id, c))
    | _ -> None)

let class_in_sig_unambiguous sg name = disambiguate (class_in_sig sg name)

let careful_class_in_sig sg name =
  match class_in_sig_unambiguous sg name with
  | Some _ as x -> x
  | None -> removed_type_in_sig sg name

let constructor_in_type (typ : TypeDecl.t) name =
  let rec find_cons = function
    | ({ TypeDecl.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`FConstructor cons)
    | _ :: tl -> find_cons tl
    | [] -> None
  in
  match typ.representation with
  | Some (Variant cons) -> find_cons cons
  | Some (Record _) | Some Extensible | None -> None

let any_in_type (typ : TypeDecl.t) name =
  let rec find_cons = function
    | ({ TypeDecl.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`FConstructor cons)
    | _ :: tl -> find_cons tl
    | [] -> None
  in
  let rec find_field = function
    | ({ TypeDecl.Field.name = name'; _ } as field) :: _ when name' = name ->
        Some (`FField field)
    | _ :: tl -> find_field tl
    | [] -> None
  in
  match typ.representation with
  | Some (Variant cons) -> find_cons cons
  | Some (Record fields) -> find_field fields
  | Some Extensible | None -> None

let any_in_typext (typext : Extension.t) name =
  let rec inner = function
    | ({ Extension.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`FExt (typext, cons))
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner typext.constructors

let any_in_comment d name =
  let rec inner xs =
    match xs with
    | elt :: rest -> (
        match elt.Odoc_model.Location_.value with
        | `Heading lbl when Ident.Name.label lbl.Label.label = name ->
            Some (`FLabel lbl)
        | _ -> inner rest)
    | [] -> None
  in
  inner d

let any_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`FModule (N.typed_module id, Delayed.get m))
    | ModuleSubstitution (id, ms) when N.module_ id = name ->
        Some (`FModule_subst ms)
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`FModuleType (N.typed_module_type id, Delayed.get mt))
    | Type (id, _, t) when N.type_ id = name ->
        Some (`FType (N.type' id, Delayed.get t))
    | TypeSubstitution (id, ts) when N.type_ id = name -> Some (`FType_subst ts)
    | Exception (id, exc) when N.exception_ id = name ->
        Some (`FExn (N.typed_exception id, exc))
    | Value (id, v) when N.value id = name ->
        Some (`FValue (N.typed_value id, Delayed.get v))
    | Class (id, _, c) when N.class_ id = name ->
        Some (`FClass (N.class' id, c))
    | ClassType (id, _, ct) when N.class_type id = name ->
        Some (`FClassType (N.class_type' id, ct))
    | Type (id, _, t) -> (
        let typ = Delayed.get t in
        match any_in_type typ name with
        | Some r -> Some (`In_type (N.type' id, typ, r))
        | None -> None)
    | TypExt typext -> any_in_typext typext name
    | Comment (`Docs d) -> any_in_comment d name
    | _ -> None)

let signature_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`FModule (N.typed_module id, Delayed.get m))
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`FModuleType (N.typed_module_type id, Delayed.get mt))
    | _ -> None)

let module_type_in_sig sg name =
  find_in_sig sg (function
    | Signature.ModuleType (id, m) when N.module_type id = name ->
        Some (`FModuleType (N.typed_module_type id, Delayed.get m))
    | _ -> None)

let value_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Value (id, m) when N.value id = name ->
        Some (`FValue (N.typed_value id, Delayed.get m))
    | _ -> None)

let value_in_sig_unambiguous sg name = disambiguate (value_in_sig sg name)

let label_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Comment (`Docs d) -> any_in_comment d name
    | _ -> None)

let exception_in_sig sg name =
  find_in_sig sg (function
    | Signature.Exception (id, e) when N.exception_ id = name ->
        Some (`FExn (N.typed_exception id, e))
    | _ -> None)

let extension_in_sig sg name =
  let rec inner t = function
    | ec :: _ when ec.Extension.Constructor.name = name -> Some (`FExt (t, ec))
    | _ :: tl -> inner t tl
    | [] -> None
  in
  find_in_sig sg (function
    | Signature.TypExt t -> inner t t.Extension.constructors
    | _ -> None)

let label_parent_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`FModule (N.typed_module id, Component.Delayed.get m))
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`FModuleType (N.typed_module_type id, Component.Delayed.get mt))
    | Type (id, _, t) when N.type_ id = name ->
        Some (`FType (N.type' id, Component.Delayed.get t))
    | Class (id, _, c) when N.class_ id = name ->
        Some (`FClass (N.class' id, c))
    | ClassType (id, _, c) when N.class_type id = name ->
        Some (`FClassType (N.class_type' id, c))
    | _ -> None)

let any_in_type_in_sig sg name =
  filter_in_sig sg (function
    | Signature.Type (id, _, t) -> (
        let t = Delayed.get t in
        match any_in_type t name with
        | Some x -> Some (`In_type (N.type' id, t, x))
        | None -> None)
    | _ -> None)

let filter_in_class_signature cs f =
  let rec inner = function
    | ClassSignature.Inherit { expr; _ } :: tl -> inner_inherit expr @ inner tl
    | it :: tl -> (
        match f it with Some x -> x :: inner tl | None -> inner tl)
    | [] -> []
  and inner_inherit = function
    | Constr _ -> []
    | Signature cs -> inner cs.items
  in
  inner cs.ClassSignature.items

let find_in_class_signature cs f =
  match filter_in_class_signature cs f with [] -> None | x :: _ -> Some x

let any_in_class_signature cs name =
  filter_in_class_signature cs (function
    | ClassSignature.Method (id, m) when N.method_ id = name ->
        Some (`FMethod (N.typed_method id, m))
    | InstanceVariable (id, iv) when N.instance_variable id = name ->
        Some (`FInstance_variable (N.typed_instance_variable id, iv))
    | _ -> None)

let method_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.Method (id, m) when N.method_ id = name ->
        Some (`FMethod (N.typed_method id, m))
    | _ -> None)

let instance_variable_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.InstanceVariable (id, iv)
      when N.instance_variable id = name ->
        Some (`FInstance_variable (N.typed_instance_variable id, iv))
    | _ -> None)
