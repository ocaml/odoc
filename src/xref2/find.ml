open Odoc_model.Names
open Component
module StringTbl = Hashtbl.Make (struct
  type t = string
  let equal : string -> string -> bool = ( = )
  let hash : string -> int = Hashtbl.hash
end)

type module_ = [ `FModule of ModuleName.t * Module.t Delayed.t ]

type module_type = [ `FModuleType of ModuleTypeName.t * ModuleType.t Delayed.t ]

type datatype = [ `FType of TypeName.t * TypeDecl.t ]

type class_ =
  [ `FClass of ClassName.t * Class.t
  | `FClassType of ClassTypeName.t * ClassType.t ]

type value = [ `FValue of ValueName.t * Value.t Delayed.t ]

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

type removed_type =
  [ `FType_removed of TypeName.t * TypeExpr.t * TypeDecl.Equation.t ]

type careful_module = [ module_ | `FModule_removed of Cpath.Resolved.module_ ]

type careful_module_type =
  [ module_type | `FModuleType_removed of ModuleType.expr ]

type careful_type = [ type_ | removed_type ]

type careful_datatype = [ datatype | removed_type ]

type careful_class = [ class_ | removed_type ]

type careful_any_in_sig =
  [ any_in_sig | careful_module | careful_module_type | removed_type ]

module N = Ident.Name

type sig_ctx = careful_any_in_sig list StringTbl.t
type type_ctx = TypeDecl.t
type typext_ctx = Extension.t
type class_sig_ctx = ClassSignature.t

(** Add a binding to a ['a list StringTbl.t]. If the binding already exists in
    the table, the new value is appended to the corresponding list instead. *)
let tbl_add_multi tbl k v =
  try StringTbl.replace tbl k (v :: StringTbl.find tbl k)
  with Not_found -> StringTbl.add tbl k [ v ]

let items_in_type (typ : TypeDecl.t) =
  let variant_items cons =
    (cons.TypeDecl.Constructor.name, `FConstructor cons)
  in
  let record_items field = (field.TypeDecl.Field.name, `FField field) in
  match typ.representation with
  | Some (Variant cons) -> List.map variant_items cons
  | Some (Record fields) -> List.map record_items fields
  | Some Extensible | None -> []

let context_of_sig sg =
  let tbl : sig_ctx = StringTbl.create 32 in
  let add name item = tbl_add_multi tbl name (item :> careful_any_in_sig) in
  let rec add_sig_item = function
    | Signature.Include i -> List.iter add_sig_item i.Include.expansion_.items
    | Signature.Module (id, _, m) ->
        let n = N.typed_module id in
        add (ModuleName.to_string n) (`FModule (n, m))
    | ModuleSubstitution (id, ms) -> add (N.module_ id) (`FModule_subst ms)
    | ModuleType (id, mt) ->
        let n = N.typed_module_type id in
        add (ModuleTypeName.to_string n) (`FModuleType (n, mt))
    | Type (id, _, t) ->
        let t = Delayed.get t in
        let n = N.type' id in
        add (TypeName.to_string n) (`FType (n, t));
        List.iter
          (fun (name, item) -> add name (`In_type (n, t, item)))
          (items_in_type t)
    | TypeSubstitution (id, ts) -> add (N.type_ id) (`FType_subst ts)
    | Exception (id, exc) ->
        let n = N.typed_exception id in
        add (ExceptionName.to_string n) (`FExn (n, exc))
    | Value (id, v) ->
        let n = N.typed_value id in
        add (ValueName.to_string n) (`FValue (n, v))
    | Class (id, _, c) ->
        let n = N.class' id in
        add (ClassName.to_string n) (`FClass (n, c))
    | ClassType (id, _, ct) ->
        let n = N.class_type' id in
        add (ClassTypeName.to_string n) (`FClassType (n, ct))
    | TypExt typext ->
        List.iter
          (fun (c : Extension.Constructor.t) -> add c.name (`FExt (typext, c)))
          typext.constructors
    | Comment (`Docs d) ->
        List.iter
          (fun elt ->
            match elt.Odoc_model.Location_.value with
            | `Heading lbl ->
                add (Ident.Name.label lbl.Label.label) (`FLabel lbl)
            | _ -> ())
          d
    | ModuleTypeSubstitution _ | Open _ | Comment `Stop -> ()
  in
  let add_removed_item = function
    | Signature.RModule (id, p) -> add (N.module_ id) (`FModule_removed p)
    | RType (id, p, eq) ->
        let n = N.type' id in
        add (TypeName.to_string n) (`FType_removed (n, p, eq))
    | RModuleType (id, p) -> add (N.module_type id) (`FModuleType_removed p)
  in
  (* Removed items are added first, so they get looked up last. Ensures that a
     non-removed module is looked up before a removed module. *)
  List.iter add_removed_item sg.Signature.removed;
  List.iter add_sig_item sg.Signature.items;
  tbl

let context_of_type typ = typ
let context_of_typext ext = ext
let context_of_class_sig csig = csig

let rec find_map f = function
  | hd :: tl -> ( match f hd with Some _ as x -> x | None -> find_map f tl)
  | [] -> None

let rec filter_map f = function
  | hd :: tl -> (
      match f hd with Some x -> x :: filter_map f tl | None -> filter_map f tl)
  | [] -> []

let find_in_sig sg name f =
  try find_map f (StringTbl.find sg name) with Not_found -> None

let filter_in_sig sg name f =
  try filter_map f (StringTbl.find sg name) with Not_found -> []

(** Returns the last element of a list. Used to implement [_unambiguous]
    functions. *)
let rec disambiguate = function
  | [ x ] -> Some x
  | [] -> None
  | _ :: tl -> disambiguate tl

let module_in_sig sg name =
  find_in_sig sg name (function #module_ as x -> Some x | _ -> None)

let type_in_sig sg name =
  find_in_sig sg name (function #type_ as x -> Some x | _ -> None)

let datatype_in_sig sg name =
  find_in_sig sg name (function #datatype as x -> Some x | _ -> None)

let careful_module_in_sig sg name =
  find_in_sig sg name (function #careful_module as x -> Some x | _ -> None)

let careful_module_type_in_sig sg name =
  find_in_sig sg name (function
    | #careful_module_type as x -> Some x
    | _ -> None)

let removed_type_in_sig sg name =
  find_in_sig sg name (function #removed_type as x -> Some x | _ -> None)

let careful_type_in_sig sg name =
  find_in_sig sg name (function #careful_type as x -> Some x | _ -> None)

let careful_datatype_in_sig sg name =
  find_in_sig sg name (function #careful_datatype as x -> Some x | _ -> None)

let class_in_sig sg name =
  filter_in_sig sg name (function #class_ as x -> Some x | _ -> None)

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

let any_in_sig sg name =
  filter_in_sig sg name (function #any_in_sig as x -> Some x | _ -> None)

let signature_in_sig sg name =
  filter_in_sig sg name (function #signature as x -> Some x | _ -> None)

let module_type_in_sig sg name =
  find_in_sig sg name (function #module_type as x -> Some x | _ -> None)

let value_in_sig sg name =
  filter_in_sig sg name (function #value as x -> Some x | _ -> None)

let value_in_sig_unambiguous sg name = disambiguate (value_in_sig sg name)

let label_in_sig sg name =
  filter_in_sig sg name (function #label as x -> Some x | _ -> None)

let exception_in_sig sg name =
  find_in_sig sg name (function #exception_ as x -> Some x | _ -> None)

let extension_in_sig sg name =
  find_in_sig sg name (function #extension as x -> Some x | _ -> None)

let label_parent_in_sig sg name =
  filter_in_sig sg name (function #label_parent as x -> Some x | _ -> None)

let any_in_type_in_sig sg name =
  filter_in_sig sg name (function
    | #any_in_type_in_sig as x -> Some x
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
