open Component

type class_type = [ `C of Class.t | `CT of ClassType.t ]

type type_ = [ `T of TypeDecl.t | class_type ]

type value = [ `V of Value.t | `E of External.t ]

type ('a, 'b) found = Found of 'a | Replaced of 'b

let careful_module_in_sig (s : Signature.t) name =
  let rec inner_removed = function
    | Signature.RModule (id, p) :: _ when Ident.Name.module_ id = name ->
        Some (Replaced p)
    | _ :: rest -> inner_removed rest
    | [] -> None
  in
  let rec inner = function
    | Signature.Module (id, _, m) :: _ when Ident.Name.module_ id = name ->
        Some (Found (Ident.Name.module' id, Delayed.get m))
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> inner_removed s.removed
  in
  inner s.items

let careful_type_in_sig (s : Signature.t) name =
  let rec inner_removed = function
    | Signature.RType (id, p) :: _ when Ident.Name.type_ id = name ->
        Some (Replaced (Ident.Name.type' id, p))
    | _ :: rest -> inner_removed rest
    | [] -> None
  in
  let rec inner = function
    | Signature.Type (id, _, m) :: _ when Ident.Name.type_ id = name ->
        Some (Found (`T (Ident.Name.type' id, Component.Delayed.get m)))
    | Signature.Class (id, _, c) :: _ when Ident.Name.class_ id = name ->
        Some (Found (`C (Ident.Name.class' id, c)))
    | Signature.ClassType (id, _, c) :: _ when Ident.Name.class_type id = name
      ->
        Some (Found (`CT (Ident.Name.class_type' id, c)))
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> inner_removed s.removed
  in
  inner s.items

let typename_of_typeid (`LType (n, _) | `LCoreType n) = n

let datatype_in_sig (s : Signature.t) name =
  let rec inner = function
    | Signature.Type (id, _, m) :: _ when Ident.Name.type_ id = name ->
        Some (Component.Delayed.get m)
    | Signature.Include i :: tl -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner tl )
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner s.items

let any_in_type (typ : TypeDecl.t) name =
  let rec find_cons = function
    | ({ TypeDecl.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`Constructor cons)
    | _ :: tl -> find_cons tl
    | [] -> None
  in
  let rec find_field = function
    | ({ TypeDecl.Field.name = name'; _ } as field) :: _ when name' = name ->
        Some (`Field field)
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
        Some (`ExtConstructor (typext, cons))
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner typext.constructors

let any_in_comment d name =
  let rec inner xs =
    match xs with
    | elt :: rest -> (
        match elt.Odoc_model.Location_.value with
        | `Heading (_, label, _) when Ident.Name.label label = name ->
            Some (`Label label)
        | _ -> inner rest )
    | [] -> None
  in
  inner d

let any_in_sig (s : Signature.t) name =
  let module N = Ident.Name in
  let rec inner_removed = function
    | Signature.RModule (id, m) :: _ when N.module_ id = name ->
        Some (`Removed (`Module (id, m)))
    | RType (id, t) :: _ when N.type_ id = name ->
        Some (`Removed (`Type (id, t)))
    | _ :: tl -> inner_removed tl
    | [] -> None
  in
  let rec inner = function
    | Signature.Module (id, rec_, m) :: _ when N.module_ id = name ->
        Some (`Module (id, rec_, m))
    | ModuleSubstitution (id, ms) :: _ when N.module_ id = name ->
        Some (`ModuleSubstitution (id, ms))
    | ModuleType (id, mt) :: _ when N.module_type id = name ->
        Some (`ModuleType (id, mt))
    | Type (id, rec_, t) :: _ when N.type_ id = name ->
        Some (`Type (id, rec_, t))
    | TypeSubstitution (id, ts) :: _ when N.type_ id = name ->
        Some (`TypeSubstitution (id, ts))
    | Exception (id, exc) :: _ when N.exception_ id = name ->
        Some (`Exception (id, exc))
    | Value (id, v) :: _ when N.value id = name ->
        Some (`Value (id, Delayed.get v))
    | External (id, vex) :: _ when N.value id = name ->
        Some (`External (id, vex))
    | Class (id, rec_, c) :: _ when N.class_ id = name ->
        Some (`Class (id, rec_, c))
    | ClassType (id, rec_, ct) :: _ when N.class_type id = name ->
        Some (`ClassType (id, rec_, ct))
    | Include inc :: tl -> (
        match inner inc.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner tl )
    | Type (id, _, t) :: tl -> (
        let typ = Delayed.get t in
        match any_in_type typ name with
        | Some (`Constructor cons) ->
            Some (`Constructor (typename_of_typeid id, typ, cons))
        | Some (`Field field) ->
            Some (`Field (typename_of_typeid id, typ, field))
        | None -> inner tl )
    | TypExt typext :: tl -> (
        match any_in_typext typext name with
        | Some _ as found -> found
        | None -> inner tl )
    | Comment (`Docs d) :: tl -> (
        match any_in_comment d name with
        | Some _ as found -> found
        | None -> inner tl )
    | _ :: tl -> inner tl
    | [] -> inner_removed s.removed
  in
  inner s.items

(** Search a module or module type *)
let signature_in_sig (s : Signature.t) name =
  let module N = Ident.Name in
  let rec inner = function
    | Signature.Module (id, rec_, m) :: _ when N.module_ id = name ->
        Some (`Module (id, rec_, m))
    | ModuleType (id, mt) :: _ when N.module_type id = name ->
        Some (`ModuleType (id, mt))
    | Include inc :: tl -> (
        match inner inc.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner tl )
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner s.items

let module_in_sig s name =
  match careful_module_in_sig s name with
  | Some (Found (_, m)) -> Some m
  | Some (Replaced _) | None -> None

let module_type_in_sig (s : Signature.t) name =
  let rec inner = function
    | Signature.ModuleType (id, m) :: _ when Ident.Name.module_type id = name ->
        Some (Delayed.get m)
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> None
  in
  inner s.items

let opt_module_type_in_sig s name =
  try Some (module_type_in_sig s name) with _ -> None

let opt_value_in_sig s name : value option =
  let rec inner = function
    | Signature.Value (id, m) :: _ when Ident.Name.value id = name ->
        Some (`V (Delayed.get m))
    | Signature.External (id, e) :: _ when Ident.Name.value id = name ->
        Some (`E e)
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some m -> Some m
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> None
  in

  inner s.Signature.items

let type_in_sig s name =
  match careful_type_in_sig s name with
  | Some (Found t) -> Some t
  | Some (Replaced _) | None -> None

let class_type_in_sig (s : Signature.t) name =
  let rec inner = function
    | Signature.Class (id, _, c) :: _ when Ident.Name.class_ id = name ->
        Some (`C c)
    | Signature.ClassType (id, _, c) :: _ when Ident.Name.class_type id = name
      ->
        Some (`CT c)
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> None
  in
  inner s.items

let opt_label_in_sig s name =
  let rec inner = function
    | Signature.Comment (`Docs d) :: rest -> (
        let rec inner' xs =
          match xs with
          | elt :: rest -> (
              match elt.Odoc_model.Location_.value with
              | `Heading (_, label, _) when Ident.Name.label label = name ->
                  Some label
              | _ -> inner' rest )
          | _ -> None
        in
        match inner' d with None -> inner rest | x -> x )
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as x -> x
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> None
  in
  inner s.Signature.items

let find_in_sig sg f =
  let rec inner = function
    | Signature.Include i :: tl -> (
        match inner i.Include.expansion_.items with
        | Some _ as x -> x
        | None -> inner tl )
    | hd :: tl -> ( match f hd with Some _ as x -> x | None -> inner tl )
    | [] -> None
  in
  inner sg.Signature.items

let exception_in_sig s name =
  find_in_sig s (function
    | Signature.Exception (id, e) when Ident.Name.exception_ id = name -> Some e
    | _ -> None)

let extension_in_sig s name =
  let rec inner = function
    | ec :: _ when ec.Extension.Constructor.name = name -> Some ec
    | _ :: tl -> inner tl
    | [] -> None
  in
  find_in_sig s (function
    | Signature.TypExt t -> inner t.Extension.constructors
    | _ -> None)

let label_parent_in_sig s name =
  let module N = Ident.Name in
  find_in_sig s (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`M (Component.Delayed.get m))
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`MT (Component.Delayed.get mt))
    | Type (id, _, t) when N.type_ id = name ->
        Some (`T (Component.Delayed.get t))
    | Class (id, _, c) when N.class_ id = name -> Some (`C c)
    | ClassType (id, _, c) when N.class_type id = name -> Some (`CT c)
    | _ -> None)

let any_in_type_in_sig s name =
  find_in_sig s (function
    | Signature.Type (id, _, t) -> (
        match any_in_type (Component.Delayed.get t) name with
        | Some x -> Some (typename_of_typeid id, x)
        | None -> None )
    | _ -> None)

let find_in_class_signature cs f =
  let rec inner = function
    | ClassSignature.Inherit ct_expr :: tl -> (
        match inner_inherit ct_expr with Some _ as x -> x | None -> inner tl )
    | it :: tl -> ( match f it with Some _ as x -> x | None -> inner tl )
    | [] -> None
  and inner_inherit = function
    | Constr _ -> None
    | Signature cs -> inner cs.items
  in
  inner cs.ClassSignature.items

let any_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.Method (id, m) when Ident.Name.method_ id = name ->
        Some (`Method m)
    | InstanceVariable (id, iv) when Ident.Name.instance_variable id = name ->
        Some (`InstanceVariable iv)
    | _ -> None)

let method_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.Method (id, m) when Ident.Name.method_ id = name -> Some m
    | _ -> None)

let instance_variable_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.InstanceVariable (id, iv)
      when Ident.Name.instance_variable id = name ->
        Some (`InstanceVariable iv)
    | _ -> None)
