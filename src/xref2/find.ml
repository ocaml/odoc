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
        Some (Found (Delayed.get m))
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
        Format.fprintf Format.err_formatter "Found replaced type %a\n%!"
          Ident.fmt id;
        Some (Replaced p)
    | _ :: rest -> inner_removed rest
    | [] -> None
  in
  let rec inner = function
    | Signature.Type (id, _, m) :: _ when Ident.Name.type_ id = name ->
        Some (Found (`T (Component.Delayed.get m)))
    | Signature.Class (id, _, c) :: _ when Ident.Name.class_ id = name ->
        Some (Found (`C c))
    | Signature.ClassType (id, _, c) :: _ when Ident.Name.class_type id = name
      ->
        Some (Found (`CT c))
    | Signature.Include i :: rest -> (
        match inner i.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner rest )
    | _ :: rest -> inner rest
    | [] -> inner_removed s.removed
  in
  inner s.items

let module_in_sig s name =
  match careful_module_in_sig s name with
  | Some (Found m) -> Some m
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
        Some (`V m)
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
