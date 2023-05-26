open Odoc_model
module ModuleName = Odoc_model.Names.ModuleName
module H = Tyxml.Html

let show_module_name h md =
  Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string md)

let rec show_ident_long h (r : Paths.Identifier.t_pv Paths.Identifier.id) =
  match r.Paths.Identifier.iv with
  | `CoreType n -> Format.fprintf h "Stdlib.%s" (Names.TypeName.to_string n)
  | `Type (md, n) ->
      Format.fprintf h "%a.%s" show_signature md (Names.TypeName.to_string n)
  | _ -> Format.fprintf h "%S" (Paths.Identifier.name r)

and show_ident_short h (r : Paths.Identifier.t_pv Paths.Identifier.id) =
  match r.Paths.Identifier.iv with
  | `Type (_, n) -> Format.fprintf h "%s" (Names.TypeName.to_string n)
  | `CoreType n -> Format.fprintf h "%s" (Names.TypeName.to_string n)
  | _ -> Format.fprintf h "%S" (Paths.Identifier.name r)

and show_module_t h = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_ident_long
        (Resolved.Module.identifier t
          :> Paths.Identifier.t_pv Paths.Identifier.id)
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x
  | `Root x -> Format.fprintf h "%s" x
  | `Apply (m, _) -> Format.fprintf h "%a(_)" show_module_t m
  | `Forward str -> Format.fprintf h "%s" str
  | `Result _ -> ()
  | `Identifier _ -> ()

and show_module_path h = function
  | `Identifier (`Module (_, md)) ->
      Format.fprintf h "<ident module %a>" show_module_name md
  | `Identifier (`Root (_, md)) ->
      Format.fprintf h "<ident root %a>" show_module_name md
  | `Identifier _ -> Format.fprintf h "<ident>"
  | `Subst _ -> Format.fprintf h "<subst>"
  | `Hidden _ -> Format.fprintf h "<hidden>"
  | `Module (pt, md) ->
      Format.fprintf h "<module %a ! %a>" show_module_path pt show_module_name
        md
  | `Canonical (pt, md) ->
      Format.fprintf h "<cano %a...%a>" show_module_path pt show_module_t md
  | `Apply _ -> Format.fprintf h "<apply>"
  | `Alias (pt, md) ->
      Format.fprintf h "<alias %a = %a>" show_module_path pt show_module_path md
  | `OpaqueModule _ -> Format.fprintf h "<opaque>"

and show_signature h sig_ =
  match sig_.iv with
  | `Root (_, name) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string name)
  | `Module (pt, mdl) ->
      Format.fprintf h "%a.%a" show_signature pt show_module_name mdl
  | `Parameter (_, p) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string p)
  | `Result t -> Format.fprintf h "%a" show_signature t
  | `ModuleType (_, p) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleTypeName.to_string p)

let rec full_name_aux : Paths.Identifier.t -> string list =
  let open Names in
  let open Paths.Identifier in
  fun x ->
    match x.iv with
    | `Root (_, name) -> [ ModuleName.to_string name ]
    | `Page (_, name) -> [ PageName.to_string name ]
    | `LeafPage (_, name) -> [ PageName.to_string name ]
    | `Module (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Parameter (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Result x -> full_name_aux (x :> t)
    | `ModuleType (parent, name) ->
        ModuleTypeName.to_string name :: full_name_aux (parent :> t)
    | `Type (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `CoreType name -> [ TypeName.to_string name ]
    | `Constructor (parent, name) ->
        ConstructorName.to_string name :: full_name_aux (parent :> t)
    | `Field (parent, name) ->
        FieldName.to_string name :: full_name_aux (parent :> t)
    | `Extension (parent, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `Exception (parent, name) ->
        ExceptionName.to_string name :: full_name_aux (parent :> t)
    | `CoreException name -> [ ExceptionName.to_string name ]
    | `Value (parent, name) ->
        ValueName.to_string name :: full_name_aux (parent :> t)
    | `Class (parent, name) ->
        ClassName.to_string name :: full_name_aux (parent :> t)
    | `ClassType (parent, name) ->
        ClassTypeName.to_string name :: full_name_aux (parent :> t)
    | `Method (parent, name) ->
        MethodName.to_string name :: full_name_aux (parent :> t)
    | `InstanceVariable (parent, name) ->
        InstanceVariableName.to_string name :: full_name_aux (parent :> t)
    | `Label (parent, name) ->
        LabelName.to_string name :: full_name_aux (parent :> t)

let fullname : [< Paths.Identifier.t_pv ] Paths.Identifier.id -> string list =
 fun n -> List.rev @@ full_name_aux (n :> Paths.Identifier.t)

let prefixname : [< Paths.Identifier.t_pv ] Paths.Identifier.id -> string =
 fun n ->
  match full_name_aux (n :> Paths.Identifier.t) with
  | [] -> ""
  | _ :: q -> String.concat "." q

let show_type_name_verbose h : Paths.Path.Type.t -> _ = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_ident_long
        (Resolved.identifier (t :> Resolved.t))
  | `Identifier (path, _hidden) ->
      let name = fullname (path :> Paths.Identifier.t) |> String.concat "." in
      Format.fprintf h "%s" name
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x
