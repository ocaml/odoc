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

let show_type_name_verbose h : Paths.Path.Type.t -> _ = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_ident_long
        (Resolved.identifier (t :> Resolved.t))
  | `Identifier (_, b) -> Format.fprintf h "IDENT%b" b
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x

