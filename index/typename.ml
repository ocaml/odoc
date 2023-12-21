(* This might be hard to maintain but it is useful *)

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
  | _ -> Format.fprintf h "%S" (r |> Paths.Identifier.fullname |> String.concat ".")

and show_module_t h p =
  Format.fprintf
    h
    "%s"
    (Odoc_document.Url.render_path (p : Paths.Path.Module.t :> Paths.Path.t))

and show_signature h sig_ =
  match sig_.iv with
  | `Root (_, name) -> Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string name)
  | `Module (pt, mdl) -> Format.fprintf h "%a.%a" show_signature pt show_module_name mdl
  | `Parameter (_, p) -> Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string p)
  | `Result t -> Format.fprintf h "%a" show_signature t
  | `ModuleType (_, p) ->
    Format.fprintf h "%s" (Odoc_model.Names.ModuleTypeName.to_string p)

let show_type_name_verbose h : Paths.Path.Type.t -> _ = function
  | `Resolved t ->
    let open Paths.Path in
    Format.fprintf h "%a" show_ident_long (Resolved.identifier (t :> Resolved.t))
  | `Identifier (path, _hidden) ->
    let name =
      (path :> Paths.Identifier.t) |> Paths.Identifier.fullname |> String.concat "."
    in
    Format.fprintf h "%s" name
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x

let to_string t = Format.asprintf "%a" show_type_name_verbose t
