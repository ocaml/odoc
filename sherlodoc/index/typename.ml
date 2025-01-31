module Path = Odoc_model.Paths.Path
module Identifier = Odoc_model.Paths.Identifier
module TypeName = Odoc_model.Names.TypeName
module ModuleName = Odoc_model.Names.ModuleName

let rec show_ident_long h (r : Identifier.t_pv Identifier.id) =
  match r.iv with
  | `Type (md, n) -> Format.fprintf h "%a.%s" show_signature md (TypeName.to_string n)
  | _ -> Format.fprintf h "%S" (r |> Identifier.fullname |> String.concat ".")

and show_signature h sig_ =
  match sig_.iv with
  | `Root (_, name) -> Format.fprintf h "%s" (ModuleName.to_string name)
  | `Module (pt, mdl) ->
      Format.fprintf h "%a.%s" show_signature pt (ModuleName.to_string mdl)
  | `Parameter (_, p) -> Format.fprintf h "%s" (ModuleName.to_string p)
  | `Result t -> Format.fprintf h "%a" show_signature t
  | `ModuleType (_, p) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleTypeName.to_string p)

let rec show_type_name_verbose h : Path.Type.t -> _ = function
  | `Resolved t ->
      (match Path.Resolved.(identifier (t :> t)) with
       | Some i -> Format.fprintf h "%a" show_ident_long i
       | None ->
           (match t with
            | `CoreType n -> Format.fprintf h "%s" (Odoc_model.Names.TypeName.to_string n)
            | _ -> Format.fprintf h "%s" "Core type"))
  | `Identifier (path, _hidden) ->
      let name = String.concat "." @@ Identifier.fullname path in
      Format.fprintf h "%s" name
  | `DotT (mdl, x) ->
      Format.fprintf
        h
        "%s.%s"
        (Odoc_document.Url.render_path (mdl :> Path.t))
        (Odoc_model.Names.TypeName.to_string x)
  | `SubstitutedT x -> show_type_name_verbose h x

let to_string t = Format.asprintf "%a" show_type_name_verbose t
