open Odoc_model
open Odoc_model.Root
module ModuleName = Odoc_model.Names.ModuleName
module H = Tyxml.Html

let fmt_to_string f =
  let b = Buffer.create 16 in
  let to_b = Format.formatter_of_buffer b in
  f to_b ;
  Format.fprintf to_b "%!" ;
  Buffer.contents b

let string_of_identifier = function
  | `Class (_, n) -> Names.ClassName.to_string n
  | `ClassType (_, n) -> Names.ClassTypeName.to_string n
  | `Constructor (_, n) -> Names.ConstructorName.to_string n
  | `Exception (_, n) -> Names.ExceptionName.to_string n
  | `Extension (_, n) -> Names.ExtensionName.to_string n
  | `Field (_, n) -> Names.FieldName.to_string n
  | `InstanceVariable (_, n) -> Names.InstanceVariableName.to_string n
  | `Label (_, n) -> Names.LabelName.to_string n
  | `Method (_, n) -> Names.MethodName.to_string n
  | `Module (_, n) -> ModuleName.to_string n
  | `ModuleType (_, n) -> Names.ModuleTypeName.to_string n
  | `Type (_, n) -> Names.TypeName.to_string n
  | `Value (_, n) -> Names.ValueName.to_string n
  | _ -> ""

let string_of_resolved = function
  | `Identifier v -> string_of_identifier v
  | r -> string_of_identifier r

let string_of_reference = function
  | `Root (r, _) -> r
  | `Dot (_, n) -> n
  | `Resolved r -> string_of_resolved r
  | r -> string_of_identifier r

let rec string_of_non_link = function
  | `Space -> H.txt " "
  | `Word w -> H.txt w
  | `Code_span s -> H.code [ H.txt s ]
  | `Raw_markup (_, s) -> H.txt s
  | `Styled (_, lst) -> string_of_link_content lst

and string_of_element = function
  | `Styled (_, lst) -> string_of_paragraph lst
  | `Reference (r, _) -> H.code [ H.txt (string_of_reference r) ]
  | `Link (_, r) -> string_of_link_content r
  | `Space -> H.txt " "
  | `Word w -> H.txt w
  | `Code_span s -> H.code [ H.txt s ]
  | `Raw_markup (_, s) -> H.txt s

and string_of_link_content lst =
  H.span
    (List.map (fun r -> string_of_non_link r.Odoc_model.Location_.value) lst)

and string_of_paragraph lst =
  H.span
    (List.map (fun elt -> string_of_element elt.Odoc_model.Location_.value) lst)

let string_of_doc = function
  | `Paragraph p -> Some (H.p [ string_of_paragraph p ])
  | `Heading (_, _, p) -> Some (H.p [ string_of_link_content p ])
  | _ -> None

let string_of_docs lst =
  List.find_map (fun elt -> string_of_doc elt.Odoc_model.Location_.value) lst

let make_root ~module_name ~digest =
  let file = Odoc_file.create_unit ~force_hidden:false module_name in
  Ok { id = `Root (None, ModuleName.make_std module_name); file; digest }

let show_module_name h md =
  Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string md)

let show_module_ident h = function
  | `Module (_, name) -> Format.fprintf h "%S" (Names.ModuleName.to_string name)
  | `Root (_, name) -> Format.fprintf h "%S" (Names.ModuleName.to_string name)
  | _ -> Format.fprintf h "!!module!!"

let rec show_module_t h = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_module_ident (Resolved.Module.identifier t)
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

and show_signature h = function
  | `Root (_, name) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleName.to_string name)
  | `Module (pt, mdl) ->
      Format.fprintf h "%a.%a" show_signature pt show_module_name mdl
  | `Parameter (_, p) ->
      Format.fprintf h "%s" (Odoc_model.Names.ParameterName.to_string p)
  | `Result t -> Format.fprintf h "%a" show_signature t
  | `ModuleType (_, p) ->
      Format.fprintf h "%s" (Odoc_model.Names.ModuleTypeName.to_string p)

let show_ident_verbose h = function
  | `Type (md, n) ->
      Format.fprintf h "%a.%s" show_signature md (Names.TypeName.to_string n)
  | `CoreType n -> Format.fprintf h "Stdlib.%s" (Names.TypeName.to_string n)
  | _ -> Format.fprintf h "show_ident?"

let show_ident_short h = function
  | `Type (_, n) -> Format.fprintf h "%s" (Names.TypeName.to_string n)
  | `CoreType n -> Format.fprintf h "%s" (Names.TypeName.to_string n)
  | _ -> Format.fprintf h "show_ident?"

let show_type_name_verbose h = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_ident_verbose (Resolved.Type.identifier t)
  | `Identifier (_, b) -> Format.fprintf h "IDENT%b" b
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x

let show_type_name_short h = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_ident_short (Resolved.Type.identifier t)
  | `Identifier (_, b) -> Format.fprintf h "IDENT%b" b
  | `Dot (_mdl, x) -> Format.fprintf h "%s" x

let strip ~prefix str =
  if String.starts_with ~prefix str
  then
    String.sub str (String.length prefix)
      (String.length str - String.length prefix)
  else str

let show_type_name ~path h t =
  let blah = fmt_to_string (fun h -> show_type_name_verbose h t) in
  let blah = strip ~prefix:path blah in
  let blah = strip ~prefix:"Stdlib." blah in
  Format.fprintf h "%s" blah

let show_moduletype_ident h = function
  | `ModuleType (_, _) -> Format.fprintf h "ident"
  | _ -> Format.fprintf h "moduletype"

let show_moduletype_name h = function
  | `Resolved t ->
      let open Paths.Path in
      Format.fprintf h "%a" show_moduletype_ident
        (Resolved.ModuleType.identifier t)
  | `Identifier (_, b) -> Format.fprintf h "IDENT%b" b
  | `Dot (mdl, x) -> Format.fprintf h "%a.%s" show_module_t mdl x

let show_label h = function
  | None -> ()
  | Some (Odoc_model.Lang.TypeExpr.Label lbl) -> Format.fprintf h "%s:" lbl
  | Some (Optional lbl) -> Format.fprintf h "?%s:" lbl

let show_type_id h = function
  | `Type (_, name) -> Printf.fprintf h "%s" (Names.TypeName.to_string name)
  | `CoreType name ->
      Printf.fprintf h "(core)%s" (Names.TypeName.to_string name)

let show_type_repr h = function
  | None -> Printf.fprintf h "no repr"
  | Some _ -> Printf.fprintf h "has repr"

let show_functor_param h = function
  | Lang.FunctorParameter.Unit -> Printf.fprintf h "UNIT"
  | Named { id = `Parameter (_, md); expr = _ } ->
      Printf.fprintf h "%s" (Odoc_model.Names.ParameterName.to_string md)

let type_no_parens = function
  | Odoc_model.Lang.TypeExpr.Var _ | Any | Constr _ | Tuple _ -> true
  | _ -> false

let rec show_type ~path ~parens h = function
  | Odoc_model.Lang.TypeExpr.Var x -> Format.fprintf h "'%s" x
  | Any -> Format.fprintf h "_"
  | Arrow (lbl, a, b) ->
      if parens then Format.fprintf h "(" ;
      Format.fprintf h "%a%a -> %a" show_label lbl
        (show_type ~path ~parens:true)
        a
        (show_type ~path ~parens:false)
        b ;
      if parens then Format.fprintf h ")"
  | Constr (name, []) -> Format.fprintf h "%a" (show_type_name ~path) name
  | Constr (name, ([ x ] as args)) when type_no_parens x ->
      Format.fprintf h "%a %a" (show_type_list ~path) args
        (show_type_name ~path) name
  | Constr (name, args) ->
      Format.fprintf h "(%a) %a" (show_type_list ~path) args
        (show_type_name ~path) name
  | Tuple args ->
      Format.fprintf h "(" ;
      show_tuple_list ~path h args ;
      Format.fprintf h ")"
  | Poly (polys, t) ->
      if parens then Format.fprintf h "(" ;
      Format.fprintf h "%a. %a" show_polys polys
        (show_type ~path ~parens:false)
        t ;
      if parens then Format.fprintf h ")"
  | _ -> Format.fprintf h "!!todo!!"

and show_polys h = function
  | [] -> failwith "show_polys: empty list"
  | [ x ] -> Format.fprintf h "'%s" x
  | x :: xs -> Format.fprintf h "'%s %a" x show_polys xs

and show_type_list ~path h = function
  | [] -> failwith "empty list"
  | [ x ] -> show_type ~path ~parens:false h x
  | x :: xs ->
      Format.fprintf h "%a, %a"
        (show_type ~path ~parens:true)
        x (show_type_list ~path) xs

and show_tuple_list ~path h = function
  | [] -> failwith "empty list"
  | [ x ] -> show_type ~path ~parens:true h x
  | x :: xs ->
      Format.fprintf h "%a * %a"
        (show_type ~path ~parens:true)
        x (show_tuple_list ~path) xs

let rec pp_path h = function
  | [] -> Format.fprintf h ""
  | x :: xs -> Format.fprintf h "%a%s." pp_path xs x
