open Result
open Odoc_model.Paths
open Odoc_model.Names
module Root = Odoc_model.Root

let functor_arg_pos (`Parameter (p, _)) =
  let rec inner_sig = function
    | `Result p -> 1 + inner_sig p
    | `Module _ | `ModuleType _ | `Root _ | `Parameter _ -> 1
  in
  inner_sig p

let render_path : Odoc_model.Paths.Path.t -> string =
  let open Odoc_model.Paths.Path in
  let rec render_resolved : Odoc_model.Paths.Path.Resolved.t -> string =
    let open Resolved in
    function
    | `Identifier id -> Identifier.name id
    | `OpaqueModule p -> render_resolved (p :> t)
    | `OpaqueModuleType p -> render_resolved (p :> t)
    | `Subst (_, p) -> render_resolved (p :> t)
    | `SubstT (_, p) -> render_resolved (p :> t)
    | `Alias (p1, p2) ->
        if Odoc_model.Paths.Path.is_hidden (`Resolved (p2 :> t)) then
          render_resolved (p1 :> t)
        else render_resolved (p2 :> t)
    | `AliasModuleType (p1, p2) ->
        if Odoc_model.Paths.Path.is_hidden (`Resolved (p2 :> t)) then
          render_resolved (p1 :> t)
        else render_resolved (p2 :> t)
    | `Hidden p -> render_resolved (p :> t)
    | `Module (p, s) -> render_resolved (p :> t) ^ "." ^ ModuleName.to_string s
    | `Canonical (_, `Resolved p) -> render_resolved (p :> t)
    | `Canonical (p, _) -> render_resolved (p :> t)
    | `CanonicalModuleType (_, `Resolved p) -> render_resolved (p :> t)
    | `CanonicalModuleType (p, _) -> render_resolved (p :> t)
    | `CanonicalType (_, `Resolved p) -> render_resolved (p :> t)
    | `CanonicalType (p, _) -> render_resolved (p :> t)
    | `Apply (rp, p) ->
        render_resolved (rp :> t)
        ^ "("
        ^ render_resolved (p :> Odoc_model.Paths.Path.Resolved.t)
        ^ ")"
    | `ModuleType (p, s) ->
        render_resolved (p :> t) ^ "." ^ ModuleTypeName.to_string s
    | `Type (p, s) -> render_resolved (p :> t) ^ "." ^ TypeName.to_string s
    | `Class (p, s) -> render_resolved (p :> t) ^ "." ^ ClassName.to_string s
    | `ClassType (p, s) ->
        render_resolved (p :> t) ^ "." ^ ClassTypeName.to_string s
  and render_path : Odoc_model.Paths.Path.t -> string = function
    | `Identifier (id, _) -> Identifier.name id
    | `Root root -> root
    | `Forward root -> root
    | `Dot (prefix, suffix) -> render_path (prefix :> t) ^ "." ^ suffix
    | `Apply (p1, p2) ->
        render_path (p1 :> t) ^ "(" ^ render_path (p2 :> t) ^ ")"
    | `Resolved rp -> render_resolved rp
  in
  render_path

module Error = struct
  type nonrec t =
    | Not_linkable of string
    | Uncaught_exn of string
    (* These should basicaly never happen *)
    | Unexpected_anchor of string

  let to_string = function
    | Not_linkable s -> Printf.sprintf "Not_linkable %S" s
    | Uncaught_exn s -> Printf.sprintf "Uncaught_exn %S" s
    | Unexpected_anchor s -> Printf.sprintf "Unexpected_anchor %S" s
end

let ( >>= ) x f = match x with Ok x -> f x | Error _ as e -> e

module Path = struct
  type source =
    [ Identifier.Page.t | Identifier.Signature.t | Identifier.ClassSignature.t ]

  type kind =
    [ `Module
    | `Page
    | `LeafPage
    | `ModuleType
    | `Argument
    | `Class
    | `ClassType
    | `File ]

  let string_of_kind : kind -> string = function
    | `Page -> "page"
    | `Module -> "module"
    | `LeafPage -> "leaf-page"
    | `ModuleType -> "module-type"
    | `Argument -> "argument"
    | `Class -> "class"
    | `ClassType -> "class-type"
    | `File -> "file"

  let pp_kind fmt kind = Format.fprintf fmt "%s" (string_of_kind kind)

  type t = { kind : kind; parent : t option; name : string }

  let mk ?parent kind name = { kind; parent; name }

  let rec from_identifier : source -> t = function
    | `Root (parent, unit_name) ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `Module in
        let page = ModuleName.to_string unit_name in
        mk ?parent kind page
    | `Page (parent, page_name) ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `Page in
        let page = PageName.to_string page_name in
        mk ?parent kind page
    | `LeafPage (parent, page_name) ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `LeafPage in
        let page = PageName.to_string page_name in
        mk ?parent kind page
    | `Module (parent, mod_name) ->
        let parent = from_identifier (parent :> source) in
        let kind = `Module in
        let page = ModuleName.to_string mod_name in
        mk ~parent kind page
    | `Parameter (functor_id, arg_name) as p ->
        let parent = from_identifier (functor_id :> source) in
        let kind = `Argument in
        let arg_num = functor_arg_pos p in
        let page =
          Printf.sprintf "%d-%s" arg_num (ParameterName.to_string arg_name)
        in
        mk ~parent kind page
    | `ModuleType (parent, modt_name) ->
        let parent = from_identifier (parent :> source) in
        let kind = `ModuleType in
        let page = ModuleTypeName.to_string modt_name in
        mk ~parent kind page
    | `Class (parent, name) ->
        let parent = from_identifier (parent :> source) in
        let kind = `Class in
        let page = ClassName.to_string name in
        mk ~parent kind page
    | `ClassType (parent, name) ->
        let parent = from_identifier (parent :> source) in
        let kind = `ClassType in
        let page = ClassTypeName.to_string name in
        mk ~parent kind page
    | `Result p -> from_identifier (p :> source)

  let from_identifier p = from_identifier (p : [< source ] :> source)

  let to_list url =
    let rec loop acc { parent; name; kind } =
      match parent with
      | None -> (kind, name) :: acc
      | Some p -> loop ((kind, name) :: acc) p
    in
    loop [] url

  let of_list l =
    let rec inner parent = function
      | [] -> parent
      | (kind, name) :: xs -> inner (Some { parent; name; kind }) xs
    in
    inner None l

  let split :
      is_dir:(kind -> bool) ->
      (kind * string) list ->
      (kind * string) list * (kind * string) list =
   fun ~is_dir l ->
    let rec inner = function
      | ((kind, _) as x) :: xs when is_dir kind ->
          let dirs, files = inner xs in
          (x :: dirs, files)
      | xs -> ([], xs)
    in
    inner l
end

module Anchor = struct
  type kind =
    [ Path.kind
    | `Section
    | `Type
    | `Extension
    | `ExtensionDecl
    | `Exception
    | `Method
    | `Val
    | `Constructor
    | `Field ]

  let string_of_kind : kind -> string = function
    | #Path.kind as k -> Path.string_of_kind k
    | `Section -> "section"
    | `Type -> "type"
    | `Extension -> "extension"
    | `ExtensionDecl -> "extension-decl"
    | `Exception -> "exception"
    | `Method -> "method"
    | `Val -> "val"
    | `Constructor -> "constructor"
    | `Field -> "field"

  let pp_kind fmt kind = Format.fprintf fmt "%s" (string_of_kind kind)

  type t = { page : Path.t; anchor : string; kind : kind }

  let anchorify_path { Path.parent; name; kind } =
    match parent with
    | None -> assert false (* We got a root, should never happen *)
    | Some page ->
        let anchor = Printf.sprintf "%s-%s" (Path.string_of_kind kind) name in
        { page; anchor; kind = (kind :> kind) }

  let add_suffix ~kind { page; anchor; _ } suffix =
    { page; anchor = anchor ^ "." ^ suffix; kind }

  let mk ~kind parent str_name =
    let page = Path.from_identifier parent in
    Ok { page; anchor = str_name; kind }

  let rec from_identifier : Identifier.t -> (t, Error.t) result =
    let open Error in
    function
    | `Module (parent, mod_name) ->
        let parent = Path.from_identifier (parent :> Path.source) in
        let kind = `Module in
        let anchor =
          Printf.sprintf "%s-%s" (Path.string_of_kind kind)
            (ModuleName.to_string mod_name)
        in
        Ok { page = parent; anchor; kind }
    | `Root _ as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `Module; anchor = "" }
    | `Page _ as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `Page; anchor = "" }
    | `LeafPage _ as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `LeafPage; anchor = "" }
    (* For all these identifiers, page names and anchors are the same *)
    | (`Parameter _ | `Result _ | `ModuleType _ | `Class _ | `ClassType _) as p
      ->
        Ok (anchorify_path @@ Path.from_identifier p)
    | `Type (parent, type_name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Type in
        Ok
          {
            page;
            anchor =
              Format.asprintf "%a-%s" pp_kind kind
                (TypeName.to_string type_name);
            kind;
          }
    | `CoreType ty_name ->
        Error (Not_linkable ("core_type:" ^ TypeName.to_string ty_name))
    | `Extension (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Extension in
        Ok
          {
            page;
            anchor =
              Format.asprintf "%a-%s" pp_kind kind
                (ExtensionName.to_string name);
            kind;
          }
    | `Exception (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Exception in
        Ok
          {
            page;
            anchor =
              Format.asprintf "%a-%s" pp_kind kind
                (ExceptionName.to_string name);
            kind;
          }
    | `CoreException name ->
        Error (Not_linkable ("core_exception:" ^ ExceptionName.to_string name))
    | `Value (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Val in
        Ok
          {
            page;
            anchor =
              Format.asprintf "%a-%s" pp_kind kind (ValueName.to_string name);
            kind;
          }
    | `Method (parent, name) ->
        let str_name = MethodName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Method in
        Ok
          { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | `InstanceVariable (parent, name) ->
        let str_name = InstanceVariableName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Val in
        Ok
          { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | `Constructor (parent, name) ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = `Constructor in
        let suffix = ConstructorName.to_string name in
        Ok (add_suffix ~kind page suffix)
    | `Field (parent, name) ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = `Field in
        let suffix = FieldName.to_string name in
        Ok (add_suffix ~kind page suffix)
    | `Label (parent, anchor) -> (
        let str_name = LabelName.to_string anchor in
        (* [Identifier.LabelParent.t] contains datatypes. [`CoreType] can't
           happen, [`Type] may not happen either but just in case, use the
           grand-parent. *)
        match parent with
        | #Path.source as parent -> mk ~kind:`Section parent str_name
        | `CoreType _ -> Error (Unexpected_anchor "core_type label parent")
        | `Type (gp, _) -> mk ~kind:`Section gp str_name)

  let polymorphic_variant ~type_ident elt =
    let name_of_type_constr te =
      match te with
      | Odoc_model.Lang.TypeExpr.Constr (path, _) ->
          render_path (path :> Odoc_model.Paths.Path.t)
      | _ ->
          invalid_arg
            "DocOckHtml.Url.Polymorphic_variant_decl.name_of_type_constr"
    in
    match from_identifier type_ident with
    | Error e -> failwith (Error.to_string e)
    | Ok url -> (
        match elt with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
            let kind = `Type in
            let suffix = name_of_type_constr te in
            add_suffix ~kind url suffix
        | Constructor { name; _ } ->
            let kind = `Constructor in
            let suffix = name in
            add_suffix ~kind url suffix)

  (** The anchor looks like
      [extension-decl-"Path.target_type"-FirstConstructor]. *)
  let extension_decl (decl : Odoc_model.Lang.Extension.t) =
    let page = Path.from_identifier (decl.parent :> Path.source) in
    let kind = `ExtensionDecl in
    let first_cons = Identifier.name (List.hd decl.constructors).id in
    let anchor = Format.asprintf "%a-%s" pp_kind kind first_cons in
    { page; kind; anchor }
end

type kind = Anchor.kind

type t = Anchor.t

let from_path page =
  { Anchor.page; anchor = ""; kind = (page.kind :> Anchor.kind) }

let from_identifier ~stop_before = function
  | #Path.source as p when not stop_before ->
      Ok (from_path @@ Path.from_identifier p)
  | p -> Anchor.from_identifier p

let kind id =
  match Anchor.from_identifier id with
  | Error e -> failwith (Error.to_string e)
  | Ok { kind; _ } -> kind
