open Result
open Odoc_model.Paths
open Odoc_model.Names
module Root = Odoc_model.Root

let functor_arg_pos : Odoc_model.Paths.Identifier.FunctorParameter.t -> int =
  let open Odoc_model.Paths.Identifier in
  fun { iv = `Parameter (p, _); _ } ->
    let rec inner_sig = function
      | `Result { iv = p; _ } -> 1 + inner_sig p
      | `Module _ | `ModuleType _ | `Root _ | `Parameter _ -> 1
    in
    inner_sig p.iv

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
    | `Alias (dest, `Resolved src) ->
        if
          Odoc_model.Paths.Path.Resolved.Module.is_hidden
            ~weak_canonical_test:false src
        then render_resolved (dest :> t)
        else render_resolved (src :> t)
    | `Alias (dest, src) ->
        if Odoc_model.Paths.Path.is_hidden (src :> Path.t) then
          render_resolved (dest :> t)
        else render_path (src :> Path.t)
    | `AliasModuleType (p1, p2) ->
        if
          Odoc_model.Paths.Path.Resolved.ModuleType.is_hidden
            ~weak_canonical_test:false p2
        then render_resolved (p1 :> t)
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
  and render_path : Odoc_model.Paths.Path.t -> string =
   fun x ->
    match x with
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
  type source_pv =
    [ Identifier.Page.t_pv
    | Identifier.Signature.t_pv
    | Identifier.ClassSignature.t_pv ]

  and source = source_pv Odoc_model.Paths.Identifier.id

  type kind =
    [ `Module
    | `Page
    | `LeafPage
    | `ModuleType
    | `Parameter of int
    | `Class
    | `ClassType
    | `File
    | `SourcePage ]

  let string_of_kind : kind -> string = function
    | `Page -> "page"
    | `Module -> "module"
    | `LeafPage -> "leaf-page"
    | `ModuleType -> "module-type"
    | `Parameter arg_num -> Printf.sprintf "argument-%d" arg_num
    | `Class -> "class"
    | `ClassType -> "class-type"
    | `File -> "file"
    | `SourcePage -> "source"

  let pp_kind fmt kind = Format.fprintf fmt "%s" (string_of_kind kind)

  type t = { kind : kind; parent : t option; name : string }

  let mk ?parent kind name = { kind; parent; name }

  let rec from_identifier : source -> t =
   fun x ->
    match x with
    | { iv = `Root (parent, unit_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `Module in
        let name = ModuleName.to_string unit_name in
        mk ?parent kind name
    | { iv = `Page (parent, page_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `Page in
        let name = PageName.to_string page_name in
        mk ?parent kind name
    | { iv = `LeafPage (parent, page_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> source))
          | None -> None
        in
        let kind = `LeafPage in
        let name = PageName.to_string page_name in
        mk ?parent kind name
    | { iv = `Module (parent, mod_name); _ } ->
        let parent = from_identifier (parent :> source) in
        let kind = `Module in
        let name = ModuleName.to_string mod_name in
        mk ~parent kind name
    | { iv = `Parameter (functor_id, arg_name); _ } as p ->
        let parent = from_identifier (functor_id :> source) in
        let arg_num = functor_arg_pos p in
        let kind = `Parameter arg_num in
        let name = ModuleName.to_string arg_name in
        mk ~parent kind name
    | { iv = `ModuleType (parent, modt_name); _ } ->
        let parent = from_identifier (parent :> source) in
        let kind = `ModuleType in
        let name = ModuleTypeName.to_string modt_name in
        mk ~parent kind name
    | { iv = `Class (parent, name); _ } ->
        let parent = from_identifier (parent :> source) in
        let kind = `Class in
        let name = ClassName.to_string name in
        mk ~parent kind name
    | { iv = `ClassType (parent, name); _ } ->
        let parent = from_identifier (parent :> source) in
        let kind = `ClassType in
        let name = ClassTypeName.to_string name in
        mk ~parent kind name
    | { iv = `Result p; _ } -> from_identifier (p :> source)

  let from_identifier p =
    from_identifier
      (p : [< source_pv ] Odoc_model.Paths.Identifier.id :> source)

  let rec source_dir_from_identifier id =
    match id.Odoc_model.Paths.Identifier.iv with
    | `SourceRoot container -> from_identifier (container :> source)
    | `SourceDir (parent, name) ->
        let parent = source_dir_from_identifier parent in
        let kind = `Page in
        mk ~parent kind name

  let source_file_from_identifier id =
    let (`SourcePage (parent, name)) = id.Odoc_model.Paths.Identifier.iv in
    let parent = source_dir_from_identifier parent in
    let kind = `SourcePage in
    mk ~parent kind name

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
      is_dir:(kind -> [ `Always | `Never | `IfNotLast ]) ->
      (kind * string) list ->
      (kind * string) list * (kind * string) list =
   fun ~is_dir l ->
    let rec inner dirs = function
      | [ ((kind, _) as x) ] when is_dir kind = `IfNotLast ->
          (List.rev dirs, [ x ])
      | ((kind, _) as x) :: xs when is_dir kind <> `Never ->
          inner (x :: dirs) xs
      | xs -> (List.rev dirs, xs)
    in
    inner [] l
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
    | `Field
    | `SourceAnchor ]

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
    | `SourceAnchor -> "source-anchor"

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
    | { iv = `Module (parent, mod_name); _ } ->
        let parent = Path.from_identifier (parent :> Path.source) in
        let kind = `Module in
        let anchor =
          Printf.sprintf "%s-%s" (Path.string_of_kind kind)
            (ModuleName.to_string mod_name)
        in
        Ok { page = parent; anchor; kind }
    | { iv = `Root _; _ } as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `Module; anchor = "" }
    | { iv = `Page _; _ } as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `Page; anchor = "" }
    | { iv = `LeafPage _; _ } as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page; kind = `LeafPage; anchor = "" }
    (* For all these identifiers, page names and anchors are the same *)
    | {
        iv = `Parameter _ | `Result _ | `ModuleType _ | `Class _ | `ClassType _;
        _;
      } as p ->
        Ok (anchorify_path @@ Path.from_identifier p)
    | { iv = `Type (parent, type_name); _ } ->
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
    | { iv = `CoreType ty_name; _ } ->
        Error (Not_linkable ("core_type:" ^ TypeName.to_string ty_name))
    | { iv = `Extension (parent, name); _ } ->
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
    | { iv = `Exception (parent, name); _ } ->
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
    | { iv = `CoreException name; _ } ->
        Error (Not_linkable ("core_exception:" ^ ExceptionName.to_string name))
    | { iv = `Value (parent, name); _ } ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Val in
        Ok
          {
            page;
            anchor =
              Format.asprintf "%a-%s" pp_kind kind (ValueName.to_string name);
            kind;
          }
    | { iv = `Method (parent, name); _ } ->
        let str_name = MethodName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Method in
        Ok
          { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | { iv = `InstanceVariable (parent, name); _ } ->
        let str_name = InstanceVariableName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = `Val in
        Ok
          { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | { iv = `Constructor (parent, name); _ } ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = `Constructor in
        let suffix = ConstructorName.to_string name in
        Ok (add_suffix ~kind page suffix)
    | { iv = `Field (parent, name); _ } ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = `Field in
        let suffix = FieldName.to_string name in
        Ok (add_suffix ~kind page suffix)
    | { iv = `Label (parent, anchor); _ } -> (
        let str_name = LabelName.to_string anchor in
        (* [Identifier.LabelParent.t] contains datatypes. [`CoreType] can't
           happen, [`Type] may not happen either but just in case, use the
           grand-parent. *)
        match parent with
        | { iv = #Path.source_pv; _ } as parent ->
            mk ~kind:`Section parent str_name
        | { iv = `CoreType _; _ } ->
            Error (Unexpected_anchor "core_type label parent")
        | { iv = `Type (gp, _); _ } -> mk ~kind:`Section gp str_name)

  let source_file_from_identifier id ~anchor =
    let kind = `SourceAnchor in
    let page = Path.source_file_from_identifier id in
    { page; anchor; kind }

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

  let source_anchor path anchor = { page = path; anchor; kind = `SourceAnchor }
end

type kind = Anchor.kind

type t = Anchor.t

let from_path page =
  { Anchor.page; anchor = ""; kind = (page.kind :> Anchor.kind) }

let from_identifier ~stop_before = function
  | { Odoc_model.Paths.Identifier.iv = #Path.source_pv; _ } as p
    when not stop_before ->
      Ok (from_path @@ Path.from_identifier p)
  | p -> Anchor.from_identifier p

let kind id =
  match Anchor.from_identifier id with
  | Error e -> failwith (Error.to_string e)
  | Ok { kind; _ } -> kind
