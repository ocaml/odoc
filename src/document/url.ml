open Odoc_model.Paths
open Odoc_model.Names
module Root = Odoc_model.Root

let render_path : Path.t -> string =
  let rec render_resolved : Path.Resolved.t -> string =
    let open Path.Resolved in
    function
    | `Identifier id -> Identifier.name id
    | `CoreType n -> TypeName.to_string n
    | `OpaqueModule p -> render_resolved (p :> t)
    | `OpaqueModuleType p -> render_resolved (p :> t)
    | `Subst (_, p) -> render_resolved (p :> t)
    | `SubstT (_, p) -> render_resolved (p :> t)
    | `Alias (dest, `Resolved src) ->
        if Path.Resolved.(is_hidden (src :> t)) then render_resolved (dest :> t)
        else render_resolved (src :> t)
    | `Alias (dest, src) ->
        if Path.is_hidden (src :> Path.t) then render_resolved (dest :> t)
        else render_path (src :> Path.t)
    | `AliasModuleType (p1, p2) ->
        if Path.Resolved.(is_hidden (p2 :> t)) then render_resolved (p1 :> t)
        else render_resolved (p2 :> t)
    | `Hidden p -> render_resolved (p :> t)
    | `Module (p, s) -> render_resolved (p :> t) ^ "." ^ ModuleName.to_string s
    | `Canonical (_, `Resolved p) -> render_resolved (p :> t)
    | `Canonical (p, _) -> render_resolved (p :> t)
    | `CanonicalModuleType (_, `Resolved p) -> render_resolved (p :> t)
    | `CanonicalModuleType (p, _) -> render_resolved (p :> t)
    | `CanonicalType (_, `Resolved p) -> render_resolved (p :> t)
    | `CanonicalType (p, _) -> render_resolved (p :> t)
    | `Substituted c -> render_resolved (c :> t)
    | `SubstitutedMT c -> render_resolved (c :> t)
    | `SubstitutedT c -> render_resolved (c :> t)
    | `SubstitutedCT c -> render_resolved (c :> t)
    | `Apply (rp, p) ->
        render_resolved (rp :> t)
        ^ "("
        ^ render_resolved (p :> Path.Resolved.t)
        ^ ")"
    | `ModuleType (p, s) ->
        render_resolved (p :> t) ^ "." ^ ModuleTypeName.to_string s
    | `Type (p, s) -> render_resolved (p :> t) ^ "." ^ TypeName.to_string s
    | `Value (p, s) -> render_resolved (p :> t) ^ "." ^ ValueName.to_string s
    | `Class (p, s) -> render_resolved (p :> t) ^ "." ^ TypeName.to_string s
    | `ClassType (p, s) -> render_resolved (p :> t) ^ "." ^ TypeName.to_string s
  and dot p s = render_path (p : Path.Module.t :> Path.t) ^ "." ^ s
  and render_path : Path.t -> string =
   fun x ->
    match x with
    | `Identifier (id, _) -> Identifier.name id
    | `Root root -> ModuleName.to_string root
    | `Forward root -> root
    | `Dot (p, s) -> dot p (ModuleName.to_string s)
    | `DotT (p, s) -> dot p (TypeName.to_string s)
    | `DotMT (p, s) -> dot p (ModuleTypeName.to_string s)
    | `DotV (p, s) -> dot p (ValueName.to_string s)
    | `Apply (p1, p2) ->
        render_path (p1 :> Path.t) ^ "(" ^ render_path (p2 :> Path.t) ^ ")"
    | `Resolved rp -> render_resolved rp
    | `Substituted m -> render_path (m :> Path.t)
    | `SubstitutedMT m -> render_path (m :> Path.t)
    | `SubstitutedT m -> render_path (m :> Path.t)
    | `SubstitutedCT m -> render_path (m :> Path.t)
  in

  render_path

module Path = struct
  type nonsrc_pv =
    [ Identifier.Page.t_pv
    | Identifier.Signature.t_pv
    | Identifier.ClassSignature.t_pv ]

  type any_pv =
    [ nonsrc_pv | Identifier.SourcePage.t_pv | Identifier.AssetFile.t_pv ]

  and any = any_pv Identifier.id

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

  let pp_disambiguating_prefix fmt = function
    | `Module | `Page | `LeafPage | `File | `SourcePage -> ()
    | kind -> Format.fprintf fmt "%s-" (string_of_kind kind)

  type t = { kind : kind; parent : t option; name : string }

  let mk ?parent kind name = { kind; parent; name }

  let rec from_identifier : any -> t =
   fun x ->
    match x with
    | { iv = `Root (parent, unit_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> any))
          | None -> None
        in
        let kind = `Module in
        let name = ModuleName.to_string unit_name in
        mk ?parent kind name
    | { iv = `Page (parent, page_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> any))
          | None -> None
        in
        let kind = `Page in
        let name = PageName.to_string page_name in
        mk ?parent kind name
    | { iv = `LeafPage (parent, page_name); _ } ->
        let parent =
          match parent with
          | Some p -> Some (from_identifier (p :> any))
          | None -> None
        in
        let kind = `LeafPage in
        let name = PageName.to_string page_name in
        mk ?parent kind name
    | { iv = `Module (parent, mod_name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `Module in
        let name = ModuleName.to_string mod_name in
        mk ~parent kind name
    | { iv = `Parameter (functor_id, arg_name); _ } as p ->
        let parent = from_identifier (functor_id :> any) in
        let arg_num = Identifier.FunctorParameter.functor_arg_pos p in
        let kind = `Parameter arg_num in
        let name = ModuleName.to_string arg_name in
        mk ~parent kind name
    | { iv = `ModuleType (parent, modt_name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `ModuleType in
        let name = ModuleTypeName.to_string modt_name in
        mk ~parent kind name
    | { iv = `Class (parent, name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `Class in
        let name = TypeName.to_string name in
        mk ~parent kind name
    | { iv = `ClassType (parent, name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `ClassType in
        let name = TypeName.to_string name in
        mk ~parent kind name
    | { iv = `Result p; _ } -> from_identifier (p :> any)
    | { iv = `SourcePage (parent, name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `SourcePage in
        mk ~parent kind name
    | { iv = `AssetFile (parent, name); _ } ->
        let parent = from_identifier (parent :> any) in
        let kind = `File in
        let name = AssetName.to_string name in
        mk ~parent kind name

  let from_identifier p = from_identifier (p : [< any_pv ] Identifier.id :> any)

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

  let rec is_prefix (url1 : t) (url2 : t) =
    match url1 with
    | { kind = `LeafPage; parent = None; name = "index" } -> true
    | { kind = `LeafPage; parent = Some p; name = "index" } -> is_prefix p url2
    | _ -> (
        if url1 = url2 then true
        else
          match url2 with
          | { parent = Some parent; _ } -> is_prefix url1 parent
          | { parent = None; _ } -> false)
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
    | `UnboxedField
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
    | `UnboxedField -> "unboxed-field"
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
    { page; anchor = str_name; kind }

  (* This is needed to ensure that references to polymorphic constructors have
     links that use the right suffix: those resolved references are turned into
     _constructor_ identifiers. *)
  let suffix_for_constructor x = x

  let rec from_identifier : Identifier.t -> t = function
    | { iv = `Module (parent, mod_name); _ } ->
        let parent = Path.from_identifier (parent :> Path.any) in
        let kind = `Module in
        let anchor =
          Printf.sprintf "%s-%s" (Path.string_of_kind kind)
            (ModuleName.to_string mod_name)
        in
        { page = parent; anchor; kind }
    | { iv = `Root _; _ } as p ->
        let page = Path.from_identifier (p :> Path.any) in
        { page; kind = `Module; anchor = "" }
    | { iv = `Page _; _ } as p ->
        let page = Path.from_identifier (p :> Path.any) in
        { page; kind = `Page; anchor = "" }
    | { iv = `LeafPage _; _ } as p ->
        let page = Path.from_identifier (p :> Path.any) in
        { page; kind = `LeafPage; anchor = "" }
    (* For all these identifiers, page names and anchors are the same *)
    | {
        iv = `Parameter _ | `Result _ | `ModuleType _ | `Class _ | `ClassType _;
        _;
      } as p ->
        anchorify_path @@ Path.from_identifier p
    | { iv = `Type (parent, type_name); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Type in
        {
          page;
          anchor =
            Format.asprintf "%a-%s" pp_kind kind (TypeName.to_string type_name);
          kind;
        }
    | { iv = `Extension (parent, name); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Extension in
        {
          page;
          anchor =
            Format.asprintf "%a-%s" pp_kind kind (ExtensionName.to_string name);
          kind;
        }
    | { iv = `ExtensionDecl (parent, name, _); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `ExtensionDecl in
        {
          page;
          anchor =
            Format.asprintf "%a-%s" pp_kind kind (ExtensionName.to_string name);
          kind;
        }
    | { iv = `Exception (parent, name); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Exception in
        {
          page;
          anchor =
            Format.asprintf "%a-%s" pp_kind kind (ExceptionName.to_string name);
          kind;
        }
    | { iv = `Value (parent, name); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Val in
        {
          page;
          anchor =
            Format.asprintf "%a-%s" pp_kind kind (ValueName.to_string name);
          kind;
        }
    | { iv = `Method (parent, name); _ } ->
        let str_name = MethodName.to_string name in
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Method in
        { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | { iv = `InstanceVariable (parent, name); _ } ->
        let str_name = InstanceVariableName.to_string name in
        let page = Path.from_identifier (parent :> Path.any) in
        let kind = `Val in
        { page; anchor = Format.asprintf "%a-%s" pp_kind kind str_name; kind }
    | { iv = `Constructor (parent, name); _ } ->
        let page = from_identifier (parent :> Identifier.t) in
        let kind = `Constructor in
        let suffix = suffix_for_constructor (ConstructorName.to_string name) in
        add_suffix ~kind page suffix
    | { iv = `Field (parent, name); _ } ->
        let page = from_identifier (parent :> Identifier.t) in
        let kind = `Field in
        let suffix = FieldName.to_string name in
        add_suffix ~kind page suffix
    | { iv = `UnboxedField (parent, name); _ } ->
        let page = from_identifier (parent :> Identifier.t) in
        let kind = `UnboxedField in
        let suffix = UnboxedFieldName.to_string name in
        add_suffix ~kind page suffix
    | { iv = `Label (parent, anchor); _ } -> (
        let str_name = LabelName.to_string anchor in
        (* [Identifier.LabelParent.t] contains datatypes. [`CoreType] can't
           happen, [`Type] may not happen either but just in case, use the
           grand-parent. *)
        match parent with
        | { iv = `Type (gp, _); _ } -> mk ~kind:`Section gp str_name
        | { iv = #Path.nonsrc_pv; _ } as p ->
            mk ~kind:`Section (p :> Path.any) str_name)
    | { iv = `SourceLocation (parent, loc); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        { page; kind = `SourceAnchor; anchor = DefName.to_string loc }
    | { iv = `SourceLocationInternal (parent, loc); _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        { page; kind = `SourceAnchor; anchor = LocalName.to_string loc }
    | { iv = `SourceLocationMod parent; _ } ->
        let page = Path.from_identifier (parent :> Path.any) in
        { page; kind = `SourceAnchor; anchor = "" }
    | { iv = `SourcePage _; _ } as p ->
        let page = Path.from_identifier (p :> Path.any) in
        { page; kind = `Page; anchor = "" }
    | { iv = `AssetFile _; _ } as p ->
        let page = Path.from_identifier p in
        { page; kind = `File; anchor = "" }

  let polymorphic_variant ~type_ident elt =
    let name_of_type_constr te =
      match te with
      | Odoc_model.Lang.TypeExpr.Constr (path, _) ->
          render_path (path :> Odoc_model.Paths.Path.t)
      | _ ->
          invalid_arg
            "DocOckHtml.Url.Polymorphic_variant_decl.name_of_type_constr"
    in
    let url = from_identifier type_ident in
    match elt with
    | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
        let kind = `Type in
        let suffix = name_of_type_constr te in
        add_suffix ~kind url suffix
    | Constructor { name; _ } ->
        let kind = `Constructor in
        let suffix = suffix_for_constructor name in
        add_suffix ~kind url suffix

  (** The anchor looks like
      [extension-decl-"Path.target_type"-FirstConstructor]. *)
  let extension_decl (decl : Odoc_model.Lang.Extension.t) =
    let page = Path.from_identifier (decl.parent :> Path.any) in
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

let from_identifier ~stop_before x =
  match x with
  | { Identifier.iv = #Path.any_pv; _ } as p when not stop_before ->
      from_path @@ Path.from_identifier p
  | p -> Anchor.from_identifier p

let from_asset_identifier p = from_path @@ Path.from_identifier p

let kind id =
  let { Anchor.kind; _ } = Anchor.from_identifier id in
  kind
