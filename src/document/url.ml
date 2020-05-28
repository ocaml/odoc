open Result
open Odoc_model.Paths
open Odoc_model.Names
module Root = Odoc_model.Root

let functor_arg_pos id =
  let rec inner = function
    | `Parameter (p, _) -> inner_sig p
    | _ -> failwith "wtf"
  and inner_sig = function
    | `Result p -> 1 + inner_sig p
    | `Module _
    | `ModuleType _
    | `Root _
    | `Parameter _ -> 1
  in inner id

let render_path : Odoc_model.Paths.Path.t -> string =
  let open Odoc_model.Paths.Path in
  let rec render_resolved : Odoc_model.Paths.Path.Resolved.t -> string =
    let open Resolved in
    function
    | `Identifier id -> Identifier.name id
    | `OpaqueModule p -> render_resolved (p :> t)
    | `OpaqueModuleType p -> render_resolved (p :> t)
    | `Subst (_, p) -> render_resolved (p :> t)
    | `SubstAlias (_, p) -> render_resolved (p :> t)
    | `SubstT (_, p) -> render_resolved (p :> t)
    | `Alias (p1, p2) ->
      if Odoc_model.Paths.Path.is_hidden (`Resolved (p2 :> t))
      then render_resolved (p1 :> t)
      else render_resolved (p2 :> t)
    | `Hidden p -> render_resolved (p :> t)
    | `Module (p, s) -> render_resolved (p :> t) ^ "." ^ (ModuleName.to_string s)
    | `Canonical (_, `Resolved p) -> render_resolved (p :> t)
    | `Canonical (p, _) -> render_resolved (p :> t)
    | `Apply (rp, p) -> render_resolved (rp :> t) ^ "(" ^ render_path (p :> Odoc_model.Paths.Path.t) ^ ")"
    | `ModuleType (p, s) -> render_resolved (p :> t) ^ "." ^ (ModuleTypeName.to_string s)
    | `Type (p, s) -> render_resolved (p :> t) ^ "." ^ (TypeName.to_string s)
    | `Class (p, s) -> render_resolved (p :> t) ^ "." ^ (ClassName.to_string s)
    | `ClassType (p, s) -> render_resolved (p :> t) ^ "." ^ (ClassTypeName.to_string s)
  and render_path : Odoc_model.Paths.Path.t -> string =
    function
    | `Root root -> root
    | `Forward root -> root
    | `Dot (prefix, suffix) -> render_path (prefix :> t) ^ "." ^ suffix
    | `Apply (p1, p2) -> render_path (p1 :> t) ^ "(" ^ render_path (p2 :> t) ^ ")"
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
    | Unexpected_anchor s ->
      Printf.sprintf "Unexpected_anchor %S" s
end

let (>>=) x f =
  match x with
  | Ok x -> f x
  | Error _ as e -> e

module Path = struct

  type source = [
    | Identifier.Page.t
    | Identifier.Signature.t
    | Identifier.ClassSignature.t
  ]

  type t = {
    kind : string ;
    parent : t option ;
    name : string ;
  }

  let last t = t.name

  let mk ?parent kind name = { kind ; parent ; name }

  let rec from_identifier : source -> t = function
    | `Root (abstr, unit_name) ->
      let parent = mk "package" abstr.Root.package in
      let kind = "module" in
      let page = UnitName.to_string unit_name in
      mk ~parent kind page
    | `Page (abstr, page_name) ->
      let parent = mk "package" abstr.Root.package in
      let kind = "page" in
      let page = PageName.to_string page_name in
      mk ~parent kind page
    | `Module (parent, mod_name) ->
      let parent = from_identifier (parent :> source) in
      let kind = "module" in
      let page = ModuleName.to_string mod_name in
      mk ~parent kind page
    | `Parameter (functor_id, arg_name) as p ->
      let parent = from_identifier (functor_id :> source) in
      let kind = "argument" in
      let arg_num = functor_arg_pos p in
      let page =
        Printf.sprintf "%d-%s" arg_num (ParameterName.to_string arg_name)
      in
      mk ~parent kind page
    | `ModuleType (parent, modt_name) ->
      let parent = from_identifier (parent :> source) in
      let kind = "module-type" in
      let page = ModuleTypeName.to_string modt_name in
      mk ~parent kind page
    | `Class (parent, name) ->
      let parent = from_identifier (parent :> source) in
      let kind = "class" in
      let page = ClassName.to_string name in
      mk ~parent kind page
    | `ClassType (parent, name) ->
      let parent = from_identifier (parent :> source) in
      let kind = "class-type" in
      let page = ClassTypeName.to_string name in
      mk ~parent kind page
    | `Result p -> from_identifier (p :> source)

  let from_identifier p = from_identifier (p : [< source] :> source)

end


module Anchor = struct

  type t = {
    page : Path.t ;
    anchor : string;
    kind : string;
  }

  let anchorify_path {Path. parent ; name ; kind} =
    match parent with
    | None -> assert false (* We got a root, should never happen *)
    | Some page ->
      let anchor = Printf.sprintf "%s-%s" kind name in
      { page ; anchor ; kind }

  let add_suffix ~kind { page ; anchor ; _ } suffix =
    { page; anchor = anchor ^ "." ^ suffix; kind }

  let rec from_identifier :
    Identifier.t -> (t, Error.t) result =
    let open Error in function
      | `Module (parent, mod_name) ->
        let parent = Path.from_identifier (parent :> Path.source) in
        let kind = "module" in
        let anchor = Printf.sprintf "%s-%s" kind (ModuleName.to_string mod_name) in
        Ok { page = parent; anchor; kind }
      | `Root _ as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page ; kind = "module" ; anchor = "" }
      | `Page _ as p ->
        let page = Path.from_identifier (p :> Path.source) in
        Ok { page ; kind = "page" ; anchor = "" }
      (* For all these identifiers, page names and anchors are the same *)
      | `Parameter _
      | `Result _
      | `ModuleType _
      | `Class _
      | `ClassType _ as p ->
        Ok (anchorify_path @@ Path.from_identifier p)
      | `Type (parent, type_name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "type" in
        Ok { page;
          anchor = Printf.sprintf "%s-%s" kind (TypeName.to_string type_name);
          kind
        }
      | `CoreType ty_name ->
        Error (Not_linkable ("core_type:"^ (TypeName.to_string ty_name)))
      | `Extension (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "extension" in
        Ok { page;
          anchor = Printf.sprintf "%s-%s" kind (ExtensionName.to_string name);
          kind }
      | `Exception (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "exception" in
        Ok { page;
          anchor = Printf.sprintf "%s-%s" kind (ExceptionName.to_string name);
          kind }
      | `CoreException name ->
        Error (Not_linkable ("core_exception:" ^ (ExceptionName.to_string name)))
      | `Value (parent, name) ->
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "val" in
        Ok { page;
          anchor = Printf.sprintf "%s-%s" kind (ValueName.to_string name);
          kind }
      | `Method (parent, name) ->
        let str_name = MethodName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "method" in
        Ok { page; anchor = Printf.sprintf "%s-%s" kind str_name; kind }
      | `InstanceVariable (parent, name) ->
        let str_name = InstanceVariableName.to_string name in
        let page = Path.from_identifier (parent :> Path.source) in
        let kind = "val" in
        Ok { page; anchor = Printf.sprintf "%s-%s" kind str_name; kind }
      | `Constructor (parent, name) ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = "constructor" in
        let suffix = ConstructorName.to_string name in
        Ok (add_suffix ~kind page suffix)
      | `Field (parent, name) ->
        from_identifier (parent :> Identifier.t) >>= fun page ->
        let kind = "field" in
        let suffix = FieldName.to_string name in
        Ok (add_suffix ~kind page suffix)
      | `Label (parent, anchor') ->
        let anchor = LabelName.to_string anchor' in
        from_identifier (parent :> Identifier.t)
        >>= function
        | { page; anchor = _; kind } ->
          (* Really ad-hoc and shitty, but it works. *)
          if kind = "page" then Ok { page; anchor; kind }
          else Ok {page; anchor; kind = "" }
        (* | _ ->
          Error (Unexpected_anchor ("label " ^ anchor)) *)

  let polymorphic_variant ~type_ident elt =
    let name_of_type_constr te =
      match te with
      | Odoc_model.Lang.TypeExpr.Constr (path, _) ->
        render_path (path :> Odoc_model.Paths.Path.t)
      | _ ->
        invalid_arg "DocOckHtml.Url.Polymorphic_variant_decl.name_of_type_constr"
    in
    match from_identifier type_ident with
    | Error e -> failwith (Error.to_string e)
    | Ok url ->
      match elt with
      | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
        let kind = "type" in
        let suffix = name_of_type_constr te in
        add_suffix ~kind url suffix
      | Constructor {name; _} ->
        let kind = "constructor" in
        let suffix = name in
        add_suffix ~kind url suffix
end

type t = Anchor.t

let from_path page = {Anchor. page ; anchor = "" ; kind = page.kind }

let page x = x.Anchor.page

let from_identifier ~stop_before = function
  | #Path.source as p when not stop_before ->
    Ok (from_path @@ Path.from_identifier p)
  | p ->
    Anchor.from_identifier p

let from_identifier_exn ~stop_before id =
  match from_identifier ~stop_before id with
  | Error e -> failwith (Error.to_string e)
  | Ok url -> url

let kind id =
  match Anchor.from_identifier id with
  | Error e -> failwith (Error.to_string e)
  | Ok { kind; _ } -> kind


(* module Of_path = struct
 *   let rec to_html : stop_before:bool -> Path.t -> _ =
 *     fun ~stop_before path ->
 *       match path with
 *       | `Root root -> [ Html.txt root ]
 *       | `Forward root -> [ Html.txt root ] (\* FIXME *\)
 *       | `Dot (prefix, suffix) ->
 *         let link = to_html ~stop_before:true (prefix :> Path.t) in
 *         link @ [ Html.txt ("." ^ suffix) ]
 *       | `Apply (p1, p2) ->
 *         let link1 = to_html ~stop_before (p1 :> Path.t) in
 *         let link2 = to_html ~stop_before (p2 :> Path.t) in
 *         link1 @ Html.txt "(":: link2 @ [ Html.txt ")" ]
 *       | `Resolved rp ->
 *         let id = Path.Resolved.identifier rp in
 *         let txt = Url.render_path path in
 *         begin match Id.href ~stop_before id with
 *         | href -> [ Html.a ~a:[ Html.a_href href ] [ Html.txt txt ] ]
 *         | exception Id.Not_linkable -> [ Html.txt txt ]
 *         | exception exn ->
 *           Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
 *           [ Html.txt txt ]
 *         end
 * end *)


  (* module Of_fragment = struct
   *   let dot prefix suffix =
   *     match prefix with
   *     | "" -> suffix
   *     | _  -> prefix ^ "." ^ suffix
   *
   *   let rec render_raw : Fragment.t -> string =
   *     fun fragment ->
   *       match fragment with
   *       | `Resolved rr -> render_resolved rr
   *       | `Dot (prefix, suffix) -> dot (render_raw (prefix :> Fragment.t)) suffix
   *
   *   and render_resolved : Fragment.Resolved.t -> string =
   *     let open Fragment.Resolved in
   *     fun fragment ->
   *       match fragment with
   *       | `Root -> ""
   *       | `Subst (_, rr) -> render_resolved (rr :> t)
   *       | `SubstAlias (_, rr) -> render_resolved (rr :> t)
   *       | `Module (rr, s) -> dot (render_resolved (rr :> t)) (ModuleName.to_string s)
   *       | `Type (rr, s) -> dot (render_resolved (rr :> t)) (TypeName.to_string s)
   *       | `Class (rr, s) -> dot (render_resolved ( rr :> t)) (ClassName.to_string s)
   *       | `ClassType (rr, s) -> dot (render_resolved (rr :> t)) (ClassTypeName.to_string s)
   *
   *   let rec to_html : stop_before:bool ->
   *     Identifier.Signature.t -> Fragment.t -> _ =
   *     fun ~stop_before id fragment ->
   *       let open Fragment in
   *       match fragment with
   *       | `Resolved `Root ->
   *         begin match Id.href ~stop_before:true (id :> Identifier.t) with
   *         | href ->
   *           [Html.a ~a:[Html.a_href href] [Html.txt (Identifier.name id)]]
   *         | exception Id.Not_linkable -> [ Html.txt (Identifier.name id) ]
   *         | exception exn ->
   *           Printf.eprintf "[FRAG] Id.href failed: %S\n%!" (Printexc.to_string exn);
   *           [ Html.txt (Identifier.name id) ]
   *         end
   *       | `Resolved rr ->
   *         let id = Resolved.identifier id (rr :> Resolved.t) in
   *         let txt = render_resolved rr in
   *         begin match Id.href ~stop_before id with
   *         | href ->
   *           [ Html.a ~a:[ Html.a_href href ] [ Html.txt txt ] ]
   *         | exception Id.Not_linkable -> [ Html.txt txt ]
   *         | exception exn ->
   *           Printf.eprintf "[FRAG] Id.href failed: %S\n%!" (Printexc.to_string exn);
   *           [ Html.txt txt ]
   *         end
   *       | `Dot (prefix, suffix) ->
   *         let link = to_html ~stop_before:true id (prefix :> Fragment.t) in
   *         link @ [ Html.txt ("." ^ suffix) ]
   * end *)
