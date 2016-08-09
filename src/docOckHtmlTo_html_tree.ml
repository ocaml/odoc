(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOck
open Types
open Paths

open Tyxml.Html

module Html_tree = DocOckHtmlHtml_tree
module Markup = DocOckHtmlMarkup

let a_href = Html_tree.Relative_link.to_sub_element

let html_dot_magic = List.map ~f:(fun x -> tot @@ toelt x)

module Html_parser = struct
  let mk_attr ((_, local), value) = Xml.string_attrib local value

  let of_string str =
    let source = `String (0, str) in
    let input  = Xmlm.make_input source in
    Xmlm.input_tree input ~data:Xml.pcdata ~el:(fun ((_, name), attrs) subs ->
      let attrs = List.map attrs ~f:mk_attr in
      match subs with
      | [] -> Xml.leaf ~a:attrs name
      | _  -> Xml.node ~a:attrs name subs
    )
    |> tot
end

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep :: tl

let functor_arg_pos { Types.FunctorArgument.id ; _ } =
  match id with
  | Identifier.Argument (_, nb, _) -> nb
  | _ ->
    let id = string_of_sexp @@ Identifier.sexp_of_t (fun _ -> Atom "") id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id)


let string_of_label = function
  | TypeExpr.Label s -> s
  | Optional s -> "?" ^ s

let apply_style (style : Types.Documentation.style option) txt =
  match style with
  | None -> span txt
  | Some Bold  -> b txt
  | Some Italic  -> i txt
  | Some Emphasize  -> em txt
  | Some Center  -> span ~a:[ a_style "text-align:center"] txt
  | Some Left  -> span ~a:[ a_style "text-align:left"] txt
  | Some Right  -> span ~a:[ a_style "text-align:right"] txt
  | Some Superscript  -> sup txt
  | Some Subscript  -> sub txt
  | Some Custom str -> span ~a:[ a_style str ] txt

let documentation (t : _ Types.Documentation.t) =
  (* It is wonderful that although each these [r] is a [Reference.t] the phantom
     type parameters are not the same so we can't merge the branches. *)
  let handle_ref (ref : _ Documentation.reference) =
    match ref with
    | Module r           -> Html_tree.Relative_link.of_reference r
    | ModuleType r       -> Html_tree.Relative_link.of_reference r
    | Type r             -> Html_tree.Relative_link.of_reference r
    | Constructor r      -> Html_tree.Relative_link.of_reference r
    | Field r            -> Html_tree.Relative_link.of_reference r
    | Extension r        -> Html_tree.Relative_link.of_reference r
    | Exception r        -> Html_tree.Relative_link.of_reference r
    | Value r            -> Html_tree.Relative_link.of_reference r
    | Class r            -> Html_tree.Relative_link.of_reference r
    | ClassType r        -> Html_tree.Relative_link.of_reference r
    | Method r           -> Html_tree.Relative_link.of_reference r
    | InstanceVariable r -> Html_tree.Relative_link.of_reference r
    | Element r          -> Html_tree.Relative_link.of_reference r
    | Section r          -> Html_tree.Relative_link.of_reference r
    | Link _
    | Custom (_,_)
      -> [ pcdata "[documentation.handle_ref TODO]" ]
  in
  (* CR trefis: the following is disgusting and doesn't work if we nest styles.
     Just use CSS and forget about b/i/em/... *)
  let rec handle_text ?style text =
    let mk_item txt = li (handle_text txt) in
    List.concat @@ List.map text ~f:(function
      | Types.Documentation.Raw str ->
        [ apply_style style [ pcdata str ] ]
      | Code str -> [ code [ pcdata str ] ]
      | PreCode str -> [ pre [ pcdata str ] ]
      | Verbatim str ->
        (* CR trefis: I don't think this is quite right. *)
        [ pre [ pcdata str ] ]
      | Style (style, txt) -> handle_text ~style txt
      | List elts -> [ul (List.map elts ~f:mk_item)]
      | Enum elts -> [ol (List.map elts ~f:mk_item)]
      | Newline -> [br ()]
      | Title (lvl, _label, txt) ->
        (* CR trefis: don't ignore the label. *)
        begin match lvl with
        | 1 -> [ h1 (html_dot_magic @@ handle_text txt) ]
        | 2 -> [ h2 (html_dot_magic @@ handle_text txt) ]
        | 3 -> [ h3 (html_dot_magic @@ handle_text txt) ]
        | 4 -> [ h4 (html_dot_magic @@ handle_text txt) ]
        | 5 -> [ h5 (html_dot_magic @@ handle_text txt) ]
        | _ -> [ h6 (html_dot_magic @@ handle_text txt) ]
        end
      | Reference (r,_) -> handle_ref r
      | Target (Some "html", str) ->
        let html = Html_parser.of_string str in
        [html]
      | Target (_, str) ->
        (* CR trefis: I treated this as verbatim but the manual says it should
           be ignored. So maybe don't generate anything here? *)
        [ pre [ pcdata str ] ]
      | Special _ -> [pcdata "TODO"]
    )
  in
  match t with
  | Error _ -> p []
  | Ok body -> div ~a:[ a_class ["doc"] ] (handle_text body.text)

let has_doc (t : _ Types.Documentation.t) =
  match t with
  | Ok body -> body.text <> []
  | Error _ -> false

let rec unit ~get_package (t : _ Types.Unit.t) : Html_tree.t =
  let package =
    match t.id with
    | Paths.Identifier.Root (a, _) -> get_package a
    | _ -> assert false
  in
  Html_tree.enter package;
  Html_tree.enter (Identifier.name t.id);
  let header_doc = documentation t.doc in
  let html, subtree =
    match t.content with
    | Module sign -> signature ~get_package sign
    | Pack packed -> pack ~get_package packed, []
  in
  Html_tree.make (div [ header_doc; html ], subtree)

and pack ~get_package (t : _ Types.Unit.Packed.t) =
  div @@ List.map t ~f:(fun x ->
    let modname = Identifier.name x.Unit.Packed.id in
    let dot_mod = "/" ^ modname ^ ".mod" in
    let md_def =
      Markup.def_div (
        Markup.keyword "module " ::
        pcdata modname ::
        pcdata " = " ::
        Html_tree.Relative_link.of_path ~get_package x.path
      )
    in
    Markup.anchor_region_div ~id:dot_mod
      [ div ~a:[ a_class ["mod"] ] [ md_def ] ]
  )

and signature ~get_package (t : _ Types.Signature.t) =
  let html_and_subtrees =
    List.map t ~f:(function
      | Types.Signature.Module md -> module_ ~get_package md
      | ModuleType mty -> module_type ~get_package mty
      | Type td -> type_decl ~get_package td, []
      | TypExt te -> extension ~get_package te, []
      | Exception e -> exn ~get_package e, []
      | Value v -> value ~get_package v, []
      | External e -> external_ ~get_package e, []
      | Class c -> class_ ~get_package c, []
      | ClassType cty -> class_type ~get_package cty, []
      | Include incl -> include_ ~get_package incl
      | Comment (Documentation doc) -> documentation doc, []
      | Comment Stop -> pcdata "", []
    )
  in
  let html, subtrees = List.split html_and_subtrees in
  div html, List.concat subtrees

and functor_argument ~get_package arg =
  let open Types.FunctorArgument in
  let name = Identifier.name arg.id in
  let nb = functor_arg_pos arg in
  let link_name = Printf.sprintf "%s.%d" name nb in
  let dot_a = Printf.sprintf "/%s.moda" link_name in
  let def_div, subtree =
    match arg.expansion with
    | None ->
      Markup.def_div (
        pcdata (Identifier.name arg.id) ::
        pcdata " : " ::
        mty ~get_package (Identifier.signature_of_module arg.id) arg.expr
      ), []
    | Some expansion ->
      Html_tree.enter ~kind:(`Arg) link_name;
      let expansion, subpages as node = module_expansion ~get_package expansion in
      let subtree = Html_tree.make node in
      Html_tree.leave ();
      Markup.def_div (
        a ~a:[ a_href ~kind:`Arg link_name ] [pcdata name] ::
        pcdata " : " ::
        mty ~get_package (Identifier.signature_of_module arg.id) arg.expr
      ), [subtree]
  in
  let region =
    Markup.anchor_region_div ~id:dot_a [div ~a:[ a_class ["moda"] ] [ def_div ]]
  in
  region, subtree

and module_expansion ~get_package (t : _ Types.Module.expansion) =
  match t with
  | Signature sg -> signature ~get_package sg
  | Functor (args, sg) ->
    let sig_html, subpages = signature ~get_package sg in
    let params, params_subpages =
      List.fold_left args ~init:([], []) ~f:(fun (args, subpages as acc) arg ->
        match arg with
        | None -> acc
        | Some arg ->
          let arg, arg_subpages = functor_argument ~get_package arg in
          (arg :: args, arg_subpages @ subpages)
      )
    in
    let html =
      div [
        h3 ~a:[ a_class ["heading"] ] [ pcdata "Parameters" ];
        div params;
        h3 ~a:[ a_class ["heading"] ] [ pcdata "Signature" ];
        sig_html
      ]
    in
    html, params_subpages @ subpages

and module_ ~get_package (t : _ Types.Module.t) =
  let modname = Identifier.name t.id in
  let dot_mod = "/" ^ modname ^ ".mod" in
  let doc = documentation t.doc in
  let md = module_decl ~get_package (Identifier.signature_of_module t.id) t.type_ in
  let modname, expansion, subtree =
    match t.expansion with
    | None -> pcdata modname, None, []
    | Some expansion ->
      Html_tree.enter ~kind:(`Mod) modname;
      let expansion, subpages as node = module_expansion ~get_package expansion in
      let subtree = Html_tree.make node in
      Html_tree.leave ();
      a ~a:[ a_href ~kind:`Mod modname ] [pcdata modname], Some expansion, [subtree]
  in
  let md_def_content = Markup.keyword "module " :: modname :: md in
  let md_def =
    ignore expansion;
    Markup.def_div md_def_content
    (* CR-someday trefis: not sure we want the following, it's noisy and ugly...
       but sometimes useful...
       Some CSS expert might make the result look more pleasant? *)
    (*
    match expansion with
    | None -> Markup.def_div md_def_content
    | Some html -> details (Markup.def_summary @@ html_dot_magic md_def_content) [html]
    *)
  in
  let region =
    Markup.anchor_region_div ~id:dot_mod
      [div ~a:[ a_class ["mod"] ] [ md_def; doc ]]
  in
  region, subtree

and module_decl ~get_package (base : _ Identifier.signature) md =
  begin match md with
  | Alias _ -> pcdata " = "
  | ModuleType _ -> pcdata " : "
  end ::
  module_decl' ~get_package base md

and extract_path_from_mt ~(default: 'a Identifier.signature) =
  let open ModuleType in
  function
  | Path (Path.Resolved r) ->
    Identifier.signature_of_module_type (Path.Resolved.identifier r)
  | With (mt, _) -> extract_path_from_mt ~default mt
  | TypeOf (Module.Alias (Path.Resolved r)) ->
    Identifier.signature_of_module (Path.Resolved.identifier r)
  | TypeOf (Module.ModuleType mt) -> extract_path_from_mt ~default mt
  | _ -> default

and module_decl' ~get_package (base : _ Identifier.signature) = function
  | Alias mod_path -> Html_tree.Relative_link.of_path ~get_package mod_path
  | ModuleType mt -> mty ~get_package (extract_path_from_mt ~default:base mt) mt

and module_type ~get_package (t : _ Types.ModuleType.t) =
  let modname = Identifier.name t.id in
  let dot_modt = "/" ^ modname ^ ".modt" in
  let doc = documentation t.doc in
  let modname, subtree =
    match t.expansion with
    | None -> pcdata modname, []
    | Some expansion ->
      Html_tree.enter ~kind:(`Mty) modname;
      let expansion, subpages as node = module_expansion ~get_package expansion in
      let subtree = Html_tree.make node in
      Html_tree.leave ();
      a ~a:[ a_href ~kind:`Mty modname ] [pcdata modname], [subtree]
  in
  let mty =
    match t.expr with
    | None -> []
    | Some expr ->
      begin match expr with
      | Path _ -> pcdata " = "
      | _ -> pcdata " : "
      end ::
      mty ~get_package (Identifier.signature_of_module_type t.id) expr
  in
  let mty_def =
    Markup.def_div (
      Markup.keyword "module type " ::
      modname ::
      mty
    )
  in
  let content = [ div ~a:[ a_class ["modtype"] ] [ mty_def; doc ] ] in
  Markup.anchor_region_div ~id:dot_modt content, subtree

and mty ~get_package (base : _ Identifier.signature) = function
  | Path mty_path -> Html_tree.Relative_link.of_path ~get_package mty_path
  | Signature _ ->
    [ Markup.keyword "sig" ; pcdata " ... " ; Markup.keyword "end" ]
  | Functor (None, expr) ->
    Markup.keyword "functor" :: pcdata " () " ::
    mty ~get_package base expr
  | Functor (Some arg, expr) ->
    let name =
      let open FunctorArgument in
      let to_print = pcdata @@ Identifier.name arg.id in
      match
        Html_tree.Relative_link.Id.href ~get_package
          ~stop_before:(arg.expansion = None) arg.id
      with
      | exception _ -> to_print
      | href -> a ~a:[ Tyxml.Html.a_href href ] [ to_print ]
    in
    Markup.keyword "functor" ::
    pcdata " (" :: name :: pcdata " : " ::
    mty ~get_package base arg.expr @
    pcdata ") -> " ::
    mty ~get_package base expr
  | With (expr, substitutions) ->
    mty ~get_package base expr @
    Markup.keyword " with " ::
    list_concat_map ~sep:(Markup.keyword " and ") substitutions
      ~f:(substitution ~get_package base)
  | TypeOf md ->
    Markup.keyword "module type of " :: module_decl' ~get_package base md

and substitution ~get_package (base : _ Identifier.signature) = function
  | ModuleEq (frag_mod, md) ->
    Markup.keyword "module " ::
    Html_tree.Relative_link.of_fragment ~get_package ~base
      (Fragment.signature_of_module frag_mod) @
    pcdata " = " ::
    module_decl' ~get_package base md
  | TypeEq (frag_typ, td) ->
    Markup.keyword "type " ::
    format_params td.Types.TypeDecl.Equation.params ::
    Html_tree.Relative_link.of_fragment ~get_package ~base (Fragment.any_sort frag_typ) @
    fst (format_manifest ~get_package td) @
    format_constraints ~get_package td.Types.TypeDecl.Equation.constraints
  | ModuleSubst (frag_mod, mod_path) ->
    Markup.keyword "module " ::
    Html_tree.Relative_link.of_fragment ~get_package ~base (Fragment.signature_of_module frag_mod) @
    pcdata " := " ::
    Html_tree.Relative_link.of_path ~get_package mod_path
  | TypeSubst (frag_typ, vars, typ_path) ->
    let params =
      pcdata begin match vars with
        | [] -> ""
        | [v] -> v ^ " "
        | _ -> "(" ^ String.concat ~sep:", " vars ^ ") "
      end
    in
    Markup.keyword "type " ::
    params ::
    Html_tree.Relative_link.of_fragment ~get_package ~base (Fragment.any_sort frag_typ) @
    pcdata " := " ::
    params ::
    Html_tree.Relative_link.of_path ~get_package typ_path

and constructor : type a. get_package:('b -> string) -> dot_typ:_ ->
  ('b, a) Identifier.t -> _ Types.TypeDecl.Constructor.argument ->
  'b Types.TypeExpr.t option -> _ =
  fun ~get_package ~dot_typ id args _ret_type ->
    let name = Identifier.name id in
    (* CR trefis: handle GADT style constructors properly. *)
    let args =
      match args with
      | Tuple [] -> []
      | Tuple lst ->
        pcdata " of " ::
        list_concat_map lst ~sep:(pcdata " * ") ~f:(type_expr ~get_package)
      | Record fields ->
        pcdata " of " ::
        record ~get_package dot_typ fields
    in
    pcdata name :: args

and format_params params =
  let format_param (desc, variance_opt) =
    let param_desc = match desc with | Types.TypeDecl.Any -> "_" | Var s -> "'" ^ s in
    match variance_opt with
    | None -> param_desc
    | Some Types.TypeDecl.Pos -> "+" ^ param_desc
    | Some Types.TypeDecl.Neg -> "-" ^ param_desc
  in
  pcdata (
    match params with
    | [] -> ""
    | [x] -> format_param x ^ " "
    | lst ->
      let params = String.concat ", " (List.map lst ~f:format_param) in
      "(" ^ params ^ ") "
  )

and format_constraints ~get_package = function
  | [] -> []
  | lst ->
    Markup.keyword " constraint " ::
    list_concat_map lst ~sep:(Markup.keyword " and ") ~f:(fun (t1, t2) ->
      type_expr ~get_package t1 @ pcdata " = " :: type_expr ~get_package t2
    )

and format_manifest ~get_package (equation : _ Types.TypeDecl.Equation.t) =
  let private_ = equation.private_ in
  match equation.manifest with
  | None -> [], private_
  | Some t ->
    let manifest =
      pcdata " = " ::
      (if private_ then Markup.keyword "private " else pcdata "") ::
      type_expr ~get_package t
    in
    manifest, false

and variant ~get_package dot_typ cstrs =
  let constructor id args res =
    let name = Identifier.name id in
    let dot_cons = Printf.sprintf "%s/%s.cons" dot_typ name in
    Markup.anchor_region_div ~id:dot_cons
      (pcdata "| " :: constructor ~get_package ~dot_typ id args res)
  in
  let rows =
    List.map cstrs ~f:(fun cstr ->
      let open Types.TypeDecl.Constructor in
      let lhs = constructor cstr.id cstr.args cstr.res in
      let rhs = documentation cstr.doc in
      tr ~a:[ a_class ["cons"] ] (
        td [ lhs ] ::
        if not (has_doc cstr.doc) then [] else [
          td [pcdata "(*"];
          td [ rhs ];
          td [pcdata "*)"];
        ]
      )
    )
  in
  table rows

and record ~get_package dot_typ fields =
  let field mutable_ id =
    let name = Identifier.name id in
    let dot_fld = Printf.sprintf "%s/%s.fld" dot_typ name in
    Markup.anchor_region_div ~id:dot_fld [
      (if mutable_ then Markup.keyword "mutable " else pcdata "");
      pcdata name
    ]
  in
  let rows =
    List.map fields ~f:(fun fld ->
      let open Types.TypeDecl.Field in
      let lhs = field fld.mutable_ fld.id in
      let rhs = documentation fld.doc in
      tr ~a:[ a_class ["fld"] ] (
        td [ lhs ] ::
        td [ pcdata " : " ] ::
        td (type_expr ~get_package fld.type_ @ [pcdata ";"]) ::
        if not (has_doc fld.doc) then [] else [
          td [pcdata "(*"];
          td [ rhs ];
          td [pcdata "*)"];
        ]
      )
    )
  in
  [pcdata "{"; table rows; pcdata "}"]

and type_decl ~get_package (t : _ Types.TypeDecl.t) =
  let tyname = Identifier.name t.id in
  let dot_typ = "/" ^ tyname ^ ".typ" in
  let params = format_params t.equation.params in
  let constraints = format_constraints ~get_package t.equation.constraints in
  let manifest, need_private = format_manifest ~get_package t.equation in
  let representation =
    match t.representation with
    | None -> []
    | Some repr ->
      pcdata " = " ::
      (if need_private then Markup.keyword "private " else pcdata "") ::
      match repr with
      | Extensible -> [pcdata ".."]
      | Variant cstrs -> [variant ~get_package dot_typ cstrs]
      | Record fields -> record ~get_package dot_typ fields
  in
  let doc = documentation t.doc in
  let tdecl_def =
    Markup.def_div (
      Markup.keyword "type " ::
      params ::
      pcdata tyname ::
      manifest @
      representation @
      constraints
    )
  in
  Markup.anchor_region_div ~id:dot_typ
    [ div ~a:[ a_class ["typ"] ] [ tdecl_def; doc ] ]

and extension ~get_package (t : _ Types.Extension.t) =
  let doc = documentation t.doc in
  let extension =
    Markup.def_div (
      Markup.keyword "type " ::
      Html_tree.Relative_link.of_path ~get_package t.type_path @
      pcdata " += " ::
      list_concat_map t.constructors ~sep:(pcdata " | ")
        ~f:(extension_constructor ~get_package)
    )
  in
  div ~a:[ a_class ["typ"] ] [ extension; doc ]

and extension_constructor ~get_package (t : _ Types.Extension.Constructor.t) =
  (* CR trefis: doc? *)
  let dot_ext = Printf.sprintf "%s.ext" (Identifier.name t.id) in
  constructor ~get_package ~dot_typ:dot_ext t.id t.args t.res

and exn ~get_package (t : _ Types.Exception.t) =
  let dot_exn = Printf.sprintf "%s.exn" (Identifier.name t.id) in
  let cstr = constructor ~get_package ~dot_typ:dot_exn t.id t.args t.res in
  let doc = documentation t.doc in
  let exn = Markup.def_div (Markup.keyword "exn " :: cstr) in
  div ~a:[ a_class ["exn"] ] [ exn; doc ]

and te_variant ~get_package (t : _ Types.TypeExpr.Variant.t) =
  let elements =
    list_concat_map t.elements ~sep:(pcdata " | ") ~f:(function
      | Types.TypeExpr.Variant.Type te -> type_expr ~get_package te
      | Constructor (name, _bool, args) ->
        let a = list_concat_map args ~sep:(pcdata " * ") ~f:(type_expr
                                                               ~get_package) in
        pcdata ("`" ^ name ^ " of ") :: a
    )
  in
  match t.kind with
  | Fixed -> pcdata "[ " :: elements @ [pcdata " ]"]
  | Open -> pcdata "[> " :: elements @ [pcdata " ]"]
  | Closed [] -> pcdata "[< " :: elements @ [pcdata " ]"]
  | Closed lst ->
    let constrs = String.concat " " lst in
    pcdata "[< " :: elements @ [pcdata (" " ^ constrs ^ " ]")]

and te_object ~get_package (t : _ Types.TypeExpr.Object.t) =
  let methods =
    list_concat_map t.methods ~f:(fun { Types.TypeExpr.Object. name; type_ } ->
      pcdata (name ^ " : ") :: type_expr ~get_package type_ @ [pcdata "; "]
    )
  in
  pcdata "< " :: methods @ [pcdata ((if t.open_ then ".. " else "") ^ ">")]

and format_type_path ~get_package ~delim params path =
  match params with
  | [] -> path
  | [param] -> type_expr ~get_package param @ pcdata " " :: path
  | params  ->
    let params = list_concat_map params ~sep:(pcdata ", ") ~f:(type_expr
                                                                 ~get_package) in
    match delim with
    | `parens   -> pcdata "(" :: params @ pcdata ") " :: path
    | `brackets -> pcdata "[" :: params @ pcdata "] " :: path

and type_expr ~get_package (t : _ Types.TypeExpr.t) =
  match t with
  | Var s -> [pcdata ("'" ^ s)]
  | Any  -> [pcdata "_"]
  | Alias (te, alias) ->
    type_expr ~get_package te @ Markup.keyword " as " :: [ pcdata alias ]
  | Arrow (None, src, dst) ->
    type_expr ~get_package src @ pcdata " -> " :: type_expr ~get_package dst
  | Arrow (Some lbl, src, dst) ->
    pcdata (string_of_label lbl ^ ":") :: type_expr ~get_package src @
    pcdata " -> " :: type_expr ~get_package dst
  | Tuple lst -> list_concat_map lst ~sep:(pcdata " * ") ~f:(type_expr
                                                               ~get_package)
  | Constr (path, args) ->
    let link = Html_tree.Relative_link.of_path ~get_package path in
    format_type_path ~get_package ~delim:(`parens) args link
  | Variant v -> te_variant ~get_package v
  | Object o -> te_object ~get_package o
  | Class (path, args) ->
    format_type_path ~get_package ~delim:(`brackets) args (Html_tree.Relative_link.of_path
                                                ~get_package path)
  | Poly (polyvars, t) ->
    pcdata (String.concat " " polyvars ^ ". ") :: type_expr ~get_package t
  | Package pkg ->
    (* CR trefis: TODO substitutions *)
    pcdata "(" :: Markup.keyword "module " ::
    Html_tree.Relative_link.of_path ~get_package pkg.path @ [pcdata ")"]

and value ~get_package (t : _ Types.Value.t) =
  let name = Identifier.name t.id in
  let dot_val = "/" ^ name ^ ".val" in
  let doc = documentation t.doc in
  let value =
    Markup.def_div (
      Markup.keyword "val " ::
      pcdata name ::
      pcdata " : " ::
      type_expr ~get_package t.type_
    )
  in
  Markup.anchor_region_div ~id:dot_val
    [ div ~a:[ a_class ["val"] ] [ value; doc ] ]

and external_ ~get_package (t : _ Types.External.t) =
  let name = Identifier.name t.id in
  let doc = documentation t.doc in
  let external_ =
    Markup.def_div (
      Markup.keyword "external " ::
      pcdata name ::
      pcdata " : " ::
      type_expr ~get_package t.type_ @
      pcdata " = " ::
      List.map t.primitives ~f:(fun p -> pcdata ("\"" ^ p ^ "\""))
    )
  in
  div ~a:[ a_class ["external"] ] [ external_; doc ]

and class_ ~get_package (t : _ Types.Class.t) =
  let name = Identifier.name t.id in
  let doc = documentation t.doc in
  let class_ =
    Markup.def_div [
      Markup.keyword "class ";
      pcdata name;
      (* TODO: complete. *)
    ]
  in
  div ~a:[ a_class ["class"] ] [ class_; doc ]

and class_type ~get_package (t : _ Types.ClassType.t) =
  let name = Identifier.name t.id in
  let doc = documentation t.doc in
  let ctyp =
    Markup.def_div [
      Markup.keyword "class type ";
      pcdata name;
      (* TODO: complete. *)
    ]
  in
  div ~a:[ a_class ["classtype"] ] [ ctyp; doc ]

and include_ ~get_package (t : _ Types.Include.t) =
  let doc = documentation t.doc in
  let included_html, tree = signature ~get_package t.expansion.content in
  let should_be_inlined =
    match t.doc with
    | Ok { tags ; _ } -> List.mem Types.Documentation.Inline ~set:tags
    | _ -> false
  in
  let incl =
    if should_be_inlined then included_html else
      let incl =
        Markup.keyword "include " ::
        module_decl' ~get_package t.parent t.decl
      in
      details (Markup.def_summary @@ html_dot_magic incl) [included_html]
  in
  div ~a:[ a_class ["include"] ] [incl; doc], tree
