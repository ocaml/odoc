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



module Location = Model.Location_
module Paths = Model.Paths
module Comment = Model.Comment
module Lang = Model.Lang
module Html = Tyxml.Html

open Utils



type rendered_item = (Html_types.div_content Html.elt) list
(* [rendered_item] should really be [dt_content], but that is bugged in TyXML
   until https://github.com/ocsigen/tyxml/pull/193 is released. *)



type ('inner, 'outer) text =
  [> `PCDATA | `Span | `A of ([> `PCDATA ] as 'inner) ] as 'outer

let a_href = Html_tree.Relative_link.to_sub_element

let functor_arg_pos { Model.Lang.FunctorArgument.id ; _ } =
  match id with
  | Paths.Identifier.Argument (_, nb, _) -> nb
  | _ ->
    failwith "TODO"
    (* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)

type ('item_kind, 'item) tagged_item = [
  | `Leaf_item of 'item_kind * 'item
  | `Nested_article of 'item
  | `Comment of Comment.docs_or_stop
]

type section = {
  anchor : string;
  text : Comment.link_content;
  children : section list;
}

type toc = section list




module type Syntax = sig
  module Obj : sig
    val close_tag_closed : string

    val close_tag_extendable : string

    val field_separator : string

    val open_tag_closed : string

    val open_tag_extendable : string
  end

  module Type : sig
    val handle_constructor_params :
         ('inner, 'outer) text Html.elt list
      -> ('inner, 'outer) text Html.elt list
      -> ('inner, 'outer) text Html.elt list

    val handle_substitution_params :
         ('inner, 'outer) text Html.elt list
      -> ('inner, 'outer) text Html.elt list
      -> ('inner, 'outer) text Html.elt list

    val handle_format_params : string -> string

    val type_def_semicolon : bool

    val private_keyword : string

    val parenthesize_constructor : bool

    module Variant : sig
      val parenthesize_params : bool
    end

    module Tuple : sig
      val element_separator : string

      val always_parenthesize : bool
    end

    module Record : sig
      val field_separator : string

      val label_value_separator : string
    end

    val var_prefix : string

    val any : string

    val arrow : [> Html_types.span | Html_types.pcdata ] Html.elt

    module Exception : sig
      val semicolon : bool
    end

    module GADT : sig
      val arrow : [> Html_types.span | Html_types.pcdata ] Html.elt
    end

    module External : sig
      val semicolon : bool

      val handle_primitives :
           string list
        -> [< Html_types.code_content_fun > `A `PCDATA `Span] Tyxml_html.elt
           list
    end
  end

  module Mod : sig
    val open_tag : string

    val close_tag : string

    val close_tag_semicolon : bool

    val include_semicolon : bool

    val functor_keyword : bool
  end

  module Class : sig
    val open_tag : string

    val close_tag : string
  end

  module Value : sig
    val variable_keyword : string

    val semicolon : bool
  end
end

(**
   Main module signature.
 *)
module type Html_generator = sig
  module Top_level_markup : sig
    val lay_out :
         item_to_id:('item -> string option)
      -> item_to_spec:('item -> string option)
      -> render_leaf_item:('item -> rendered_item * Comment.docs)
      -> render_nested_article:('item -> rendered_item * Html_tree.t list)
      -> (_, 'item) tagged_item list
      -> rendered_item * toc * Html_tree.t list

    val render_toc :
      toc -> [> Html_types.flow5_without_header_footer] Html.elt list
  end

  module Type_declaration : sig
    val type_decl : Lang.TypeDecl.t -> rendered_item * Comment.docs

    val extension : Lang.Extension.t -> rendered_item * Comment.docs

    val exn : Lang.Exception.t -> rendered_item * Comment.docs

    val format_params :
         ?delim:[`parens | `brackets]
      -> Lang.TypeDecl.param list
      -> [> `PCDATA] Html.elt

    val format_manifest :
         ?compact_variants:bool
      -> Lang.TypeDecl.Equation.t
      -> ('inner, 'outer) text Html.elt list * bool

    val format_constraints :
         (Lang.TypeExpr.t * Lang.TypeExpr.t) list
      -> ('inner, 'outer) text Html.elt list
  end

  module Type_expression : sig
    val type_expr :
         ?needs_parentheses:bool
      -> Lang.TypeExpr.t
      -> ('inner, 'outer) text Html.elt list

    val format_type_path :
         delim:[`parens | `brackets]
      -> Lang.TypeExpr.t list
      -> ('inner, 'outer) text Html.elt list
      -> ('inner, 'outer) text Html.elt list
  end

  module Value : sig
    val value : Lang.Value.t -> rendered_item * Comment.docs

    val external_ : Lang.External.t -> rendered_item * Comment.docs
  end

  module Page : sig
    val compilation_unit :
      ?theme_uri:Html_tree.uri -> Lang.Compilation_unit.t -> Html_tree.t

    val page : ?theme_uri:Html_tree.uri -> Lang.Page.t -> Html_tree.t
  end

  module Class : sig
    val class_ :
         ?theme_uri:Html_tree.uri
      -> Lang.Class.t
      -> Html_types.article_content Html.elt list * Html_tree.t list

    val class_type :
         ?theme_uri:Html_tree.uri
      -> Lang.ClassType.t
      -> Html_types.article_content Html.elt list * Html_tree.t list
  end

  module Module : sig
    val signature :
         ?theme_uri:Html_tree.uri
      -> Lang.Signature.t
      -> Html_types.div_content Html.elt list * toc * Html_tree.t list
  end
end

(**
   Main functor to create an {!To_html_tree.Html_generator}
 *)
module Make (Syn : Syntax) = struct
module Type_expression :
sig
  val type_expr :
    ?needs_parentheses:bool ->
    Lang.TypeExpr.t ->
      (('inner, 'outer) text Html.elt) list

  val format_type_path :
    delim:[ `parens | `brackets ] ->
    Lang.TypeExpr.t list ->
    (('inner, 'outer) text Html.elt) list ->
      (('inner, 'outer) text Html.elt) list
end =
struct
  let rec te_variant
    : 'inner 'outer. Model.Lang.TypeExpr.Variant.t ->
        ('inner, 'outer) text Html.elt list
  = fun (t : Model.Lang.TypeExpr.Variant.t) ->
    let elements =
      list_concat_map t.elements ~sep:(Markup.keyword " | ") ~f:(function
        | Model.Lang.TypeExpr.Variant.Type te -> type_expr te
        | Constructor (name, _bool, args) ->
          let constr = "`" ^ name in
          match args with
          | [] -> [ Html.pcdata constr ]
          | _ ->
            let args =
              list_concat_map args ~sep:(Html.pcdata Syn.Type.Tuple.element_separator) ~f:type_expr
            in
            if Syn.Type.Variant.parenthesize_params
            then Html.pcdata (constr ^ "(") :: args @ [ Html.pcdata ")" ]
            else Html.pcdata (constr ^ " of ") :: args
      )
    in
    match t.kind with
    | Fixed -> Html.pcdata "[ " :: elements @ [Html.pcdata " ]"]
    | Open -> Html.pcdata "[> " :: elements @ [Html.pcdata " ]"]
    | Closed [] -> Html.pcdata "[< " :: elements @ [Html.pcdata " ]"]
    | Closed lst ->
      let constrs = String.concat " " lst in
      Html.pcdata "[< " :: elements @ [Html.pcdata (" " ^ constrs ^ " ]")]

  and te_object
    : 'inner 'outer. Model.Lang.TypeExpr.Object.t ->
        ('inner, 'outer) text Html.elt list
  = fun (t : Model.Lang.TypeExpr.Object.t) ->
    let fields =
      list_concat_map t.fields ~f:(function
        | Model.Lang.TypeExpr.Object.Method {name; type_} ->
            (Html.pcdata (name ^ " : ") :: type_expr type_)
            @ [Html.pcdata Syn.Obj.field_separator]
        | Inherit type_ ->
            type_expr type_ @ [Html.pcdata Syn.Obj.field_separator] )
    in
    let open_tag =
        if t.open_ then Html.pcdata Syn.Obj.open_tag_extendable
        else Html.pcdata Syn.Obj.open_tag_closed
    in
    let close_tag =
        if t.open_ then Html.pcdata Syn.Obj.close_tag_extendable
        else Html.pcdata Syn.Obj.close_tag_closed
    in
    (open_tag :: fields) @ [close_tag]

  and format_type_path
    : 'inner 'outer. delim:[ `parens | `brackets ]
    -> Model.Lang.TypeExpr.t list -> ('inner, 'outer) text Html.elt list
    -> ('inner, 'outer) text Html.elt list
  = fun ~delim params path ->
    match params with
    | [] -> path
    | [param] ->
        let param = (type_expr ~needs_parentheses:true param) in
        let args =
          if Syn.Type.parenthesize_constructor
          then  Html.pcdata "(" :: param @ [Html.pcdata ")"]
          else param
        in
      Syn.Type.handle_constructor_params path args
    | params  ->
      let params =
        list_concat_map params ~sep:(Html.pcdata ",\194\160")
          ~f:type_expr
      in
      let params = match delim with
        | `parens   -> Html.pcdata "(" :: params @ [Html.pcdata ")"]
        | `brackets -> Html.pcdata "[" :: params @ [Html.pcdata "]"]
      in
      Syn.Type.handle_constructor_params path params

  and type_expr
    : 'inner 'outer. ?needs_parentheses:bool
    -> Model.Lang.TypeExpr.t -> ('inner, 'outer) text Html.elt list
  = fun ?(needs_parentheses=false) t ->
    match t with
    | Var s -> [Markup.Type.var (Syn.Type.var_prefix ^ s)]
    | Any  -> [Markup.Type.var Syn.Type.any]
    | Alias (te, alias) ->
      type_expr ~needs_parentheses:true te @
      Markup.keyword " as " :: [ Html.pcdata alias ]
    | Arrow (None, src, dst) ->
      let res =
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Syn.Type.arrow :: Html.pcdata " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
    | Arrow (Some lbl, src, dst) ->
      let res =
        Markup.ML.label lbl @ Html.pcdata ":" ::
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Syn.Type.arrow :: Html.pcdata " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
    | Tuple lst ->
      let res =
        list_concat_map lst ~sep:(Markup.keyword Syn.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:true)
      in
      if Syn.Type.Tuple.always_parenthesize || needs_parentheses then
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
      else
        res
    | Constr (path, args) ->
      let link = Html_tree.Relative_link.of_path ~stop_before:false path in
      format_type_path ~delim:(`parens) args link
    | Variant v -> te_variant v
    | Object o -> te_object o
    | Class (path, args) ->
      format_type_path ~delim:(`brackets) args
        (Html_tree.Relative_link.of_path ~stop_before:false path)
    | Poly (polyvars, t) ->
      Html.pcdata (String.concat " " polyvars ^ ". ") :: type_expr t
    | Package pkg ->
      Html.pcdata "(" :: Markup.keyword "module " ::
      Html_tree.Relative_link.of_path ~stop_before:false pkg.path @
      begin match pkg.substitutions with
      | [] -> []
      | lst ->
        Html.pcdata " " :: Markup.keyword "with" :: Html.pcdata " " ::
        list_concat_map ~sep:(Markup.keyword " and ") lst
          ~f:(package_subst pkg.path)
      end
      @ [Html.pcdata ")"]

  and package_subst
    : 'inner 'outer.
      Paths.Path.module_type -> Paths.Fragment.type_ * Model.Lang.TypeExpr.t
    -> ('inner, 'outer) text Html.elt list
    = fun pkg_path (frag_typ, te) ->
    Markup.keyword "type " ::
    (match pkg_path with
    | Paths.Path.Resolved rp ->
      let base =
        Paths.Identifier.signature_of_module_type
          (Paths.Path.Resolved.identifier rp)
      in
      Html_tree.Relative_link.of_fragment ~base
        (Paths.Fragment.any_sort frag_typ)
    | _ ->
      [Html.pcdata
        (Html_tree.render_fragment (Paths.Fragment.any_sort frag_typ))]) @
    Html.pcdata " " :: Markup.keyword "=" :: Html.pcdata " " ::
    type_expr te
end
open Type_expression



(* Also handles constructor declarations for exceptions and extensible
   variants, and exposes a few helpers used in formatting classes and signature
   constraints. *)
module Type_declaration :
sig
  val type_decl : Lang.TypeDecl.t -> rendered_item * Comment.docs
  val extension : Lang.Extension.t -> rendered_item * Comment.docs
  val exn : Lang.Exception.t -> rendered_item * Comment.docs

  val format_params :
    ?delim:[ `parens | `brackets ] ->
    Lang.TypeDecl.param list ->
      [> `PCDATA ] Html.elt

  val format_manifest :
    ?compact_variants:bool ->
    Lang.TypeDecl.Equation.t ->
      ((('inner, 'outer) text Html.elt) list) * bool

  val format_constraints :
    (Lang.TypeExpr.t * Lang.TypeExpr.t) list ->
      (('inner, 'outer) text Html.elt) list
end =
struct
  let record fields =
    let field mutable_ id typ =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let name = Paths.Identifier.name id in
        let cell =
          Html.td ~a:[ Html.a_class ["def"; kind ] ]
            [Html.a ~a:[Html.a_href ("#" ^ anchor); Html.a_class ["anchor"]] []
            ; Html.code (
                (if mutable_ then Markup.keyword "mutable " else Html.pcdata "")
                :: (Html.pcdata name)
                :: (Html.pcdata Syn.Type.Record.label_value_separator)
                :: (type_expr typ)
                @  [Html.pcdata Syn.Type.Record.field_separator]
              )
            ]
        in
        anchor, cell
    in
    let rows =
      fields |> List.map (fun fld ->
        let open Model.Lang.TypeDecl.Field in
        let anchor, lhs = field fld.mutable_ fld.id fld.type_ in
        let rhs = Documentation.to_html fld.doc in
        let rhs = (rhs :> (Html_types.td_content Html.elt) list) in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] (
          lhs ::
          if not (Documentation.has_doc fld.doc) then [] else [
            Html.td ~a:[ Html.a_class ["doc"] ] rhs
          ]
        )
      )
    in
    [ Html.code [Html.pcdata "{"]
    ; Html.table ~a:[ Html.a_class ["record"] ] rows
    ; Html.code [Html.pcdata "}"]]



  let constructor
    : 'b. 'b Paths.Identifier.t -> Model.Lang.TypeDecl.Constructor.argument
    -> Model.Lang.TypeExpr.t option
    -> [> `Code | `PCDATA | `Table ] Html.elt list
  = fun id args ret_type ->
      let name = Paths.Identifier.name id in
      let cstr =
        Html.span
          ~a:[Html.a_class [Url.kind_of_id_exn id]] [Html.pcdata name]
      in
      let is_gadt, ret_type =
        match ret_type with
        | None -> false, []
        | Some te ->
          let constant =
            match args with
            | Tuple [] -> true
            | _ -> false
          in
          let ret_type =
            Html.pcdata " " ::
            (if constant then Markup.keyword ":" else Syn.Type.GADT.arrow) ::
            Html.pcdata " " ::
            type_expr te
          in
          true, ret_type
      in
      match args with
      | Tuple [] -> [ Html.code (cstr :: ret_type) ]
      | Tuple lst ->
        let params = list_concat_map lst
          ~sep:(Markup.keyword Syn.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:is_gadt)
        in
        [ Html.code (
            cstr ::
            (
              if Syn.Type.Variant.parenthesize_params
              then Html.pcdata "(" :: params @ [ Html.pcdata ")" ]
              else
                Markup.keyword (if is_gadt then " : " else " of ") :: params
            )
            @ ret_type
          )
        ]
      | Record fields ->
        Html.code [ cstr; Markup.keyword (if is_gadt then " : " else " of ") ]
        :: record fields
        @ [ Html.code ret_type ]



  let variant cstrs : [> Html_types.table ] Html.elt =
    let constructor id args res =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let cell =
          Html.td ~a:[ Html.a_class ["def"; kind ] ] (
            Html.a ~a:[Html.a_href ("#" ^ anchor); Html.a_class ["anchor"]]
              [] ::
            Html.code [Markup.keyword "| " ] ::
            constructor id args res
          )
        in
        anchor, cell
    in
    let rows =
      cstrs |> List.map (fun cstr ->
        let open Model.Lang.TypeDecl.Constructor in
        let anchor, lhs = constructor cstr.id cstr.args cstr.res in
        let rhs = Documentation.to_html cstr.doc in
        let rhs = (rhs :> (Html_types.td_content Html.elt) list) in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] (
          lhs ::
          if not (Documentation.has_doc cstr.doc) then [] else [
            Html.td ~a:[ Html.a_class ["doc"] ] rhs
          ]
        )
      )
    in
    Html.table ~a:[ Html.a_class ["variant"] ] rows



  let extension_constructor (t : Model.Lang.Extension.Constructor.t) =
    (* TODO doc *)
    constructor t.id t.args t.res

  let extension (t : Model.Lang.Extension.t) =
    let extension =
      Html.code (
        Markup.keyword "type " ::
        Html_tree.Relative_link.of_path ~stop_before:false t.type_path @
        [ Markup.keyword " += " ]
      ) ::
      list_concat_map t.constructors ~sep:(Html.code [Markup.keyword " | "])
        ~f:extension_constructor
      @ ( if Syn.Type.type_def_semicolon then [ Markup.keyword ";" ] else [] )
    in
    extension, t.doc



  let exn (t : Model.Lang.Exception.t) =
    let cstr = constructor t.id t.args t.res in
    let exn = Html.code [ Markup.keyword "exception " ] :: cstr
      @ ( if Syn.Type.Exception.semicolon then [ Markup.keyword ";" ] else [] ) in
    exn, t.doc



  let polymorphic_variant ~type_ident (t : Model.Lang.TypeExpr.Variant.t) =
    let row item =
      let kind_approx, cstr =
        match item with
        | Model.Lang.TypeExpr.Variant.Type te ->
          "unknown", [Html.code (type_expr te)]
        | Constructor (name, _bool, args) ->
          let cstr = "`" ^ name in
          "constructor",
          match args with
          | [] -> [Html.code [ Html.pcdata cstr ]]
          | _ ->
            let params = list_concat_map args
              ~sep:(Markup.keyword Syn.Type.Tuple.element_separator)
              ~f:type_expr
            in
            [ Html.code (
                Html.pcdata cstr ::
                (
                if Syn.Type.Variant.parenthesize_params
                then Html.pcdata "(" :: params @ [ Html.pcdata ")" ]
                else Markup.keyword " of " :: params
                )
              )
            ]
      in
      try
        let { Url.Anchor. name = anchor; kind } =
          Url.Anchor.Polymorphic_variant_decl.from_element ~type_ident item
        in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] [
          Html.td ~a:[ Html.a_class ["def"; kind] ] (
            Html.a ~a:[
              Tyxml.Html.a_href ("#" ^ anchor); Html.a_class ["anchor"] ] [] ::
            Html.code [Markup.keyword "| " ] ::
            cstr
          );
          (* TODO: retrieve doc comments. *)
        ]
      with Failure s ->
        Printf.eprintf "ERROR: %s\n%!" s;
        Html.tr [
          Html.td ~a:[ Html.a_class ["def"; kind_approx] ] (
            Html.code [Markup.keyword "| " ] ::
            cstr
          );
          (* TODO: retrieve doc comments. *)
        ]
    in
    let table =
      Html.table ~a:[Html.a_class ["variant"]] (List.map row t.elements) in
    match t.kind with
    | Fixed ->
      Html.code [Html.pcdata "[ "] :: table :: [Html.code [Html.pcdata " ]"]]
    | Open ->
      Html.code [Html.pcdata "[> "] :: table :: [Html.code [Html.pcdata " ]"]]
    | Closed [] ->
      Html.code [Html.pcdata "[< "] :: table :: [Html.code [Html.pcdata " ]"]]
    | Closed lst ->
      let constrs = String.concat " " lst in
      Html.code [Html.pcdata "[< "] :: table ::
        [Html.code [Html.pcdata (" " ^ constrs ^ " ]")]]



  let format_params
    : 'row. ?delim:[`parens | `brackets] -> Model.Lang.TypeDecl.param list
    -> ([> `PCDATA ] as 'row) Html.elt
  = fun ?(delim=`parens) params ->
    let format_param (desc, variance_opt) =
      let param_desc =
        match desc with
        | Model.Lang.TypeDecl.Any -> "_"
        | Var s -> "'" ^ s
      in
      match variance_opt with
      | None -> param_desc
      | Some Model.Lang.TypeDecl.Pos -> "+" ^ param_desc
      | Some Model.Lang.TypeDecl.Neg -> "-" ^ param_desc
    in
    Html.pcdata (
      match params with
      | [] -> ""
      | [x] -> format_param x |> Syn.Type.handle_format_params
      | lst ->
        let params = String.concat ", " (List.map format_param lst) in
        (match delim with `parens -> "(" | `brackets -> "[")
        ^ params ^
        (match delim with `parens -> ")" | `brackets -> "]")
    )

  let format_constraints
    : 'inner_row 'outer_row. (_ * _) list ->
    ([> `PCDATA | `Span
    | `A of ([> `PCDATA ] as 'inner_row) ] as 'outer_row) Html.elt list
    = function
    | [] -> []
    | lst ->
      Markup.keyword " constraint " ::
      list_concat_map lst ~sep:(Markup.keyword " and ") ~f:(fun (t1, t2) ->
        type_expr t1 @ Html.pcdata " = " :: type_expr t2
      )

  let format_manifest
    : 'inner_row 'outer_row. ?compact_variants:bool
    -> Model.Lang.TypeDecl.Equation.t
    -> ('inner_row, 'outer_row) text Html.elt list * bool
  = fun ?(compact_variants=true) equation ->
    let _ = compact_variants in (* TODO *)
    let private_ = equation.private_ in
    match equation.manifest with
    | None -> [], private_
    | Some t ->
      let manifest =
        Markup.keyword " = " ::
        (if private_ then Markup.keyword (Syn.Type.private_keyword ^ " ") else Html.pcdata "") ::
        type_expr t
      in
      manifest, false



  let type_decl (t : Lang.TypeDecl.t) =
    let tyname = Paths.Identifier.name t.id in
    let params = format_params t.equation.params in
    let constraints = format_constraints t.equation.constraints in
    let manifest, need_private =
      match t.equation.manifest with
      | Some (Model.Lang.TypeExpr.Variant variant) ->
        let manifest =
          Markup.keyword " = " ::
          (if t.equation.private_ then
            Markup.keyword (Syn.Type.private_keyword ^ " ")
          else
            Html.pcdata "") ::
          polymorphic_variant ~type_ident:t.id variant
        in
        manifest, false
      | _ ->
        let manifest, need_private = format_manifest t.equation in
        Utils.optional_code manifest, need_private
    in
    let representation =
      match t.representation with
      | None -> []
      | Some repr ->
        Html.code [
          Markup.keyword " = ";
          if need_private then Markup.keyword (Syn.Type.private_keyword ^ " ") else Html.pcdata ""
        ] ::
        match repr with
        | Extensible -> [Html.code [Markup.keyword  ".."]]
        | Variant cstrs -> [variant cstrs]
        | Record fields -> record fields
    in
    let tdecl_def =
      Html.code (
          [ Markup.keyword "type "]
          @ ( Syn.Type.handle_constructor_params [Html.pcdata tyname] [params] )
      ) ::
      manifest @
      representation @
      Utils.optional_code constraints
      @ ( if Syn.Type.type_def_semicolon then [ Markup.keyword ";" ] else [] )
    in
    tdecl_def, t.doc
end
open Type_declaration



module Value :
sig
  val value : Lang.Value.t -> rendered_item * Comment.docs
  val external_ : Lang.External.t -> rendered_item * Comment.docs
end =
struct
  let value (t : Model.Lang.Value.t) =
    let name = Paths.Identifier.name t.id in
    let value =
      Markup.keyword (Syn.Value.variable_keyword ^ " ") ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_
      @ ( if Syn.Value.semicolon then [ Markup.keyword ";" ] else [] )
    in
    [Html.code value], t.doc

  let external_ (t : Model.Lang.External.t) =
    let name = Paths.Identifier.name t.id in
    let external_ =
      Markup.keyword "external " ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_ @
      Html.pcdata " = " ::
      Syn.Type.External.handle_primitives t.primitives
      @ ( if Syn.Type.External.semicolon then [ Markup.keyword ";" ] else [] )
    in
    [Html.code external_], t.doc
end
open Value



(* This chunk of code is responsible for laying out signatures and class
   signatures: the parts of OCaml that contain other parts as nested items.

   Each item is either

   - a leaf, like a type declaration or a value,
   - something that has a nested signature/class signature, or
   - a comment.

   Comments can contain section headings, and the top-level markup code is also
   responsible for generating a table of contents. As a result, it must compute
   the nesting of sections.

   This is also a good opportunity to properly nest everything in <section>
   tags. Even though that is not strictly required by HTML, we carry out the
   computation for it anyway when computing nesting for the table of
   contents.

   Leaf items are set in <dl> tags – the name and any definition in <dt>, and
   documentation in <dd>. Multiple adjacent undocumented leaf items of the same
   kind are set as sibling <dt>s in one <dl>, until one of them has
   documentation. This indicates groups like:

{[
type sync
type async
(** Documentation for both types. *)
]}

   Nested signatures are currently marked up with <article> tags. The top-level
   layout code is eventually indirectly triggered recursively for laying them
   out, as well. *)

module Top_level_markup :
sig
  val lay_out :
    item_to_id:('item -> string option) ->
    item_to_spec:('item -> string option) ->
    render_leaf_item:('item -> rendered_item * Comment.docs) ->
    render_nested_article:('item -> rendered_item * Html_tree.t list) ->
    ((_, 'item) tagged_item) list ->
      rendered_item * toc * Html_tree.t list

  val render_toc :
    toc -> ([> Html_types.flow5_without_header_footer ] Html.elt) list
end =
struct
  (* Just some type abbreviations. *)
  type html = Html_types.flow5 Html.elt
  type comment_html = Html_types.flow5_without_header_footer Html.elt



  let add_anchor item_to_id item html =
    match item_to_id item with
    | None ->
      html,
      []
    | Some anchor_text ->
      let anchor =
        Html.a
          ~a:[Html.a_href ("#" ^ anchor_text); Html.a_class ["anchor"]]
          []
      in
      anchor::html,
      [Html.a_id anchor_text]


(* Adds spec class to the list of existing item attributes. *)
  let add_spec item_to_spec item a =
    match item_to_spec item with
    | Some spec -> Html.a_class ["spec " ^ spec] :: a
    | None -> a


  (* "Consumes" adjacent leaf items of the same kind, until one is found with
     documentation. Then, joins all their definitions, and the documentation of
     the last item (if any), into a <dl> element. The rendered <dl> element is
     paired with the list of unconsumed items remaining in the input. *)
  let leaf_item_group
      item_to_id item_to_spec render_leaf_item first_item_kind items
      : html * 'item list =

    let rec consume_leaf_items_until_one_is_documented =
        fun items acc ->

      match items with
      | (`Leaf_item (this_item_kind, item))::items
          when this_item_kind = first_item_kind ->

        let html, maybe_docs = render_leaf_item item in
        (* Temporary coercion until https://github.com/ocsigen/tyxml/pull/193
           is released in TyXML; see also type [rendered_item]. *)
        let html = List.map Html.Unsafe.coerce_elt html in
        let html, maybe_id = add_anchor item_to_id item html in
        let a = add_spec item_to_spec item maybe_id in
        let html = Html.dt ~a html in
        let acc = html::acc in

        begin match maybe_docs with
        | [] ->
          consume_leaf_items_until_one_is_documented items acc
        | docs ->
          let docs = Documentation.to_html docs in
          let docs = (docs :> (Html_types.dd_content Html.elt) list) in
          let docs = Html.dd docs in
          List.rev (docs::acc), items
        end

      | _ ->
        List.rev acc, items
    in

    let rendered_item_group, remaining_items =
      consume_leaf_items_until_one_is_documented items [] in

    Html.dl rendered_item_group, remaining_items



  (* When we encounter a stop comment, [(**/**)], we read everything until the
     next stop comment, if there is one, and discard it. The returned item list
     is the signature items following the next stop comment, if there are
     any. *)
  let rec skip_everything_until_next_stop_comment : 'item list -> 'item list =
    function
    | [] -> []
    | item::items ->
      match item with
      | `Comment `Stop -> items
      | _ -> skip_everything_until_next_stop_comment items



  (* Reads comment content until the next heading, or the end of comment, and
     renders it as HTML. The returned HTML is paired with the remainder of the
     comment, which will either start with the next section heading in the
     comment, or be empty if there are no section headings. *)
  let render_comment_until_heading_or_end
      : Comment.docs -> comment_html list * Comment.docs = fun docs ->

    let rec scan_comment acc docs =
      match docs with
      | [] -> List.rev acc, docs
      | block::rest ->
        match block.Location.value with
        | `Heading _ -> List.rev acc, docs
        | _ -> scan_comment (block::acc) rest
    in
    let included, remaining = scan_comment [] docs in
    let docs = Documentation.to_html included in
    docs, remaining



  (* The sectioning functions take several arguments, and return "modified"
     instances of them as results. So, it is convenient to group them into a
     record type. This is most useful for the return type, as otherwise there is
     no way to give names to its components.

     The components themselves are:

     - The current comment being read. When non-empty, this is progressively
       replaced with its tail until it is exhausted.
     - The general signature items to be read. These are read one at a time when
       there is no current comment. Upon encountering a comment, it becomes the
       "current comment," and the sectioning functions read it one block element
       at a time, scanning for section headings.
     - A reversed accumulator of the rendered signature items.
     - A reversed accumulator of the table of contents.
     - An accumulator of the subpages generated for nested signatures.

     The record is also convenient for passing around the two item-rendering
     functions. *)
  type ('kind, 'item) sectioning_state = {
    input_items : (('kind, 'item) tagged_item) list;
    input_comment : Comment.docs;

    acc_html : html list;
    acc_toc : toc;
    acc_subpages : Html_tree.t list;

    item_to_id : 'item -> string option;
    item_to_spec : 'item -> string option;
    render_leaf_item : 'item -> rendered_item * Comment.docs;
    render_nested_article : 'item -> rendered_item * Html_tree.t list;
  }

  let finish_section state =
    {state with
      acc_html = List.rev state.acc_html;
      acc_toc = List.rev state.acc_toc;
    }

  let is_deeper_section_level =
    let level_to_int = function
      | `Title -> 1
      | `Section -> 2
      | `Subsection -> 3
      | `Subsubsection -> 4
    in
    fun other_level ~than ->
      level_to_int other_level > level_to_int than



  let rec section_items section_level state =
    match state.input_items with
    | [] ->
      finish_section state

    | tagged_item::input_items ->

      match tagged_item with
      | `Leaf_item (kind, _) ->
        let html, input_items =
          leaf_item_group
            state.item_to_id
            state.item_to_spec
            state.render_leaf_item
            kind
            state.input_items
        in
        section_items section_level {state with
            input_items;
            acc_html = html::state.acc_html;
          }

      | `Nested_article item ->
        let html, subpages = state.render_nested_article item in
        let html, maybe_id = add_anchor state.item_to_id item html in
        let a = add_spec state.item_to_spec item maybe_id in
        section_items section_level {state with
            input_items;
            acc_html = (Html.article ~a html)::state.acc_html;
            acc_subpages = state.acc_subpages @ subpages;
          }

      | `Comment `Stop ->
        let input_items = skip_everything_until_next_stop_comment input_items in
        section_items section_level {state with
            input_items;
          }

      | `Comment (`Docs input_comment) ->
        section_comment section_level {state with
            input_items;
            input_comment;
          }



  and section_comment section_level state =
    match state.input_comment with
    | [] ->
      section_items section_level state

    | element::input_comment ->

      match element.Location.value with
      | `Heading (level, label, content) ->
        if not (is_deeper_section_level level ~than:section_level) then
          finish_section state

        else
          (* We have a deeper section heading in a comment within this section.
             Parse it recursively. We start the nested HTML by parsing the
             section heading itself, and anything that follows it in the current
             comment, up to the next section heading, if any. All of this
             comment matter goes into a <header> element. The nested HTML will
             then be extended recursively by parsing more structure items,
             including, perhaps, additional comments in <aside> elements. *)
          let heading_html = Documentation.to_html [element] in
          let more_comment_html, input_comment =
            render_comment_until_heading_or_end input_comment in
          let html = Html.header (heading_html @ more_comment_html) in
          let nested_section_state =
            {state with
              input_comment = input_comment;
              acc_html = [html];
              acc_toc = [];
            }
          in
          let nested_section_state =
            section_comment level nested_section_state in

          (* Wrap the nested section in a <section> element, and extend the
             table of contents. *)
          let html = Html.section nested_section_state.acc_html in

          let Paths.Identifier.Label (_, label) = label in
          let toc_entry =
            {
              anchor = label;
              text = content;
              children = nested_section_state.acc_toc;
            }
          in

          (* Continue parsing after the nested section. In practice, we have
             either run out of items, or the first thing in the input will be
             another section heading – the nested section will have consumed
             everything else. *)
          section_comment section_level {nested_section_state with
              acc_html = html::state.acc_html;
              acc_toc = toc_entry::state.acc_toc;
            }

      | _ ->
        let html, input_comment =
          render_comment_until_heading_or_end state.input_comment in
        let html = (html :> (Html_types.aside_content Html.elt) list) in
        section_comment section_level {state with
            input_comment;
            acc_html = (Html.aside html)::state.acc_html;
          }



  let lay_out ~item_to_id ~item_to_spec ~render_leaf_item ~render_nested_article items =
    let initial_state =
      {
        input_items = items;
        input_comment = [];

        acc_html = [];
        acc_toc = [];
        acc_subpages = [];

        item_to_id;
        item_to_spec;
        render_leaf_item;
        render_nested_article;
      }
    in
    let state = section_items `Title initial_state in
    state.acc_html, state.acc_toc, state.acc_subpages



  let render_toc toc =
    let rec section the_section : Html_types.li_content Html.elt list =
      let text = Documentation.link_content_to_html the_section.text in
      let text =
        (text
          : Html_types.phrasing_without_interactive Html.elt list
          :> (Html_types.flow5_without_interactive Html.elt) list)
      in
      let link =
        Html.a
          ~a:[Html.a_href ("#" ^ the_section.anchor)] text
      in
      match the_section.children with
      | [] -> [link]
      | _ -> [link; sections the_section.children]

    and sections the_sections =
      the_sections
      |> List.map (fun the_section -> Html.li (section the_section))
      |> Html.ul

    in

    match toc with
    | [] -> []
    | _ -> [Html.nav ~a:[Html.a_class ["toc"]] [sections toc]]
end

(* TODO Figure out when this function would fail. It is currently pasted from
   [make_def], but the [make_spec] version doesn't have a [failwith]. *)
let path_to_id path =
  match Url.from_identifier ~stop_before:true path with
  | Error _ ->
    None
  | Ok {anchor; _} ->
    Some anchor



module Class :
sig
  val class_ :
    ?theme_uri:Html_tree.uri -> Lang.Class.t ->
      ((Html_types.article_content Html.elt) list) * (Html_tree.t list)

  val class_type :
    ?theme_uri:Html_tree.uri -> Lang.ClassType.t ->
      ((Html_types.article_content Html.elt) list) * (Html_tree.t list)
end =
struct
  let class_signature_item_to_id : Lang.ClassSignature.item -> _ = function
    | Method {id; _} -> path_to_id id
    | InstanceVariable {id; _} -> path_to_id id
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let class_signature_item_to_spec : Lang.ClassSignature.item -> _ = function
    | Method _ -> Some "method"
    | InstanceVariable _ -> Some "instance-variable"
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let tag_class_signature_item : Lang.ClassSignature.item -> _ = fun item ->
    match item with
    | Method _ -> `Leaf_item (`Method, item)
    | InstanceVariable _ -> `Leaf_item (`Variable, item)
    | Constraint _ -> `Leaf_item (`Constraint, item)
    | Inherit _ -> `Leaf_item (`Inherit, item)

    | Comment comment -> `Comment comment

  let rec render_class_signature_item : Lang.ClassSignature.item -> _ = function
    | Method m -> method_ m
    | InstanceVariable v -> instance_variable v
    | Constraint (t1, t2) -> format_constraints [(t1, t2)], []
    | Inherit (Signature _) -> assert false (* Bold. *)
    | Inherit class_type_expression ->
      (Markup.keyword "inherit " ::
       class_type_expr class_type_expression),
      []

    | Comment _ -> assert false

  and class_signature (c : Lang.ClassSignature.t) =
    (* FIXME: use [t.self] *)
    let tagged_items = List.map tag_class_signature_item c.items in
    Top_level_markup.lay_out
      ~item_to_id:class_signature_item_to_id
      ~item_to_spec:class_signature_item_to_spec
      ~render_leaf_item:render_class_signature_item
      ~render_nested_article:(fun _ -> assert false)
      tagged_items

  and method_ (t : Model.Lang.Method.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then Markup.keyword "virtual " else Html.pcdata "" in
    let private_ =
      if t.private_ then Markup.keyword "private " else Html.pcdata "" in
    let method_ =
      Markup.keyword "method " ::
      private_ ::
      virtual_ ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_
    in
    [Html.code method_], t.doc

  and instance_variable (t : Model.Lang.InstanceVariable.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then Markup.keyword "virtual " else Html.pcdata "" in
    let mutable_ =
      if t.mutable_ then Markup.keyword "mutable " else Html.pcdata "" in
    let val_ =
      Markup.keyword "val " ::
      mutable_ ::
      virtual_ ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_
    in
    [Html.code val_], t.doc

  and class_type_expr
    : 'inner_row 'outer_row. Model.Lang.ClassType.expr
    -> ('inner_row, 'outer_row) text Html.elt list
    = fun (cte : Model.Lang.ClassType.expr) ->
      match cte with
      | Constr (path, args) ->
        let link = Html_tree.Relative_link.of_path ~stop_before:false path in
        format_type_path ~delim:(`brackets) args link
      | Signature _ ->
        [ Markup.keyword Syn.Class.open_tag ; Html.pcdata " ... " ; Markup.keyword Syn.Class.close_tag ]

  and class_decl
    : 'inner_row 'outer_row. Model.Lang.Class.decl
    -> ('inner_row, 'outer_row) text Html.elt list
    = fun (cd : Model.Lang.Class.decl) ->
      match cd with
      | ClassType expr -> class_type_expr expr
      (* TODO: factorize the following with [type_expr] *)
      | Arrow (None, src, dst) ->
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Syn.Type.arrow :: Html.pcdata " " :: class_decl dst
      | Arrow (Some lbl, src, dst) ->
        Markup.ML.label lbl @ Html.pcdata ":" ::
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Syn.Type.arrow :: Html.pcdata " " :: class_decl dst

  and class_ ?theme_uri (t : Model.Lang.Class.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then Markup.keyword "virtual " else Html.pcdata "" in
    let cd = class_decl t.type_ in
    let cname, subtree =
      match t.expansion with
      | None -> Html.pcdata name, []
      | Some csig ->
        Html_tree.enter ~kind:(`Class) name;
        let doc = Documentation.to_html t.doc in
        let doc = (doc :> (Html_types.div_content Html.elt) list) in
        let expansion, _, _ = class_signature csig in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make ?theme_uri expansion [] in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Class name ] [Html.pcdata name], [subtree]
    in
    let class_def_content =
      Markup.keyword "class " ::
      virtual_ ::
      params ::
      cname ::
      Html.pcdata " : " ::
      cd
    in
    let region =
      [Html.code class_def_content]
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree

  and class_type ?theme_uri (t : Model.Lang.ClassType.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then Markup.keyword "virtual " else Html.pcdata "" in
    let expr = class_type_expr t.expr in
    let cname, subtree =
      match t.expansion with
      | None -> Html.pcdata name, []
      | Some csig ->
        Html_tree.enter ~kind:(`Cty) name;
        let doc = Documentation.to_html t.doc in
        let doc = (doc :> (Html_types.div_content Html.elt) list) in
        let expansion, _, _ = class_signature csig in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make ?theme_uri expansion [] in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Cty name ] [Html.pcdata name], [subtree]
    in
    let ctyp =
      Markup.keyword "class type " ::
      virtual_ ::
      params ::
      cname ::
      Html.pcdata " = " ::
      expr
    in
    let region =
      [Html.code ctyp]
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree
end
open Class



module Module :
sig
  val signature :
    ?theme_uri:Html_tree.uri -> Lang.Signature.t ->
      (Html_types.div_content Html.elt) list * toc * Html_tree.t list
end =
struct
  let signature_item_to_id : Lang.Signature.item -> _ = function
    | Type {id; _} -> path_to_id id
    | Exception {id; _} -> path_to_id id
    | Value {id; _} -> path_to_id id
    | External {id; _} -> path_to_id id
    | Module {id; _} -> path_to_id id
    | ModuleType {id; _} -> path_to_id id
    | Class {id; _} -> path_to_id id
    | ClassType {id; _} -> path_to_id id
    | TypExt _
    | Include _
    | Comment _ -> None

  let signature_item_to_spec : Lang.Signature.item -> _ = function
    | Type _ -> Some "type"
    | Exception _ -> Some "exception"
    | Value _ -> Some "value"
    | External _ -> Some "external"
    | Module _ -> Some "module"
    | ModuleType _ -> Some "module-type"
    | Class _ -> Some "class"
    | ClassType _ -> Some "class-type"
    | TypExt _
    | Include _
    | Comment _ -> None

  let tag_signature_item : Lang.Signature.item -> _ = fun item ->
    match item with
    | Type _ -> `Leaf_item (`Type, item)
    | TypExt _ -> `Leaf_item (`Extension, item)
    | Exception _ -> `Leaf_item (`Exception, item)
    | Value _ -> `Leaf_item (`Value, item)
    | External _ -> `Leaf_item (`External, item)

    | Module _
    | ModuleType _
    | Include _
    | Class _
    | ClassType _ -> `Nested_article item

    | Comment comment -> `Comment comment

  let rec render_leaf_signature_item : Lang.Signature.item -> _ = function
    | Type t -> type_decl t
    | TypExt e -> extension e
    | Exception e -> exn e
    | Value v -> value v
    | External e -> external_ e
    | _ -> assert false

  and render_nested_signature_or_class
      : ?theme_uri:Html_tree.uri -> Lang.Signature.item -> _ = fun ?theme_uri item ->
    match item with
    | Module m -> module_ ?theme_uri m
    | ModuleType m -> module_type ?theme_uri m
    | Class c -> class_ ?theme_uri c
    | ClassType c -> class_type ?theme_uri c
    | Include m -> include_ m
    | _ -> assert false

  and signature ?theme_uri s =
    let tagged_items = List.map tag_signature_item s in
    Top_level_markup.lay_out
      ~item_to_id:signature_item_to_id
      ~item_to_spec:signature_item_to_spec
      ~render_leaf_item:render_leaf_signature_item
      ~render_nested_article:(render_nested_signature_or_class ?theme_uri)
      tagged_items

  and functor_argument
    : 'row. ?theme_uri:Html_tree.uri -> Model.Lang.FunctorArgument.t
    -> Html_types.div_content Html.elt list * Html_tree.t list
  = fun ?theme_uri arg ->
    let open Model.Lang.FunctorArgument in
    let name = Paths.Identifier.name arg.id in
    let nb = functor_arg_pos arg in
    let link_name = Printf.sprintf "%d-%s" nb name in
    let def_div, subtree =
      match arg.expansion with
      | None ->
        (
          Html.pcdata (Paths.Identifier.name arg.id) ::
          Html.pcdata " : " ::
          mty (Paths.Identifier.signature_of_module arg.id) arg.expr
        ), []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match arg.expr with
            | Signature sg -> Model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Html_tree.enter ~kind:(`Arg) link_name;
        let (docs, subpages) = module_expansion expansion in
        let subtree = Html_tree.make ?theme_uri docs subpages in
        Html_tree.leave ();
        (
          Html.a ~a:[ a_href ~kind:`Arg link_name ] [Html.pcdata name] ::
          Html.pcdata " : " ::
          mty (Paths.Identifier.signature_of_module arg.id) arg.expr
        ), [subtree]
    in
    let region = [Html.code def_div] in
    region, subtree

  and module_expansion
    : ?theme_uri:Html_tree.uri -> Model.Lang.Module.expansion
    -> Html_types.div_content_fun Html.elt list * Html_tree.t list
  = fun ?theme_uri t ->
    match t with
    | AlreadyASig -> assert false
    | Signature sg ->
      let expansion, _, subpages = signature sg in
      expansion, subpages
    | Functor (args, sg) ->
      let sig_html, _, subpages = signature sg in
      let params, params_subpages =
        List.fold_left (fun (args, subpages as acc) arg ->
          match arg with
          | None -> acc
          | Some arg ->
            let arg, arg_subpages = functor_argument ?theme_uri arg in
            (arg @ args, arg_subpages @ subpages)
        )
        ([], []) args
      in
      let html =
        Html.h3 ~a:[ Html.a_class ["heading"] ] [ Html.pcdata "Parameters" ] ::
        Html.dl (List.map Html.Unsafe.coerce_elt params) ::
        Html.h3 ~a:[ Html.a_class ["heading"] ] [ Html.pcdata "Signature" ] ::
        sig_html
      in
      html, params_subpages @ subpages

  and module_
      : ?theme_uri:Html_tree.uri -> Model.Lang.Module.t ->
          Html_types.article_content Html.elt list * Html_tree.t list
      = fun ?theme_uri t ->
    let modname = Paths.Identifier.name t.id in
    let md =
      module_decl (Paths.Identifier.signature_of_module t.id)
        (match t.display_type with
        | None -> t.type_
        | Some t -> t)
    in
    let modname, subtree =
      match t.expansion with
      | None -> Html.pcdata modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.type_ with
            | ModuleType (Model.Lang.ModuleType.Signature sg) ->
              Model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Html_tree.enter ~kind:(`Mod) modname;
        let doc = Documentation.to_html t.doc in
        let doc = (doc :> (Html_types.div_content Html.elt) list) in
        let expansion, subpages = module_expansion ?theme_uri expansion in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make ?theme_uri expansion subpages in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mod modname ] [Html.pcdata modname], [subtree]
    in
    let md_def_content = Markup.keyword "module " :: modname :: md @ ( if Syn.Mod.close_tag_semicolon then [ Markup.keyword ";" ] else [] ) in
    let region =
      [Html.code md_def_content]
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree

  and module_decl (base : Paths.Identifier.signature) md =
    begin match md with
    | Alias _ -> Html.pcdata " = "
    | ModuleType _ -> Html.pcdata " : "
    end ::
    module_decl' base md

  and extract_path_from_mt ~(default: Paths.Identifier.signature) =
    let open Model.Lang.ModuleType in
    function
    | Path (Paths.Path.Resolved r) ->
      Paths.Identifier.signature_of_module_type
        (Paths.Path.Resolved.identifier r)
    | With (mt, _) -> extract_path_from_mt ~default mt
    | TypeOf (Model.Lang.Module.Alias (Paths.Path.Resolved r)) ->
      Paths.Identifier.signature_of_module (Paths.Path.Resolved.identifier r)
    | TypeOf (Model.Lang.Module.ModuleType mt) ->
      extract_path_from_mt ~default mt
    | _ -> default

  and module_decl'
    : 'inner_row 'outer_row. Paths.Identifier.signature
    -> Model.Lang.Module.decl
    -> ('inner_row, 'outer_row) text Html.elt list
  = fun base -> function
    | Alias mod_path ->
      Html_tree.Relative_link.of_path ~stop_before:true mod_path
    | ModuleType mt -> mty (extract_path_from_mt ~default:base mt) mt

  and module_type ?theme_uri (t : Model.Lang.ModuleType.t) =
    let modname = Paths.Identifier.name t.id in
    let mty =
      match t.expr with
      | None -> []
      | Some expr ->
        begin match expr with
        | Signature _
        | Path _ -> Html.pcdata " = "
        | _ -> Html.pcdata " : "
        end ::
        mty (Paths.Identifier.signature_of_module_type t.id) expr
    in
    let modname, subtree =
      match t.expansion with
      | None -> Html.pcdata modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.expr with
            | Some (Signature sg) -> Model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Html_tree.enter ~kind:(`Mty) modname;
        let doc = Documentation.to_html t.doc in
        let doc = (doc :> (Html_types.div_content Html.elt) list) in
        let expansion, subpages = module_expansion expansion in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make ?theme_uri expansion subpages in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mty modname ] [Html.pcdata modname], [subtree]
    in
    let mty_def =
      (
        Markup.keyword "module type " ::
        modname ::
        mty
        @ ( if Syn.Mod.close_tag_semicolon then [ Markup.keyword ";" ] else [] )
      )
    in
    let region =
      [Html.code mty_def]
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree

  and mty
    : 'inner_row 'outer_row.
      Paths.Identifier.signature -> Model.Lang.ModuleType.expr
    -> ('inner_row, 'outer_row) text Html.elt list
  = fun (base : Paths.Identifier.signature) -> function
    | Path mty_path ->
      Html_tree.Relative_link.of_path ~stop_before:true mty_path
    | Signature _ ->
      [ Markup.keyword Syn.Mod.open_tag ; Html.pcdata " ... " ; Markup.keyword Syn.Mod.close_tag ]
    | Functor (None, expr) ->
        ( if Syn.Mod.functor_keyword then [ Markup.keyword "functor" ] else [] ) @ Html.pcdata " () " ::
      mty base expr
    | Functor (Some arg, expr) ->
      let name =
        let open Model.Lang.FunctorArgument in
        let to_print = Html.pcdata @@ Paths.Identifier.name arg.id in
        match
          Html_tree.Relative_link.Id.href
            ~stop_before:(arg.expansion = None) arg.id
        with
        | exception _ -> to_print
        | href -> Html.a ~a:[ Html.a_href href ] [ to_print ]
      in
      ( if Syn.Mod.functor_keyword then [ Markup.keyword "functor" ] else [] ) @
      Html.pcdata " (" :: name :: Html.pcdata " : " ::
      mty base arg.expr @
      [Html.pcdata ")"; Html.pcdata " "] @ Syn.Type.arrow :: Html.pcdata " " ::
      mty base expr
    | With (expr, substitutions) ->
      mty base expr @
      Markup.keyword " with " ::
      list_concat_map ~sep:(Markup.keyword " and ") substitutions
        ~f:(substitution base)
    | TypeOf md ->
      Markup.keyword "module type of " :: module_decl' base md

  and substitution
    : 'inner_row 'outer_row. Paths.Identifier.signature ->
        Model.Lang.ModuleType.substitution
    -> ('inner_row, 'outer_row) text Html.elt list
  = fun base -> function
    | ModuleEq (frag_mod, md) ->
      Markup.keyword "module " ::
      Html_tree.Relative_link.of_fragment ~base
        (Paths.Fragment.signature_of_module frag_mod)
      @ Html.pcdata " = " ::
      module_decl' base md
    | TypeEq (frag_typ, td) ->
      Markup.keyword "type " ::
      (Syn.Type.handle_substitution_params
        (Html_tree.Relative_link.of_fragment
          ~base (Paths.Fragment.any_sort frag_typ))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) @
      fst (format_manifest td) @
      format_constraints td.Model.Lang.TypeDecl.Equation.constraints
    | ModuleSubst (frag_mod, mod_path) ->
      Markup.keyword "module " ::
      Html_tree.Relative_link.of_fragment
        ~base (Paths.Fragment.signature_of_module frag_mod) @
      Html.pcdata " := " ::
      Html_tree.Relative_link.of_path ~stop_before:true mod_path
    | TypeSubst (frag_typ, td) ->
      Markup.keyword "type " ::
      (Syn.Type.handle_substitution_params
        (Html_tree.Relative_link.of_fragment
          ~base (Paths.Fragment.any_sort frag_typ))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) @
      Html.pcdata " := " ::
      match td.Lang.TypeDecl.Equation.manifest with
      | None -> assert false (* cf loader/cmti *)
      | Some te ->
        type_expr te

  and include_ (t : Model.Lang.Include.t) =
    let docs = Documentation.to_html t.doc in
    let docs = (docs :> (Html_types.div_content Html.elt) list) in
    let included_html, _, tree = signature t.expansion.content in
    let should_be_inlined =
      let is_inline_tag element =
        element.Model.Location_.value = `Tag `Inline in
      List.exists is_inline_tag t.doc
    in
    let should_be_open =
      let is_open_tag element = element.Model.Location_.value = `Tag `Open in
      let is_closed_tag element =
        element.Model.Location_.value = `Tag `Closed in
      if List.exists is_open_tag t.doc then
        true
      else
        !Html_tree.open_details && not (List.exists is_closed_tag t.doc)
    in
    let incl =
      if should_be_inlined then
        included_html
      else
        let incl =
          Html.code (
            Markup.keyword "include " ::
            module_decl' t.parent t.decl
            @ ( if Syn.Mod.include_semicolon then [ Markup.keyword ";" ] else [] )
          )
        in
        (* FIXME: I'd like to add an anchor here, but I don't know what id to
           give it... *)
        [
          Html.details ~a:(if should_be_open then [Html.a_open ()] else [])
            (Html.summary [Html.span ~a:[Html.a_class ["def"]] [incl]])
            included_html
        ]
    in
    [
      (* TODO The coercion is temporary until TyXML with
         https://github.com/ocsigen/tyxml/pull/193 is available. *)
      Html.Unsafe.coerce_elt
        (Html.div ~a:[Html.a_class ["spec"; "include"]]
          [Html.div ~a:[Html.a_class ["doc"]]
            (docs @ incl)])
    ],
    tree
end
open Module



module Page :
sig
  val compilation_unit : ?theme_uri:Html_tree.uri -> Lang.Compilation_unit.t -> Html_tree.t
  val page : ?theme_uri:Html_tree.uri -> Lang.Page.t -> Html_tree.t
end =
struct
  let pack
    : Model.Lang.Compilation_unit.Packed.t ->
        Html_types.div_content Html.elt list
  = fun t ->
    let open Model.Lang in
    t
    |> List.map begin fun x ->
      let modname = Paths.Identifier.name x.Compilation_unit.Packed.id in
      let md_def =
        Markup.keyword "module " ::
        Html.pcdata modname ::
        Html.pcdata " = " ::
        Html_tree.Relative_link.of_path ~stop_before:false x.path
      in
      [Html.code md_def]
    end
    |> List.flatten
    |> fun definitions ->
      [Html.article definitions]



  let compilation_unit ?theme_uri (t : Model.Lang.Compilation_unit.t) : Html_tree.t =
    let package =
      match t.id with
      | Model.Paths.Identifier.Root (a, _) -> a.package
      | _ -> assert false
    in
    Html_tree.enter package;
    Html_tree.enter (Paths.Identifier.name t.id);
    let header_docs = Documentation.to_html t.doc in
    let header_docs, html, subtree =
      match t.content with
      | Module sign ->
        let html, toc, subpages = signature ?theme_uri sign in
        let header_docs =
          match toc with
          | [] -> header_docs
          | _ -> header_docs @ (Top_level_markup.render_toc toc)
        in
        header_docs, html, subpages
      | Pack packed ->
        header_docs, pack packed, []
    in
    Html_tree.make ~header_docs ?theme_uri html subtree



  let page ?theme_uri (t : Model.Lang.Page.t) : Html_tree.t =
    let package, name =
      match t.name with
      | Model.Paths.Identifier.Page (a, name) -> a.package, name
    in
    Html_tree.enter package;
    Html_tree.enter ~kind:`Page name;
    let html = Documentation.to_html t.content in
    let html = (html :> (Html_types.div_content Html.elt) list) in
    Html_tree.make ?theme_uri html []
end
include Page
end
