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



type rendered_item = (Html_types.div_content Html.elt) list
(* [rendered_item] should really be [dt_content], but that is bugged in TyXML
   until https://github.com/ocsigen/tyxml/pull/193 is released. *)



let relax_docs_type docs =
  (docs :> (Html_types.div_content Html.elt) list)

let docs_to_general_html docs =
  relax_docs_type (Documentation.to_html docs)

type ('inner, 'outer) text =
  [> `PCDATA | `Span | `A of ([> `PCDATA ] as 'inner) ] as 'outer

let a_href = Html_tree.Relative_link.to_sub_element

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep :: tl

let functor_arg_pos { Model.Lang.FunctorArgument.id ; _ } =
  match id with
  | Paths.Identifier.Argument (_, nb, _) -> nb
  | _ ->
    failwith "TODO"
    (* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)



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
      list_concat_map t.elements ~sep:(Html.pcdata " | ") ~f:(function
        | Model.Lang.TypeExpr.Variant.Type te -> type_expr te
        | Constructor (name, _bool, args) ->
          let constr = "`" ^ name in
          match args with
          | [] -> [ Html.pcdata constr ]
          | _ ->
            let args =
              list_concat_map args ~sep:(Html.pcdata " * ") ~f:type_expr
            in
            Html.pcdata (constr ^ " of ") :: args
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
        | Model.Lang.TypeExpr.Object.Method { name; type_ } ->
          Html.pcdata (name ^ " : ") :: type_expr type_ @ [Html.pcdata "; "]
        | Inherit type_ ->
          type_expr type_ @ [Html.pcdata "; "]
      )
    in
    Html.pcdata "< " ::
      fields @ [Html.pcdata ((if t.open_ then ".. " else "") ^ ">")]

  and format_type_path
    : 'inner 'outer. delim:[ `parens | `brackets ]
    -> Model.Lang.TypeExpr.t list -> ('inner, 'outer) text Html.elt list
    -> ('inner, 'outer) text Html.elt list
  = fun ~delim params path ->
    match params with
    | [] -> path
    | [param] ->
      type_expr ~needs_parentheses:true param @ Html.pcdata " " :: path
    | params  ->
      let params =
        list_concat_map params ~sep:(Html.pcdata ",\194\160")
          ~f:type_expr
      in
      match delim with
      | `parens   -> Html.pcdata "(" :: params @ Html.pcdata ")\194\160" :: path
      | `brackets -> Html.pcdata "[" :: params @ Html.pcdata "]\194\160" :: path

  and type_expr
    : 'inner 'outer. ?needs_parentheses:bool
    -> Model.Lang.TypeExpr.t -> ('inner, 'outer) text Html.elt list
  = fun ?(needs_parentheses=false) t ->
    match t with
    | Var s -> [Markup.Type.var ("'" ^ s)]
    | Any  -> [Markup.Type.var "_"]
    | Alias (te, alias) ->
      type_expr ~needs_parentheses:true te @
      Markup.keyword " as " :: [ Html.pcdata alias ]
    | Arrow (None, src, dst) ->
      let res =
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Markup.arrow :: Html.pcdata " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
    | Arrow (Some lbl, src, dst) ->
      let res =
        Markup.label lbl @ Html.pcdata ":" ::
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Markup.arrow :: Html.pcdata " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
    | Tuple lst ->
      let res =
        list_concat_map lst ~sep:(Markup.keyword " * ")
          ~f:(type_expr ~needs_parentheses:true)
      in
      if not needs_parentheses then
        res
      else
        Html.pcdata "(" :: res @ [Html.pcdata ")"]
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
                :: (Html.pcdata " : ")
                :: (type_expr typ)
                @  [Html.pcdata ";"]
              )
            ]
        in
        anchor, cell
    in
    let rows =
      fields |> List.map (fun fld ->
        let open Model.Lang.TypeDecl.Field in
        let anchor, lhs = field fld.mutable_ fld.id fld.type_ in
        let rhs = relax_docs_type (Documentation.to_html fld.doc) in
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
            (if constant then Markup.keyword ":" else Markup.arrow) ::
            Html.pcdata " " ::
            type_expr te
          in
          true, ret_type
      in
      match args with
      | Tuple [] -> [ Html.code (cstr :: ret_type) ]
      | Tuple lst ->
        [ Html.code (
            cstr ::
            Markup.keyword (if is_gadt then " : " else " of ") ::
            list_concat_map lst ~sep:(Markup.keyword " * ")
              ~f:(type_expr ~needs_parentheses:is_gadt)
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
        let rhs = relax_docs_type (Documentation.to_html cstr.doc) in
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
    in
    (* FIXME: really want to use the kind "extension" here? *)
    (* Inlined [Markup.make_spec] as we don't have an id (which implies we don't
      have an anchor either). *)
    (* TODO Fix this junk. *)
    (* TODO make_spec needs to be modified to make the anchor optional. *)
    Markup.make_spec ~id:(CoreType "fixme") extension, t.doc



  let exn (t : Model.Lang.Exception.t) =
    let cstr = constructor t.id t.args t.res in
    let exn = Html.code [ Markup.keyword "exception " ] :: cstr in
    Markup.make_spec ~id:t.id exn, t.doc



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
            [ Html.code (
                Html.pcdata cstr ::
                Markup.keyword " of " ::
                list_concat_map args ~sep:(Markup.keyword " * ")
                  ~f:type_expr
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
      | [x] -> format_param x ^ " "
      | lst ->
        let params = String.concat ", " (List.map format_param lst) in
        (match delim with `parens -> "(" | `brackets -> "[")
        ^ params ^
        (match delim with `parens -> ") " | `brackets -> "] ")
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
        (if private_ then Markup.keyword "private " else Html.pcdata "") ::
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
            Markup.keyword "private "
          else
            Html.pcdata "") ::
          polymorphic_variant ~type_ident:t.id variant
        in
        manifest, false
      | _ ->
        let manifest, need_private = format_manifest t.equation in
        [Html.code manifest], need_private
    in
    let representation =
      match t.representation with
      | None -> []
      | Some repr ->
        Html.code [
          Markup.keyword " = ";
          if need_private then Markup.keyword "private " else Html.pcdata ""
        ] ::
        match repr with
        | Extensible -> [Html.code [Markup.keyword  ".."]]
        | Variant cstrs -> [variant cstrs]
        | Record fields -> record fields
    in
    let tdecl_def =
      Html.code [
        Markup.keyword "type ";
        params;
        Html.pcdata tyname;
      ] ::
      manifest @
      representation @
      [Html.code constraints]
    in
    Markup.make_spec ~id:t.id tdecl_def, t.doc
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
      Markup.keyword "val " ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_
    in
    Markup.make_def ~id:t.id ~code:value, t.doc

  let external_ (t : Model.Lang.External.t) =
    let name = Paths.Identifier.name t.id in
    let external_ =
      Markup.keyword "external " ::
      Html.pcdata name ::
      Html.pcdata " : " ::
      type_expr t.type_ @
      Html.pcdata " = " ::
      List.map (fun p -> Html.pcdata ("\"" ^ p ^ "\" ")) t.primitives
    in
    Markup.make_def ~id:t.id ~code:external_, t.doc
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

type ('item_kind, 'item) tagged_item = [
  | `Leaf_item of 'item_kind * 'item
  | `Nested_article of 'item
  | `Comment of Comment.docs_or_stop
]

type toc = [ `Section of string * toc ] list

module Top_level_markup :
sig
  val lay_out :
    render_leaf_item:('item -> rendered_item * Comment.docs) ->
    render_nested_article:('item -> rendered_item * Html_tree.t list) ->
    ((_, 'item) tagged_item) list ->
      rendered_item * toc * Html_tree.t list

  val render_toc :
    toc -> ([> Html_types.flow5_without_header_footer ] Html.elt) list
end =
struct
  type content = Html_types.flow5 Html.elt



  (* "Consumes" adjacent leaf items of the same kind, until one is found with
     documentation. Then, joins all their definitions, and the documentation of
     the last item (if any), into a <dl> element. *)
  let leaf_item_group
      : render_leaf_item:_ -> 'kind -> 'item list -> (content * 'item list) =
      fun ~render_leaf_item first_item_kind items ->

    let rec consume_leaf_items_until_one_is_documented =
        fun items acc ->

      match items with
      | (`Leaf_item (this_item_kind, item))::items
          when this_item_kind = first_item_kind ->

        let rendered_item, maybe_docs = render_leaf_item item in
        (* Temporary coercion until https://github.com/ocsigen/tyxml/pull/193
           is released in TyXML; see also type [rendered_item]. *)
        let rendered_item = List.map Html.Unsafe.coerce_elt rendered_item in
        let rendered_item = Html.dt rendered_item in
        let acc = rendered_item::acc in
        begin match maybe_docs with
        | [] ->
          consume_leaf_items_until_one_is_documented items acc
        | docs ->
          let docs = docs_to_general_html docs in
          let docs = Html.dd docs in
          List.rev (docs::acc), items
        end

      | _ ->
        List.rev acc, items
    in

    let rendered_item_group, remaining_items =
      consume_leaf_items_until_one_is_documented items [] in

    Html.dl rendered_item_group, remaining_items



  let rec skip_everything_until_next_stop_comment : 'item list -> 'item list =
    function
    | [] -> []
    | item::items ->
      match item with
      | `Comment `Stop -> items
      | _ -> skip_everything_until_next_stop_comment items



  type ('kind, 'item) sectioning_state = {
    current_comment : Comment.docs;
    items : (('kind, 'item) tagged_item) list;
    heading_is_adjacent : bool;
    rendered : content list;
    toc : toc;
    subpages : Html_tree.t list;
  }

  let finish_section state =
    {state with
      rendered = List.rev state.rendered;
      toc = List.rev state.toc;
    }

  let is_deeper_section_level =
    let level_to_int = function
      | `Title -> 1
      | `Section -> 2
      | `Subsection -> 3
      | `Subsubsection -> 4
    in
    fun ~than other_level ->
      level_to_int other_level > level_to_int than

  let render_comment_until_heading_or_end
      : Comment.docs -> content list * Comment.docs = fun docs ->

    let rec scan_comment acc docs =
      match docs with
      | [] -> List.rev acc, docs
      | block::rest ->
        match block.Location.value with
        | `Heading _ -> List.rev acc, docs
        | _ -> scan_comment (block::acc) rest
    in
    let included, remaining = scan_comment [] docs in
    docs_to_general_html included, remaining

  let lay_out ~render_leaf_item ~render_nested_article items =

    let rec section_content section_level state =

      match state.current_comment with
      | block::rest ->
        begin match block.Location.value with
        | `Heading (level, label, _) ->
          if not (is_deeper_section_level ~than:section_level level) then
            finish_section state
          else
            let nested_result =
              section_content level {state with
                  current_comment = rest;
                  heading_is_adjacent = true;
                  rendered = docs_to_general_html [block];
                  toc = [];
                }
            in
            let subsection = Html.section nested_result.rendered in

            let Paths.Identifier.Label (_, label) = label in
            let toc_entry = `Section (label, nested_result.toc) in

            section_content section_level {nested_result with
                heading_is_adjacent = false;
                rendered = subsection::state.rendered;
                toc = toc_entry::state.toc;
              }

        | _ ->
          let rendered_docs, comment_remaining =
            render_comment_until_heading_or_end state.current_comment in
          let rendered_docs =
            if state.heading_is_adjacent then
              (* TODO Remove coercion. *)
              let heading_markup = state.rendered in
              [
                Html.header
                  (List.map
                    Html.Unsafe.coerce_elt (heading_markup @ rendered_docs))
              ]
            else
              (Html.aside rendered_docs)::state.rendered
          in
          section_content section_level {state with
              current_comment = comment_remaining;
              heading_is_adjacent = false;
              rendered = rendered_docs;
            }
        end

      | [] ->
        let state =
          if not state.heading_is_adjacent then
            state
          else
            (* TODO Remove coercion. *)
            let heading_markup =
              List.map Html.Unsafe.coerce_elt state.rendered in
            {state with
              heading_is_adjacent = false;
              rendered = [Html.header heading_markup];
            }
        in

        match state.items with
        | [] ->
          finish_section state

        | item::rest ->
          match item with
          | `Leaf_item (kind, _) ->
            let rendered_group, remaining_items =
              leaf_item_group ~render_leaf_item kind state.items in
            section_content section_level {state with
                items = remaining_items;
                rendered = rendered_group::state.rendered;
              }

          | `Nested_article item ->
            let rendered_item, more_subpages = render_nested_article item in
            let rendered_item = Html.article rendered_item in
            section_content section_level {state with
                items = rest;
                rendered = rendered_item::state.rendered;
                subpages = state.subpages @ more_subpages;
              }

          | `Comment `Stop ->
            let remaining_items =
              skip_everything_until_next_stop_comment rest in
            section_content section_level {state with
                items = remaining_items;
              }

          | `Comment (`Docs docs) ->
            section_content section_level {state with
              current_comment = docs;
              items = rest;
            }
    in

    let initial_state =
      {
        current_comment = [];
        items;
        heading_is_adjacent = false;
        rendered = [];
        toc = [];
        subpages = []
      }
    in
    let result = section_content `Title initial_state in

    result.rendered, result.toc, result.subpages



  (* TODO This is preliminary markup, we'll figure out how to structure nesting
     later. *)
  let render_toc toc =
    let rec traverse acc = function
      | [] -> acc
      | (`Section (anchor, children))::more ->
        let acc = anchor::acc in
        let acc = traverse acc children in
        traverse acc more
    in
    traverse [] toc
    |> List.rev_map (fun anchor ->
      [Html.br (); Html.a ~a:[Html.a_href ("#" ^ anchor)] [Html.pcdata anchor]])
    |> List.flatten
    |> Html.nav
    |> fun html -> [html]
end



module Class :
sig
  val class_ :
    Lang.Class.t ->
      ((Html_types.article_content Html.elt) list) * (Html_tree.t list)

  val class_type :
    Lang.ClassType.t ->
      ((Html_types.article_content Html.elt) list) * (Html_tree.t list)
end =
struct
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
    Markup.make_def ~id:t.id ~code:method_, t.doc

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
    Markup.make_def ~id:t.id ~code:val_, t.doc

  and class_type_expr
    : 'inner_row 'outer_row. Model.Lang.ClassType.expr
    -> ('inner_row, 'outer_row) text Html.elt list
    = fun (cte : Model.Lang.ClassType.expr) ->
      match cte with
      | Constr (path, args) ->
        let link = Html_tree.Relative_link.of_path ~stop_before:false path in
        format_type_path ~delim:(`brackets) args link
      | Signature _ ->
        [ Markup.keyword "object" ; Html.pcdata " ... " ; Markup.keyword "end" ]

  and class_decl
    : 'inner_row 'outer_row. Model.Lang.Class.decl
    -> ('inner_row, 'outer_row) text Html.elt list
    = fun (cd : Model.Lang.Class.decl) ->
      match cd with
      | ClassType expr -> class_type_expr expr
      (* TODO: factorize the following with [type_expr] *)
      | Arrow (None, src, dst) ->
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Markup.arrow :: Html.pcdata " " :: class_decl dst
      | Arrow (Some lbl, src, dst) ->
        Markup.label lbl @ Html.pcdata ":" ::
        type_expr ~needs_parentheses:true src @
        Html.pcdata " " :: Markup.arrow :: Html.pcdata " " :: class_decl dst

  and class_ (t : Model.Lang.Class.t) =
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
        let doc = docs_to_general_html t.doc in
        let expansion, _, _ = class_signature csig in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make expansion [] in
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
      Markup.make_def ~id:t.id ~code:class_def_content
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree

  and class_type (t : Model.Lang.ClassType.t) =
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
        let doc = docs_to_general_html t.doc in
        let expansion, _, _ = class_signature csig in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make expansion [] in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Class name ] [Html.pcdata name], [subtree]
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
      Markup.make_def ~id:t.id ~code:ctyp
        (* ~doc:(relax_docs_type (Documentation.first_to_html t.doc)) *)
    in
    region, subtree
end
open Class



module Module :
sig
  val signature :
    Lang.Signature.t ->
      (Html_types.div_content Html.elt) list * toc * Html_tree.t list
end =
struct
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

  and render_nested_signature_or_class : Lang.Signature.item -> _ = function
    | Module m -> module_ m
    | ModuleType m -> module_type m
    | Class c -> class_ c
    | ClassType c -> class_type c
    | Include m -> include_ m
    | _ -> assert false

  and signature s =
    let tagged_items = List.map tag_signature_item s in
    Top_level_markup.lay_out
      ~render_leaf_item:render_leaf_signature_item
      ~render_nested_article:render_nested_signature_or_class
      tagged_items

  and functor_argument
    : 'row. Model.Lang.FunctorArgument.t
    -> Html_types.div_content Html.elt list * Html_tree.t list
  = fun arg ->
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
        let subtree = Html_tree.make docs subpages in
        Html_tree.leave ();
        (
          Html.a ~a:[ a_href ~kind:`Arg link_name ] [Html.pcdata name] ::
          Html.pcdata " : " ::
          mty (Paths.Identifier.signature_of_module arg.id) arg.expr
        ), [subtree]
    in
    let region = Markup.make_def ~id:arg.id ~code:def_div in
    region, subtree

  and module_expansion
    : Model.Lang.Module.expansion
    -> Html_types.div_content_fun Html.elt list * Html_tree.t list
  = fun t ->
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
            let arg, arg_subpages = functor_argument arg in
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
      : Model.Lang.Module.t ->
          Html_types.article_content Html.elt list * Html_tree.t list
      = fun t ->
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
        let doc = docs_to_general_html t.doc in
        let expansion, subpages = module_expansion expansion in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make expansion subpages in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mod modname ] [Html.pcdata modname], [subtree]
    in
    let md_def_content = Markup.keyword "module " :: modname :: md in
    let region =
      Markup.make_def ~id:t.id ~code:md_def_content
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

  and module_type (t : Model.Lang.ModuleType.t) =
    let modname = Paths.Identifier.name t.id in
    let mty =
      match t.expr with
      | None -> []
      | Some expr ->
        begin match expr with
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
        let doc = docs_to_general_html t.doc in
        let expansion, subpages = module_expansion expansion in
        let expansion =
          match doc with
          | [] -> expansion
          | _ -> Html.div ~a:[ Html.a_class ["doc"] ] doc :: expansion
        in
        let subtree = Html_tree.make expansion subpages in
        Html_tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mty modname ] [Html.pcdata modname], [subtree]
    in
    let mty_def =
      (
        Markup.keyword "module type " ::
        modname ::
        mty
      )
    in
    let region =
      Markup.make_def ~id:t.id ~code:mty_def
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
      [ Markup.keyword "sig" ; Html.pcdata " ... " ; Markup.keyword "end" ]
    | Functor (None, expr) ->
      Markup.keyword "functor" :: Html.pcdata " () " ::
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
      Markup.keyword "functor" ::
      Html.pcdata " (" :: name :: Html.pcdata " : " ::
      mty base arg.expr @
      Html.pcdata ") -> " ::
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
      format_params td.Model.Lang.TypeDecl.Equation.params ::
      Html_tree.Relative_link.of_fragment
        ~base (Paths.Fragment.any_sort frag_typ) @
      fst (format_manifest td) @
      format_constraints td.Model.Lang.TypeDecl.Equation.constraints
    | ModuleSubst (frag_mod, mod_path) ->
      Markup.keyword "module " ::
      Html_tree.Relative_link.of_fragment
        ~base (Paths.Fragment.signature_of_module frag_mod) @
      Html.pcdata " := " ::
      Html_tree.Relative_link.of_path ~stop_before:true mod_path
    | TypeSubst (frag_typ, vars, typ_path) ->
      let params =
        Html.pcdata begin match vars with
          | [] -> ""
          | [v] -> v ^ "\194\160"
          | _ -> "(" ^ String.concat ",\194\160" vars ^ ")\194\160"
        end
      in
      Markup.keyword "type " ::
      params ::
      Html_tree.Relative_link.of_fragment
        ~base (Paths.Fragment.any_sort frag_typ) @
      Html.pcdata " := " ::
      params ::
      Html_tree.Relative_link.of_path ~stop_before:false typ_path

  and include_ (t : Model.Lang.Include.t) =
    let docs = docs_to_general_html t.doc in
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
          )
        in
        (* FIXME: I'd like to add an anchor here, but I don't know what id to
           give it... *)
        [
          Html.details ~a:(if should_be_open then [Html.a_open ()] else [])
            (Markup.def_summary [incl])
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
  val compilation_unit : Lang.Compilation_unit.t -> Html_tree.t
  val page : Lang.Page.t -> Html_tree.t
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
      Markup.make_def ~id:x.Compilation_unit.Packed.id ~code:md_def
    end
    |> List.flatten
    |> fun definitions ->
      [Html.article definitions]



  let compilation_unit (t : Model.Lang.Compilation_unit.t) : Html_tree.t =
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
        let html, toc, subpages = signature sign in
        let header_docs =
          match toc with
          | [] -> header_docs
          | _ -> header_docs @ (Top_level_markup.render_toc toc)
        in
        header_docs, html, subpages
      | Pack packed ->
        header_docs, pack packed, []
    in
    Html_tree.make ~header_docs html subtree



  let page (t : Model.Lang.Page.t) : Html_tree.t =
    let package, name =
      match t.name with
      | Model.Paths.Identifier.Page (a, name) -> a.package, name
    in
    Html_tree.enter package;
    Html_tree.enter ~kind:`Page name;
    let html = relax_docs_type (Documentation.to_html t.content) in
    Html_tree.make html []
end
include Page
