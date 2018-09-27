module Location = Model.Location_
module Paths = Model.Paths
module Comment = Model.Comment
module Lang = Model.Lang
module Html = Tyxml.Html
open Utils

(**
   Utility types and functions
  *)
type rendered_item = Html_types.div_content Html.elt list

(* [rendered_item] should really be [dt_content], but that is bugged in TyXML
   until https://github.com/ocsigen/tyxml/pull/193 is released. *)

type ('inner, 'outer) text =
  [> `PCDATA | `Span | `A of ([> `PCDATA] as 'inner)] as 'outer

(* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)

type ('item_kind, 'item) tagged_item =
  [ `Leaf_item of 'item_kind * 'item
  | `Nested_article of 'item
  | `Comment of Comment.docs_or_stop ]

type section =
  {anchor: string; text: Comment.link_content; children: section list}

type toc = section list

(**
  HTML Generation Syntax definition module.

  @see {!To_re_html_tree} and {!To_ml_html_tree}
  *)
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

module Make (Syn : Syntax) : Html_generator
