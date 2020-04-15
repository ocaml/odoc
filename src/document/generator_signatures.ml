open Types
module Lang = Odoc_model.Lang



type rendered_item = DocumentedSrc.t

type text = Format.formatter -> unit

type ('item_kind, 'item) tagged_item = [
  | `Leaf_item of 'item_kind * 'item
  | `Nested_article of 'item
  | `Comment of Odoc_model.Comment.docs_or_stop
]

(** HTML generation syntax customization module. See {!To_re_html_tree} and
    {!To_ml_html_tree}. *)
module type SYNTAX =
sig
  module Obj :
  sig
    val close_tag_closed : string
    val close_tag_extendable : string
    val field_separator : string
    val open_tag_closed : string
    val open_tag_extendable : string
  end

  module Type :
  sig
    val annotation_separator : string

    val handle_constructor_params : text -> text -> text
    val handle_substitution_params : text -> text -> text

    val handle_format_params : string -> string
    val type_def_semicolon : bool
    val private_keyword : string
    val parenthesize_constructor : bool

    module Variant :
    sig
      val parenthesize_params : bool
    end

    module Tuple :
    sig
      val element_separator : string
      val always_parenthesize : bool
    end

    module Record :
    sig
      val field_separator : string
    end

    val var_prefix : string

    val any : string

    val arrow : text

    module Exception :
    sig
      val semicolon : bool
    end

    module GADT :
    sig
      val arrow : text
    end

    module External :
    sig
      val semicolon : bool
      val handle_primitives :
        string list -> Inline.t
    end
  end

  module Mod :
  sig
    val open_tag : text
    val close_tag : text
    val close_tag_semicolon : bool
    val include_semicolon : bool
    val functor_keyword : bool
  end

  module Class :
  sig
    val open_tag : text
    val close_tag : text
  end

  module Value :
  sig
    val variable_keyword : string
    val semicolon : bool
  end
end



module type GENERATOR =
sig
  module Top_level_markup :
  sig
    type heading_level_shift

    val lay_out :
      heading_level_shift option ->
      item_to_id:('item -> string option) ->
      item_to_spec:('item -> string option) ->
      render_leaf_item:('item -> rendered_item * Odoc_model.Comment.docs) ->
      render_nested_article:
        (heading_level_shift -> 'item ->
          rendered_item * Odoc_model.Comment.docs * Block.t) ->
      (_, 'item) tagged_item list ->
      Block.t * Block.t

  val lay_out_page :
    Odoc_model.Comment.docs -> Block.t * Block.t

  end

  module Page :
  sig
    val compilation_unit :
      ?theme_uri:string -> Lang.Compilation_unit.t -> Block.t
    val page : ?theme_uri:string -> Lang.Page.t -> Block.t
  end
end
