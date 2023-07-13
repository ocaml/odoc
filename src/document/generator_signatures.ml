open Types
module Lang = Odoc_model.Lang

type rendered_item = DocumentedSrc.t

type text = Codefmt.t

(** HTML generation syntax customization module. See {!ML} and
    {!Reason}. *)
module type SYNTAX = sig
  module Obj : sig
    val close_tag_closed : string

    val close_tag_extendable : string

    val field_separator : string

    val open_tag_closed : string

    val open_tag_extendable : string
  end

  module Type : sig
    val annotation_separator : string

    val handle_constructor_params : text -> text -> text

    val handle_substitution_params : text -> text -> text

    val handle_format_params : string -> string

    val type_def_semicolon : bool

    val private_keyword : string

    val parenthesize_constructor : bool

    module Variant : sig
      val parenthesize_params : bool
    end

    module Tuple : sig
      val element_separator : text

      val always_parenthesize : bool
    end

    module Record : sig
      val field_separator : string
    end

    val var_prefix : string

    val any : string

    val arrow : text

    module Exception : sig
      val semicolon : bool
    end

    module GADT : sig
      val arrow : text
    end

    module External : sig
      val semicolon : bool

      val handle_primitives : string list -> Inline.t
    end
  end

  module Mod : sig
    val open_tag : text

    val close_tag : text

    val close_tag_semicolon : bool

    val include_semicolon : bool

    val functor_keyword : bool

    val functor_contraction : bool
  end

  module Class : sig
    val open_tag : text

    val close_tag : text
  end

  module Value : sig
    val variable_keyword : string

    val semicolon : bool
  end

  module Comment : sig
    val markers : string * string
  end
end

module type GENERATOR = sig
  val compilation_unit : Lang.Compilation_unit.t -> Document.t

  val page : Lang.Page.t -> Document.t

  val source_tree : Lang.SourceTree.t -> Document.t list

  val source_page :
    Odoc_model.Paths.Identifier.SourcePage.t ->
    Syntax_highlighter.infos ->
    Lang.Source_info.infos ->
    string ->
    Document.t

  val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> text

  val record : Lang.TypeDecl.Field.t list -> DocumentedSrc.one list
end
