type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

module rec Class : sig
  type t = string list
end =
  Class

and InternalLink : sig
  type resolved = Url.t * Inline.t

  type unresolved = Inline.t

  type t = Resolved of resolved | Unresolved of Inline.t
end =
  InternalLink

and Raw_markup : sig
  type target = Odoc_model.Comment.raw_markup_target

  and t = target * string
end =
  Raw_markup

and Source : sig
  type t = token list

  and tag = string option

  and token = Elt of Inline.t | Tag of tag * t
end =
  Source

and Inline : sig
  type entity = string

  type href = string

  type preformatted = { begin_ : bool; end_ : bool }

  type t = one list

  and one = { attr : Class.t; preformatted : preformatted; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t
end =
  Inline

and Description : sig
  type one = { attr : Class.t; key : Inline.t; definition : Block.t }

  type t = one list
end =
  Description

and Heading : sig
  type t = { label : string option; level : int; title : Inline.t }
end =
  Heading

and Block : sig
  type lang_tag = string option

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of Description.t
    | Source of lang_tag * Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t

  and list_type = Ordered | Unordered
end =
  Block

and DocumentedSrc : sig
  type 'a documented = {
    attrs : Class.t;
    anchor : Url.Anchor.t option;
    code : 'a;
    doc : Block.t;
    markers : string * string;
  }

  type t = one list

  and one =
    | Code of Source.t
    | Documented of Inline.t documented
    | Nested of t documented
    | Subpage of Subpage.t
    | Alternative of Alternative.t
end =
  DocumentedSrc

and Alternative : sig
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
    summary : Source.t;
    expansion : DocumentedSrc.t;
    url : Url.Path.t;
  }

  type t = Expansion of expansion
end =
  Alternative

and Subpage : sig
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Page.t }
end =
  Subpage

and Include : sig
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Item.t list; summary : Source.t }
end =
  Include

and Item : sig
  type 'a item = {
    attr : Class.t;
    anchor : Url.Anchor.t option;
    content : 'a;
    doc : Block.t;
  }

  type declaration = DocumentedSrc.t item

  type text = Block.t

  type t =
    | Text of text
    | Heading of Heading.t
    | Declaration of DocumentedSrc.t item
    | Include of Include.t item
end =
  Item

and Page : sig
  type t = {
    title : string;
    header : Item.t list;
    items : Item.t list;
    url : Url.Path.t;
  }
end =
  Page

let rec last = function
  | [] -> invalid_arg "last"
  | [ x ] -> x
  | _ :: xs -> last xs

(* Checking whether an Inline.desc starts with or ends with preformatted text.
   This is only an approximation as we did not check whether the text is empty
   [Text ""] or the styled inline is empty [Style (_, [])]. *)
let rec is_inline_preformatted =
  let open Inline in
  function
  | Text _ | Linebreak -> { begin_ = false; end_ = false }
  | Entity _ | Source _ -> { begin_ = true; end_ = true }
  | Styled (_, is) | Link (_, is) -> is_inline_list_preformatted is
  | InternalLink il -> is_internallink_preformatted il
  (* Ideally, the markup should be parsed *)
  | Raw_markup _ -> { begin_ = false; end_ = false }

and is_inline_list_preformatted = function
  | [] -> { begin_ = false; end_ = false }
  | l ->
      {
        begin_ = (List.hd l).preformatted.begin_;
        end_ = (last l).preformatted.end_;
      }

and is_internallink_preformatted = function
  | Resolved (_, is) | Unresolved is -> is_inline_list_preformatted is

let inline ?(attr = []) desc =
  let preformatted = is_inline_preformatted desc in
  Inline.{ attr; preformatted; desc }

let block ?(attr = []) desc = Block.{ attr; desc }
