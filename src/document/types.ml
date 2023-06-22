type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

module rec Class : sig
  type t = string list
end =
  Class

and InternalLink : sig
  type target = Resolved of Url.t | Unresolved

  type t = { target : target; content : Inline.t; tooltip : string option }
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

and Math : sig
  type t = string
end =
  Math

and Inline : sig
  type entity = string

  type href = string

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Math of Math.t
    | Raw_markup of Raw_markup.t
end =
  Inline

and Description : sig
  type one = { attr : Class.t; key : Inline.t; definition : Block.t }

  type t = one list
end =
  Description

and Heading : sig
  type t = {
    label : string option;
    level : int;
    title : Inline.t;
    source_anchor : Url.t option;
        (** Used for the source link of the item displayed on the page. *)
  }
end =
  Heading

and Block : sig
  type lang_tag = string

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of Description.t
    | Source of lang_tag * Source.t
    | Math of Math.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t
    | Table of t Table.t

  and list_type = Ordered | Unordered
end =
  Block

and Table : sig
  type alignment = Left | Center | Right | Default

  type 'a t = {
    data : ('a * [ `Header | `Data ]) list list;
    align : alignment list;
  }
end =
  Table

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
    source_anchor : Url.Anchor.t option;
  }

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
    preamble : Item.t list;
    items : Item.t list;
    url : Url.Path.t;
    source_anchor : Url.t option;
        (** Url to the corresponding source code. Might be a whole source file
            or a sub part. *)
  }
end =
  Page

and Source_page : sig
  type info = Syntax of string | Anchor of string | Link of Url.Anchor.t

  type code = span list
  and span = Tagged_code of info * code | Plain_code of string

  type t = { url : Url.Path.t; contents : code }
end =
  Source_page

and Asset : sig
  type t = { url : Url.Path.t; src : Fpath.t }
end =
  Asset

module Document = struct
  type t = Page of Page.t | Source_page of Source_page.t | Asset of Asset.t
end

let inline ?(attr = []) desc = Inline.{ attr; desc }

let block ?(attr = []) desc = Block.{ attr; desc }
