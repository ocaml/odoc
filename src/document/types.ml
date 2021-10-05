type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]
(** The type for styling text.
  { ul
    {- [`Bold], sets / styles text in bold. }
    {- [`Italic], sets text in bold. }
    {- [`Emphasis], sets text in emphasised or noticeable format. }
    {- [`Superscript], sets text (usually smaller than the rest of the text)
       slightly above the normal line }
    {- [`Subscript], sets text (usually smaller than the rest of the text)
       slightly below the normal line.} }*)

module rec Class : sig
  type t = string list
end =
  Class

(** InternalLink. 
  
      An internal link is one that is formed as a result of references, either
      resolved or unresolved *)
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

(** The module Source is for any content that is OCaml syntax as
    long as it's not part of the doc comment. *)
and Source : sig
  type t = token list

  and tag = string option

  and token = Elt of Inline.t | Tag of tag * t
end =
  Source

and Inline : sig
  type entity = string
  (** type entity is for characters such as arrows *)

  type href = string
  (** type for a link in a documentation comment. *)

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t  (** style applied to a particular text/data *)
    | Link of href * t
        (**  a link to a given address specified by the author 
      as [odoc] link, that is, [ {{: string} inline-text}] *)
    | InternalLink of InternalLink.t
        (** the link generated as a result of resolved/unresolved
      references. *)
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
  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Inline of Inline.t  (** text/data in a single line. *)
    | Paragraph of Inline.t  (** multi line text*)
    | List of list_type * t list
    | Description of Description.t
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t

  and list_type = Ordered | Unordered
end =
  Block

(** DocumentedSrc is for any OCaml syntax that has documentation attached to it. *)
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

(** Alternative is for OCaml syntaxes that are expandable, but as will
    representable in summary form. *)
and Alternative : sig
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
    summary : Source.t;
        (** record field summary is for module that can
    represented in summary form, that is, [module X : sig ... end]. *)
    expansion : DocumentedSrc.t;
        (** record field expansion is for a module
    that can be represented in expandable form. for example {[
          module Foo X : sig
            type t
          end
        ]} *)
    url : Url.Path.t;
        (** field url is the resolved/unresolved reference of module. *)
  }

  type t = Expansion of expansion
end =
  Alternative

(** module Subpage is lower level of a module. *)
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
    doc : Block.t;  (** doc comment. *)
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

let inline ?(attr = []) desc = Inline.{ attr; desc }

let block ?(attr = []) desc = Block.{ attr; desc }
