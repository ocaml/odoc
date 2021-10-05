type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]
(** The type for styling text.
  { ul
    {- [`Bold], sets / styles text in bold. }
    {- [`Italic], sets text in bold. }
    {- [`Emphasis], sets text in emphasised or noticeable format. }
    {- [`Superscript], sets text (usually smaller than the rest of the text)
       slightly above the normal line }
    {- [`Subscript], sets text (usually smaller than the rest of the text)
       slightly below the normal line. }} *)

module rec Class : sig
  type t = string list
end =
  Class

(** InternalLink. 
  
      An internal link is one that is formed as a result of references, either
      resolved or unresolved *)
and InternalLink : sig
  (** {1: internal_link InternalLink} *)

  type resolved = Url.t * Inline.t
  (** type for resolved references. *)

  type unresolved = Inline.t
  (** type for unresolved reference. *)

  type t = Resolved of resolved | Unresolved of Inline.t

  (** The type for internal link. *)
end =
  InternalLink

(** Raw_markup 

   This module defines types for raw markup*)
and Raw_markup : sig
  type target = Odoc_model.Comment.raw_markup_target

  and t = target * string
  (** The type for raw_markup *)
end =
  Raw_markup

(** The module Source is for any content that is OCaml syntax as
    long as it's not part of the doc comment. *)
and Source : sig
  type t = token list

  and tag = string option
  (* TODO: I don't know how well to phrase this! *)

  and token = Elt of Inline.t | Tag of tag * t
end =
  Source

(** The Inline module handles data lined up in a single line *)
and Inline : sig
  type entity = string
  (** type entity is for characters such as arrows *)

  (*TODO: this isn't comprehensive enough! *)

  type href = string
  (** type for a link in a documentation comment. *)

  type t = one list
  (** They type for Inline*)

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t
        (** type for description
    {ul
      {- [Text] is the data in text format.}
      {- [Entity] is  (*TODO: Not sure of this! *).}
      {- [Linebreak] is the line break between text.}
      {- [Styled] is the style applied to a particular text/data.}
      {- [Link] is a link to a given address specified by the author 
         as [odoc] link, that is, [ {{: string} inline-text}].}
      {- [InternalLink] is the link generated as a result of resolved/unresolved
          references.}
      {- [Source] is any content considered to be OCaml syntax, except OCaml comments
          and doc comments.}
      {- [Raw_markup] is raw markup.} (*TODO: This may require an improvement! *)
    }*)
end =
  Inline

(* TODO: Not sure on how to translate this to human language. *)
and Description : sig
  type one = { attr : Class.t; key : Inline.t; definition : Block.t }

  type t = one list
end =
  Description

(** The Heading module is for text specified as heading. *)
and Heading : sig
  type t = {
    label : string option;  (** [label] for the heading. *)
    level : int;
        (** [level] is the heading level. For example [1] in [{1 this is the heading}]. *)
    title : Inline.t;  (** [title] is the text to appear as a heading. *)
  }
  (** type of heading. *)
end =
  Heading

(** The module Block is for multi line data, either paragraph, verbatim, or multi-line
    source code, lists, et cetera. *)
and Block : sig
  type t = one list
  (** type for Block. *)

  and one = { attr : Class.t; desc : desc }
  (** TODO: Not sure about this! *)

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of Description.t
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t
        (** type for description
     {ul
       {- [Inline] is for text/data in a single line.}
       {- [Paragraph] is for multi line text. }
       {- [List] is for list items.}
       {- [Descriptions] (*TODO: Not sure! *).}
       {- [Source] is the OCaml syntax.} (** Not sure! *)
     }
  *)

  and list_type =
    | Ordered
    | Unordered
        (** type of the list
    {ul
    {- [Ordered] is for ordered list.}
    {- [Unordered] is for unordered list.}
    }
  *)
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
    representable in sunmmary form. *)
and Alternative : sig
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
        (** record field status how modules should be represented
     {ul
     {- [`Inline] means the module should be represented in a single line.}
     {- [`Open] means the module should be opened on rendering.}
     {- [`Closed] means the module should be closed on rendering.}
     {- [`Default] is the default of how a module should be represented.} (* TODO: Not sure of the default behavior *)
     }
    *)
    summary : Source.t;
        (** record field summary is for module that can represented in summary
        form, that is, [module X : sig ... end] *)
    expansion : DocumentedSrc.t;
        (** record field expansion is for a module that can be represented in expandable form.
        for example {[
          module Foo X : sig
            type t
          end
        ]} *)
    url : Url.Path.t;
        (** field url is the resolved/unresolved reference of module. *)
  }

  type t = Expansion of expansion  (** type of Alternative module. *)
end =
  Alternative

(** module Subpage is lower level of a module. *)
and Subpage : sig
  type status = [ `Inline | `Open | `Closed | `Default ]
  (** type status is for the representable form of the subpage. *)

  type t = { status : status; content : Page.t }
  (** type of the subpage*)
end =
  Subpage

(** The module Include is for included items in a module. *)
and Include : sig
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Item.t list; summary : Source.t }
end =
  Include

(** Item is for sinlge item in OCaml syntax. *)
and Item : sig
  type 'a item = {
    attr : Class.t;
    anchor : Url.Anchor.t option;
    content : 'a;
    doc : Block.t;  (** The doc comment. *)
  }

  type declaration = DocumentedSrc.t item
  (** type declaration is for declaration of an item that might have doc comments
      attached to it. *)

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
