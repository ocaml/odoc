type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

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

and Source : sig
  type t = token list

  and tag = string option
  (** any OCaml keyword, forexample, module, sig et cetera. *)

  and token =
    | Elt of Inline.t
        (** Elt is anything part of the inline content, including the white space. *)
    | Tag of tag * t
end =
  Source

(** Inline is for content wrapped in one line. *)
and Inline : sig
  type entity = string

  type href = string

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity  (** special characters such as `->`. *)
    | Linebreak
    | Styled of style * t
    | Link of href * t
        (**  A link to a given address, that is, [ {{: string} inline-text}]. *)
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t
end =
  Inline

(** description for [odoc] documentation tags*)
and Description : sig
  type one = {
    attr : Class.t;  (** [odoc]-specific documentation tag. *)
    key : Inline.t;  (** key for doc tag. *)
    definition : Block.t;  (** the value for a doc tag. *)
  }

  type t = one list
end =
  Description

and Heading : sig
  type t = { label : string option; level : int; title : Inline.t }
end =
  Heading

(** Block is for multi-inline content. *)
and Block : sig
  type t = one list

  and one = {
    attr : Class.t;
        (** forexample `at-tags` for
        @<{{:https://ocaml.github.io/odoc/odoc_for_authors.html#tags}odoc-specific>}
        documentation tags. *)
    desc : desc;
  }

  and desc =
    | Inline of Inline.t  (** text/data in a single line. *)
    | Paragraph of Inline.t  (** multi line text. *)
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

(** Alternative is for OCaml syntaxes that are expandable, but as well
    representable in summary form. *)
and Alternative : sig
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
    summary : Source.t;
        (** summary of a module, that is, [module X : sig ... end]. *)
    expansion : DocumentedSrc.t;
        (** expandable form of a module, for example {[
          module Foo X : sig
            type t
          end
        ]} *)
    url : Url.Path.t;  (** resolved/unresolved reference of module. *)
  }

  type t = Expansion of expansion
end =
  Alternative

(**  Subpage is the lower level of a module. *)
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
    title : string;  (** The filename. *)
    header : Item.t list;
        (** Is the `Module + filename`, however, for subpages, it is
      `kind of the subpage + filename`. for example `Class + filename`. *)
    items : Item.t list;
        (** other contents of the page, forexample, body, footer etc. *)
    url : Url.Path.t;
  }
end =
  Page

let inline ?(attr = []) desc = Inline.{ attr; desc }

let block ?(attr = []) desc = Block.{ attr; desc }
