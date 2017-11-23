type style =
  | Bold
  | Italic
  | Emphasize
  | Center
  | Left
  | Right
  | Superscript
  | Subscript
  | Custom of string

type reference =
  | Element of Paths.Reference.any
  | Link of string
  | Custom of string * string

type see =
  | Url of string
  | File of string
  | Doc of string

type text = text_element list

and text_element =
  | Raw of string
  | Code of string
  | PreCode of string
  | Verbatim of string
  | Style of style * text
  | List of text list
  | Enum of text list
  | Newline
  | Title of int * Paths.Identifier.label option * text
  | Reference of reference * text option
  | Target of string option * string
  | Special of special

and tag =
  | Author of string
  | Version of string
  | See of see * text
  | Since of string
  | Before of string * text
  | Deprecated of text
  | Param of string * text
  | Raise of string * text
  | Return of text
  | Inline
  | Tag of string * text
  | Canonical of Paths.Path.module_ * Paths.Reference.module_

and special =
  | Modules of (Paths.Reference.module_ * text) list
  | Index

module Error =
struct
  module Position =
  struct
    type t = {
      line : int;
      column : int;
    }
  end

  module Offset =
  struct
    type t = {
      start: Position.t;
      finish: Position.t;
    }
  end

  module Location =
  struct
    type t = {
      filename: string;
      start: Position.t;
      finish: Position.t;
    }
  end

  type t = {
    origin: Paths.Identifier.any; (** TODO remove this *)
    offset: Offset.t;
    location: Location.t option;
    message: string;
  }
end

type body = {
  text: text;
  tags: tag list;
}

type t =
  | Ok of body
  | Error of Error.t

type comment =
  | Documentation of t
  | Stop
