type break_hierarchy = Aesthetic | Simple | Line | Paragraph | Separation

type row_size =
  | Empty
  | Small  (** text only *)
  | Large  (** No table *)
  | Huge  (** tables **)

type elt =
  | Txt of string list
  | Section of section
  | Verbatim of string
  | Internal_ref of reference
  | External_ref of string * t option
  | Label of string
  | Raw of string
  | Tag of string * t
  | Style of [ `Emphasis | `Bold | `Superscript | `Subscript | `Italic ] * t
  | Code_block of t
  | Inlined_code of t
  | Code_fragment of t
  | Break of break_hierarchy
  | List of list_info
  | Description of (t * t) list
  | Indented of t
  | Layout_table of layout_table
  | Table of table
  | Ligaturable of string

and section = { level : int; label : string option; content : t }

and list_info = { typ : Odoc_document.Types.Block.list_type; items : t list }

and layout_table = { row_size : row_size; tbl : t list list }

and alignment = Odoc_document.Types.Table.alignment

and table = { align : alignment list; data : t list list }

and t = elt list

and reference = { short : bool; target : string; text : t option }
