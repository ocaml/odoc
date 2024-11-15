(** Abstract syntax tree representing ocamldoc comments *)

type +'a warning = [ `Warning of 'a * Warning.t ]

type inline_element =
  [ `Space of string
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Styled of Ast.style * inline_element Ast.with_location list
  | `Reference of
    Ast.reference_kind
    * string Ast.with_location
    * inline_element Ast.with_location list
  | `Link of string * inline_element Ast.with_location list
  | `Math_span of string  (** @since 2.0.0 *)
  | Ast.inline_element warning ]

type 'a cell = 'a Ast.with_location list * [ `Header | `Data ]
type 'a row = 'a cell list
type 'a grid = 'a row list
type 'a abstract_table = 'a grid * Ast.alignment option list option

and nestable_block_element =
  [ `Paragraph of inline_element Ast.with_location list
  | `Code_block of Ast.code_block
  | `Verbatim of string
  | `Modules of string Ast.with_location list
  | `List of
    Ast.list_kind
    * Ast.list_syntax
    * nestable_block_element Ast.with_location list list
  | `Table of table
  | `Math_block of string  (** @since 2.0.0 *)
  | `Media of
    Ast.reference_kind * Ast.media_href Ast.with_location * string * Ast.media
  | Ast.nestable_block_element warning ]

and table = nestable_block_element abstract_table * [ `Light | `Heavy ]

type internal_tag =
  [ `Canonical of string Ast.with_location
  | `Inline
  | `Open
  | `Closed
  | `Hidden ]
(** Internal tags are used to exercise fine control over the output of odoc. They
    are never rendered in the output *)

type ocamldoc_tag =
  [ `Author of string
  | `Deprecated of nestable_block_element Ast.with_location list
  | `Param of string * nestable_block_element Ast.with_location list
  | `Raise of string * nestable_block_element Ast.with_location list
  | `Return of nestable_block_element Ast.with_location list
  | `See of
    [ `Url | `File | `Document ]
    * string
    * nestable_block_element Ast.with_location list
  | `Since of string
  | `Before of string * nestable_block_element Ast.with_location list
  | `Version of string ]

type tag = [ ocamldoc_tag | internal_tag | Ast.tag warning ]
type heading = int * string option * inline_element Ast.with_location list

type block_element =
  [ nestable_block_element | `Heading of heading | `Tag of tag ]

type t = block_element Ast.with_location list

module Unpack = struct
  let children f =
    List.fold_left
      (fun (elts, warnings) lelt ->
        let elt, elt_warnings = f lelt.Loc.value in
        ( Loc.{ value = elt; location = lelt.Loc.location } :: elts,
          elt_warnings @ warnings ))
      ([], [])

  let rec inline : inline_element -> Ast.inline_element * Warning.t list =
    function
    | `Warning (node, warning) -> (node, [ warning ])
    | `Code_span s -> (`Code_span s, [])
    | `Link (s, child_elts) ->
        let children, warnings = children inline child_elts in
        (`Link (s, children), warnings)
    | `Math_span s -> (`Math_span s, [])
    | `Raw_markup (s1, s2) -> (`Raw_markup (s1, s2), [])
    | `Reference (k, s, child_elts) ->
        let children, warnings = children inline child_elts in
        (`Reference (k, s, children), warnings)
    | `Space s -> (`Space s, [])
    | `Styled (s, child_elts) ->
        let children, warnings = children inline child_elts in
        (`Styled (s, children), warnings)
    | `Word s -> (`Word s, [])

  let rec nestable_block :
      nestable_block_element -> Ast.nestable_block_element * Warning.t list =
    function
    | `Warning (node, warning) -> (node, [ warning ])
    | `Paragraph child_elts ->
        let child_elts, warnings = children inline child_elts in
        (`Paragraph child_elts, warnings)
    | `List (kind, syntax, child_elts) ->
        let child_elts, warnings =
          List.map (children nestable_block) child_elts |> List.split
        in
        (`List (kind, syntax, child_elts), List.flatten warnings)
    | `Table _ -> assert false
    | (`Code_block _ | `Verbatim _ | `Modules _ | `Math_block _ | `Media _) as
      node ->
        (node, [])

  and table : table -> Ast.table * Warning.t list =
   fun ((grid, alignment), syntax) ->
    let rec go row_acc warning_acc :
        nestable_block_element grid ->
        Ast.nestable_block_element grid * Warning.t list = function
      | row :: rows ->
          let row, warnings =
            List.map
              (fun (cell : nestable_block_element cell) ->
                let cell, tag = cell in
                let cell, warnings = children nestable_block cell in
                ((cell, tag), warnings))
              row
            |> List.split
          in
          go (row :: row_acc) (List.flatten warnings @ warning_acc) rows
      | [] -> (row_acc, warning_acc)
    in
    let grid, warnings = go [] [] grid in
    (((grid, alignment), syntax), warnings)

  let tag : tag -> Ast.tag * Warning.t list = function
    | `Warning (node, warning) -> (node, [ warning ])
    | `Deprecated child_elts ->
        let child_elts, warnings = children nestable_block child_elts in
        (`Deprecated child_elts, warnings)
    | ( `Param (name, child_elts)
      | `Raise (name, child_elts)
      | `Before (name, child_elts) ) as node -> (
        let child_elts, warnings = children nestable_block child_elts in
        match node with
        | `Param _ -> (`Param (name, child_elts), warnings)
        | `Raise _ -> (`Raise (name, child_elts), warnings)
        | `Before _ -> (`Before (name, child_elts), warnings))
    | `Return child_elts ->
        let child_elts, warnings = children nestable_block child_elts in
        (`Return child_elts, warnings)
    | `See (ref_kind, ref, child_elts) ->
        let child_elts, warnings = children nestable_block child_elts in
        (`See (ref_kind, ref, child_elts), warnings)
    | ( `Author _ | `Since _ | `Version _ | `Closed | `Open | `Hidden
      | `Canonical _ | `Inline ) as t ->
        (t, [])
end

let unpack : t -> Ast.t * Warning.t list =
 fun self ->
  let open Loc in
  let go (nodes, warnings) :
      block_element with_location -> Ast.t * Warning.t list = function
    | { value = `Heading (n, s, child_elts); _ } as loc ->
        let child_elts, warnings = Unpack.children Unpack.inline child_elts in
        let heading = `Heading (n, s, child_elts) in
        ({ loc with value = heading } :: nodes, warnings)
    | { value = `Tag tag; _ } as loc ->
        let tag, tag_warnings = Unpack.tag tag in
        ({ loc with value = `Tag tag } :: nodes, tag_warnings @ warnings)
    | { value = #nestable_block_element as node; _ } as loc ->
        let node, node_warnings = Unpack.nestable_block node in
        let node = (node :> Ast.block_element) in
        ({ loc with value = node } :: nodes, node_warnings @ warnings)
  in
  List.fold_left go ([], []) self
