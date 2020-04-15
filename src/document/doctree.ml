open Types

module Toc = struct

  type t = one list

  and one = {
    anchor : string;
    text : Inline.t;
    children : t
  }

  let rec walk_items ~on_nested current_level acc (t : Item.t list) =
    match t with
    | [] -> List.rev acc, []
    | b :: rest ->
      match b with
      | Text _
      | Declarations (_, _)
      | Declaration (_, _)
        -> walk_items ~on_nested current_level acc rest
      | Nested
          ({ content = { status; items; _ }; _ }, _) ->
        if on_nested status then
          walk_items ~on_nested current_level acc (items @ rest)
        else
          walk_items ~on_nested current_level acc rest
      | Section (doc, items) ->
        walk_items ~on_nested current_level acc (doc@items@rest)
      | Heading { label = None; _ } ->
        walk_items ~on_nested current_level acc rest
      | Heading { label = Some label; level; title } ->
        if level > current_level then
          let children, rest = walk_items ~on_nested level [] rest in
          let toc_entry = { anchor = label; text = title; children } in
          walk_items ~on_nested current_level (toc_entry :: acc) rest
        else
          List.rev acc, t

  let on_nested_default : Nested.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let compute ?(on_nested=on_nested_default) t =
    let rec loop items =
      let toc, rest = walk_items ~on_nested 0 [] items in
      match rest with
      | [] -> toc
      | l -> toc @ loop l
    in
    loop t

end
