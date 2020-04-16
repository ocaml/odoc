open Types

module Rewire = struct

  type ('a, 'h) action =
    | Rec of 'a
    | Skip
    | Heading of 'h * int

  let walk ~classify ~node items =
    let rec loop current_level acc l =
      match l with
      | [] -> List.rev acc, []
      | b :: rest ->
        match classify b with
        | Skip -> loop current_level acc rest
        | Rec l -> loop current_level acc (l @ rest)
        | Heading (h, level) ->
          if level > current_level then
            let children, rest = loop level [] rest in
            loop current_level (node h children :: acc) rest
          else
            List.rev acc, l
    in
    let trees, rest = loop (-1) [] items in
    assert (rest = []);
    trees

end

module Toc = struct

  type t = one list

  and one = {
    anchor : string;
    text : Inline.t;
    children : t
  }

  let classify ~on_nested (i : Item.t) : _ Rewire.action = match i with
    | Text _
    | Declarations (_, _)
    | Declaration (_, _)
      -> Skip
    | Nested ({ content = { status; items; _ }; _ }, _) ->
      if on_nested status then Rec items else Skip
    | Section (doc, items) ->
      Rec (doc@items)
    | Heading { label = None ; _ } -> Skip
    | Heading { label = Some label; level; title } ->
      Heading ((label, title), level)

  let node (anchor, text) children = { anchor; text; children}

  let on_nested_default : Nested.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let compute ?(on_nested=on_nested_default) t =
    Rewire.walk
      ~classify:(classify ~on_nested)
      ~node
      t

end
