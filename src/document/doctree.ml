open Types

module Take = struct

  type ('a, 'b, 'c) action =
    | Rec of 'a list
    | Skip
    | Accum of 'b list
    | Stop_and_keep
    | Stop_and_accum of 'b list * 'c option

  let until ~classify items =
    let rec loop acc = function
      | [] -> List.rev acc, None, []
      | b :: rest ->
        match classify b with
        | Skip -> loop acc rest
        | Rec x -> loop acc (x @ rest)
        | Accum v -> loop (List.rev_append v acc) rest
        | Stop_and_keep ->
          List.rev acc, None, (b :: rest)
        | Stop_and_accum (v, e) ->
          List.rev_append acc v, e, rest
    in
    loop [] items

end

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

  let classify ~on_sub (i : Item.t) : _ Rewire.action = match i with
    | Text _
    | Declaration _
      -> Skip
    | Subpage { content = { status; content; _ }; _ } ->
      if on_sub status then
        Rec (match content with Items i -> i | Page p -> p.items)
      else Skip
    | Heading { label = None ; _ } -> Skip
    | Heading { label = Some label; level; title } ->
      Heading ((label, title), level)

  let node (anchor, text) children = { anchor; text; children}

  let on_sub_default : Subpage.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let compute ?(on_sub=on_sub_default) t =
    Rewire.walk
      ~classify:(classify ~on_sub) ~node
      t

end


module Subpages = struct

  let rec walk_documentedsrc (l : DocumentedSrc.t) =
    Utils.flatmap l ~f:(function
      | DocumentedSrc.Code _ -> []
      | Documented _ -> []
      | Nested { code ; _ } -> walk_documentedsrc code
      | Subpage p -> [p]
    )

  let walk_items (l : Item.t list) =
    Utils.flatmap l ~f:(function
      | Item.Text _ -> []
      | Heading _ -> []
      | Declaration { content ; _ } -> walk_documentedsrc content
      | Subpage { content ; _ } -> [content]
    )

  let compute (p : Page.t) = walk_items (p.header @ p.items)

end
