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
      | [] -> (List.rev acc, None, [])
      | b :: rest -> (
          match classify b with
          | Skip -> loop acc rest
          | Rec x -> loop acc (x @ rest)
          | Accum v -> loop (List.rev_append v acc) rest
          | Stop_and_keep -> (List.rev acc, None, b :: rest)
          | Stop_and_accum (v, e) -> (List.rev_append acc v, e, rest))
    in
    loop [] items
end

module Rewire = struct
  type ('a, 'h) action = Rec of 'a | Skip | Heading of 'h * int

  let walk ~classify ~node items =
    let rec loop current_level acc l =
      match l with
      | [] -> (List.rev acc, [])
      | b :: rest -> (
          match classify b with
          | Skip -> loop current_level acc rest
          | Rec l -> loop current_level acc (l @ rest)
          | Heading (h, level) ->
              if level > current_level then
                let children, rest = loop level [] rest in
                loop current_level (node h children :: acc) rest
              else (List.rev acc, l))
    in
    let trees, rest = loop (-1) [] items in
    assert (rest = []);
    trees
end

module Toc : sig
  type t = one list

  and one = { url : Url.t; text : Inline.t; children : t }

  val compute :
    Url.Path.t -> on_sub:(Include.status -> bool) -> Item.t list -> t
end = struct
  type t = one list

  and one = { url : Url.t; text : Inline.t; children : t }

  let classify ~on_sub (i : Item.t) : _ Rewire.action =
    match i with
    | Text _ | Declaration _ -> Skip
    | Include { content = { status; content; _ }; _ } ->
        if on_sub status then Rec content else Skip
    | Heading { label = None; _ } -> Skip
    | Heading { label = Some label; level; title } ->
        Heading ((label, title), level)

  let node mkurl (anchor, text) children =
    { url = mkurl anchor; text; children }

  let compute page ~on_sub t =
    let mkurl anchor = { Url.Anchor.page; anchor; kind = `LeafPage } in
    Rewire.walk ~classify:(classify ~on_sub) ~node:(node mkurl) t
end

module Subpages : sig
  val compute : Page.t -> Subpage.t list
end = struct
  let rec walk_documentedsrc (l : DocumentedSrc.t) =
    Utils.flatmap l ~f:(function
      | DocumentedSrc.Code _ -> []
      | Documented _ -> []
      | Nested { code; _ } -> walk_documentedsrc code
      | Subpage p -> [ p ]
      | Alternative (Expansion r) -> walk_documentedsrc r.expansion)

  let rec walk_items (l : Item.t list) =
    Utils.flatmap l ~f:(function
      | Item.Text _ -> []
      | Heading _ -> []
      | Declaration { content; _ } -> walk_documentedsrc content
      | Include i -> walk_items i.content.content)

  let compute (p : Page.t) = walk_items (p.header @ p.items)
end

module Shift = struct
  type state = { englobing_level : int; current_level : int }

  let start = { englobing_level = 0; current_level = 0 }

  let shift st x =
    let level = st.englobing_level + x in
    ({ st with current_level = level }, level)

  let enter { current_level; _ } i =
    { englobing_level = current_level + i; current_level }

  let rec walk_documentedsrc ~on_sub shift_state (l : DocumentedSrc.t) =
    match l with
    | [] -> []
    | ((Code _ | Documented _) as h) :: rest ->
        h :: walk_documentedsrc ~on_sub shift_state rest
    | Nested ds :: rest ->
        let ds =
          { ds with code = walk_documentedsrc ~on_sub shift_state ds.code }
        in
        Nested ds :: walk_documentedsrc ~on_sub shift_state rest
    | Subpage subp :: rest ->
        let subp = subpage ~on_sub shift_state subp in
        Subpage subp :: walk_documentedsrc ~on_sub shift_state rest
    | Alternative (Expansion r) :: rest ->
        let expansion = walk_documentedsrc ~on_sub shift_state r.expansion in
        Alternative (Expansion { r with expansion })
        :: walk_documentedsrc ~on_sub shift_state rest

  and subpage ~on_sub shift_state (subp : Subpage.t) =
    match on_sub (`Page subp) with
    | None -> subp
    | Some i ->
        let shift_state = enter shift_state i in
        let page = subp.content in
        let content =
          {
            page with
            header = walk_item ~on_sub shift_state page.header;
            items = walk_item ~on_sub shift_state page.items;
          }
        in
        { subp with content }

  and include_ ~on_sub shift_state (subp : Include.t) =
    match on_sub (`Include subp) with
    | None -> subp
    | Some i ->
        let shift_state = enter shift_state i in
        let content = walk_item ~on_sub shift_state subp.content in
        { subp with content }

  and walk_item ~on_sub shift_state (l : Item.t list) =
    match l with
    | [] -> []
    | Heading { label; level; title } :: rest ->
        let shift_state, level = shift shift_state level in
        Item.Heading { label; level; title }
        :: walk_item ~on_sub shift_state rest
    | Include subp :: rest ->
        let content = include_ ~on_sub shift_state subp.content in
        let subp = { subp with content } in
        Item.Include subp :: walk_item ~on_sub shift_state rest
    | Declaration decl :: rest ->
        let decl =
          {
            decl with
            content = walk_documentedsrc ~on_sub shift_state decl.content;
          }
        in
        Declaration decl :: walk_item ~on_sub shift_state rest
    | Text txt :: rest -> Text txt :: walk_item ~on_sub shift_state rest

  let compute ~on_sub i =
    let shift_state = start in
    walk_item ~on_sub shift_state i
end
