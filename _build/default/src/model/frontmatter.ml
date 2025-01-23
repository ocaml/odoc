type child = Page of string | Dir of string | Module of string

type short_title = Comment.link_content

type line =
  | Children_order of child Location_.with_location list
  | Short_title of short_title
  | Toc_status of [ `Open | `Hidden ]
  | Order_category of string

type children_order = child Location_.with_location list Location_.with_location

type t = {
  children_order : children_order option;
  short_title : short_title option;
  toc_status : [ `Open | `Hidden ] option;
  order_category : string option;
}

let empty =
  {
    children_order = None;
    short_title = None;
    toc_status = None;
    order_category = None;
  }

let update ~tag_name ~loc v new_v =
  match v with
  | None -> Some new_v
  | Some _ ->
      Error.raise_warning (Error.make "Duplicated @%s entry" tag_name loc);
      v

let apply fm line =
  match line.Location_.value with
  | Toc_status x ->
      let toc_status =
        update ~tag_name:"short_title" ~loc:line.location fm.toc_status x
      in
      { fm with toc_status }
  | Short_title t ->
      let short_title =
        update ~tag_name:"short_title" ~loc:line.location fm.short_title t
      in
      { fm with short_title }
  | Children_order children_order ->
      let children_order = Location_.same line children_order in
      let children_order =
        update ~tag_name:"children_order" ~loc:line.location fm.children_order
          children_order
      in
      { fm with children_order }
  | Order_category name ->
      let order_category =
        update ~tag_name:"order_category" ~loc:line.location fm.order_category
          name
      in
      { fm with order_category }

let parse_child c =
  let mod_prefix = "module-" in
  if Astring.String.is_suffix ~affix:"/" c then
    let c = String.sub c 0 (String.length c - 1) in
    Dir c
  else if Astring.String.is_prefix ~affix:mod_prefix c then
    let l = String.length mod_prefix in
    let c = String.sub c l (String.length c - l) in
    Module c
  else Page c

type tag_payload = Comment.nestable_block_element Location_.with_location list

let parse_children_order loc (co : tag_payload) =
  let rec parse_words acc words =
    match words with
    | [] -> Result.Ok (Location_.at loc (Children_order (List.rev acc)))
    | ({ Location_.value = `Word word; _ } as w) :: tl ->
        parse_words ({ w with value = parse_child word } :: acc) tl
    | { Location_.value = `Space; _ } :: tl -> parse_words acc tl
    | { location; _ } :: _ ->
        Error
          (Error.make "Only words are accepted when specifying children order"
             location)
  in
  match co with
  | [ { Location_.value = `Paragraph words; _ } ] -> parse_words [] words
  | _ ->
      Error
        (Error.make "Only words are accepted when specifying children order" loc)

let parse_short_title loc (t : tag_payload) =
  match t with
  | [ { Location_.value = `Paragraph words; _ } ] ->
      let short_title = Comment.link_content_of_inline_elements words in
      Result.Ok (Location_.at loc (Short_title short_title))
  | _ ->
      Error
        (Error.make
           "Short titles cannot contain other block than a single paragraph" loc)

let parse_toc_status loc (t : tag_payload) =
  match t with
  | [
   { Location_.value = `Paragraph [ { Location_.value = `Word "open"; _ } ]; _ };
  ] ->
      Result.Ok (Location_.at loc (Toc_status `Open))
  | [
   {
     Location_.value = `Paragraph [ { Location_.value = `Word "hidden"; _ } ];
     _;
   };
  ] ->
      Result.Ok (Location_.at loc (Toc_status `Hidden))
  | _ ->
      Error
        (Error.make "@toc_status can only take the 'open' and 'hidden' value"
           loc)

let parse_order_category loc (t : tag_payload) =
  match t with
  | [ { Location_.value = `Paragraph [ { Location_.value = `Word w; _ } ]; _ } ]
    ->
      Result.Ok (Location_.at loc (Order_category w))
  | _ ->
      Error
        (Error.make "@order_category can only take a single word as value" loc)

let of_lines lines =
  Error.catch_warnings @@ fun () -> List.fold_left apply empty lines
