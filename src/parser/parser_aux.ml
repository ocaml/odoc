let wrap_location :
    Lexing.position * Lexing.position -> 'a -> 'a Loc.with_location =
 fun pos value ->
  let location = Loc.of_position pos in
  { location; value }

let not_empty : 'a list -> bool = function _ :: _ -> true | _ -> false
let has_content : string -> bool = fun s -> String.length s > 0

let trim_start = function Loc.{ value = `Space _; _ } :: xs -> xs | xs -> xs

(* Wrap a list of `inline_element` in a `Paragraph *)
let paragraph :
    Ast.inline_element Loc.with_location list ->
    Ast.nestable_block_element Loc.with_location =
 fun elts ->
  let span = Loc.span @@ List.map Loc.location elts in
  let elts = trim_start elts in
  Loc.at span @@ `Paragraph elts

type align_error =
  | Invalid_align (* An invalid align cell *)
  | Not_align (* Not an align cell *)

(* This could be made a bit more specific by allowing Space elements only at
   the beginning and end *)
let valid_elements (cell : Ast.inline_element list) : string option =
  let rec go acc = function
    | `Word _ :: _ when Option.is_some acc -> None
    | `Word word :: rest -> go (Some word) rest
    | `Space _ :: rest -> go acc rest
    | _ :: _ -> None
    | [] -> acc
  in
  go None cell

let valid_word word =
  match String.length word with
  | 0 -> Ok None
  | 1 -> (
      match word.[0] with
      | ':' -> Ok (Some `Center)
      | '-' -> Ok None
      | _ -> Error Not_align)
  | len ->
      if String.for_all (Char.equal '-') (String.sub word 1 (len - 2)) then
        match (word.[0], word.[pred len]) with
        | ':', '-' -> Ok (Some `Left)
        | '-', ':' -> Ok (Some `Right)
        | ':', ':' -> Ok (Some `Center)
        | '-', '-' -> Ok None
        | _ -> Error Invalid_align
      else Error Not_align

let valid_align_cell (cell : Ast.inline_element Loc.with_location list) =
  match List.map Loc.value cell |> valid_elements with
  | Some word -> valid_word word
  | None -> Error Not_align

let sequence : ('elt, 'err) result list -> ('elt list, 'err) result =
 fun list ->
  let rec go acc : ('elt, 'err) result list -> ('elt list, 'err) result =
    function
    | Ok x :: xs -> go (x :: acc) xs
    | Error err :: _ -> Error err
    | [] -> Ok (List.rev acc)
  in
  go [] list

(* NOTE: (@FayCarsons)
   When we get something that doesn't look like an align at all, we check to see if we've gotten
   any valid aligns, if so we assume that the cell being considered is supposed to be an align and treat it as an error,
   otherwise we assume the row is not supposed to be an align row
   This is somewhat error prone, using i.e. '--' to denote an empty table cell seems reasonable
*)
let valid_align_row (row : Ast.inline_element Loc.with_location list list) :
    (Ast.alignment option list, align_error) result =
  let align, not_align =
    List.map valid_align_cell row
    |> List.partition (function
         | Ok _ | Error Invalid_align -> true
         | _ -> false)
  in
  match (align, not_align) with
  | _ :: _, _ :: _ -> Error Invalid_align
  | _ :: _, [] -> sequence align
  | _ -> Error Not_align

(* Merges inline elements within a cell into a single paragraph element, and tags cells w/ tag *)
let merged_tagged_row tag : 'a Loc.with_location list list -> 'b =
  List.map (fun elts -> ([ paragraph elts ], tag))
let as_data = merged_tagged_row `Data
let as_header = merged_tagged_row `Header

let is_valid_align row = Result.is_ok @@ valid_align_row row

(*
  - If the first row is the alignment row then the rest should be data 
  - Otherwise the first should be the headers, the second align, and the rest data 
  - If there's only one row and it's not the align row, then it's data 
  *)
let construct_table :
    span:Loc.span ->
    Ast.inline_element Loc.with_location list list list ->
    Ast.nestable_block_element Loc.with_location =
 fun ~span grid ->
  match grid with
  | [ only_row ] -> (
      match valid_align_row only_row with
      | Ok align -> Loc.at span @@ `Table (([ [] ], Some align), `Light)
      | _ -> Loc.at span @@ `Table (([ as_data only_row ], None), `Light))
  | align :: data when is_valid_align align ->
      let align = Result.get_ok @@ valid_align_row align in
      Loc.at span @@ `Table ((List.map as_data data, Some align), `Light)
  | header :: align :: data when is_valid_align align ->
      let align = Result.get_ok @@ valid_align_row align in
      Loc.at span
      @@ `Table ((as_header header :: List.map as_data data, Some align), `Light)
  | data -> Loc.at span @@ `Table ((List.map as_data data, None), `Light)

let unclosed_table
    ?(data :
       Ast.inline_element Loc.with_location list list list Writer.t option)
    ~span warning : Ast.nestable_block_element Loc.with_location Writer.t =
  let node =
    match data with
    | Some data ->
        Writer.map
          ~f:(fun data ->
            Loc.at span @@ `Table ((List.map as_data data, None), `Light))
          data
    | None ->
        let inner = Loc.at span @@ `Table (([], None), `Light) in
        Writer.return inner
  in
  Writer.warning warning node

let media_kind =
  let open Tokens in
  function Audio -> `Audio | Video -> `Video | Image -> `Image

let href_of_media =
  let open Tokens in
  function Reference name -> `Reference name | Link uri -> `Link uri

let split_simple_media Loc.{ location; value = media, target } =
  (Loc.at location media, target)

let split_replacement_media Loc.{ location; value = media, target, content } =
  (Loc.at location media, target, content)

let rec inline_element_inner : Ast.inline_element -> string = function
  | `Space s -> s
  | `Word s -> s
  | `Styled (_, s) -> children s
  | `Code_span s -> s
  | `Raw_markup (_, s) -> s
  | `Reference (_, _, s) -> children s
  | `Link (_, s) -> children s
  | `Math_span s -> s

and children s =
  List.fold_left
    (fun acc elt -> acc ^ inline_element_inner (Loc.value elt))
    "" s

let legal_module_list : Ast.inline_element Loc.with_location list -> bool =
 fun xs ->
  not_empty xs
  && List.for_all (function `Word _ | `Space _ -> true | _ -> false)
     @@ List.map Loc.value xs

let light_list_item :
    Tokens.token ->
    Ast.nestable_block_element Loc.with_location ->
    [< `Ordered of Ast.nestable_block_element
    | `Unordered of Ast.nestable_block_element ]
    Loc.with_location =
 fun start ->
  let open Tokens in
  Loc.map (fun item ->
      match start with
      | MINUS -> `Unordered item
      | PLUS -> `Ordered item
      | _ -> assert false (* unreachable *))

let or_insert = function None -> Option.some | o -> Fun.const o

let split_light_list_items :
    [< `Ordered of Ast.nestable_block_element
    | `Unordered of Ast.nestable_block_element ]
    Loc.with_location
    list ->
    [< `Ordered | `Unordered ]
    * Ast.nestable_block_element Loc.with_location list =
 fun items ->
  let rec go acc list_kind = function
    | Loc.{ value = `Ordered x; location } :: xs ->
        go (Loc.at location x :: acc) (or_insert list_kind `Ordered) xs
    | Loc.{ value = `Unordered x; location } :: xs ->
        go (Loc.at location x :: acc) (or_insert list_kind `Unordered) xs
    | [] -> (Option.get list_kind, List.rev acc)
  in
  let list_kind, elements = go [] None items in
  (list_kind, elements)

let tag_with_content_start_point : Tokens.tag_with_content -> Loc.point option =
  function
  | Before { start; _ }
  | Raise { start; _ }
  | Param { start; _ }
  | See { start; _ } ->
      Some start
  | _ -> None

let to_ref : Tokens.uri_kind -> [ `Url | `File | `Document ] = function
  | URL -> `Url
  | File -> `File
  | Document -> `Document

let tag_with_content
    (content : Ast.nestable_block_element Loc.with_location list) :
    Tokens.tag_with_content -> Ast.tag = function
  | DEPRECATED -> `Deprecated content
  | Before { inner; _ } -> `Before (inner, content)
  | Raise { inner; _ } -> `Raise (inner, content)
  | Param { inner; _ } -> `Param (inner, content)
  | See { inner = kind, href; _ } -> `See (to_ref kind, href, content)
  | RETURN -> `Return content
  | CHILDREN_ORDER -> `Children_order content
  | TOC_STATUS -> `Toc_status content
  | ORDER_CATEGORY -> `Order_category content
  | SHORT_TITLE -> `Short_title content

let tag_bare : Tokens.tag Loc.with_location -> Ast.tag = function
  | { value = Author s; _ } -> `Author s.inner
  | { value = Since s; _ } -> `Since s.inner
  | { value = Version s; _ } -> `Version s.inner
  | { value = Canonical s; _ } as loc -> `Canonical { loc with value = s.inner }
  | { value = INLINE; _ } -> `Inline
  | { value = OPEN; _ } -> `Open
  | { value = CLOSED; _ } -> `Closed
  | { value = HIDDEN; _ } -> `Hidden

let ast_style : Tokens.style -> Ast.style = function
  | Bold -> `Bold
  | Italic -> `Italic
  | Emphasis -> `Emphasis
  | Superscript -> `Superscript
  | Subscript -> `Subscript
