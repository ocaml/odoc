(* What we need in the markdown generator:
   Special syntaxes:
   - Pandoc's heading attributes
*)

type inlines =
  | String of string
  | Join of inlines * inlines
  | Link of string * inlines
  | Anchor of string
  | Linebreak
  | Noop
  | Space

type blocks =
  | ConcatB of blocks * blocks
  | Block of inlines
  | CodeBlock of inlines
  | List of list_type * blocks list
  | Raw_markup of string
  | Prefixed_block of string * blocks  (** Prefix every lines of blocks. *)

and list_type = Ordered | Unordered

let ordered_list bs = List (Ordered, bs)

let unordered_list bs = List (Unordered, bs)

(* Make sure to never leave a [Noop] in the result, which would cause unwanted
   spaces. *)
let ( ++ ) left right = Join (left, right)

let blocks above below = ConcatB (above, below)

let ( +++ ) = blocks

(** Returns two substrings of [s]: The beginning of [s] until [c] occurs, the
    rest without the first [c]. Raise [Not_found]. *)
let string_cut_at_char s c =
  let len = String.length s in
  let i = String.index s c in
  (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))

let rec text s =
  try
    (* Escape backticks. *)
    let left, right = string_cut_at_char s '`' in
    String left ++ String "\\`" ++ text right
  with Not_found -> String s

let line_break = Linebreak

let noop = Noop

let space = Space

let bold i = Join (String "**", Join (i, String "**"))

let italic i = Join (String "_", Join (i, String "_"))

let subscript i = Join (String "<sub>", Join (i, String "</sub>"))

let superscript i = Join (String "<sup>", Join (i, String "</sup>"))

let code_span s =
  let left, right =
    if String.contains s '`' then (String "`` ", String " ``")
    else (String "`", String "`")
  in
  Join (left, Join (String s, right))

let link ~href i = Link (href, i)

let anchor' i = Anchor i

let raw_markup s = Raw_markup s

let paragraph i = Block i

let code_block i = CodeBlock i

let quote_block b = Prefixed_block ("> ", b)

let noop_block = Block Noop

let heading level i =
  let make_hashes n = String.make n '#' in
  let hashes = make_hashes level in
  Block (String hashes ++ String " " ++ i)

(** [split_on_char] is not available on [< 4.04]. *)
let rec iter_lines f s i =
  try
    let i' = String.index_from s i '\n' in
    f (String.sub s i (i' - i));
    iter_lines f s (i' + 1)
  with Not_found ->
    let len = String.length s in
    if i < len then f (String.sub s i (len - i))

(** Every lines that [f] formats are prefixed and written in [sink].
    Inefficient. *)
let with_prefixed_formatter prefix sink f =
  let s = Format.asprintf "%t" f in
  iter_lines (Format.fprintf sink "%s%s@\n" prefix) s 0

let pp_list_item fmt list_type (b : blocks) n pp_blocks =
  match list_type with
  | Unordered -> Format.fprintf fmt "- @[%a@]@\n" pp_blocks b
  | Ordered -> Format.fprintf fmt "%d. @[%a@]@\n" (n + 1) pp_blocks b

let rec pp_inlines fmt i =
  match i with
  | String s -> Format.fprintf fmt "%s" s
  | Join (left, right) ->
      Format.fprintf fmt "%a%a" pp_inlines left pp_inlines right
  | Link (href, i) -> Format.fprintf fmt "[%a](%s)" pp_inlines i href
  | Anchor s -> Format.fprintf fmt "<a id=\"%s\"></a>" s
  | Linebreak -> Format.fprintf fmt "@\n"
  | Noop -> ()
  | Space -> Format.fprintf fmt "@ "

let rec pp_blocks fmt b =
  match b with
  | ConcatB (Block Noop, b) | ConcatB (b, Block Noop) -> pp_blocks fmt b
  | ConcatB (above, below) ->
      Format.fprintf fmt "%a@\n%a" pp_blocks above pp_blocks below
  | Block i -> Format.fprintf fmt "@[%a@]@\n" pp_inlines i
  | CodeBlock i -> Format.fprintf fmt "```@\n%a@\n```" pp_inlines i
  | List (list_type, l) ->
      let rec pp_list n l =
        match l with
        | [] -> ()
        | [ x ] -> pp_list_item fmt list_type x n pp_blocks
        | x :: rest ->
            pp_list_item fmt list_type x n pp_blocks;
            Format.fprintf fmt "@\n";
            pp_list (n + 1) rest
      in
      pp_list 0 l
  | Raw_markup s -> Format.fprintf fmt "%s" s
  | Prefixed_block (p, b) ->
      with_prefixed_formatter p fmt (fun fmt -> pp_blocks fmt b)
