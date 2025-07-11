(* Part of this code is:
   Copyright (c) 2020 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC *)

let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let block_line_of_string s =
  let flush s start last acc =
    let sub = String.sub s start (last - start + 1) in
    sub :: acc
  in
  (* cuts [s] on newlines *)
  let rec loop s acc max start k =
    if k > max then List.rev (flush s start max acc)
    else if not (s.[k] = '\n' || s.[k] = '\r') then loop s acc max start (k + 1)
    else
      let acc = flush s start (k - 1) acc in
      let next = k + 1 in
      let start =
        if s.[k] = '\r' && next <= max && s.[next] = '\n' then next + 1
        else next
      in
      loop s acc max start start
  in
  loop s [] (String.length s - 1) 0 0

type label = { key : string; text : string list }

module Inline = struct
  type t =
    | Break
    | Inlines of t list
    | Text of string (* plain text *)
    | Code_span of string list (* `code` *)
    | Emphasis of t (* *emphasis* *)
    | Strong_emphasis of t (* **strong emphasis** *)
    | Image of link (* ![alt text](url) *)
    | Link of link (* [link text](url) *)
    | Raw_html of string list (* <div></div> *)
  and link = { text : t; url : string option }

  let is_empty = function Text "" | Inlines [] -> true | _ -> false
end

module Block = struct
  open Odoc_utils (* For Int.max *)

  type code_block = { info_string : string option; code : string list }
  type list_type = Unordered | Ordered

  type id = [ `Auto of string | `Id of string ]
  type heading = { level : int; inline : Inline.t; id : id option }

  module Table = struct
    type sep = [ `Left | `Center | `Right ] option
    type row =
      [ `Header of Inline.t list | `Sep of sep list | `Data of Inline.t list ]
    type t = { col_count : int; rows : row list }

    let col_count rows =
      let rec loop c = function
        | (`Header cols | `Data cols) :: rs ->
            loop (Int.max (List.length cols) c) rs
        | `Sep cols :: rs -> loop (Int.max (List.length cols) c) rs
        | [] -> c
      in
      loop 0 rows

    let make rows = { col_count = col_count rows; rows }

    let rows t = t.rows

    let parse_sep_row cs =
      let rec loop acc = function
        | [] -> Some (List.rev acc)
        | (Inline.Text s, ("", "")) :: cs -> (
            if s = "" then None
            else
              let max = String.length s - 1 in
              let first_colon = s.[0] = ':' and last_colon = s.[max] = ':' in
              let first = if first_colon then 1 else 0 in
              let last = if last_colon then max - 1 else max in
              match
                for i = first to last do
                  if s.[i] <> '-' then raise Exit
                done
              with
              | exception Exit -> None
              | () ->
                  let count = last - first + 1 in
                  let sep =
                    match (first_colon, last_colon) with
                    | false, false -> None
                    | true, true -> Some `Center
                    | true, false -> Some `Left
                    | false, true -> Some `Right
                  in
                  loop ((sep, count) :: acc) cs)
        | _ -> None
      in
      loop [] cs
  end

  type t =
    | Blank_line
    | Blocks of t list
    | Code_block of code_block (* ``` xxx ``` *)
    | Heading of heading (* # heading *)
    | Html_block of string list (* raw html *)
    | Unordered_list of t list (* - item *)
    | Ordered_list of t list (* 1. item *)
    | Paragraph of Inline.t (* paragraph *)
    | Table of Table.t
  let empty = Blocks []
end

type doc = Block.t

module Heterogeneous_dict = struct
  (* Type identifiers *)
  module Type = struct
    type (_, _) eq = Equal : ('a, 'a) eq
    module Id = struct
      type _ id = ..
      module type ID = sig
        type t
        type _ id += Id : t id
      end
      type 'a t = (module ID with type t = 'a)

      let make (type a) () : a t =
        (module struct
          type t = a
          type _ id += Id : t id
        end)

      let provably_equal (type a b) ((module A) : a t) ((module B) : b t) :
          (a, b) eq option =
        match A.Id with B.Id -> Some Equal | _ -> None

      let uid (type a) ((module A) : a t) =
        Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
    end
  end

  module M = Map.Make (Int)
  type 'a key = 'a Type.Id.t
  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let key = Type.Id.make
  let empty = M.empty
  let mem k m = M.mem (Type.Id.uid k) m
  let add k v m = M.add (Type.Id.uid k) (B (k, v)) m
  let tag k m = add k () m
  let remove k m = M.remove (Type.Id.uid k) m
  let find : type a. a key -> t -> a option =
   fun k m ->
    match M.find_opt (Type.Id.uid k) m with
    | None -> None
    | Some (B (k', v)) -> (
        match Type.Id.provably_equal k k' with
        | None -> assert false
        | Some Type.Equal -> Some v)
end

type t = {
  init_context : context -> doc -> unit;
  inline : inline;
  block : block;
}

and context = {
  renderer : t;
  mutable state : Heterogeneous_dict.t;
  b : Buffer.t;
}

and inline = context -> Inline.t -> unit
and block = context -> Block.t -> unit

module Context = struct
  type t = context
  let make renderer b = { renderer; b; state = Heterogeneous_dict.empty }

  let buffer c = c.b

  module State = struct
    type 'a t = 'a Heterogeneous_dict.key
    let make = Heterogeneous_dict.key
    let find c st = Heterogeneous_dict.find st c.state
    let get c st = Option.get (Heterogeneous_dict.find st c.state)
    let set c st = function
      | None -> c.state <- Heterogeneous_dict.remove st c.state
      | Some s -> c.state <- Heterogeneous_dict.add st s c.state
  end

  let init c d = c.renderer.init_context c d

  let byte r c = Buffer.add_char r.b c
  let utf_8_uchar r u = Buffer.add_utf_8_uchar r.b u
  let string c s = Buffer.add_string c.b s
  let inline c i = c.renderer.inline c i
  let block c b = c.renderer.block c b
  let doc (c : context) d =
    init c d;
    c.renderer.block c d
end

type indent = [ `I of int | `L of int * string * int * Uchar.t option ]

type state = {
  newline_to_output : string;
  mutable start_of_text : bool;
  mutable identation_stack : indent list;
}

let state : state Context.State.t = Context.State.make ()
let get_state c = Context.State.get c state
let init_context c _d =
  Context.State.set c state
    (Some
       { newline_to_output = "\n"; start_of_text = true; identation_stack = [] })

module Char_set = Set.Make (Char)

let esc_parens = Char_set.of_list [ '('; ')' ]
let esc_quote = Char_set.singleton '\''
let esc_dquote = Char_set.singleton '\"'
let esc_link_label = Char_set.of_list [ '['; ']'; '\\' ]

let buffer_add_dec_esc b c =
  Buffer.add_string b "&#";
  Buffer.add_string b (Int.to_string (Char.code c));
  Buffer.add_char b ';'

let buffer_add_bslash_esc b c =
  Buffer.add_char b '\\';
  Buffer.add_char b c

let buffer_add_escaped_string ?(esc_ctrl = true) b cs s =
  let flush b max start i =
    if start <= max then Buffer.add_substring b s start (i - start)
  in
  let rec loop b s max start i =
    if i > max then flush b max start i
    else
      let next = i + 1 in
      let c = String.get s i in
      if Char_set.mem c cs then (
        flush b max start i;
        buffer_add_bslash_esc b c;
        loop b s max next next)
      else if esc_ctrl && is_control c then (
        flush b max start i;
        buffer_add_dec_esc b c;
        loop b s max next next)
      else loop b s max start next
  in
  loop b s (String.length s - 1) 0 0

let escaped_string ?esc_ctrl c cs s =
  buffer_add_escaped_string ?esc_ctrl (Context.buffer c) cs s

let buffer_add_escaped_text b s =
  let esc_first b s =
    match s.[0] with
    | ('-' | '+' | '_' | '=') as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c;
        true
    | _ -> false
  in
  let esc_amp s max next =
    next <= max && (is_letter s.[next] || s.[next] = '#')
  in
  let esc_tilde s max prev next =
    (not (Char.equal prev '~')) && next <= max && s.[next] = '~'
  in
  let esc_item_marker s i =
    if i = 0 || i > 9 (* marker has from 1-9 digits *) then false
    else
      let k = ref (i - 1) in
      while !k >= 0 && is_digit s.[!k] do
        decr k
      done;
      !k < 0
  in
  let flush b max start i =
    if start <= max then Buffer.add_substring b s start (i - start)
  in
  let rec loop b s max start prev i =
    if i > max then flush b max start i
    else
      let next = i + 1 in
      let c = String.get s i in
      if is_control c then (
        flush b max start i;
        buffer_add_dec_esc b c;
        loop b s max next c next)
      else
        match c with
        | ('#' | '`') when not (Char.equal prev c) ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | '~' when esc_tilde s max prev next ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | '&' when esc_amp s max next ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | '!' when i = max ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | ('.' | ')') when esc_item_marker s i ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | '\\' | '<' | '>' | '[' | ']' | '*' | '_' | '$' | '|' ->
            flush b max start i;
            buffer_add_bslash_esc b c;
            loop b s max next c next
        | _ -> loop b s max start c next
  in
  let max = String.length s - 1 in
  if max < 0 then ()
  else if esc_first b s then loop b s max 1 s.[0] 1
  else loop b s max 0 '\x00' 0

let escaped_text c s = buffer_add_escaped_text (Context.buffer c) s

let nchars c n char =
  for _i = 1 to n do
    Context.byte c char
  done

let newline c =
  (* Block generally introduce newlines, except the first one. *)
  let state = get_state c in
  if state.start_of_text then state.start_of_text <- false
  else Context.string c state.newline_to_output

let push_indent c n =
  let state = get_state c in
  state.identation_stack <- n :: state.identation_stack

let pop_indent c =
  let state = get_state c in
  match state.identation_stack with
  | [] -> ()
  | ns -> state.identation_stack <- List.tl ns

let rec indent c =
  let rec loop c acc = function
    | [] -> acc
    | (`I n as i) :: is ->
        nchars c n ' ';
        loop c (i :: acc) is
    | `L (before, m, after, task) :: is ->
        nchars c before ' ';
        Context.string c m;
        nchars c after ' ';
        let after =
          match task with
          | None -> after
          | Some u ->
              Context.byte c '[';
              Context.utf_8_uchar c u;
              Context.string c "] ";
              after + 4
        in
        (* On the next call we'll just indent for the list item *)
        loop c (`I (before + String.length m + after) :: acc) is
    | _ -> []
  in
  let state = get_state c in
  state.identation_stack <- loop c [] (List.rev state.identation_stack)

and link_label_lines c lines = escaped_tight_block_lines c esc_link_label lines

and escaped_tight_block_lines c cs = function
  | [] -> ()
  | l :: ls ->
      let tight c blanks =
        Context.string c blanks;
        escaped_string c cs l
      in
      let line c l =
        newline c;
        indent c;
        tight c l
      in
      tight c l;
      List.iter (line c) ls

let block_lines c = function
  | [] -> ()
  | l :: ls ->
      let line c l =
        newline c;
        indent c;
        Context.string c l
      in
      Context.string c l;
      List.iter (line c) ls

let break c =
  Context.string c " ";
  newline c;
  indent c

let code_span c cs =
  nchars c 1 '`';
  List.iter (Context.string c) cs;
  nchars c 1 '`'

let emphasis c i =
  let delim = '*' in
  Context.byte c delim;
  Context.inline c i;
  Context.byte c delim

let strong_emphasis c i =
  let delim = '*' in
  Context.byte c delim;
  Context.byte c delim;
  Context.inline c i;
  Context.byte c delim;
  Context.byte c delim

let link_title c open_delim title =
  match title with
  | None -> ()
  | Some lines ->
      let open', close, escapes =
        match open_delim with
        | '\"' as delim -> (delim, delim, esc_dquote)
        | '\'' as delim -> (delim, delim, esc_quote)
        | '(' -> ('(', ')', esc_parens)
        | _ -> ('\"', '\"', esc_dquote)
      in
      Context.byte c open';
      escaped_tight_block_lines c escapes lines;
      Context.byte c close

let link c (l : Inline.link) =
  Context.byte c '[';
  Context.inline c l.text;
  Context.byte c ']';
  Context.byte c '(';
  (match l.url with
  | None -> ()
  | Some dest -> escaped_string c esc_parens dest);
  Context.byte c ')'

let image c l =
  Context.byte c '!';
  link c l
let text c t = escaped_text c t

let inline c = function
  | Inline.Text t -> text c t
  | Inline.Link l -> link c l
  | Inline.Break -> break c
  | Inline.Emphasis e -> emphasis c e
  | Inline.Code_span cs -> code_span c cs
  | Inline.Image i -> image c i
  | Inline.Inlines is -> List.iter (Context.inline c) is
  | Inline.Strong_emphasis e -> strong_emphasis c e
  | Inline.Raw_html html -> List.iter (Context.string c) html

let blank_line c l =
  newline c;
  indent c;
  Context.string c l

let string_node_option c = function None -> () | Some s -> Context.string c s

let code_block c (cb : Block.code_block) =
  let opening, closing =
    let char, len = ('`', 3) in
    let f = String.make len char in
    (f, Some f)
  in
  let info_string = cb.info_string in
  let code = cb.code in
  newline c;
  push_indent c (`I 0);
  indent c;
  Context.string c opening;
  string_node_option c info_string;
  if code <> [] then (
    newline c;
    indent c;
    block_lines c code);
  (match closing with
  | None -> ()
  | Some close ->
      newline c;
      indent c;
      Context.string c close);
  pop_indent c

let heading c (h : Block.heading) =
  newline c;
  nchars c h.level '#';
  if not (Inline.is_empty h.inline) then Context.byte c ' ' else ();
  Context.inline c h.inline

let html_block c h =
  newline c;
  indent c;
  block_lines c h

let unordered_item c marker i =
  let before = 0 in
  let after = 1 in
  let task = None in
  push_indent c (`L (before, marker, after, task));
  Context.block c i;
  pop_indent c

let ordered_item c num i =
  let before = 0 in
  let marker = Int.to_string num ^ "." in
  let after = 1 in
  let task = None in
  push_indent c (`L (before, marker, after, task));
  Context.block c i;
  pop_indent c;
  num + 1

let unordered_list c l = List.iter (unordered_item c "-") l

let ordered_list c l = ignore (List.fold_left (ordered_item c) 1 l)

let paragraph c p =
  newline c;
  indent c;
  nchars c 0 ' ';
  Context.inline c p;
  Context.string c ""

let table c t =
  let col c i =
    Context.byte c '|';
    Context.byte c ' ';
    Context.inline c i;
    Context.byte c ' '
  in
  let sep c align =
    let len = 3 in
    Context.byte c '|';
    Context.byte c ' ';
    (match align with
    | None -> nchars c len '-'
    | Some `Left ->
        Context.byte c ':';
        nchars c (len - 1) '-'
    | Some `Center ->
        Context.byte c ':';
        nchars c (len - 2) '-';
        Context.byte c ':'
    | Some `Right ->
        nchars c (len - 1) '-';
        Context.byte c ':');
    Context.byte c ' '
  in
  let row c (row : Block.Table.row) =
    match row with
    | `Header cols | `Data cols ->
        newline c;
        indent c;
        if cols = [] then Context.byte c '|' else List.iter (col c) cols;
        Context.byte c '|'
    | `Sep seps ->
        newline c;
        indent c;
        if seps = [] then Context.byte c '|' else List.iter (sep c) seps;
        Context.byte c '|'
  in
  List.iter (row c) (Block.Table.rows t)

let block c = function
  | Block.Blank_line -> blank_line c ""
  | Block.Blocks bs -> List.iter (Context.block c) bs
  | Block.Code_block cb -> code_block c cb
  | Block.Heading h -> heading c h
  | Block.Html_block h -> html_block c h
  | Block.Unordered_list l -> unordered_list c l
  | Block.Ordered_list l -> ordered_list c l
  | Block.Paragraph p -> paragraph c p
  | Block.Table t -> table c t

let to_string d =
  let t = { init_context; inline; block } in
  let buffer = Buffer.create 1024 in
  let ctx = Context.make t buffer in
  Context.doc ctx d;
  Buffer.contents buffer
