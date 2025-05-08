module Cmarkit_data = struct
  module Uset = struct
    include Set.Make (Uchar)
    let of_array =
      let add acc u = add (Uchar.unsafe_of_int u) acc in
      Array.fold_left add empty
  end

  module Umap = struct
    include Map.Make (Uchar)
    let of_array =
      let add acc (u, f) = add (Uchar.unsafe_of_int u) f acc in
      Array.fold_left add empty
  end

  let whitespace_uset = Uset.of_array Data_uchar.whitespace
  let punctuation_uset = Uset.of_array Data_uchar.punctuation
  let case_fold_umap = Umap.of_array Data_uchar.case_fold

  let unicode_version = Data_uchar.unicode_version
  let is_unicode_whitespace u = Uset.mem u whitespace_uset
  let is_unicode_punctuation u = Uset.mem u punctuation_uset
  let unicode_case_fold u = Umap.find_opt u case_fold_umap

  (* HTML entity data. *)

  module String_map = Map.Make (String)
end

(* TODO: Remove Meta module *)
module Meta = struct
  type t = unit
  let none = ()
end

(* TODO: Remove Meta.t from node *)
type 'a node = 'a * Meta.t

module Ascii = struct
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_hex_digit = function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false

  let hex_digit_to_int = function
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> Char.code c - 0x37
    | 'a' .. 'f' as c -> Char.code c - 0x57
    | _ -> assert false

  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false

  let is_white = function
    | '\x20' | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> true
    | _ -> false

  let is_punct = function
    (* https://spec.commonmark.org/current/#ascii-punctuation-character *)
    | '!' | '\"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
    | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\'
    | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
        true
    | _ -> false

  let is_blank = function ' ' | '\t' -> true | _ -> false

  let caseless_starts_with ~prefix s =
    let get = String.get in
    let len_a = String.length prefix in
    let len_s = String.length s in
    if len_a > len_s then false
    else
      let max_idx_a = len_a - 1 in
      let rec loop s i max =
        if i > max then true
        else
          let c =
            match get s i with
            | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32))
            | c -> c
          in
          if get prefix i <> c then false else loop s (i + 1) max
      in
      loop s 0 max_idx_a

  let match' ~sub s ~start =
    (* assert (start + String.length sub - 1 < String.length s) *)
    try
      for i = 0 to String.length sub - 1 do
        if s.[start + i] <> sub.[i] then raise_notrace Exit
      done;
      true
    with Exit -> false

  let caseless_match ~sub s ~start =
    (* assert (start + String.length sub - 1 < String.length s) *)
    try
      for i = 0 to String.length sub - 1 do
        let c =
          match s.[start + i] with
          | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32))
          | c -> c
        in
        if c <> sub.[i] then raise_notrace Exit
      done;
      true
    with Exit -> false

  let lowercase_sub s first len =
    let b = Bytes.create len in
    for i = 0 to len - 1 do
      let c =
        match s.[first + i] with
        | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32))
        | c -> c
      in
      Bytes.set b i c
    done;
    Bytes.unsafe_to_string b
end

module Match = struct
  let rec first_non_blank s ~last ~start =
    if start > last then last + 1
    else
      match s.[start] with
      | ' ' | '\t' -> first_non_blank s ~last ~start:(start + 1)
      | _ -> start

  let autolink_email s ~last ~start =
    (* https://spec.commonmark.org/current/#email-address
     Via the ABNF "<" email ">" with email defined by:
     https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address *)
    let is_atext_plus_dot = function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?'
      | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '.' ->
          true
      | _ -> false
    in
    let is_let_dig = Ascii.is_alphanum in
    let is_let_dig_hyp c = Ascii.is_alphanum c || c = '-' in
    let rec label_seq s last k =
      let rec loop s last c k =
        if k > last then None
        else if is_let_dig_hyp s.[k] && c <= 63 then loop s last (c + 1) (k + 1)
        else if c > 63 || not (is_let_dig s.[k - 1]) then None
        else
          match s.[k] with
          | '>' -> Some k
          | '.' -> label_seq s last (k + 1)
          | _ -> None
      in
      if k > last || not (is_let_dig s.[k]) then None else loop s last 1 (k + 1)
    in
    let rec atext_seq s last k =
      if k > last then None
      else if is_atext_plus_dot s.[k] then atext_seq s last (k + 1)
      else if s.[k] = '@' && is_atext_plus_dot s.[k - 1] then
        label_seq s last (k + 1)
      else None
    in
    if start > last || s.[start] <> '<' then None
    else atext_seq s last (start + 1)
end

module Layout = struct
  type blanks = string
  type nonrec string = string
  type nonrec char = char
  type count = int
  type indent = int
  let string ?(meta = Meta.none) s = (s, meta)
  let empty = string ""
end

module Block_line = struct
  let _list_of_string flush s =
    (* cuts [s] on newlines *)
    let rec loop s acc max start k =
      if k > max then List.rev (flush s start max acc)
      else if not (s.[k] = '\n' || s.[k] = '\r') then
        loop s acc max start (k + 1)
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

  let flush ?(meta = Meta.none) s start last acc =
    let sub = String.sub s start (last - start + 1) in
    (sub, meta) :: acc

  let flush_tight ?(meta = Meta.none) s start last acc =
    (* If [s] has newlines, blanks after newlines are layout *)
    if start > last then ("", ("", meta)) :: acc
    else
      match acc with
      | [] (* On the first line the blanks are legit *) ->
          ("", (String.sub s start (last - start + 1), meta)) :: acc
      | acc ->
          let nb = Match.first_non_blank s ~last ~start in
          ( String.sub s start (nb - 1 - start + 1),
            (String.sub s nb (last - nb + 1), meta) )
          :: acc

  (* Block lines *)

  type t = string node

  let to_string = fst
  let list_of_string ?meta s = _list_of_string (flush ?meta) s

  (* Tight lines *)

  type tight = Layout.blanks * t

  let tight_to_string l = fst (snd l)
  let tight_list_of_string ?meta s = _list_of_string (flush_tight ?meta) s

  (* Blank lines *)

  type blank = Layout.blanks node
end

module Label = struct
  type key = string
  type t = { meta : Meta.t; key : key; text : Block_line.tight list }
  let make ?(meta = Meta.none) ~key text = { key; text; meta }
  let with_meta meta l = { l with meta }
  let meta t = t.meta
  let key t = t.key
  let text t = t.text
  let text_to_string t =
    String.concat " " (List.map Block_line.tight_to_string t.text)

  let compare l0 l1 = String.compare l0.key l1.key

  (* Definitions *)

  module Map = Map.Make (String)
  type def = ..
  type defs = def Map.t

  (* Resolvers *)

  type context =
    [ `Def of t option * t | `Ref of [ `Link | `Image ] * t * t option ]

  type resolver = context -> t option
  let default_resolver = function
    | `Def (None, k) -> Some k
    | `Def (Some _, _k) -> None
    | `Ref (_, _, k) -> k
end

module Link_definition = struct
  type layout = {
    indent : Layout.indent;
    angled_dest : bool;
    before_dest : Block_line.blank list;
    after_dest : Block_line.blank list;
    title_open_delim : Layout.char;
    after_title : Block_line.blank list;
  }

  let layout_for_dest dest =
    let needs_angles c = Ascii.is_control c || c = ' ' in
    let angled_dest = String.exists needs_angles dest in
    {
      indent = 0;
      angled_dest;
      before_dest = [];
      after_dest = [];
      title_open_delim = '\"';
      after_title = [];
    }

  let default_layout =
    {
      indent = 0;
      angled_dest = false;
      before_dest = [];
      after_dest = [];
      title_open_delim = '\"';
      after_title = [];
    }

  type t = {
    layout : layout;
    label : Label.t option;
    defined_label : Label.t option;
    dest : string node option;
    title : Block_line.tight list option;
  }

  let make ?defined_label ?label ?dest ?title () =
    let layout =
      match dest with
      | None -> default_layout
      | Some (d, _) -> layout_for_dest d
    in
    let defined_label =
      match defined_label with None -> label | Some d -> d
    in
    { layout; label; defined_label; dest; title }

  let layout ld = ld.layout
  let label ld = ld.label
  let defined_label ld = ld.defined_label
  let dest ld = ld.dest
  let title ld = ld.title

  type Label.def += Def of t node
end

module Inline = struct
  type t = ..

  module Autolink = struct
    type t = { is_email : bool; link : string node }
    let is_email a = a.is_email
    let link a = a.link
    let make link =
      let is_email =
        let l = String.concat "" [ "<"; fst link; ">" ] in
        match Match.autolink_email l ~last:(String.length l - 1) ~start:0 with
        | None -> false
        | Some _ -> true
      in
      { is_email; link }
  end

  module Break = struct
    type type' = [ `Hard | `Soft ]
    type t = {
      layout_before : Layout.blanks node;
      type' : type';
      layout_after : Layout.blanks node;
    }

    let make ?(layout_before = Layout.empty) ?(layout_after = Layout.empty)
        type' =
      { layout_before; type'; layout_after }

    let type' b = b.type'
    let layout_before b = b.layout_before
    let layout_after b = b.layout_after
  end

  module Code_span = struct
    type t = {
      backtick_count : Layout.count;
      code_layout : Block_line.tight list;
    }

    let make ~backtick_count code_layout = { backtick_count; code_layout }

    let min_backtick_count ~min counts =
      let rec loop min = function
        | c :: cs -> if min <> c then min else loop (c + 1) cs
        | [] -> min
      in
      loop min (List.sort Int.compare counts)

    let of_string ?(meta = Meta.none) = function
      | "" -> { backtick_count = 1; code_layout = [ ("", ("", meta)) ] }
      | s ->
          (* This finds out the needed backtick count, whether spaces are needed,
           and treats blanks after newline as layout *)
          let max = String.length s - 1 in
          let need_sp = s.[0] = '`' || s.[max] = '`' in
          let s = if need_sp then String.concat "" [ " "; s; " " ] else s in
          let backtick_counts, code_layout =
            let rec loop bt_counts acc max btc start k =
              match k > max with
              | true ->
                  (* assert (btc = 0) because of [need_sp] *)
                  ( bt_counts,
                    if acc = [] then [ ("", (s, meta)) ]
                    else List.rev (Block_line.flush_tight ~meta s start max acc)
                  )
              | false ->
                  if s.[k] = '`' then
                    loop bt_counts acc max (btc + 1) start (k + 1)
                  else
                    let bt_counts =
                      if btc > 0 then btc :: bt_counts else bt_counts
                    in
                    if not (s.[k] = '\n' || s.[k] = '\r') then
                      loop bt_counts acc max 0 start (k + 1)
                    else
                      let acc =
                        Block_line.flush_tight ~meta s start (k - 1) acc
                      in
                      let start =
                        if k + 1 <= max && s.[k] = '\r' && s.[k + 1] = '\n' then
                          k + 2
                        else k + 1
                      in
                      loop bt_counts acc max 0 start start
            in
            loop [] [] max 0 0 0
          in
          let backtick_count = min_backtick_count ~min:1 backtick_counts in
          { backtick_count; code_layout }

    let backtick_count cs = cs.backtick_count
    let code_layout cs = cs.code_layout
    let code cs =
      (* Extract code, see https://spec.commonmark.org/0.30/#code-spans *)
      let sp c = Char.equal c ' ' in
      let s = List.map Block_line.tight_to_string cs.code_layout in
      let s = String.concat " " s in
      if s = "" then ""
      else if
        s.[0] = ' '
        && s.[String.length s - 1] = ' '
        && not (String.for_all sp s)
      then String.sub s 1 (String.length s - 2)
      else s
  end

  module Emphasis = struct
    type inline = t
    type t = { delim : Layout.char; inline : inline }
    let make ?(delim = '*') inline = { delim; inline }
    let inline e = e.inline
    let delim e = e.delim
  end

  module Link = struct
    type inline = t

    type reference_layout = [ `Collapsed | `Full | `Shortcut ]
    type reference =
      [ `Inline of Link_definition.t node
      | `Ref of reference_layout * Label.t * Label.t ]

    type t = { text : inline; reference : reference }

    let make text reference = { text; reference }
    let text l = l.text
    let reference l = l.reference
    let referenced_label l =
      match l.reference with `Inline _ -> None | `Ref (_, _, k) -> Some k

    let reference_definition defs l =
      match l.reference with
      | `Inline ld -> Some (Link_definition.Def ld)
      | `Ref (_, _, def) -> Label.Map.find_opt (Label.key def) defs

    let is_unsafe l =
      let allowed_data_url l =
        let allowed =
          [ "image/gif"; "image/png"; "image/jpeg"; "image/webp" ]
        in
        (* Extract mediatype from data:[<mediatype>][;base64],<data> *)
        match String.index_from_opt l 4 ',' with
        | None -> false
        | Some j ->
            let k =
              match String.index_from_opt l 4 ';' with None -> j | Some k -> k
            in
            let t = String.sub l 5 (min j k - 5) in
            List.mem t allowed
      in
      Ascii.caseless_starts_with ~prefix:"javascript:" l
      || Ascii.caseless_starts_with ~prefix:"vbscript:" l
      || Ascii.caseless_starts_with ~prefix:"file:" l
      || Ascii.caseless_starts_with ~prefix:"data:" l
         && not (allowed_data_url l)
  end

  module Raw_html = struct
    type t = Block_line.tight list
  end

  module Text = struct
    type t = string
  end

  type t +=
    | Autolink of Autolink.t node
    | Break of Break.t node
    | Code_span of Code_span.t node
    | Emphasis of Emphasis.t node
    | Image of Link.t node
    | Inlines of t list node
    | Link of Link.t node
    | Raw_html of Raw_html.t node
    | Strong_emphasis of Emphasis.t node
    | Text of Text.t node

  let empty = Inlines ([], Meta.none)

  let err_unknown = "Unknown Cmarkit.Inline.t type extension"

  (* Extensions *)

  module Strikethrough = struct
    type nonrec t = t
    let make = Fun.id
    let inline = Fun.id
  end

  module Math_span = struct
    type t = { display : bool; tex_layout : Block_line.tight list }
    let make ~display tex_layout = { display; tex_layout }
    let display ms = ms.display
    let tex_layout ms = ms.tex_layout
    let tex ms =
      let s = List.map Block_line.tight_to_string ms.tex_layout in
      String.concat " " s
  end

  type t +=
    | Ext_strikethrough of Strikethrough.t node
    | Ext_math_span of Math_span.t node

  (* Functions on inlines *)

  let is_empty = function Text ("", _) | Inlines ([], _) -> true | _ -> false

  let ext_none _ = invalid_arg err_unknown
  let meta ?(ext = ext_none) = function
    | Autolink (_, m)
    | Break (_, m)
    | Code_span (_, m)
    | Emphasis (_, m)
    | Image (_, m)
    | Inlines (_, m)
    | Link (_, m)
    | Raw_html (_, m)
    | Strong_emphasis (_, m)
    | Text (_, m) ->
        m
    | Ext_strikethrough (_, m) -> m
    | Ext_math_span (_, m) -> m
    | i -> ext i

  let rec normalize ?(ext = ext_none) = function
    | ( Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
      | Inlines ([], _)
      | Ext_math_span _ ) as i ->
        i
    | Image (l, m) -> Image ({ l with text = normalize ~ext l.text }, m)
    | Link (l, m) -> Link ({ l with text = normalize ~ext l.text }, m)
    | Inlines ([ i ], _) -> i
    | Emphasis (e, m) ->
        Emphasis ({ e with inline = normalize ~ext e.inline }, m)
    | Strong_emphasis (e, m) ->
        Strong_emphasis ({ e with inline = normalize ~ext e.inline }, m)
    | Inlines (i :: is, m) -> (
        let rec loop acc = function
          | Inlines (is', _) :: is ->
              loop acc (List.rev_append (List.rev is') is)
          | (Text (t', _) as i') :: is -> (
              match acc with
              | Text (t, _) :: acc ->
                  let i = Text (t ^ t', ()) in
                  loop (i :: acc) is
              | _ -> loop (normalize ~ext i' :: acc) is)
          | i :: is -> loop (normalize ~ext i :: acc) is
          | [] -> List.rev acc
        in
        let is = loop [ normalize ~ext i ] is in
        match is with [ i ] -> i | _ -> Inlines (is, m))
    | Ext_strikethrough (i, m) -> Ext_strikethrough (normalize ~ext i, m)
    | i -> ext i

  let ext_none = ext_none
  let to_plain_text ?(ext = ext_none) ~break_on_soft i =
    let push s acc = (s :: List.hd acc) :: List.tl acc in
    let newline acc = [] :: List.rev (List.hd acc) :: List.tl acc in
    let rec loop ~break_on_soft acc = function
      | Autolink (a, _) :: is ->
          let acc = push (String.concat "" [ "<"; fst a.link; ">" ]) acc in
          loop ~break_on_soft acc is
      | Break ({ type' = `Hard; _ }, _) :: is ->
          loop ~break_on_soft (newline acc) is
      | Break ({ type' = `Soft; _ }, _) :: is ->
          let acc = if break_on_soft then newline acc else push " " acc in
          loop ~break_on_soft acc is
      | Code_span (cs, _) :: is ->
          loop ~break_on_soft (push (Code_span.code cs) acc) is
      | Emphasis ({ inline; _ }, _) :: is
      | Strong_emphasis ({ inline; _ }, _) :: is ->
          loop ~break_on_soft acc (inline :: is)
      | Inlines (is', _) :: is ->
          loop ~break_on_soft acc (List.rev_append (List.rev is') is)
      | Link (l, _) :: is | Image (l, _) :: is ->
          loop ~break_on_soft acc (l.text :: is)
      | Raw_html _ :: is -> loop ~break_on_soft acc is
      | Text (t, _) :: is -> loop ~break_on_soft (push t acc) is
      | Ext_strikethrough (i, _) :: is -> loop ~break_on_soft acc (i :: is)
      | Ext_math_span (m, _) :: is ->
          loop ~break_on_soft (push (Math_span.tex m) acc) is
      | i :: is -> loop ~break_on_soft acc (ext ~break_on_soft i :: is)
      | [] -> List.rev (List.rev (List.hd acc) :: List.tl acc)
    in
    loop ~break_on_soft ([] :: []) [ i ]

  let id ?buf ?ext i =
    let text = to_plain_text ?ext ~break_on_soft:false i in
    let s = String.concat "\n" (List.map (String.concat "") text) in
    let b =
      match buf with
      | Some b ->
          Buffer.reset b;
          b
      | None -> Buffer.create 256
    in
    let[@inline] collapse_blanks b ~prev_byte =
      (* Collapses non initial white *)
      if Ascii.is_blank prev_byte && Buffer.length b <> 0 then
        Buffer.add_char b '-'
    in
    let rec loop b s max ~prev_byte k =
      if k > max then Buffer.contents b
      else
        match s.[k] with
        | (' ' | '\t') as prev_byte -> loop b s max ~prev_byte (k + 1)
        | ('_' | '-') as c ->
            collapse_blanks b ~prev_byte;
            Buffer.add_char b c;
            loop b s max ~prev_byte:c (k + 1)
        | _ ->
            let () = collapse_blanks b ~prev_byte in
            let d = String.get_utf_8_uchar s k in
            let u = Uchar.utf_decode_uchar d in
            let u = match Uchar.to_int u with 0x0000 -> Uchar.rep | _ -> u in
            let k' = k + Uchar.utf_decode_length d in
            if Cmarkit_data.is_unicode_punctuation u then
              loop b s max ~prev_byte:'\x00' k'
            else
              let () =
                match Cmarkit_data.unicode_case_fold u with
                | None -> Buffer.add_utf_8_uchar b u
                | Some fold -> Buffer.add_string b fold
              in
              let prev_byte = s.[k] in
              loop b s max ~prev_byte k'
    in
    loop b s (String.length s - 1) ~prev_byte:'\x00' 0
end

module Block = struct
  type t = ..

  module Blank_line = struct
    type t = Layout.blanks
  end

  module Block_quote = struct
    type nonrec t = { indent : Layout.indent; block : t }
    let make ?(indent = 0) block = { indent; block }
    let indent bq = bq.indent
    let block bq = bq.block
  end

  module Code_block = struct
    type fenced_layout = {
      indent : Layout.indent;
      opening_fence : Layout.string node;
      closing_fence : Layout.string node option;
    }

    let default_fenced_layout =
      {
        indent = 0;
        opening_fence = Layout.empty;
        closing_fence = Some Layout.empty;
      }

    type layout = [ `Indented | `Fenced of fenced_layout ]
    type t = {
      layout : layout;
      info_string : string node option;
      code : string node list;
    }

    let make ?(layout = `Fenced default_fenced_layout) ?info_string code =
      let layout =
        match (info_string, layout) with
        | Some _, `Indented -> `Fenced default_fenced_layout
        | _, layout -> layout
      in
      { layout; info_string; code }

    let layout cb = cb.layout
    let info_string cb = cb.info_string
    let code cb = cb.code

    let make_fence cb =
      let rec loop char counts = function
        | [] -> counts
        | (c, _) :: cs ->
            let max = String.length c - 1 in
            let k = ref 0 in
            while !k <= max && c.[!k] = char do
              incr k
            done;
            loop char (if !k <> 0 then !k :: counts else counts) cs
      in
      let char =
        match cb.info_string with
        | Some (i, _) when String.exists (Char.equal '`') i -> '~'
        | None | Some _ -> '`'
      in
      let counts = loop char [] cb.code in
      ( char,
        Inline.Code_span.min_backtick_count (* not char specific *)
          ~min:3 counts )

    let language_of_info_string s =
      let rec next_white s max i =
        if i > max || Ascii.is_white s.[i] then i else next_white s max (i + 1)
      in
      if s = "" then None
      else
        let max = String.length s - 1 in
        let white = next_white s max 0 in
        let rem_first = Match.first_non_blank s ~last:max ~start:white in
        let lang = String.sub s 0 white in
        if lang = "" then None
        else Some (lang, String.sub s rem_first (max - rem_first + 1))

    let is_math_block = function
      | None -> false
      | Some (i, _) -> (
          match language_of_info_string i with
          | Some ("math", _) -> true
          | Some _ | None -> false)
  end

  module Heading = struct
    type atx_layout = {
      indent : Layout.indent;
      after_opening : Layout.blanks;
      closing : Layout.string;
    }

    let default_atx_layout = { indent = 0; after_opening = ""; closing = "" }

    type setext_layout = {
      leading_indent : Layout.indent;
      trailing_blanks : Layout.blanks;
      underline_indent : Layout.indent;
      underline_count : Layout.count node;
      underline_blanks : Layout.blanks;
    }

    type layout = [ `Atx of atx_layout | `Setext of setext_layout ]
    type id = [ `Auto of string | `Id of string ]
    type t = { layout : layout; level : int; inline : Inline.t; id : id option }

    let make ?id ?(layout = `Atx default_atx_layout) ~level inline =
      let max = match layout with `Atx _ -> 6 | `Setext _ -> 2 in
      let level = Int.max 1 (Int.min level max) in
      { layout; level; inline; id }

    let layout h = h.layout
    let level h = h.level
    let inline h = h.inline
    let id h = h.id
  end

  module Html_block = struct
    type t = string node list
  end

  module List_item = struct
    type block = t
    type t = {
      before_marker : Layout.indent;
      marker : Layout.string node;
      after_marker : Layout.indent;
      block : block;
      ext_task_marker : Uchar.t node option;
    }

    let make ?(before_marker = 0) ?(marker = Layout.empty) ?(after_marker = 1)
        ?ext_task_marker block =
      { before_marker; marker; after_marker; block; ext_task_marker }

    let block i = i.block
    let before_marker i = i.before_marker
    let marker i = i.marker
    let after_marker i = i.after_marker
    let ext_task_marker i = i.ext_task_marker
    let task_status_of_task_marker u =
      match Uchar.to_int u with
      | 0x0020 -> `Unchecked
      | 0x0078 (* x *)
      | 0x0058 (* X *)
      | 0x2713 (* ✓ *)
      | 0x2714 (* ✔ *)
      | 0x10102 (* 𐄂 *)
      | 0x1F5F8 (* 🗸*) ->
          `Checked
      | 0x007E (* ~ *) -> `Cancelled
      | _ -> `Other u
  end

  module List' = struct
    type type' = [ `Unordered of Layout.char | `Ordered of int * Layout.char ]
    type t = { type' : type'; tight : bool; items : List_item.t node list }

    let make ?(tight = true) type' items = { type'; tight; items }

    let type' l = l.type'
    let tight l = l.tight
    let items l = l.items
  end

  module Paragraph = struct
    type t = {
      leading_indent : Layout.indent;
      inline : Inline.t;
      trailing_blanks : Layout.blanks;
    }

    let make ?(leading_indent = 0) ?(trailing_blanks = "") inline =
      { leading_indent; inline; trailing_blanks }

    let inline p = p.inline
    let leading_indent p = p.leading_indent
    let trailing_blanks p = p.trailing_blanks
  end

  module Thematic_break = struct
    type t = { indent : Layout.indent; layout : Layout.string }
    let make ?(indent = 0) ?(layout = "---") () = { indent; layout }
    let indent t = t.indent
    let layout t = t.layout
  end

  type t +=
    | Blank_line of Layout.blanks node
    | Block_quote of Block_quote.t node
    | Blocks of t list node
    | Code_block of Code_block.t node
    | Heading of Heading.t node
    | Html_block of Html_block.t node
    | Link_reference_definition of Link_definition.t node
    | List of List'.t node
    | Paragraph of Paragraph.t node
    | Thematic_break of Thematic_break.t node

  let empty = Blocks ([], Meta.none)

  (* Extensions *)

  module Table = struct
    type align = [ `Left | `Center | `Right ]
    type sep = align option * Layout.count
    type cell_layout = Layout.blanks * Layout.blanks
    type row =
      [ `Header of (Inline.t * cell_layout) list
      | `Sep of sep node list
      | `Data of (Inline.t * cell_layout) list ]

    type t = {
      indent : Layout.indent;
      col_count : int;
      rows : (row node * Layout.blanks) list;
    }

    let col_count rows =
      let rec loop c = function
        | (((`Header cols | `Data cols), _), _) :: rs ->
            loop (Int.max (List.length cols) c) rs
        | ((`Sep cols, _), _) :: rs -> loop (Int.max (List.length cols) c) rs
        | [] -> c
      in
      loop 0 rows

    let make ?(indent = 0) rows = { indent; col_count = col_count rows; rows }
    let indent t = t.indent
    let col_count t = t.col_count
    let rows t = t.rows

    let parse_sep_row cs =
      let rec loop acc = function
        | [] -> Some (List.rev acc)
        | (Inline.Text (s, meta), ("", "")) :: cs -> (
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
                  loop (((sep, count), meta) :: acc) cs)
        | _ -> None
      in
      loop [] cs
  end

  module Footnote = struct
    type nonrec t = {
      indent : Layout.indent;
      label : Label.t;
      defined_label : Label.t option;
      block : t;
    }

    let make ?(indent = 0) ?defined_label:d label block =
      let defined_label = match d with None -> Some label | Some d -> d in
      { indent; label; defined_label; block }

    let indent fn = fn.indent
    let label fn = fn.label
    let defined_label fn = fn.defined_label
    let block fn = fn.block

    type Label.def += Def of t node
    let stub label defined_label =
      Def ({ indent = 0; label; defined_label; block = empty }, Meta.none)
  end

  type t +=
    | Ext_math_block of Code_block.t node
    | Ext_table of Table.t node
    | Ext_footnote_definition of Footnote.t node

  (* Functions on blocks *)

  let err_unknown = "Unknown Cmarkit.Block.t type extension"

  let ext_none _ = invalid_arg err_unknown
  let meta ?(ext = ext_none) = function
    | Blank_line (_, m)
    | Block_quote (_, m)
    | Blocks (_, m)
    | Code_block (_, m)
    | Heading (_, m)
    | Html_block (_, m)
    | Link_reference_definition (_, m)
    | List (_, m)
    | Paragraph (_, m)
    | Thematic_break (_, m)
    | Ext_math_block (_, m)
    | Ext_table (_, m)
    | Ext_footnote_definition (_, m) ->
        m
    | b -> ext b

  let rec normalize ?(ext = ext_none) = function
    | ( Blank_line _ | Code_block _ | Heading _ | Html_block _
      | Link_reference_definition _ | Paragraph _ | Thematic_break _
      | Blocks ([], _)
      | Ext_math_block _ | Ext_table _ ) as b ->
        b
    | Block_quote (b, m) ->
        let b = { b with block = normalize ~ext b.block } in
        Block_quote (b, m)
    | List (l, m) ->
        let item (i, meta) =
          let block = List_item.block i in
          ({ i with List_item.block = normalize ~ext block }, meta)
        in
        List ({ l with items = List.map item l.items }, m)
    | Blocks (b :: bs, m) -> (
        let rec loop acc = function
          | Blocks (bs', _) :: bs ->
              loop acc (List.rev_append (List.rev bs') bs)
          | b :: bs -> loop (normalize ~ext b :: acc) bs
          | [] -> List.rev acc
        in
        let bs = loop [ normalize ~ext b ] bs in
        match bs with [ b ] -> b | _ -> Blocks (bs, m))
    | Ext_footnote_definition (fn, m) ->
        let fn = { fn with block = normalize ~ext fn.block } in
        Ext_footnote_definition (fn, m)
    | b -> ext b

  let rec defs ?(ext = fun _b _defs -> invalid_arg err_unknown)
      ?(init = Label.Map.empty) = function
    | Blank_line _ | Code_block _ | Heading _ | Html_block _ | Paragraph _
    | Thematic_break _ | Ext_math_block _ | Ext_table _ ->
        init
    | Block_quote (b, _) -> defs ~ext ~init (Block_quote.block b)
    | Blocks (bs, _) -> List.fold_left (fun init b -> defs ~ext ~init b) init bs
    | List (l, _) ->
        let add init (i, _) = defs ~ext ~init (List_item.block i) in
        List.fold_left add init l.items
    | Link_reference_definition ld -> (
        match Link_definition.defined_label (fst ld) with
        | None -> init
        | Some def ->
            Label.Map.add (Label.key def) (Link_definition.Def ld) init)
    | Ext_footnote_definition fn ->
        let init =
          match Footnote.defined_label (fst fn) with
          | None -> init
          | Some def -> Label.Map.add (Label.key def) (Footnote.Def fn) init
        in
        defs ~ext ~init (Footnote.block (fst fn))
    | b -> ext init b
end

module Doc = struct
  type t = { nl : Layout.string; block : Block.t; defs : Label.defs }
  let make ?(nl = "\n") ?(defs = Label.Map.empty) block = { nl; block; defs }
  let empty = make (Block.Blocks ([], Meta.none))
  let nl d = d.nl
  let block d = d.block
  let defs d = d.defs
  let unicode_version = Data_uchar.unicode_version
  let commonmark_version = "0.30"
end

(* Heterogeneous dictionaries *)

module Dict = struct
  (* Type identifiers, can be deleted once we require 5.1 *)
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
  init_context : context -> Doc.t -> unit;
  inline : inline;
  block : block;
  doc : doc;
}

and context = {
  renderer : t;
  mutable state : Dict.t;
  b : Buffer.t;
  mutable document : Doc.t;
}

and inline = context -> Inline.t -> bool
and block = context -> Block.t -> bool
and doc = context -> Doc.t -> bool

let nop _ _ = ()
let none _ _ = false

let make ?(init_context = nop) ?(inline = none) ?(block = none) ?(doc = none) ()
    =
  { init_context; inline; block; doc }

let compose g f =
  let init_context c d =
    g.init_context c d;
    f.init_context c d
  in
  let block c b = f.block c b || g.block c b in
  let inline c i = f.inline c i || g.inline c i in
  let doc c d = f.doc c d || g.doc c d in
  { init_context; inline; block; doc }

let _init_context r = r.init_context
let _inline r = r.inline
let _block r = r.block
let _doc r = r.doc

module Context = struct
  type t = context
  let make renderer b =
    { renderer; b; state = Dict.empty; document = Doc.empty }

  let buffer c = c.b
  let renderer c = c.renderer
  let get_document (c : context) = c.document
  let get_defs (c : context) = Doc.defs c.document

  module State = struct
    type 'a t = 'a Dict.key
    let make = Dict.key
    let find c st = Dict.find st c.state
    let get c st = Option.get (Dict.find st c.state)
    let set c st = function
      | None -> c.state <- Dict.remove st c.state
      | Some s -> c.state <- Dict.add st s c.state
  end

  let init c d = c.renderer.init_context c d

  let invalid_inline _ = invalid_arg "Unknown Inline.t case"
  let invalid_block _ = invalid_arg "Unknown Block.t case"
  let unhandled_doc _ = invalid_arg "Unhandled Doc.t"

  let byte r c = Buffer.add_char r.b c
  let utf_8_uchar r u = Buffer.add_utf_8_uchar r.b u
  let string c s = Buffer.add_string c.b s
  let inline c i = ignore (c.renderer.inline c i || invalid_inline i)
  let block c b = ignore (c.renderer.block c b || invalid_block b)
  let doc (c : context) d =
    c.document <- d;
    init c d;
    ignore (c.renderer.doc c d || unhandled_doc d);
    c.document <- Doc.empty
end

let doc_to_string r d =
  let b = Buffer.create 1024 in
  let c = Context.make r b in
  Context.doc c d;
  Buffer.contents b

let buffer_add_doc r b d = Context.doc (Context.make r b) d

type indent =
  [ `I of int
  | `L of int * string * int * Uchar.t option
  | `Q of int
  | `Fn of int * Label.t ]

type state = {
  nl : string; (* newline to output. *)
  mutable sot : bool; (* start of text *)
  mutable indents : indent list; (* indentation stack. *)
}

let state : state Context.State.t = Context.State.make ()
let get_state c = Context.State.get c state
let init_context c d =
  Context.State.set c state (Some { nl = Doc.nl d; sot = true; indents = [] })

module Char_set = Set.Make (Char)

let esc_angles = Char_set.of_list [ '<'; '>' ]
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
      else if esc_ctrl && Ascii.is_control c then (
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
    next <= max && (Ascii.is_letter s.[next] || s.[next] = '#')
  in
  let esc_tilde s max prev next =
    (not (Char.equal prev '~')) && next <= max && s.[next] = '~'
  in
  let esc_item_marker s i =
    if i = 0 || i > 9 (* marker has from 1-9 digits *) then false
    else
      let k = ref (i - 1) in
      while !k >= 0 && Ascii.is_digit s.[!k] do
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
      if Ascii.is_control c then (
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

let string_node_option c = function
  | None -> ()
  | Some (s, _) -> Context.string c s
let nchars c n char =
  for _i = 1 to n do
    Context.byte c char
  done

let newline c =
  (* Block generally introduce newlines, except the first one. *)
  let st = get_state c in
  if st.sot then st.sot <- false else Context.string c st.nl

let push_indent c n =
  let st = get_state c in
  st.indents <- n :: st.indents
let pop_indent c =
  let st = get_state c in
  match st.indents with [] -> () | ns -> st.indents <- List.tl ns

let rec indent c =
  let rec loop c acc = function
    | [] -> acc
    | (`I n as i) :: is ->
        nchars c n ' ';
        loop c (i :: acc) is
    | (`Q n as i) :: is ->
        nchars c n ' ';
        Context.byte c '>';
        Context.byte c ' ';
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
    | `Fn (before, label) :: is ->
        nchars c before ' ';
        Context.byte c '[';
        link_label_lines c (Label.text label);
        Context.string c "]:";
        (* On the next call we'll just indent to ^ for the footnote  *)
        loop c (`I (before + 1) :: acc) is
  in
  let st = get_state c in
  st.indents <- loop c [] (List.rev st.indents)

and link_label_lines c lines = escaped_tight_block_lines c esc_link_label lines

and escaped_tight_block_lines c cs = function
  | [] -> ()
  | l :: ls ->
      let tight c (blanks, (l, _)) =
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
  | (l, _) :: ls ->
      let line c (l, _) =
        newline c;
        indent c;
        Context.string c l
      in
      Context.string c l;
      List.iter (line c) ls

let tight_block_lines c = function
  | [] -> ()
  | l :: ls ->
      let tight c (blanks, (l, _)) =
        Context.string c blanks;
        Context.string c l
      in
      let line c l =
        newline c;
        indent c;
        tight c l
      in
      tight c l;
      List.iter (line c) ls

let autolink c a =
  Context.byte c '<';
  Context.string c (fst (Inline.Autolink.link a));
  Context.byte c '>'

let break c b =
  let layout_before = fst (Inline.Break.layout_before b) in
  let layout_after = fst (Inline.Break.layout_after b) in
  let before, after =
    match Inline.Break.type' b with
    | `Soft -> (layout_before, layout_after)
    | `Hard ->
        ((if layout_before = "" then "  " else layout_before), layout_after)
  in
  Context.string c before;
  newline c;
  indent c;
  Context.string c after

let code_span c cs =
  nchars c (Inline.Code_span.backtick_count cs) '`';
  tight_block_lines c (Inline.Code_span.code_layout cs);
  nchars c (Inline.Code_span.backtick_count cs) '`'

let emphasis c e =
  let delim = Inline.Emphasis.delim e and i = Inline.Emphasis.inline e in
  let delim = if not (delim = '*' || delim = '_') then '*' else delim in
  Context.byte c delim;
  Context.inline c i;
  Context.byte c delim

let strong_emphasis c e =
  let delim = Inline.Emphasis.delim e and i = Inline.Emphasis.inline e in
  let delim = if not (delim = '*' || delim = '_') then '*' else delim in
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

let link_definition c ld =
  let layout = Link_definition.layout ld in
  block_lines c layout.before_dest;
  (match Link_definition.dest ld with
  | None -> ()
  | Some (dest, _) ->
      if layout.angled_dest then (
        Context.byte c '<';
        escaped_string c esc_angles dest;
        Context.byte c '>')
      else escaped_string c esc_parens dest);
  if
    layout.after_dest = []
    && Option.is_some (Link_definition.dest ld)
    && Option.is_some (Link_definition.title ld)
  then Context.byte c ' ' (* at least a space is needed *);
  block_lines c layout.after_dest;
  link_title c layout.title_open_delim (Link_definition.title ld);
  block_lines c layout.after_title

let link c l =
  match Inline.Link.reference l with
  | `Inline (ld, _) ->
      Context.byte c '[';
      Context.inline c (Inline.Link.text l);
      Context.byte c ']';
      Context.byte c '(';
      link_definition c ld;
      Context.byte c ')'
  | `Ref (`Shortcut, label, _) ->
      Context.byte c '[';
      link_label_lines c (Label.text label);
      Context.byte c ']'
  | `Ref (`Collapsed, label, _) ->
      Context.byte c '[';
      link_label_lines c (Label.text label);
      Context.byte c ']';
      Context.string c "[]"
  | `Ref (`Full, label, _) ->
      Context.byte c '[';
      Context.inline c (Inline.Link.text l);
      Context.byte c ']';
      Context.byte c '[';
      link_label_lines c (Label.text label);
      Context.byte c ']'

let inlines c is = List.iter (Context.inline c) is
let image c l =
  Context.byte c '!';
  link c l
let raw_html c h = tight_block_lines c h
let text c t = escaped_text c t

let strikethrough c s =
  let i = Inline.Strikethrough.inline s in
  Context.string c "~~";
  Context.inline c i;
  Context.string c "~~"

let math_span c ms =
  let sep = if Inline.Math_span.display ms then "$$" else "$" in
  Context.string c sep;
  tight_block_lines c (Inline.Math_span.tex_layout ms);
  Context.string c sep

let inline c = function
  | Inline.Autolink (a, _) ->
      autolink c a;
      true
  | Inline.Break (b, _) ->
      break c b;
      true
  | Inline.Code_span (cs, _) ->
      code_span c cs;
      true
  | Inline.Emphasis (e, _) ->
      emphasis c e;
      true
  | Inline.Image (i, _) ->
      image c i;
      true
  | Inline.Inlines (is, _) ->
      inlines c is;
      true
  | Inline.Link (l, _) ->
      link c l;
      true
  | Inline.Raw_html (html, _) ->
      raw_html c html;
      true
  | Inline.Strong_emphasis (e, _) ->
      strong_emphasis c e;
      true
  | Inline.Text (t, _) ->
      text c t;
      true
  | Inline.Ext_strikethrough (s, _) ->
      strikethrough c s;
      true
  | Inline.Ext_math_span (m, _) ->
      math_span c m;
      true
  | _ ->
      Context.string c "<!-- Unknown Cmarkit inline -->";
      true

let blank_line c l =
  newline c;
  indent c;
  Context.string c l

let block_quote c bq =
  push_indent c (`Q (Block.Block_quote.indent bq));
  Context.block c (Block.Block_quote.block bq);
  pop_indent c

let code_block c cb =
  match Block.Code_block.layout cb with
  | `Indented ->
      newline c;
      push_indent c (`I 4);
      indent c;
      block_lines c (Block.Code_block.code cb);
      pop_indent c
  | `Fenced f ->
      let opening, closing =
        match fst f.opening_fence with
        | "" ->
            let char, len = Block.Code_block.make_fence cb in
            let f = String.make len char in
            (f, Some f)
        | opening -> (opening, Option.map fst f.closing_fence)
      in
      let info_string = Block.Code_block.info_string cb in
      let code = Block.Code_block.code cb in
      newline c;
      push_indent c (`I f.indent);
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

let heading c h =
  newline c;
  indent c;
  match Block.Heading.layout h with
  | `Atx { indent; after_opening; closing } ->
      let inline = Block.Heading.inline h in
      nchars c indent ' ';
      nchars c (Block.Heading.level h) '#';
      if after_opening = "" && not (Inline.is_empty inline) then
        Context.byte c ' '
      else Context.string c after_opening;
      Context.inline c inline;
      Context.string c closing
  | `Setext l ->
      let u =
        match Block.Heading.level h with 1 -> '=' | 2 -> '-' | _ -> '-'
      in
      nchars c l.leading_indent ' ';
      Context.inline c (Block.Heading.inline h);
      Context.string c l.trailing_blanks;
      newline c;
      indent c;
      nchars c l.underline_indent ' ';
      nchars c (fst l.underline_count) u;
      Context.string c l.underline_blanks

let html_block c h =
  newline c;
  indent c;
  block_lines c h

let link_reference_definition c ld =
  newline c;
  indent c;
  nchars c (Link_definition.layout ld).indent ' ';
  Context.byte c '[';
  (match Link_definition.label ld with
  | None -> ()
  | Some label -> escaped_tight_block_lines c esc_link_label (Label.text label));
  Context.string c "]:";
  link_definition c ld

let unordered_item c marker (i, _) =
  let before = Block.List_item.before_marker i in
  let after = Block.List_item.after_marker i in
  let task = Option.map fst (Block.List_item.ext_task_marker i) in
  push_indent c (`L (before, marker, after, task));
  Context.block c (Block.List_item.block i);
  pop_indent c

let ordered_item c sep num (i, _) =
  let before = Block.List_item.before_marker i in
  let marker = fst (Block.List_item.marker i) in
  let marker = if marker = "" then Int.to_string num ^ sep else marker in
  let after = Block.List_item.after_marker i in
  let task = Option.map fst (Block.List_item.ext_task_marker i) in
  push_indent c (`L (before, marker, after, task));
  Context.block c (Block.List_item.block i);
  pop_indent c;
  num + 1

let list c l =
  match Block.List'.type' l with
  | `Unordered marker ->
      let marker = match marker with '*' | '-' | '+' -> marker | _ -> '*' in
      let marker = String.make 1 marker in
      List.iter (unordered_item c marker) (Block.List'.items l)
  | `Ordered (start, sep) ->
      let sep = if sep <> '.' && sep <> ')' then '.' else sep in
      let sep = String.make 1 sep in
      ignore (List.fold_left (ordered_item c sep) start (Block.List'.items l))

let paragraph c p =
  newline c;
  indent c;
  nchars c (Block.Paragraph.leading_indent p) ' ';
  Context.inline c (Block.Paragraph.inline p);
  Context.string c (Block.Paragraph.trailing_blanks p)

let thematic_break c t =
  let ind = Block.Thematic_break.indent t in
  let break = Block.Thematic_break.layout t in
  let break = if break = "" then "---" else break in
  newline c;
  indent c;
  nchars c ind ' ';
  Context.string c break

let table c t =
  let col c (i, (before, after)) =
    Context.byte c '|';
    Context.string c before;
    Context.inline c i;
    Context.string c after
  in
  let sep c ((align, len), _) =
    Context.byte c '|';
    match align with
    | None -> nchars c len '-'
    | Some `Left ->
        Context.byte c ':';
        nchars c len '-'
    | Some `Center ->
        Context.byte c ':';
        nchars c len '-';
        Context.byte c ':'
    | Some `Right ->
        nchars c len '-';
        Context.byte c ':'
  in
  let row c = function
    | (`Header cols, _), blanks | (`Data cols, _), blanks ->
        newline c;
        indent c;
        if cols = [] then Context.byte c '|' else List.iter (col c) cols;
        Context.byte c '|';
        Context.string c blanks
    | (`Sep seps, _), blanks ->
        newline c;
        indent c;
        if seps = [] then Context.byte c '|' else List.iter (sep c) seps;
        Context.byte c '|';
        Context.string c blanks
  in
  push_indent c (`I (Block.Table.indent t));
  List.iter (row c) (Block.Table.rows t);
  pop_indent c

let footnote c fn =
  push_indent c (`Fn (Block.Footnote.indent fn, Block.Footnote.label fn));
  Context.block c (Block.Footnote.block fn);
  pop_indent c

let block c = function
  | Block.Blank_line (l, _) ->
      blank_line c l;
      true
  | Block.Block_quote (b, _) ->
      block_quote c b;
      true
  | Block.Blocks (bs, _) ->
      List.iter (Context.block c) bs;
      true
  | Block.Code_block (cb, _) ->
      code_block c cb;
      true
  | Block.Heading (h, _) ->
      heading c h;
      true
  | Block.Html_block (h, _) ->
      html_block c h;
      true
  | Block.Link_reference_definition (ld, _) ->
      link_reference_definition c ld;
      true
  | Block.List (l, _) ->
      list c l;
      true
  | Block.Paragraph (p, _) ->
      paragraph c p;
      true
  | Block.Thematic_break (t, _) ->
      thematic_break c t;
      true
  | Block.Ext_math_block (cb, _) ->
      code_block c cb;
      true
  | Block.Ext_table (t, _) ->
      table c t;
      true
  | Block.Ext_footnote_definition (t, _) ->
      footnote c t;
      true
  | _ ->
      newline c;
      indent c;
      Context.string c "<!-- Unknown Cmarkit block -->";
      true

let doc c d =
  Context.block c (Doc.block d);
  true

let renderer () = make ~init_context ~inline ~block ~doc ()
