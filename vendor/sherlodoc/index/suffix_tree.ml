module Doc = struct
  type 'a t =
    { uid : 'a
    ; text : string
    }

  let length t = String.length t.text + 1

  type 'a v =
    | Terminal of 'a
    | Char of char

  let get t i = if i >= String.length t.text then Terminal t.uid else Char t.text.[i]
  let sub { text; _ } i = String.sub text i (String.length text - i)
end

module Buf = struct
  (* Cache small strings as slices in one bigstring. *)

  module String_hashtbl = Hashtbl.Make (struct
      type t = string

      let equal = String.equal
      let hash = Hashtbl.hash
    end)

  type t =
    { buffer : Buffer.t
    ; cache : int String_hashtbl.t
    ; mutable contents : string option
    }

  let make () =
    { buffer = Buffer.create 16; cache = String_hashtbl.create 16; contents = None }

  let contents t =
    match t.contents with
    | Some contents -> contents
    | None ->
      let contents = Buffer.contents t.buffer in
      t.contents <- Some contents ;
      contents

  let get t i = Buffer.nth t.buffer i

  let add { buffer; cache; contents } substr =
    assert (contents = None) ;
    match String_hashtbl.find_opt cache substr with
    | Some start -> start
    | None ->
      let start = Buffer.length buffer in
      Buffer.add_string buffer substr ;
      let stop = Buffer.length buffer in
      assert (stop - start = String.length substr) ;
      for idx = 1 to String.length substr - 1 do
        String_hashtbl.add
          cache
          (String.sub substr idx (String.length substr - idx))
          (start + idx)
      done ;
      start
end

module Entry = Db.Entry

module Uid = struct
  type t = int

  let gen = ref 0

  let make () =
    let u = !gen in
    gen := u + 1 ;
    u
end

module Terminals = struct
  type t = Entry.t list

  let empty = []
  let singleton x = [ x ]

  let add ~hint x xs =
    match hint with
    | Some (prev_xs, xxs) when prev_xs == xs -> xxs
    | _ -> x :: xs

  let hash = Hashtbl.hash

  let rec equal xs ys =
    match xs, ys with
    | [], [] -> true
    | x :: xs, y :: ys when x == y -> equal xs ys
    | _ -> false

  let equal xs ys = xs == ys || equal xs ys

  let mem x = function
    | y :: _ -> Entry.equal x y
    | _ -> false
end

module Char_map = Map.Make (Char)

type node =
  { mutable start : int
  ; mutable len : int
  ; mutable suffix_link : node option
  ; mutable terminals : Terminals.t
  ; mutable children : node Char_map.t
  }

type t =
  { buffer : Buf.t
  ; root : node
  }

let make_root () =
  { start = 0
  ; len = 0
  ; suffix_link = None
  ; terminals = Terminals.empty
  ; children = Char_map.empty
  }

let make buffer = { root = make_root (); buffer }

let split_at ~str node len =
  let split_chr = Buf.get str (node.start + len) in
  let new_node =
    { start = node.start
    ; len
    ; suffix_link = None
    ; terminals = Terminals.empty
    ; children = Char_map.singleton split_chr node
    }
  in
  node.start <- node.start + len + 1 ;
  node.len <- node.len - 1 - len ;
  new_node

let lcp i_str i j_str j j_len =
  let j_stop = j + j_len in
  let rec go_lcp i j =
    if i >= String.length i_str || j >= j_stop
    then i
    else (
      let i_chr, j_chr = i_str.[i], Buf.get j_str j in
      if i_chr <> j_chr then i else go_lcp (i + 1) (j + 1))
  in
  let i' = go_lcp i j in
  i' - i

let make_leaf ~prev_leaf ~buffer ~doc str_start =
  let start =
    match prev_leaf with
    | None ->
      let substr = Doc.sub doc (str_start - 1) in
      let start = Buf.add buffer substr in
      start + 1
    | Some (prev_leaf, _depth, _) ->
      let doc_len = Doc.length doc in
      prev_leaf.start + prev_leaf.len - (doc_len - str_start) + 1
  in
  let len = Doc.length doc - str_start - 1 in
  assert (start > 0) ;
  { start
  ; len
  ; suffix_link = None
  ; terminals = Terminals.singleton doc.Doc.uid
  ; children = Char_map.empty
  }

let set_suffix_link ~prev ~depth node =
  match prev with
  | Some (prev, prev_depth) when depth = prev_depth ->
    begin
      match prev.suffix_link with
      | None -> prev.suffix_link <- Some node
      | Some node' -> assert (node == node')
    end ;
    None
  | _ -> prev

let add_document trie doc =
  let root = trie.root in
  let set_leaf ?debug:_ ~prev_leaf ~depth node =
    if node == root
    then None
    else begin
      begin
        match prev_leaf with
        | None -> ()
        | Some (prev_leaf, prev_depth, _) ->
          assert (prev_depth = depth) ;
          begin
            match prev_leaf.suffix_link with
            | None -> prev_leaf.suffix_link <- Some node
            | Some node' -> assert (node' == node)
          end
      end ;
      Some (node, depth - 1)
    end
  in
  let rec go ~prev ~prev_leaf ~depth node i =
    let prev = set_suffix_link ~prev ~depth node in
    if i >= Doc.length doc
    then assert (depth = 0)
    else (
      let chr = Doc.get doc i in
      let i, depth = i + 1, depth + 1 in
      match chr with
      | Terminal doc_uid ->
        if not (Terminals.mem doc_uid node.terminals)
        then begin
          let hint =
            Option.map
              (fun (t, _, prev_terminals) -> prev_terminals, t.terminals)
              prev_leaf
          in
          let prev_terminals = node.terminals in
          node.terminals <- Terminals.add ~hint doc_uid node.terminals ;
          let prev_leaf =
            match set_leaf ~debug:"0" ~prev_leaf ~depth node with
            | None -> None
            | Some (t, depth) -> Some (t, depth, prev_terminals)
          in
          follow_suffix ~prev ~prev_leaf ~parent:node ~depth ~i
        end
      | Char chr -> begin
        match Char_map.find chr node.children with
        | child ->
          assert (depth >= 0) ;
          assert (i - depth >= 0) ;
          assert (i < Doc.length doc) ;
          let len = lcp doc.Doc.text i trie.buffer child.start child.len in
          let i, depth = i + len, depth + len in
          assert (i < Doc.length doc) ;
          if len = child.len
          then
            if not (Char_map.is_empty child.children)
            then go ~prev ~prev_leaf ~depth child i
            else add_leaf ~prev_leaf ~node ~child ~depth ~i ~len
          else begin
            let new_child = split_at ~str:trie.buffer child len in
            node.children <- Char_map.add chr new_child node.children ;
            let prev = set_suffix_link ~prev ~depth new_child in
            assert (prev = None) ;
            add_leaf ~prev_leaf ~node ~child:new_child ~depth ~i ~len
          end
        | exception Not_found ->
          let new_leaf = make_leaf ~prev_leaf ~buffer:trie.buffer ~doc i in
          node.children <- Char_map.add chr new_leaf node.children ;
          let prev_leaf =
            set_leaf ~debug:"1" ~prev_leaf ~depth:(depth + Doc.length doc - i) new_leaf
          in
          let prev_leaf =
            match prev_leaf with
            | None -> None
            | Some (t, depth) -> Some (t, depth, Terminals.empty)
          in
          follow_suffix ~prev ~prev_leaf ~parent:node ~depth ~i
      end)
  and add_leaf ~prev_leaf ~node ~child ~depth ~i ~len =
    match Doc.get doc i with
    | Terminal doc_uid ->
      if not (Terminals.mem doc_uid child.terminals)
      then begin
        let hint =
          Option.map (fun (t, _, prev_terminals) -> prev_terminals, t.terminals) prev_leaf
        in
        let prev_terminals = child.terminals in
        child.terminals <- Terminals.add ~hint doc_uid child.terminals ;
        let prev_leaf =
          match set_leaf ~debug:"2" ~prev_leaf ~depth:(depth + 1) child with
          | None -> None
          | Some (t, depth) -> Some (t, depth, prev_terminals)
        in
        assert (Doc.length doc - i = 1) ;
        begin
          match child.suffix_link with
          | None ->
            let i, depth = i - len, depth - len in
            follow_suffix ~prev:None ~prev_leaf ~parent:node ~depth ~i
          | Some next_child ->
            let depth = depth - 1 in
            go ~prev:None ~prev_leaf:None ~depth next_child i
        end
      end
    | Char new_chr ->
      let new_leaf = make_leaf ~prev_leaf ~buffer:trie.buffer ~doc (i + 1) in
      let prev_leaf =
        set_leaf ~debug:"3" ~prev_leaf ~depth:(depth + Doc.length doc - i) new_leaf
      in
      let prev_leaf =
        match prev_leaf with
        | None -> None
        | Some (t, depth) -> Some (t, depth, Terminals.empty)
      in
      child.children <- Char_map.add new_chr new_leaf child.children ;
      let prev = Some (child, depth - 1) in
      let i, depth = i - len, depth - len in
      follow_suffix ~prev ~prev_leaf ~parent:node ~depth ~i
  and follow_suffix ~prev ~prev_leaf ~parent ~depth ~i =
    match parent.suffix_link with
    | None -> begin
      let i = i - depth + 1 in
      go ~prev:None ~prev_leaf ~depth:0 root i
    end
    | Some next ->
      assert (depth >= 2) ;
      assert (next != root) ;
      go ~prev ~prev_leaf ~depth:(depth - 2) next (i - 1)
  in
  go ~prev:None ~prev_leaf:None ~depth:0 root 0

let add_suffixes t text elt = add_document t { Doc.text; uid = elt }

module Terminals_cache = Hashtbl.Make (Terminals)
module Seen = Set.Make (Db.Entry)

let export_terminals ~cache_term ~is_summary ts =
  try Terminals_cache.find cache_term ts with
  | Not_found ->
    let terminals =
      if ts = []
      then Db.String_automata.Empty
      else if is_summary
      then Db.String_automata.Summary (Array.of_list ts)
      else Db.String_automata.Terminals (Array.of_list ts)
    in
    let result = Uid.make (), terminals in
    Terminals_cache.add cache_term ts result ;
    result

type result =
  { uid : Uid.t
  ; t : Db.String_automata.node
  ; min : Entry.t
  ; seen : Seen.t
  }

let size_of_terminals = function
  | Db.String_automata.Empty -> 1
  | Summary arr | Terminals arr -> Array.length arr

let rec export ~cache ~cache_term ~summarize ~is_root node =
  let is_summary = summarize && not is_root in
  let children =
    Char_map.bindings
    @@ Char_map.map (export ~cache ~cache_term ~summarize ~is_root:false) node.children
  in
  let children =
    List.sort
      (fun (a_chr, { min = a; _ }) (b_chr, { min = b; _ }) ->
        match Entry.compare a b with
        | 0 -> Char.compare a_chr b_chr
        | c -> c)
      children
  in
  let children_seen =
    List.fold_left (fun acc (_, child) -> Seen.union acc child.seen) Seen.empty children
  in
  let seen = List.fold_left (fun acc e -> Seen.add e acc) children_seen node.terminals in
  let terminals =
    if is_summary
    then List.of_seq (Seen.to_seq seen)
    else
      List.sort Entry.compare
      @@ List.filter (fun e -> not (Seen.mem e children_seen)) node.terminals
  in
  let min_child =
    match children with
    | [] -> None
    | (_, { min = elt; _ }) :: _ -> Some elt
  in
  let min_terminal =
    match terminals with
    | [] -> None
    | hd :: _ -> Some hd
  in
  let min_child, terminals =
    match min_child, min_terminal with
    | None, None -> failwith "suffix_tree: empty node"
    | None, Some min_terminal -> min_terminal, terminals
    | Some min_child, None -> min_child, min_child :: terminals
    | Some min_child, Some min_terminal ->
      if Db.Entry.compare min_child min_terminal < 0
      then min_child, min_child :: terminals
      else min_terminal, terminals
  in
  assert (min_child == Seen.min_elt seen) ;
  assert (terminals <> []) ;
  let terminals_uid, terminals = export_terminals ~cache_term ~is_summary terminals in
  let children_uids = List.map (fun (chr, { uid; _ }) -> chr, uid) children in
  let key = node.start, node.len, terminals_uid, children_uids in
  try Hashtbl.find cache key with
  | Not_found ->
    let children =
      Array.of_list @@ List.map (fun (_, { t = child; _ }) -> child) children
    in
    let size = size_of_terminals terminals in
    let size =
      if is_summary
      then size
      else
        Array.fold_left
          (fun acc child -> acc + child.Db.String_automata.size)
          size
          children
    in
    let children = if Array.length children = 0 then None else Some children in
    let node =
      { Db.String_automata.start = node.start; len = node.len; size; terminals; children }
    in
    let result = { uid = Uid.make (); t = node; min = min_child; seen } in
    Hashtbl.add cache key result ;
    result

let export ~summarize { buffer; root = t } =
  let str = Buf.contents buffer in
  if String.length str = 0
  then { Db.String_automata.str; t = Db.String_automata.empty () }
  else begin
    let cache = Hashtbl.create 16 in
    let cache_term = Terminals_cache.create 16 in
    let { t; _ } = export ~cache ~cache_term ~summarize ~is_root:true t in
    { Db.String_automata.str; t }
  end
