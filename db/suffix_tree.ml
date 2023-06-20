module type SET = sig
  type t
  type elt

  val of_list : elt list -> t
  val is_empty : t -> bool
end

module Doc = struct
  type 'a t =
    { uid : 'a
    ; text : string
    }

  let length t = String.length t.text + 1

  type 'a v =
    | Terminal of 'a
    | Char of char

  let get t i =
    if i >= String.length t.text then Terminal t.uid else Char t.text.[i]

  let sub { text; _ } i = String.sub text i (String.length text - i)
end

module Buf = struct
  module Cache = Hashtbl.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  type t =
    { buffer : Buffer.t
    ; cache : int Cache.t
    }

  let make () = { buffer = Buffer.create 16; cache = Cache.create 16 }
  let contents t = Buffer.contents t.buffer
  let get t i = Buffer.nth t.buffer i

  let add { buffer; cache } substr =
    match Cache.find cache substr with
    | start -> start
    | exception Not_found ->
        let start = Buffer.length buffer in
        Buffer.add_string buffer substr ;
        let stop = Buffer.length buffer in
        assert (stop - start = String.length substr) ;
        for idx = 1 to String.length substr - 1 do
          Cache.add cache
            (String.sub substr idx (String.length substr - idx))
            (start + idx)
        done ;
        start
end

module Make (S : SET) = struct
  module Terminals = struct
    type t = S.elt list

    let empty = []
    let singleton x = [ x ]

    let add ~hint x xs =
      match hint with
      | Some (prev_xs, xxs) when prev_xs == xs -> xxs
      | _ -> x :: xs

    let hash = Hashtbl.hash
    let equal = List.equal ( == )

    let mem x = function
      | y :: _ -> x == y
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

  type writer =
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

  let make () = { root = make_root (); buffer = Buf.make () }

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
      else
        let i_chr, j_chr = i_str.[i], Buf.get j_str j in
        if i_chr <> j_chr then i else go_lcp (i + 1) (j + 1)
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
      else
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
                let len =
                  lcp doc.Doc.text i trie.buffer child.start child.len
                in
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
                let new_leaf =
                  make_leaf ~prev_leaf ~buffer:trie.buffer ~doc i
                in
                node.children <- Char_map.add chr new_leaf node.children ;
                let prev_leaf =
                  set_leaf ~debug:"1" ~prev_leaf
                    ~depth:(depth + Doc.length doc - i)
                    new_leaf
                in
                let prev_leaf =
                  match prev_leaf with
                  | None -> None
                  | Some (t, depth) -> Some (t, depth, Terminals.empty)
                in
                follow_suffix ~prev ~prev_leaf ~parent:node ~depth ~i
          end
    and add_leaf ~prev_leaf ~node ~child ~depth ~i ~len =
      match Doc.get doc i with
      | Terminal doc_uid ->
          if not (Terminals.mem doc_uid child.terminals)
          then begin
            let hint =
              Option.map
                (fun (t, _, prev_terminals) -> prev_terminals, t.terminals)
                prev_leaf
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
          let new_leaf =
            make_leaf ~prev_leaf ~buffer:trie.buffer ~doc (i + 1)
          in
          let prev_leaf =
            set_leaf ~debug:"3" ~prev_leaf
              ~depth:(depth + Doc.length doc - i)
              new_leaf
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

  module Automata = struct
    module Uid = struct
      let gen = ref 0

      let make () =
        let u = !gen in
        gen := u + 1 ;
        u
    end

    module Hterm = Hashtbl.Make (Terminals)

    module T = struct
      type node =
        { start : int
        ; len : int
        ; terminals : S.t
        ; children : node array
        }

      type t =
        { str : string
        ; t : node
        }

      let array_find ~str chr arr =
        let rec go i =
          if i >= Array.length arr
          then raise Not_found
          else
            let node = arr.(i) in
            if chr = str.[node.start - 1] then node else go (i + 1)
        in
        go 0

      let lcp i_str i j_str j j_len =
        let j_stop = j + j_len in
        let rec go_lcp i j =
          if i >= String.length i_str || j >= j_stop
          then i
          else
            let i_chr, j_chr = i_str.[i], j_str.[j] in
            if i_chr <> j_chr then i else go_lcp (i + 1) (j + 1)
        in
        let i' = go_lcp i j in
        i' - i

      let rec find ~str node pattern i =
        if i >= String.length pattern
        then node
        else
          let chr = pattern.[i] in
          let child = array_find ~str chr node.children in
          find_lcp ~str child pattern (i + 1)

      and find_lcp ~str child pattern i =
        let n = lcp pattern i str child.start child.len in
        if i + n = String.length pattern
        then { child with start = child.start + n }
        else if n = child.len
        then find ~str child pattern (i + n)
        else raise Not_found

      let find t pattern =
        let child = find ~str:t.str t.t pattern 0 in
        { str = t.str; t = child }

      let rec collapse acc t =
        let acc = if S.is_empty t.terminals then acc else t.terminals :: acc in
        Array.fold_left collapse acc t.children

      let collapse t = collapse [] t.t
    end

    let export_terminals ~cache_term ts =
      try Hterm.find cache_term ts
      with Not_found ->
        let result = Uid.make (), S.of_list ts in
        Hterm.add cache_term ts result ;
        result

    let rec export ~cache ~cache_term node =
      let terminals_uid, terminals =
        export_terminals ~cache_term node.terminals
      in
      let children =
        Char_map.bindings
        @@ Char_map.map (export ~cache ~cache_term) node.children
      in
      let children_uids = List.map (fun (chr, (uid, _)) -> chr, uid) children in
      let key = node.start, node.len, terminals_uid, children_uids in
      try Hashtbl.find cache key
      with Not_found ->
        let children =
          Array.of_list @@ List.map (fun (_, (_, child)) -> child) children
        in
        let node =
          { T.start = node.start; len = node.len; terminals; children }
        in
        let result = Uid.make (), node in
        Hashtbl.add cache key result ;
        result

    let clear ~str t =
      let cache = Hashtbl.create 16 in
      let cache_term = Hterm.create 16 in
      let _, t = export ~cache ~cache_term t in
      { T.str; t }
  end

  type reader = Automata.T.t

  let export t =
    let str = Buf.contents t.buffer in
    Automata.clear ~str t.root

  let find = Automata.T.find
  let to_sets = Automata.T.collapse
end
