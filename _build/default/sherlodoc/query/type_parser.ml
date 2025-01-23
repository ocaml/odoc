
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WORD of (
# 17 "sherlodoc/query/type_parser.mly"
      (string)
# 15 "sherlodoc/query/type_parser.ml"
  )
    | STAR
    | POLY of (
# 18 "sherlodoc/query/type_parser.mly"
      (string)
# 21 "sherlodoc/query/type_parser.ml"
  )
    | PARENS_OPEN
    | PARENS_CLOSE
    | EOF
    | COMMA
    | ARROW
    | ANY
  
end

include MenhirBasics

# 4 "sherlodoc/query/type_parser.mly"
  
  module Printf = struct
    (* Without the following placeholder, [menhir_fail] induces
       a large dependency to [camlinternalFormat] in the js bundle. *)
    let eprintf _ = ()
  end

  open Db.Typexpr

# 44 "sherlodoc/query/type_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState03 : (('s, _menhir_box_main) _menhir_cell1_PARENS_OPEN, _menhir_box_main) _menhir_state
    (** State 03.
        Stack shape : PARENS_OPEN.
        Start symbol: main. *)

  | MenhirState06 : (('s, _menhir_box_main) _menhir_cell1_typs _menhir_cell0_WORD, _menhir_box_main) _menhir_state
    (** State 06.
        Stack shape : typs WORD.
        Start symbol: main. *)

  | MenhirState07 : (('s, _menhir_box_main) _menhir_cell1_WORD, _menhir_box_main) _menhir_state
    (** State 07.
        Stack shape : WORD.
        Start symbol: main. *)

  | MenhirState11 : (('s, _menhir_box_main) _menhir_cell1_typ2, _menhir_box_main) _menhir_state
    (** State 11.
        Stack shape : typ2.
        Start symbol: main. *)

  | MenhirState13 : (('s, _menhir_box_main) _menhir_cell1_typ1, _menhir_box_main) _menhir_state
    (** State 13.
        Stack shape : typ1.
        Start symbol: main. *)

  | MenhirState19 : (('s, _menhir_box_main) _menhir_cell1_typ, _menhir_box_main) _menhir_state
    (** State 19.
        Stack shape : typ.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_typ = 
  | MenhirCell1_typ of 's * ('s, 'r) _menhir_state * (Db.Typexpr.t)

and ('s, 'r) _menhir_cell1_typ1 = 
  | MenhirCell1_typ1 of 's * ('s, 'r) _menhir_state * (Db.Typexpr.t)

and ('s, 'r) _menhir_cell1_typ2 = 
  | MenhirCell1_typ2 of 's * ('s, 'r) _menhir_state * (Db.Typexpr.t)

and ('s, 'r) _menhir_cell1_typs = 
  | MenhirCell1_typs of 's * ('s, 'r) _menhir_state * (Db.Typexpr.t list)

and ('s, 'r) _menhir_cell1_PARENS_OPEN = 
  | MenhirCell1_PARENS_OPEN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WORD = 
  | MenhirCell1_WORD of 's * ('s, 'r) _menhir_state * (
# 17 "sherlodoc/query/type_parser.mly"
      (string)
# 102 "sherlodoc/query/type_parser.ml"
)

and 's _menhir_cell0_WORD = 
  | MenhirCell0_WORD of 's * (
# 17 "sherlodoc/query/type_parser.mly"
      (string)
# 109 "sherlodoc/query/type_parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (Db.Typexpr.t) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 120 "sherlodoc/query/type_parser.ml"
     : (string list))

let _menhir_action_02 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 128 "sherlodoc/query/type_parser.ml"
     : (string list))

let _menhir_action_03 =
  fun x ->
    (
# 58 "sherlodoc/query/type_parser.mly"
           ( [x] )
# 136 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_action_04 =
  fun x xs ->
    (
# 59 "sherlodoc/query/type_parser.mly"
                                               ( x::xs )
# 144 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_action_05 =
  fun x ->
    (
# 58 "sherlodoc/query/type_parser.mly"
           ( [x] )
# 152 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_action_06 =
  fun x xs ->
    (
# 59 "sherlodoc/query/type_parser.mly"
                                               ( x::xs )
# 160 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_action_07 =
  fun t ->
    (
# 26 "sherlodoc/query/type_parser.mly"
              ( t )
# 168 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_08 =
  fun t ->
    (
# 30 "sherlodoc/query/type_parser.mly"
           ( t )
# 176 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_09 =
  fun a b ->
    (
# 31 "sherlodoc/query/type_parser.mly"
                       ( Arrow (a, b) )
# 184 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_10 =
  fun () ->
    (
# 47 "sherlodoc/query/type_parser.mly"
        ( Any )
# 192 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_11 =
  fun w ->
    (
# 48 "sherlodoc/query/type_parser.mly"
           ( Poly w )
# 200 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_12 =
  fun w ->
    (
# 49 "sherlodoc/query/type_parser.mly"
           ( Constr (w, []) )
# 208 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_13 =
  fun () ->
    (
# 39 "sherlodoc/query/type_parser.mly"
    ( Any )
# 216 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_14 =
  fun ts ->
    (
# 40 "sherlodoc/query/type_parser.mly"
            ( tuple ts )
# 224 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_15 =
  fun ts w ws ->
    (
# 41 "sherlodoc/query/type_parser.mly"
                                 (
      List.fold_left (fun acc w -> Constr (w, [acc])) (Constr (w, ts)) ws
    )
# 234 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_16 =
  fun xs ->
    (
# 35 "sherlodoc/query/type_parser.mly"
                         ( tuple xs )
# 242 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t))

let _menhir_action_17 =
  fun t ->
    (
# 53 "sherlodoc/query/type_parser.mly"
           ( [t] )
# 250 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_action_18 =
  fun ts ->
    (
# 54 "sherlodoc/query/type_parser.mly"
                                                  ( ts )
# 258 "sherlodoc/query/type_parser.ml"
     : (Db.Typexpr.t list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ANY ->
        "ANY"
    | ARROW ->
        "ARROW"
    | COMMA ->
        "COMMA"
    | EOF ->
        "EOF"
    | PARENS_CLOSE ->
        "PARENS_CLOSE"
    | PARENS_OPEN ->
        "PARENS_OPEN"
    | POLY _ ->
        "POLY"
    | STAR ->
        "STAR"
    | WORD _ ->
        "WORD"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_23 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let t = _v in
          let _v = _menhir_action_07 t in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let w = _v in
      let _v = _menhir_action_12 w in
      _menhir_goto_typ0 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typ0 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let t = _v in
      let _v = _menhir_action_17 t in
      _menhir_goto_typs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typs : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WORD _v_0 ->
          let _menhir_stack = MenhirCell1_typs (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell0_WORD (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WORD _v_1 ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState06
          | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
              let _v_2 = _menhir_action_01 () in
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
          | _ ->
              _eRR ())
      | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
          let ts = _v in
          let _v = _menhir_action_14 ts in
          _menhir_goto_typ1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_WORD (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WORD _v_0 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState07
      | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
          let _v_1 = _menhir_action_01 () in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_WORD -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_WORD (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_02 x xs in
      _menhir_goto_list_WORD_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_WORD_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState06 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typs _menhir_cell0_WORD -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_WORD (_menhir_stack, w) = _menhir_stack in
      let MenhirCell1_typs (_menhir_stack, _menhir_s, ts) = _menhir_stack in
      let ws = _v in
      let _v = _menhir_action_15 ts w ws in
      _menhir_goto_typ1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typ1 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_typ1 (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState13 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WORD _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | POLY _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | PARENS_OPEN ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ANY ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
              _menhir_reduce_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok)
      | ARROW | COMMA | EOF | PARENS_CLOSE ->
          let x = _v in
          let _v = _menhir_action_05 x in
          _menhir_goto_list1_typ1_STAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let w = _v in
      let _v = _menhir_action_11 w in
      _menhir_goto_typ0 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PARENS_OPEN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WORD _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | POLY _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | PARENS_OPEN ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ANY ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARROW | COMMA | PARENS_CLOSE | STAR ->
          _menhir_reduce_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_10 () in
      _menhir_goto_typ0 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_reduce_13 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_13 () in
      _menhir_goto_typ1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list1_typ1_STAR_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let xs = _v in
      let _v = _menhir_action_16 xs in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_stack = MenhirCell1_typ2 (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState11 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WORD _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | POLY _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | PARENS_OPEN ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ANY ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
              _menhir_reduce_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok)
      | COMMA | EOF | PARENS_CLOSE ->
          let t = _v in
          let _v = _menhir_action_08 t in
          _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_typ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_23 _menhir_stack _v _tok
      | MenhirState19 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState19 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WORD _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | POLY _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | PARENS_OPEN ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ANY ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARROW | COMMA | PARENS_CLOSE | STAR ->
              _menhir_reduce_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
          | _ ->
              _eRR ())
      | PARENS_CLOSE ->
          let x = _v in
          let _v = _menhir_action_03 x in
          _menhir_goto_list1_typ_COMMA_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_list1_typ_COMMA_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState03 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PARENS_OPEN -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_PARENS_OPEN (_menhir_stack, _menhir_s) = _menhir_stack in
      let ts = _v in
      let _v = _menhir_action_18 ts in
      _menhir_goto_typs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_typ (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_04 x xs in
      _menhir_goto_list1_typ_COMMA_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typ2 -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ2 (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_09 a b in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typ1 -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ1 (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_06 x xs in
      _menhir_goto_list1_typ1_STAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WORD _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | POLY _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | PARENS_OPEN ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ANY ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARROW | EOF | STAR ->
          _menhir_reduce_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
