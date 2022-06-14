open! Compat
open Types

type out = Source.t

module State = struct
  type t = {
    context : (out * Source.tag) Stack.t;
    mutable current : out;
    mutable ignore_all : int;
  }

  let create () = { context = Stack.create (); current = []; ignore_all = 0 }

  let push state elt =
    if state.ignore_all = 0 then state.current <- elt :: state.current

  let push_ignore state = state.ignore_all <- state.ignore_all + 1

  let pop_ignore state =
    state.ignore_all <-
      (if state.ignore_all > 0 then state.ignore_all - 1 else 0)

  let enter state tag =
    if state.ignore_all = 0 then (
      let previous_elt = state.current in
      Stack.push (previous_elt, tag) state.context;
      state.current <- [];
      ())

  let leave state =
    if state.ignore_all = 0 then (
      let current_elt = List.rev state.current in
      let previous_elt, tag = Stack.pop state.context in
      state.current <- Tag (tag, current_elt) :: previous_elt;
      ())

  let rec flush state =
    if Stack.is_empty state.context then List.rev state.current
    else (
      leave state;
      flush state)
end

(** Modern implementation using semantic tags, Only for 4.08+ *)

(*
module Tag = struct

  type Format.stag +=
    | Elt of Inline.t
    | Tag of Source.tag
    | Ignore

  let setup_tags formatter state0 =
    let stag_functions =
      let mark_open_stag = function
        | Elt elt -> State.push state0 (Elt elt); ""
        | Tag tag -> State.enter state0 tag; ""
        | Format.String_tag "" -> State.enter state0 None; ""
        | Format.String_tag tag -> State.enter state0 (Some tag); ""
        | Ignore -> State.push_ignore state0; ""
        | _ -> ""
      and mark_close_stag = function
        | Elt _ -> ""
        | Tag _
        | Format.String_tag _ -> State.leave state0; ""
        | Ignore -> State.pop_ignore state0; ""
        | _ -> ""
      in {Format.
        print_open_stag = (fun _ -> ());
        print_close_stag = (fun _ -> ());
        mark_open_stag; mark_close_stag;
      }
    in
    Format.pp_set_tags formatter true;
    Format.pp_set_formatter_stag_functions formatter stag_functions;
    ()

  let elt ppf elt =
    Format.pp_open_stag ppf (Elt elt);
    Format.pp_print_as ppf (Utils.compute_length_inline elt) "";
    Format.pp_close_stag ppf ()

  let ignore ppf txt =
    Format.pp_open_stag ppf Ignore;
    Format.fprintf ppf "%t" txt;
    Format.pp_close_stag ppf ()
end
*)

(** Ugly terrible implementation of Format Semantic tags for OCaml < 4.08.
    Please get rid of it as soon as possible. *)
module Tag = struct
  let setup_tags formatter state0 =
    let tag_functions =
      let get_tag s =
        let prefix_tag = "tag:" and prefix_ignore = "ignore-tag" in
        let l = String.length prefix_tag in
        if String.length s > l && String.sub s 0 l = prefix_tag then
          let elt : Inline.t = Marshal.from_string s l in
          `Elt elt
        else if s = prefix_ignore then `Ignore
        else `String s
      in
      let mark_open_tag s =
        match get_tag s with
        | `Ignore ->
            State.push_ignore state0;
            ""
        | `Elt elt ->
            State.push state0 (Elt elt);
            ""
        | `String "" ->
            State.enter state0 None;
            ""
        | `String tag ->
            State.enter state0 (Some tag);
            ""
      and mark_close_tag s =
        match get_tag s with
        | `Ignore ->
            State.pop_ignore state0;
            ""
        | `Elt _ -> ""
        | `String _ ->
            State.leave state0;
            ""
      in
      {
        Format.print_open_tag = (fun _ -> ());
        print_close_tag = (fun _ -> ());
        mark_open_tag;
        mark_close_tag;
      }
    in
    Format.pp_set_tags formatter true;
    Format.pp_set_formatter_tag_functions formatter tag_functions;
    ()

  let elt ppf (elt : Inline.t) =
    Format.fprintf ppf "@{<tag:%s>%t@}" (Marshal.to_string elt []) (fun fmt ->
        Format.pp_print_as fmt (Utils.compute_length_inline elt) "")

  let ignore ppf txt = Format.fprintf ppf "@{<ignore-tag>%t@}" txt
end
[@@alert "-deprecated--deprecated"]

type t = Format.formatter -> unit

let make () =
  let open Inline in
  let state0 = State.create () in
  let push elt = State.push state0 (Elt elt) in
  let push_text s = if state0.ignore_all = 0 then push [ inline @@ Text s ] in

  let formatter =
    let out_string s i j = push_text (String.sub s i j) in
    let out_flush () = () in
    Format.make_formatter out_string out_flush
  in

  (* out_functions is only available in OCaml>=4.06 *)
  (* let out_functions = {Format.
   *   out_string = (fun i j s -> push_text @@ String.sub i j s );
   *   out_flush = (fun () -> ());
   *   out_newline = (fun () -> push [inline @@ Linebreak]);
   *   out_spaces = (fun n -> push_text (String.make n ' '));
   *   out_indent = (fun n -> push_text (String.make n ' '))
   * }
   * in
   * let formatter = Format.formatter_of_out_functions out_functions in *)
  Tag.setup_tags formatter state0;
  Format.pp_set_margin formatter 80;
  ( (fun () ->
      Format.pp_print_flush formatter ();
      State.flush state0),
    formatter )

let spf fmt =
  let flush, ppf = make () in
  Format.kfprintf (fun _ -> flush ()) ppf fmt

let pf = Format.fprintf

let elt t ppf = Tag.elt ppf t

let entity e ppf = elt [ inline @@ Inline.Entity e ] ppf

let ignore t ppf = Tag.ignore ppf t

let ( ++ ) f g ppf =
  f ppf;
  g ppf

let span ?(attr = "") f ppf = pf ppf "@{<%s>%t@}" attr f

let txt s ppf = Format.pp_print_string ppf s

let noop (_ : Format.formatter) = ()

let break i j ppf = Format.pp_print_break ppf i j

let cut = break 0 0

let sp = break 1 0

let rec list ?sep ~f = function
  | [] -> noop
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = list ?sep ~f xs in
      match sep with None -> hd ++ tl | Some sep -> hd ++ sep ++ tl)

let box_hv t ppf = pf ppf "@[<hv 2>%t@]" t

let box_hv_no_indent t ppf = pf ppf "@[<hv 0>%t@]" t

let render f = spf "@[<hv 2>%t@]" (span f)

let code ?attr f = [ inline ?attr @@ Inline.Source (render f) ]

let documentedSrc f = [ DocumentedSrc.Code (render f) ]

let codeblock ?attr f =
  [ block ?attr @@ Block.Source (Comment.default_lang_tag, render f) ]

let keyword keyword ppf = pf ppf "@{<keyword>%s@}" keyword

module Infix = struct
  let ( ++ ) = ( ++ )
end
