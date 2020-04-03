open Types

type out = Source.t

type Format.stag +=
  | Elt of Inline.t
  | Tag of Source.tag

module State = struct

  type t = {
    context : (out * Source.tag) Stack.t ;
    mutable current : out ;
  }

  let create () = { context = Stack.create () ; current = [] }

  let push state elt =
    state.current <- elt :: state.current
  
  let enter state tag =
    let previous_elt = state.current in
    Stack.push (previous_elt, tag) state.context;
    state.current <- [];
    ()

  let leave state =
    let current_elt = List.rev state.current in
    let previous_elt, tag = Stack.pop state.context in
    state.current <- Tag (tag, current_elt) :: previous_elt;
    ()

  let rec flush state =
    if Stack.is_empty state.context then
      List.rev state.current
    else
      (leave state; flush state)

end
  
let make () =
  let open Inline in
  let state0 = State.create () in
  let push elt = State.push state0 (Elt elt) in
  let push_text s = push [inline @@ Text s] in
  let out_functions = {Format.
    out_string = (fun s i j -> push_text (String.sub s i j));
    out_flush = (fun () -> ());
    out_newline = (fun () -> push [inline @@ Linebreak]);
    out_spaces = (fun n -> push_text (String.make n ' '));
    out_indent = (fun n -> push_text (String.make n ' '))
  }
  and stag_functions =
    let mark_open_stag = function
      | Elt elt -> push elt; ""
      | Tag tag -> State.enter state0 tag; ""
      | Format.String_tag "" -> State.enter state0 None; ""
      | Format.String_tag tag -> State.enter state0 (Some tag); ""
      | _ -> ""
    and mark_close_stag = function
      | Elt _ -> ""
      | Tag _ 
      | Format.String_tag _ -> State.leave state0; ""
      | _ -> ""
    in {Format.
      print_open_stag = (fun _ -> ());
      print_close_stag = (fun _ -> ());
      mark_open_stag; mark_close_stag;
    }
  in
  let formatter = Format.formatter_of_out_functions out_functions in
  Format.pp_set_tags formatter true;
  Format.pp_set_formatter_stag_functions formatter stag_functions;
  (fun () -> Format.pp_print_flush formatter (); State.flush state0),
  formatter

let elt ppf e =
  Format.pp_open_stag ppf (Elt e);
  Format.pp_close_stag ppf ()

let entity e ppf = elt ppf [inline @@ Inline.Entity e]

let spf fmt =
  let flush, ppf = make () in
  Format.kfprintf (fun _ -> flush ()) ppf fmt

let pf = Format.fprintf

(** Transitory hackish API *)

let (++) f g ppf = f ppf; g ppf
let span f ppf =
  Format.pp_open_stag ppf (Tag None);
  f ppf ;
  Format.pp_close_stag ppf ()
let df = Format.dprintf
let txt s ppf = Format.pp_print_string ppf s
let noop (_ : Format.formatter) = ()

let (!) (pp : _ Fmt.t) x ppf = pp ppf x

let rec list ?sep ~f = function
  | [] -> noop
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list ?sep ~f xs in
    match sep with
    | None -> hd ++ tl
    | Some sep -> hd ++ sep ++ tl

let render f = spf "%t" f
let code ?attr f =
  [inline ?attr @@ Inline.Source (render f)]
let documentedSrc ?(attr=[]) f =
  [DocumentedSrc.Code { attr ; code = render f }]
let codeblock ?attr f =
  [block ?attr @@ Block.Source (render f)]

let keyword keyword ppf =
  Format.pp_open_stag ppf (Tag (Some "keyword"));
  Format.pp_print_string ppf keyword ;
  Format.pp_close_stag ppf ()

module Infix = struct
  let (!) = (!)
  let (++) = (++)
end
