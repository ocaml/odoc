(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Octavius.Types
open DocOckTypes.Documentation

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let read_style = function
  | SK_bold -> Bold
  | SK_italic -> Italic
  | SK_emphasize -> Emphasize
  | SK_center -> Center
  | SK_left -> Left
  | SK_right -> Right
  | SK_superscript -> Superscript
  | SK_subscript -> Subscript
  | SK_custom s -> Custom s

exception InvalidReference of string

let read_longident s =
  let open DocOckPaths.Reference in
  let rec loop : 'k. string -> int -> ('a, [< kind] as 'k) t option =
    fun s pos ->
      try
        let idx = String.rindex_from s pos '.' in
        let name = String.sub s (idx + 1) (pos - idx) in
        if String.length name = 0 then None
        else
          match loop s (idx - 1) with
          | None -> None
          | Some parent -> Some (Dot(parent, name))
      with Not_found ->
        let name = String.sub s 0 (pos + 1) in
        if String.length name = 0 then None
        else Some (Root name)
  in
    match loop s (String.length s - 1) with
    | None -> raise (InvalidReference s)
    | Some r -> r

let read_reference rk s =
  match rk with
  | RK_module -> Module (read_longident s)
  | RK_module_type -> ModuleType (read_longident s)
  | RK_type -> Type (read_longident s)
  | RK_exception -> Exception (read_longident s)
  | RK_recfield -> Field (read_longident s)
  | RK_const -> Constructor (read_longident s)
  | RK_value -> Value (read_longident s)
  | RK_class -> Class (read_longident s)
  | RK_class_type -> ClassType (read_longident s)
  | RK_attribute -> InstanceVariable (read_longident s)
  | RK_method -> Method (read_longident s)
  | RK_element -> Element (read_longident s)
  | RK_section -> Section (read_longident s)
  | RK_link -> Link s
  | RK_custom k -> Custom(k, s)

let read_special_reference = function
  | SRK_module_list mds ->
      Modules (List.map read_longident mds)
  | SRK_index_list -> Index

let rec read_text_element parent
  : Octavius.Types.text_element -> 'a text_element =
  function
  | Raw s -> Raw s
  | Code s -> Code s
  | PreCode s -> PreCode s
  | Verbatim s -> Verbatim s
  | Style(sk, txt) -> Style(read_style sk, read_text parent txt)
  | List l -> List (List.map (read_text parent) l)
  | Enum l -> Enum (List.map (read_text parent) l)
  | Newline -> Newline
  | Title(i, l, txt) -> begin
      let txt = read_text parent txt in
        match l with
        | None -> Title(i, None, txt)
        | Some name ->
            let id = DocOckPaths.Identifier.Label(parent, name) in
              Title(i, Some id, txt)
    end
  | Ref(rk, s, txt) ->
      Reference(read_reference rk s, opt_map (read_text parent) txt)
  | Special_ref srk -> Special (read_special_reference srk)
  | Target (target, code) -> Target (target, code)

and read_text parent txt = List.map (read_text_element parent) txt

let read_see = function
  | See_url s -> Url s
  | See_file s -> File s
  | See_doc s -> Doc s


let read_tag parent : Octavius.Types.tag -> 'a tag = function
  | Author s -> Author s
  | Version v -> Version v
  | See (r, t) -> See (read_see r, read_text parent t)
  | Since s -> Since s
  | Before (s, t) -> Before (s, read_text parent t)
  | Deprecated t -> Deprecated (read_text parent t)
  | Param (s, t) -> Param (s, read_text parent t)
  | Raised_exception (s, t) -> Raise (s, read_text parent t)
  | Return_value t -> Return (read_text parent t)
  | Inline -> Inline
  | Custom (s, t) -> Tag (s, read_text parent t)

let empty_body = { text = []; tags = []; }

let empty = Ok empty_body

let read_offset err =
  let open Octavius.Errors in
  let loc = err.location in
  let start =
    { Error.Position.
        line = loc.start.line;
        column = loc.start.column; }
  in
  let finish =
    { Error.Position.
        line = loc.finish.line;
        column = loc.finish.column; }
  in
    { Error.Offset.start; finish; }

let read_position offset pos =
  let open Lexing in
  let open Error in
  let off_line = offset.Position.line in
  let off_column = offset.Position.column in
  let line = pos.pos_lnum + off_line - 1 in
  let column =
    if off_line = 1 then
      (pos.pos_cnum - pos.pos_bol) + off_column + 3
    else off_column
  in
  { Position.line; column }

let read_location offset pos =
  let open Lexing in
  let open Error in
  if pos.pos_cnum >= 0 then begin
    let filename = pos.pos_fname in
    let start = read_position offset.Offset.start pos in
    let finish = read_position offset.Offset.finish pos in
    Some { Location.filename; start; finish }
  end else None

let read_error origin err pos =
  let open Error in
  let origin = DocOckPaths.Identifier.any origin in
  let offset = read_offset err in
  let location = read_location offset pos in
  let message = Octavius.Errors.message err.Octavius.Errors.error in
    { origin; offset; location; message }

let attribute_location loc =
  let open Lexing in
  let open Location in
  let open Error in
  let start = loc.loc_start in
  let finish = loc.loc_end in
  if start.pos_cnum >= 0 && finish.pos_cnum >= 0 then begin
    let filename = start.pos_fname in
    let read_pos pos =
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      { Position.line; column }
    in
    let start = read_pos start in
    let finish = read_pos finish in
    Some { Location.filename; start; finish }
  end else None

let invalid_attribute_error origin loc =
  let open Error in
  let origin = DocOckPaths.Identifier.any origin in
  let offset =
    let zero_pos = { Position.line = 0; column = 0 } in
    { Offset.start = zero_pos; finish = zero_pos }
  in
  let location = attribute_location loc in
  let message = "Invalid documentation attribute" in
    { origin; offset; location; message }

let invalid_reference_error origin loc s =
  let open Error in
  let origin = DocOckPaths.Identifier.any origin in
  (* TODO get an actual offset *)
  let dummy = { Position.line = 0; column = 0} in
  let offset = { Offset.start = dummy; finish = dummy } in
  (* TODO get an accurate location *)
  let location = attribute_location loc in
  let message = "Invalid reference: \"" ^ s ^ "\"" in
    {origin; offset; location; message}


let read_attributes parent id attrs =
  let rec loop first acc : _ -> 'a t = function
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc}, payload) :: rest -> begin
        match DocOckPayload.read payload with
        | Some (str, loc) -> begin
            let start_pos = loc.Location.loc_start in
            let lexbuf = Lexing.from_string str in
            match Octavius.parse lexbuf with
            | Octavius.Ok (text, tags) -> begin
                try
                  let text = read_text parent text in
                  let text = if first then text else Newline :: text in
                  let tags = List.map (read_tag parent) tags in
                  let acc =
                    { text = acc.text @ text;
                      tags = acc.tags @ tags; }
                  in
                  loop false acc rest
                with InvalidReference s ->
                  Error (invalid_reference_error id loc s)
              end
            | Octavius.Error err -> Error (read_error id err start_pos)
          end
        | None -> Error (invalid_attribute_error id loc)
      end
    | _ :: rest -> loop first acc rest
    | [] -> Ok acc
  in
    loop true empty_body attrs

let read_comment parent : Parsetree.attribute -> 'a comment option =
  function
  | ({Location.txt =
        ("text" | "ocaml.text"); loc}, payload) -> begin
      match DocOckPayload.read payload with
      | Some ("/*", loc) -> Some Stop
      | Some (str, loc) ->
          let lexbuf = Lexing.from_string str in
          let start_pos = loc.Location.loc_start in
          let doc =
            match Octavius.parse lexbuf with
            | Octavius.Ok(text, tags) -> begin
                try
                  let text = read_text parent text in
                  let tags = List.map (read_tag parent) tags in
                  Ok {text; tags}
                with InvalidReference s ->
                  Error (invalid_reference_error parent loc s)
              end
            | Octavius.Error err -> Error (read_error parent err start_pos)
          in
          Some (Documentation doc)
      | None ->
          let doc = Error (invalid_attribute_error parent loc) in
            Some (Documentation doc)
    end
  | _ -> None

let read_comments parent attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
         match read_comment parent attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms
