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



module Paths = Model.Paths



let empty_body = []

let empty : Model.Comment.docs = empty_body



let read_payload =
  let open Parsetree in function
  | PStr[{ pstr_desc =
             Pstr_eval({ pexp_desc =
                           Pexp_constant( Parsetree.Pconst_string(str, _));
                         pexp_loc = loc;
                         _
                       }, _)
         ; _ }] -> Some(str, loc)
  | _ -> None

let read_attributes parent _id attrs =
  let ocaml_deprecated = ref None in
  let rec loop first nb_deprecated acc
      : _ -> (Model.Comment.docs, Model.Error.t) result =
    function
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}, payload) :: rest -> begin
        match read_payload payload with
        | Some (str, loc) -> begin
            let _start_pos = loc.Location.loc_start in
            let parsed =
              Parser_.parse ~containing_definition:parent ~comment_text:str in
            match parsed with
            | Ok comment -> begin
                loop false 0 (acc @ comment) rest
              end
            | Error err -> Error err
          end
        | None -> (* TODO *) assert false
      end
    | _ :: rest -> loop first nb_deprecated acc rest
    | [] -> begin
        match nb_deprecated, !ocaml_deprecated with
        | 0, Some _tag -> Ok acc
        | _, _ -> Ok acc
      end
  in
    loop true 0 empty_body attrs
  |> Model.Error.get_value_or_convey_error_by_exception

let read_string parent loc str : Model.Comment.docs_or_stop =
  let _start_pos = loc.Location.loc_start in
  let doc : Model.Comment.docs =
    Parser_.parse ~containing_definition:parent ~comment_text:str
    |> Model.Error.get_value_or_convey_error_by_exception
  in
  `Docs doc

let read_comment parent
    : Parsetree.attribute -> Model.Comment.docs_or_stop option =

  function
  | ({Location.txt =
        ("text" | "ocaml.text"); loc = _loc}, payload) -> begin
      match read_payload payload with
      | Some ("/*", _loc) -> Some `Stop
      | Some (str, loc) -> Some (read_string parent loc str)
      | None ->
        (* TODO *)
        assert false
          (* let doc : Model.Comment.t =
            Error (invalid_attribute_error parent loc) in
            Some (Documentation doc) *)
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
