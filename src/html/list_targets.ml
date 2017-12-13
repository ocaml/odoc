(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

open StdLabels
open Model.Paths

let functor_arg_pos { Model.Lang.FunctorArgument.id ; _ } =
  match id with
  | Identifier.Argument (_, nb, _) -> nb
  | _ ->
    failwith "TODO"
    (* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)

let rec unit ~package (t : Model.Lang.Compilation_unit.t) : string list =
  let name = Printf.sprintf "%s/%s" package (Identifier.name t.id) in
  let rest =
    match t.content with
    | Module sign -> signature ~prefix:name sign
    | Pack _ -> []
  in
  name :: rest

and signature ~prefix (t : Model.Lang.Signature.t) =
  List.concat (
    List.map t ~f:(function
      | Model.Lang.Signature.Module md -> module_ ~prefix md
      | ModuleType mty -> module_type ~prefix mty
      | Type _ -> []
      | TypExt _ -> []
      | Exception _ -> []
      | Value _ -> []
      | External _ -> []
      | Class _ -> []
      | ClassType _ -> []
      | Include incl -> include_ ~prefix incl
      | Comment (`Docs _) -> []
      | Comment `Stop -> []
    )
  )

and functor_argument ~prefix arg =
  let open Model.Lang.FunctorArgument in
  match arg.expansion with
  | None -> []
  | Some expansion ->
    let name = Identifier.name arg.id in
    let nb = functor_arg_pos arg in
    (* FIXME: reuse [Url] somehow. *)
    let page = Printf.sprintf "%s/argument-%d-%s" prefix nb name in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages

and module_expansion ~prefix (t : Model.Lang.Module.expansion) =
  match t with
  | AlreadyASig -> [] (* FIXME. *)
  | Signature sg -> signature ~prefix sg
  | Functor (args, sg) ->
    let subpages = signature ~prefix sg in
    List.fold_left args ~init:subpages ~f:(fun subpages arg ->
      match arg with
      | None -> subpages
      | Some arg ->
        let arg_subpages = functor_argument ~prefix arg in
        arg_subpages @ subpages
    )

and module_ ~prefix (t : Model.Lang.Module.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    let page = Printf.sprintf "%s/%s" prefix (Identifier.name t.id) in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages

and module_type ~prefix (t : Model.Lang.ModuleType.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    (* FIXME: reuse [Url] somehow. *)
    let page = Printf.sprintf "%s/module-type-%s" prefix (Identifier.name t.id) in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages


and include_ ~prefix (t : Model.Lang.Include.t) =
  signature ~prefix t.expansion.content
