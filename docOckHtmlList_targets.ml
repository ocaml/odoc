open DocOck
open Types
open Paths

open Html5.M

let functor_arg_pos { Types.FunctorArgument.id ; _ } =
  match id with
  | Identifier.Argument (_, nb, _) -> nb
  | _ ->
    let id = string_of_sexp @@ Identifier.sexp_of_t (fun _ -> Atom "") id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id)

let rec unit ~package:_ (t : _ Types.Unit.t) : string list =
  let name = Identifier.name t.id in
  let rest =
    match t.content with
    | Module sign -> signature ~prefix:name sign
    | Pack   pack -> []
  in
  name :: rest

and signature ~prefix (t : _ Types.Signature.t) =
  List.concat (
    List.map t ~f:(function
      | Types.Signature.Module md -> module_ ~prefix md
      | ModuleType mty -> module_type ~prefix mty
      | Type td -> []
      | TypExt te -> []
      | Exception e -> []
      | Value v -> []
      | External e -> []
      | Class c -> []
      | ClassType cty -> []
      | Include incl -> include_ ~prefix incl
      | Comment (Documentation doc) -> []
      | Comment Stop -> []
    )
  )

and functor_argument ~prefix arg =
  let open Types.FunctorArgument in
  match arg.expansion with
  | None -> []
  | Some expansion ->
    let name = Identifier.name arg.id in
    let nb = functor_arg_pos arg in
    let page = Printf.sprintf "%s/%s.%d.moda" prefix name nb in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages

and module_expansion ~prefix (t : _ Types.Module.expansion) =
  match t with
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

and module_ ~prefix (t : _ Types.Module.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    let page = Printf.sprintf "%s/%s.mod" prefix (Identifier.name t.id) in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages

and module_type ~prefix (t : _ Types.ModuleType.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    let page = Printf.sprintf "%s/%s.modt" prefix (Identifier.name t.id) in
    let subpages = module_expansion ~prefix:page expansion in
    page :: subpages


and include_ ~prefix (t : _ Types.Include.t) =
  signature ~prefix t.expansion.content
