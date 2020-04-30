open StdLabels

let rec unit (t : Odoc_model.Lang.Compilation_unit.t) =
  let url = Url.Path.from_identifier t.id in
  let rest =
    match t.content with
    | Module sign -> signature sign
    | Pack _ -> []
  in
  url :: rest

and signature (t : Odoc_model.Lang.Signature.t) =
  let rec add_items ~don't acc = function
    | [] -> List.concat (List.rev acc)
    | i :: is ->
      match i with
      | Odoc_model.Lang.Signature.Comment `Stop ->
        add_items ~don't:(not don't) acc is
      | _ when don't ->
        add_items ~don't acc is
      | Module (_, md) ->
        add_items ~don't (module_ md :: acc) is
      | ModuleType mty ->
        add_items ~don't (module_type mty :: acc) is
      | Include incl ->
        add_items ~don't (include_ incl :: acc) is
      | ModuleSubstitution _
      | TypeSubstitution _
      | Type _
      | TypExt _
      | Exception _
      | Value _
      | External _
      | Class _
      | ClassType _
      | Comment (`Docs _) -> add_items ~don't acc is
  in
  add_items ~don't:false [] t

and functor_argument arg =
  let open Odoc_model.Lang.FunctorParameter in
  match arg.expansion with
  | None -> []
  | Some expansion ->
    let url = Url.Path.from_identifier arg.id in
    let subpages = module_expansion expansion in
    url :: subpages

and module_expansion (t : Odoc_model.Lang.Module.expansion) =
  match t with
  | AlreadyASig -> [] (* FIXME. *)
  | Signature sg -> signature sg
  | Functor (args, sg) ->
    let subpages = signature sg in
    List.fold_left args ~init:subpages ~f:(fun subpages arg ->
      match arg with
      | Odoc_model.Lang.FunctorParameter.Unit -> subpages
      | Named arg ->
        let arg_subpages = functor_argument arg in
        arg_subpages @ subpages
    )

and module_ (t : Odoc_model.Lang.Module.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    let expansion =
      match expansion with
      | AlreadyASig ->
        begin match t.type_ with
        | ModuleType (Odoc_model.Lang.ModuleType.Signature sg) ->
          Odoc_model.Lang.Module.Signature sg
        | _ -> assert false
        end
      | e -> e
    in
    let url = Url.Path.from_identifier t.id in
    let subpages = module_expansion expansion in
    url :: subpages

and module_type (t : Odoc_model.Lang.ModuleType.t) =
  match t.expansion with
  | None -> []
  | Some expansion ->
    let expansion = match expansion with
      | AlreadyASig ->
        begin match t.expr with
        | Some (Signature sg) -> Odoc_model.Lang.Module.Signature sg
        | _ -> assert false
        end
      | e -> e
    in
    let url = Url.Path.from_identifier t.id in
    let subpages = module_expansion expansion in
    url :: subpages

and include_ (t : Odoc_model.Lang.Include.t) =
  signature t.expansion.content

and page (t : Odoc_model.Lang.Page.t) =
  [Url.Path.from_identifier t.name]

