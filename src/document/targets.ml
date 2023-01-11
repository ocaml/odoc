open StdLabels

let rec unit (t : Odoc_model.Lang.Compilation_unit.t) =
  let url = Url.Path.from_identifier t.id in
  let rest =
    match t.content with Module sign -> signature sign | Pack _ -> []
  in
  url :: rest

and signature (t : Odoc_model.Lang.Signature.t) =
  let rec add_items ~don't acc = function
    | [] -> List.concat (List.rev acc)
    | i :: is -> (
        match i with
        | Odoc_model.Lang.Signature.Comment `Stop ->
            add_items ~don't:(not don't) acc is
        | _ when don't -> add_items ~don't acc is
        | Module (_, md) -> add_items ~don't (module_ md :: acc) is
        | ModuleType mty -> add_items ~don't (module_type mty :: acc) is
        | Include incl -> add_items ~don't (include_ incl :: acc) is
        | Open _ | ModuleSubstitution _ | ModuleTypeSubstitution _
        | TypeSubstitution _ | Type _ | TypExt _ | Exception _ | Value _
        | Class _ | ClassType _
        | Comment (`Docs _) ->
            add_items ~don't acc is)
  in
  add_items ~don't:false [] t.items

and simple_expansion (t : Odoc_model.Lang.ModuleType.simple_expansion) =
  match t with
  | Signature sg -> signature sg
  | Functor (p, expn) ->
      let subpages =
        match p with Unit -> [] | Named { expr; _ } -> module_type_expr expr
      in
      subpages @ simple_expansion expn

and module_type_expr (t : Odoc_model.Lang.ModuleType.expr) =
  let open Odoc_model.Lang.ModuleType in
  let opt_expansion e_opt =
    match e_opt with Some e -> simple_expansion e | None -> []
  in
  match t with
  | Signature sg -> signature sg
  | Functor (f_parameter, e) ->
      let sub =
        match f_parameter with Unit -> [] | Named f -> module_type_expr f.expr
      in
      sub @ module_type_expr e
  | Path { p_expansion = e_opt; _ }
  | With { w_expansion = e_opt; _ }
  | TypeOf { t_expansion = e_opt; _ } ->
      opt_expansion e_opt

and module_ (t : Odoc_model.Lang.Module.t) =
  let url = Url.Path.from_identifier t.id in
  let subpages =
    match t.type_ with
    | Alias (_, Some e) -> simple_expansion e
    | Alias (_, None) -> []
    | ModuleType expr -> module_type_expr expr
  in
  url :: subpages

and module_type (t : Odoc_model.Lang.ModuleType.t) =
  match t.expr with
  | None -> []
  | Some expr ->
      let url = Url.Path.from_identifier t.id in
      let subpages = module_type_expr expr in
      url :: subpages

and include_ (t : Odoc_model.Lang.Include.t) = signature t.expansion.content

and page (t : Odoc_model.Lang.Page.t) = [ Url.Path.from_identifier t.name ]
