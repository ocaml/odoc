open Lang

type item =
  | CompilationUnit of Compilation_unit.t
  | TypeDecl of TypeDecl.t
  | Module of Module.t
  | Value of Value.t
  | Exception of Exception.t
  | ClassType of ClassType.t
  | Method of Method.t
  | Class of Class.t
  | Extension of Extension.t
  | ModuleType of ModuleType.t
  | Doc of Paths.Identifier.LabelParent.t * Comment.docs_or_stop

let rec unit ~f acc u =
  let acc = f acc (CompilationUnit u) in
  match u.content with
  | Module m -> signature ~f (u.id :> Paths.Identifier.LabelParent.t) acc m
  | Pack _ -> acc

and page ~f acc p =
  let open Page in
  docs ~f (p.name :> Paths.Identifier.LabelParent.t) acc (`Docs p.content)

and signature ~f id acc (s : Signature.t) =
  List.fold_left
    (signature_item ~f (id :> Paths.Identifier.LabelParent.t))
    acc s.items

and signature_item ~f id acc s_item =
  match s_item with
  | Module (_, m) -> module_ ~f (m.id :> Paths.Identifier.LabelParent.t) acc m
  | ModuleType mt ->
      module_type ~f (mt.id :> Paths.Identifier.LabelParent.t) acc mt
  | ModuleSubstitution _ -> acc
  | ModuleTypeSubstitution _ -> acc
  | Open _ -> acc
  | Type (_, t_decl) -> type_decl ~f acc t_decl
  | TypeSubstitution _ -> acc
  | TypExt te -> type_extension ~f acc te
  | Exception exc -> exception_ ~f acc exc
  | Value v -> value ~f acc v
  | Class (_, cl) -> class_ ~f (cl.id :> Paths.Identifier.LabelParent.t) acc cl
  | ClassType (_, clt) ->
      class_type ~f (clt.id :> Paths.Identifier.LabelParent.t) acc clt
  | Include i -> include_ ~f id acc i
  | Comment d -> docs ~f id acc d

and docs ~f id acc d = f acc (Doc (id, d))

and include_ ~f id acc inc = signature ~f id acc inc.expansion.content

and class_type ~f id acc ct =
  (* This check is important because [is_internal] does not work on children of
     internal items. This means that if [Fold] did not make this check here,
     it would be difficult to filter for internal items afterwards. This also
     applies to the same check in functions bellow. *)
  if Paths.Identifier.is_hidden ct.id then acc
  else
    let acc = f acc (ClassType ct) in
    match ct.expansion with
    | None -> acc
    | Some cs -> class_signature ~f id acc cs

and class_signature ~f id acc ct_expr =
  List.fold_left (class_signature_item ~f id) acc ct_expr.items

and class_signature_item ~f id acc item =
  match item with
  | Method m -> f acc (Method m)
  | InstanceVariable _ -> acc
  | Constraint _ -> acc
  | Inherit _ -> acc
  | Comment d -> docs ~f id acc d

and class_ ~f id acc cl =
  if Paths.Identifier.is_hidden cl.id then acc
  else
    let acc = f acc (Class cl) in
    match cl.expansion with
    | None -> acc
    | Some cl_signature -> class_signature ~f id acc cl_signature

and exception_ ~f acc exc =
  if Paths.Identifier.is_hidden exc.id then acc else f acc (Exception exc)

and type_extension ~f acc te = f acc (Extension te)

and value ~f acc v =
  if Paths.Identifier.is_hidden v.id then acc else f acc (Value v)

and module_ ~f id acc m =
  if Paths.Identifier.is_hidden m.id then acc
  else
    let acc = f acc (Module m) in
    match m.type_ with
    | Alias (_, None) -> acc
    | Alias (_, Some s_e) -> simple_expansion ~f id acc s_e
    | ModuleType mte -> module_type_expr ~f id acc mte

and type_decl ~f acc td =
  if Paths.Identifier.is_hidden td.id then acc else f acc (TypeDecl td)

and module_type ~f id acc mt =
  if Paths.Identifier.is_hidden mt.id then acc
  else
    let acc = f acc (ModuleType mt) in
    match mt.expr with
    | None -> acc
    | Some mt_expr -> module_type_expr ~f id acc mt_expr

and simple_expansion ~f id acc s_e =
  match s_e with
  | Signature sg -> signature ~f id acc sg
  | Functor (p, s_e) ->
      let acc = functor_parameter ~f acc p in
      simple_expansion ~f id acc s_e

and module_type_expr ~f id acc mte =
  match mte with
  | Signature s -> signature ~f id acc s
  | Functor (fp, mt_expr) ->
      let acc = functor_parameter ~f acc fp in
      module_type_expr ~f id acc mt_expr
  | With { w_expansion = Some sg; _ } -> simple_expansion ~f id acc sg
  | TypeOf { t_expansion = Some sg; _ } -> simple_expansion ~f id acc sg
  | Path { p_expansion = Some sg; _ } -> simple_expansion ~f id acc sg
  | Path { p_expansion = None; _ } -> acc
  | With { w_expansion = None; _ } -> acc
  | TypeOf { t_expansion = None; _ } -> acc

and functor_parameter ~f acc fp =
  match fp with
  | Unit -> acc
  | Named n ->
      module_type_expr ~f (n.id :> Paths.Identifier.LabelParent.t) acc n.expr
