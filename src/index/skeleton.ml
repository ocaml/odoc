open Odoc_model.Lang
open Odoc_model.Paths

open Odoc_utils

type t = Entry.t Tree.t

module Entry = struct
  open Odoc_model.Comment

  let of_comp_unit (u : Compilation_unit.t) =
    let has_expansion = true in
    let doc =
      match u.content with Pack _ -> [] | Module m -> m.doc.elements
    in
    Entry.entry ~id:u.id ~doc ~kind:(Module { has_expansion })
      ~source_loc:u.source_loc_jane

  let of_module (m : Module.t) =
    let has_expansion =
      match m.type_ with Alias (_, None) -> false | _ -> true
    in
    Entry.entry ~id:m.id ~doc:m.doc.elements ~kind:(Module { has_expansion })
      ~source_loc:m.source_loc_jane

  let of_module_type (mt : ModuleType.t) =
    let has_expansion =
      match mt.expr with
      | Some expr -> (
          match expr with
          | Signature _ -> true
          | Functor _ -> true
          | Path { p_expansion = Some _; _ } -> true
          | With { w_expansion = Some _; _ } -> true
          | TypeOf { t_expansion = Some _; _ } -> true
          | _ -> false)
      | _ -> true
    in
    Entry.entry ~id:mt.id ~doc:mt.doc.elements
      ~kind:(ModuleType { has_expansion }) ~source_loc:mt.source_loc_jane

  let of_type_decl (td : TypeDecl.t) =
    let kind =
      Entry.TypeDecl
        {
          canonical = td.canonical;
          equation = td.equation;
          representation = td.representation;
        }
    in
    Entry.entry ~id:td.id ~doc:td.doc.elements ~kind
      ~source_loc:td.source_loc_jane

  let varify_params =
    List.mapi (fun i param ->
        match param.TypeDecl.desc with
        | Var name -> TypeExpr.Var name
        | Any -> Var (Printf.sprintf "tv_%i" i))

  let of_constructor id_parent params source_loc (c : TypeDecl.Constructor.t) =
    let args = c.args in
    let res =
      match c.res with
      | Some res -> res
      | None ->
          let params = varify_params params in
          TypeExpr.Constr
            ( `Identifier
                ((id_parent :> Odoc_model.Paths.Identifier.Path.Type.t), false),
              params )
    in
    let kind = Entry.Constructor { args; res } in
    Entry.entry ~id:c.id ~doc:c.doc.elements ~kind ~source_loc

  let of_field id_parent params source_loc (field : TypeDecl.Field.t) =
    let params = varify_params params in
    let parent_type =
      TypeExpr.Constr
        ( `Identifier
            ((id_parent :> Odoc_model.Paths.Identifier.Path.Type.t), false),
          params )
    in
    let kind =
      Entry.Field
        { mutable_ = field.mutable_; type_ = field.type_; parent_type }
    in
    Entry.entry ~id:field.id ~doc:field.doc.elements ~kind ~source_loc

  let of_exception (exc : Exception.t) =
    let res =
      match exc.res with
      | None ->
          TypeExpr.Constr
            ( `Resolved (`CoreType (Odoc_model.Names.TypeName.make_std "exn")),
              [] )
      | Some x -> x
    in
    let kind = Entry.Exception { args = exc.args; res } in
    Entry.entry ~id:exc.id ~doc:exc.doc.elements ~kind
      ~source_loc:exc.source_loc_jane

  let of_value (v : Value.t) =
    let kind = Entry.Value { value = v.value; type_ = v.type_ } in
    Entry.entry ~id:v.id ~doc:v.doc.elements ~kind ~source_loc:v.source_loc_jane

  let of_extension_constructor type_path params (v : Extension.Constructor.t) =
    let res =
      match v.res with
      | Some res -> res
      | None ->
          let params = varify_params params in
          TypeExpr.Constr (type_path, params)
    in
    let kind = Entry.ExtensionConstructor { args = v.args; res } in
    Entry.entry ~id:v.id ~doc:v.doc.elements ~kind ~source_loc:None

  let of_class (cl : Class.t) =
    let kind = Entry.Class { virtual_ = cl.virtual_; params = cl.params } in
    Entry.entry ~id:cl.id ~doc:cl.doc.elements ~kind
      ~source_loc:cl.source_loc_jane

  let of_class_type (ct : ClassType.t) =
    let kind =
      Entry.Class_type { virtual_ = ct.virtual_; params = ct.params }
    in
    Entry.entry ~id:ct.id ~doc:ct.doc.elements ~kind
      ~source_loc:ct.source_loc_jane

  let of_method (m : Method.t) =
    let kind =
      Entry.Method
        { virtual_ = m.virtual_; private_ = m.private_; type_ = m.type_ }
    in
    Entry.entry ~id:m.id ~doc:m.doc.elements ~kind ~source_loc:None

  let of_docs id source_loc doc =
    Entry.entry ~id ~doc:doc.elements ~kind:Doc ~source_loc
end

let if_non_hidden id f =
  if Identifier.is_hidden (id :> Identifier.t) then [] else f ()

let filter_signature items =
  List.fold_left
    (fun (keep, acc) item ->
      match item with
      | Signature.Comment `Stop -> (not keep, acc)
      | _ -> if keep then (keep, item :: acc) else (keep, acc))
    (true, []) items
  |> snd |> List.rev

let filter_class_signature items =
  List.fold_left
    (fun (keep, acc) item ->
      match item with
      | ClassSignature.Comment `Stop -> (not keep, acc)
      | _ -> if keep then (keep, item :: acc) else (keep, acc))
    (true, []) items
  |> snd |> List.rev

let rec unit (u : Compilation_unit.t) =
  let entry = Entry.of_comp_unit u in
  let children =
    match u.content with
    | Pack _ -> []
    | Module m -> signature (u.id :> Identifier.LabelParent.t) m
  in
  { Tree.node = entry; children }

and signature id (s : Signature.t) =
  let items = filter_signature s.items in
  List.concat_map (signature_item (id :> Identifier.LabelParent.t)) items

and signature_item id s_item =
  match s_item with
  | Module (_, m) -> module_ (m.id :> Identifier.LabelParent.t) m
  | ModuleType mt -> module_type (mt.id :> Identifier.LabelParent.t) mt
  | ModuleSubstitution _ -> []
  | ModuleTypeSubstitution _ -> []
  | Open _ -> []
  | Type (_, t_decl) -> type_decl t_decl
  | TypeSubstitution _ -> []
  | TypExt te -> type_ext te
  | Exception exc -> exception_ exc
  | Value v -> value v
  | Class (_, cl) -> class_ (cl.id :> Identifier.LabelParent.t) cl
  | ClassType (_, clt) -> class_type (clt.id :> Identifier.LabelParent.t) clt
  | Include i -> include_ id i
  | Comment d -> docs id d

and type_ext te =
  List.map (constructor_extension te.type_path te.type_params) te.constructors

and constructor_extension type_path params ec =
  Tree.leaf @@ Entry.of_extension_constructor type_path params ec

and module_ id m =
  if_non_hidden m.id @@ fun () ->
  let entry = Entry.of_module m in
  let children =
    match m.type_ with
    | Alias (_, None) -> []
    | Alias (_, Some s_e) -> simple_expansion id s_e
    | ModuleType mte -> module_type_expr id mte
  in
  [ { Tree.node = entry; children } ]

and module_type id mt =
  if_non_hidden mt.id @@ fun () ->
  let entry = Entry.of_module_type mt in
  let children =
    match mt.expr with
    | None -> []
    | Some mt_expr -> module_type_expr id mt_expr
  in
  [ { Tree.node = entry; children } ]

and type_decl td =
  if_non_hidden td.id @@ fun () ->
  let entry = Entry.of_type_decl td in
  let children =
    match td.representation with
    | None -> []
    | Some (Variant cl) ->
        List.concat_map (constructor td.id td.equation.params td.source_loc_jane) cl
    | Some (Record fl) -> List.concat_map (field td.id td.equation.params td.source_loc_jane) fl
    | Some Extensible -> []
  in
  [ { Tree.node = entry; children } ]

and constructor type_id params source_loc c =
  let entry = Entry.of_constructor type_id params source_loc c in
  [ Tree.leaf entry ]

and field type_id params source_loc f =
  let entry = Entry.of_field type_id params source_loc f in
  [ Tree.leaf entry ]

and exception_ exc =
  if_non_hidden exc.id @@ fun () ->
  let entry = Entry.of_exception exc in
  [ Tree.leaf entry ]

and value v =
  if_non_hidden v.id @@ fun () ->
  let entry = Entry.of_value v in
  [ Tree.leaf entry ]

and class_ id cl =
  if_non_hidden cl.id @@ fun () ->
  let entry = Entry.of_class cl in
  let children =
    match cl.expansion with
    | None -> []
    | Some cl_signature -> class_signature id cl_signature
  in
  [ { Tree.node = entry; children } ]

and class_type id ct =
  if_non_hidden ct.id @@ fun () ->
  let entry = Entry.of_class_type ct in
  let children =
    match ct.expansion with None -> [] | Some cs -> class_signature id cs
  in
  [ { Tree.node = entry; children } ]

and include_ id inc = signature id inc.expansion.content

and docs id d =
  match d with
  | `Stop -> []
  | `Docs d ->
      let entry = Entry.of_docs id None d in
      [ Tree.leaf entry ]

and simple_expansion id s_e =
  match s_e with
  | Signature sg -> signature id sg
  | Functor (p, s_e) ->
      let _extra_entries = functor_parameter p in
      simple_expansion id s_e

and module_type_expr id mte =
  match mte with
  | Signature s -> signature id s
  | Functor (fp, mt_expr) ->
      let _extra_entries = functor_parameter fp in
      module_type_expr id mt_expr
  | With { w_expansion = Some sg; _ } -> simple_expansion id sg
  | TypeOf { t_expansion = Some sg; _ } -> simple_expansion id sg
  | Path { p_expansion = Some sg; _ } -> simple_expansion id sg
  | Strengthen { s_expansion = Some sg; _ } -> simple_expansion id sg
  | Path { p_expansion = None; _ } -> []
  | With { w_expansion = None; _ } -> []
  | TypeOf { t_expansion = None; _ } -> []
  | Strengthen { s_expansion = None; _ } -> []

and class_signature id ct_expr =
  let items = filter_class_signature ct_expr.items in
  List.concat_map (class_signature_item id) items

and class_signature_item id item =
  match item with
  | Method m ->
      let entry = Entry.of_method m in
      [ Tree.leaf entry ]
  | InstanceVariable _ -> []
  | Constraint _ -> []
  | Inherit _ -> []
  | Comment d -> docs id d

and functor_parameter fp =
  match fp with
  | Unit -> []
  | Named n -> module_type_expr (n.id :> Identifier.LabelParent.t) n.expr

let from_unit u = unit u

let from_page (p : Page.t) =
  match p with
  | { name; content; _ } ->
      let entry = Entry.of_docs name None content in
      Tree.leaf entry
