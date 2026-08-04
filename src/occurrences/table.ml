module H = Hashtbl.Make (Odoc_model.Paths.Identifier)

type t = internal_item H.t
and internal_item = { direct : int; indirect : int; sub : t }
type key = Odoc_model.Paths.Identifier.t

type item = { direct : int; indirect : int }

let internal_to_item : internal_item -> item =
 fun { direct; indirect; _ } -> { direct; indirect }

let v_item () = { direct = 0; indirect = 0; sub = H.create 0 }

let v () = H.create 0

let add ?(quantity = 1) tbl id =
  let rec add ?(kind = `Indirect) id =
    let incr htbl id =
      let { direct; indirect; sub } =
        try H.find htbl id with Not_found -> v_item ()
      in
      let direct, indirect =
        match kind with
        | `Direct -> (direct + quantity, indirect)
        | `Indirect -> (direct, indirect + quantity)
      in
      H.replace htbl id { direct; indirect; sub };
      sub
    in
    let do_ parent =
      let htbl = add (parent :> key) in
      incr htbl id
    in
    match id with
    | `InstanceVariable (parent, _) -> do_ parent
    | `Parameter (parent, _) -> do_ parent
    | `Module (parent, _) -> do_ parent
    | `ModuleType (parent, _) -> do_ parent
    | `Method (parent, _) -> do_ parent
    | `Field (parent, _) -> do_ parent
    | `UnboxedField (parent, _) -> do_ parent
    | `Extension (parent, _) -> do_ parent
    | `Type (parent, _) -> do_ parent
    | `Constructor (parent, _) -> do_ parent
    | `Exception (parent, _) -> do_ parent
    | `ExtensionDecl (parent, _, _) -> do_ parent
    | `Class (parent, _) -> do_ parent
    | `Value (parent, _) -> do_ parent
    | `ClassType (parent, _) -> do_ parent
    | `Root _ -> incr tbl id
    | `SourcePage _ | `Page _ | `LeafPage _ | `SourceLocation _ | `Label _
    | `SourceLocationMod _ | `Result _ | `AssetFile _
    | `SourceLocationInternal _ ->
        assert false
  in
  let _htbl = add ~kind:`Direct id in
  ()

let rec get t id =
  let do_ parent =
    get t (parent :> key) |> function
    | None -> None
    | Some { sub; _ } -> ( try Some (H.find sub id) with Not_found -> None)
  in
  match id with
  | `InstanceVariable (parent, _) -> do_ parent
  | `Parameter (parent, _) -> do_ parent
  | `Module (parent, _) -> do_ parent
  | `ModuleType (parent, _) -> do_ parent
  | `Method (parent, _) -> do_ parent
  | `Field (parent, _) -> do_ parent
  | `UnboxedField (parent, _) -> do_ parent
  | `Extension (parent, _) -> do_ parent
  | `ExtensionDecl (parent, _, _) -> do_ parent
  | `Type (parent, _) -> do_ parent
  | `Constructor (parent, _) -> do_ parent
  | `Exception (parent, _) -> do_ parent
  | `Class (parent, _) -> do_ parent
  | `Value (parent, _) -> do_ parent
  | `ClassType (parent, _) -> do_ parent
  | `Root _ -> ( try Some (H.find t id) with Not_found -> None)
  | `SourcePage _ | `Page _ | `LeafPage _ | `SourceLocation _ | `Label _
  | `SourceLocationMod _ | `Result _ | `AssetFile _ | `SourceLocationInternal _
    ->
      None

let get t id =
  match get t id with None -> None | Some i -> Some (internal_to_item i)

let rec iter f tbl =
  H.iter
    (fun id v ->
      iter f v.sub;
      let v = internal_to_item v in
      f id v)
    tbl

module Strip = struct
  open Odoc_model.Paths.Identifier
  let rec strip_sig_path : Signature.t -> Signature.t =
   fun x ->
    match x with
    | `Root (_, name) -> Mk.root (None, name)
    | `Module (p, name) -> Mk.module_ (strip_sig_path p, name)
    | `Parameter (p, name) -> Mk.parameter (strip_sig_path p, name)
    | `Result p -> Mk.result (strip_sig_path p)
    | `ModuleType (p, name) -> Mk.module_type (strip_sig_path p, name)

  and strip_class_sig_path : ClassSignature.t -> ClassSignature.t =
   fun x ->
    match x with
    | `Class (p, name) -> Mk.class_ (strip_sig_path p, name)
    | `ClassType (p, name) -> Mk.class_type (strip_sig_path p, name)

  and strip_datatype_path : DataType.t -> DataType.t =
   fun x ->
    match x with `Type (p, name) -> Mk.type_ (strip_sig_path p, name)

  and strip_field_parent_path : FieldParent.t -> FieldParent.t =
   fun x ->
    match x with
    | #Signature.t as v -> (strip_sig_path v :> FieldParent.t)
    | #DataType.t as v ->
        (strip_datatype_path v :> FieldParent.t)

  and strip_unboxed_field_parent_path :
      UnboxedFieldParent.t -> UnboxedFieldParent.t =
   fun x ->
    match x with
    | #DataType.t as v ->
        (strip_datatype_path v :> UnboxedFieldParent.t)

  and strip_label_parent_path : LabelParent.t -> LabelParent.t =
   fun x ->
    match x with
    | #Signature.t as v -> (strip_sig_path v :> LabelParent.t)
    | #DataType.t as v ->
        (strip_datatype_path v :> LabelParent.t)
    | #ClassSignature.t as v ->
        (strip_class_sig_path v :> LabelParent.t)
    | `Page _ | `LeafPage _ -> x

  and strip : t -> t =
   fun x ->
    match x with
    | #Signature.t as v -> (strip_sig_path v :> t)
    | #ClassSignature.t as v -> (strip_class_sig_path v :> t)
    | #DataType.t as v -> (strip_datatype_path v :> t)
    | `InstanceVariable (p, name) ->
        Mk.instance_variable (strip_class_sig_path p, name)
    | `Method (p, name) -> Mk.method_ (strip_class_sig_path p, name)
    | `Field (p, name) -> Mk.field (strip_field_parent_path p, name)
    | `UnboxedField (p, name) ->
        Mk.unboxed_field (strip_unboxed_field_parent_path p, name)
    | `Label (p, name) -> Mk.label (strip_label_parent_path p, name)
    | `Exception (p, name) -> Mk.exception_ (strip_sig_path p, name)
    | `Extension (p, name) -> Mk.extension (strip_sig_path p, name)
    | `Value (p, name) -> Mk.value (strip_sig_path p, name)
    | `ExtensionDecl (p, name, args) ->
        Mk.extension_decl (strip_sig_path p, (name, args))
    | `Constructor (p, name) ->
        Mk.constructor (strip_datatype_path p, name)
    | ( `AssetFile (_, _)
       | `SourceLocationMod _ | `SourceLocation _ | `Page _ | `LeafPage _
       | `SourcePage _ | `SourceLocationInternal _ ) ->
        x

  let rec strip_table tbl =
    let t2 = v () in
    H.iter
      (fun key v -> H.add t2 (strip key) { v with sub = strip_table v.sub })
      tbl;
    t2
end

let strip_table = Strip.strip_table
