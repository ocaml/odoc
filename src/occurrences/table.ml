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
    match id.iv with
    | `InstanceVariable (parent, _) -> do_ parent
    | `Parameter (parent, _) -> do_ parent
    | `Module (parent, _) -> do_ parent
    | `ModuleType (parent, _) -> do_ parent
    | `Method (parent, _) -> do_ parent
    | `Field (parent, _) -> do_ parent
    | `Extension (parent, _) -> do_ parent
    | `Type (parent, _) -> do_ parent
    | `CoreType _ -> incr tbl id
    | `Constructor (parent, _) -> do_ parent
    | `Exception (parent, _) -> do_ parent
    | `ExtensionDecl (parent, _, _) -> do_ parent
    | `Class (parent, _) -> do_ parent
    | `Value (parent, _) -> do_ parent
    | `ClassType (parent, _) -> do_ parent
    | `Root _ -> incr tbl id
    | `SourcePage _ | `Page _ | `LeafPage _ | `SourceLocation _
    | `CoreException _ | `Label _ | `SourceLocationMod _ | `Result _
    | `AssetFile _ | `SourceLocationInternal _ ->
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
  match id.iv with
  | `InstanceVariable (parent, _) -> do_ parent
  | `Parameter (parent, _) -> do_ parent
  | `Module (parent, _) -> do_ parent
  | `ModuleType (parent, _) -> do_ parent
  | `Method (parent, _) -> do_ parent
  | `Field (parent, _) -> do_ parent
  | `Extension (parent, _) -> do_ parent
  | `ExtensionDecl (parent, _, _) -> do_ parent
  | `Type (parent, _) -> do_ parent
  | `Constructor (parent, _) -> do_ parent
  | `Exception (parent, _) -> do_ parent
  | `Class (parent, _) -> do_ parent
  | `Value (parent, _) -> do_ parent
  | `ClassType (parent, _) -> do_ parent
  | `Root _ -> ( try Some (H.find t id) with Not_found -> None)
  | `SourcePage _ | `Page _ | `LeafPage _ | `CoreType _ | `SourceLocation _
  | `CoreException _ | `Label _ | `SourceLocationMod _ | `Result _
  | `AssetFile _ | `SourceLocationInternal _ ->
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
