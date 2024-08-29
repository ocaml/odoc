type line =
  | Children_order of Paths.Reference.Page.t list
  | KV of string * string
  | V of string

type t = { children_order : Paths.Reference.Page.t list option }

let empty = { children_order = None }

let apply fm line =
  match (line, fm) with
  | Children_order children_order, { children_order = None } ->
      { children_order = Some children_order }
  | Children_order _, { children_order = Some _ } ->
      (* TODO raise warning about duplicate children field *) fm
  | KV _, _ | V _, _ -> (* TODO raise warning *) fm

let parse s =
  let entries =
    s |> String.split_on_char '\n'
    |> List.map (fun l ->
           l |> fun x ->
           Astring.String.cut ~sep:":" x |> function
           | Some ("children", v) ->
               let refs =
                 Astring.String.fields v
                 |> List.map (fun name : Paths.Reference.Page.t ->
                        `Page_path (`TRelativePath, [ name ]))
               in
               Children_order refs
           | Some (k, v) -> KV (k, v)
           | None -> V x)
  in
  List.fold_left apply empty entries
