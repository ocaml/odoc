type child = Page of string | Dir of string

type line =
  | Children_order of child Location_.with_location list
  | KV of string * string
  | V of string

type children_order = child Location_.with_location list Location_.with_location

type t = { children_order : children_order option }

let empty = { children_order = None }

let apply fm line =
  match (line.Location_.value, fm) with
  | Children_order children_order, { children_order = None } ->
      { children_order = Some (Location_.same line children_order) }
  | Children_order _, { children_order = Some _ } ->
      (* TODO raise warning about duplicate children field *) fm
  | KV _, _ | V _, _ -> (* TODO raise warning *) fm

let parse_child c =
  if Astring.String.is_suffix ~affix:"/" c then
    let c = String.sub c 0 (String.length c - 1) in
    Dir c
  else Page c

let parse s =
  let entries =
    s.Location_.value
    |> Astring.String.cuts ~sep:"\n"
    |> List.map (fun l ->
           let v =
             Astring.String.cut ~sep:":" l |> function
             | Some ("children", v) ->
                 let refs =
                   v
                   |> Astring.String.fields ~empty:false
                   |> List.map parse_child
                   |> List.map (Location_.same s)
                 in
                 Children_order refs
             | Some (k, v) -> KV (k, v)
             | None -> V l
           in
           Location_.same s v)
  in
  List.fold_left apply empty entries
