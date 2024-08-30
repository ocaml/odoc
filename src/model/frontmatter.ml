type child = Page of string | Dir of string

type line = Children_order of child list | KV of string * string | V of string

type t = { children_order : child list option }

let empty = { children_order = None }

let apply fm line =
  match (line, fm) with
  | Children_order children_order, { children_order = None } ->
      { children_order = Some children_order }
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
    s |> String.split_on_char '\n'
    |> List.map (fun l ->
           l |> fun x ->
           Astring.String.cut ~sep:":" x |> function
           | Some ("children", v) ->
               let refs =
                 v |> Astring.String.fields ~empty:false |> List.map parse_child
               in
               Children_order refs
           | Some (k, v) -> KV (k, v)
           | None -> V x)
  in
  List.fold_left apply empty entries
