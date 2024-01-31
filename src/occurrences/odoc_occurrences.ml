module Table = Table

let of_impl ~include_hidden unit htbl =
  let incr tbl p =
    let open Odoc_model.Paths.Path.Resolved in
    let p = (p :> t) in
    let id = identifier p in
    if (not (is_hidden p)) || include_hidden then Table.add tbl id
  in
  let open Odoc_model.Lang in
  List.iter
    (function
      | Source_info.Module { documentation = Some (`Resolved p); _ }, _ ->
          incr htbl p
      | Value { documentation = Some (`Resolved p); _ }, _ -> incr htbl p
      | ModuleType { documentation = Some (`Resolved p); _ }, _ -> incr htbl p
      | Type { documentation = Some (`Resolved p); _ }, _ -> incr htbl p
      | _ -> ())
    unit.Implementation.source_info

let aggregate ~tbl ~data =
  Table.iter
    (fun id { Table.direct; _ } -> Table.add ~quantity:direct tbl id)
    data
