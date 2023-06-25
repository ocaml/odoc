let get = Array.get

let rec succ_ge ~compare elt arr lo hi =
  let elt_lo = get arr lo in
  if compare elt_lo elt >= 0
  then elt_lo
  else if lo = hi
  then (* in that case, above branch should have been triggered *)
    assert false
  else if lo = hi - 1
  then (
    let elt_hi = get arr hi in
    assert (compare elt_hi elt >= 0) ;
    elt_hi)
  else
    let mid = (lo + hi) / 2 in
    let elt' = get arr mid in
    let comp = compare elt' elt in
    if comp = 0
    then elt'
    else if comp > 0
    then succ_ge ~compare elt arr lo mid
    else succ_ge ~compare elt arr mid hi

let succ_ge ~compare elt arr =
  if Array.length arr = 0
  then None
  else
    let lo = 0 and hi = Array.length arr in
    if not (compare (get arr (hi - 1)) elt >= 0)
    then None
    else Some (succ_ge ~compare elt arr lo hi)

let rec succ_gt ~compare elt arr lo hi =
  let elt_lo = get arr lo in
  if compare elt_lo elt > 0
  then elt_lo
  else if lo = hi
  then (* in that case, above branch should have been triggered *)
    assert false
  else if lo = hi - 1
  then (
    (* lo is already checked above *)
    let elt_hi = get arr hi in
    assert (compare elt_hi elt > 0) ;
    elt_hi)
  else
    let mid = (lo + hi) / 2 in
    let elt' = get arr mid in
    let comp = compare elt' elt in
    if comp = 0
    then get arr (mid + 1)
    else if comp > 0
    then succ_gt ~compare elt arr lo mid
    else succ_gt ~compare elt arr mid hi

let succ_gt ~compare elt arr =
  if Array.length arr = 0
  then None
  else
    let lo = 0 and hi = Array.length arr in
    if not (compare (get arr (hi - 1)) elt > 0)
    then None
    else Some (succ_gt ~compare elt arr lo hi)

let succ_gt_exn ~compare elt arr =
  match succ_gt ~compare elt arr with
  | None -> raise Not_found
  | Some v -> v

let succ_ge_exn ~compare elt arr =
  match succ_ge ~compare elt arr with
  | None -> raise Not_found
  | Some v -> v
