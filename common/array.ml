open Common_
include Stdlib.Array

let equal (a : 'a -> 'a -> bool) arr arr' =
  if arr == arr' then true else length arr = length arr' && for_all2 a arr arr'

let hash (a : 'a -> int) arr = Hashtbl.hash (map a arr)

let rec succ_ge ~compare elt arr lo hi =
  let elt_lo = get arr lo in
  if ge ~compare elt_lo elt
  then elt_lo
  else if lo = hi
  then (* in that case, above branch should have been triggered *)
    assert false
  else if lo = hi - 1
  then (
    let elt_hi = get arr hi in
    assert (ge ~compare elt_hi elt) ;
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
  if length arr = 0
  then None
  else
    let lo = 0 and hi = length arr in
    if not (ge ~compare (get arr (hi - 1)) elt)
    then None
    else Some (succ_ge ~compare elt arr lo hi)

let rec succ_gt ~compare elt arr lo hi =
  let elt_lo = get arr lo in
  if gt ~compare elt_lo elt
  then elt_lo
  else if lo = hi
  then (* in that case, above branch should have been triggered *)
    assert false
  else if lo = hi - 1
  then (
    (* lo is already checked above *)
    let elt_hi = get arr hi in
    assert (gt ~compare elt_hi elt) ;
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
  if length arr = 0
  then None
  else
    let lo = 0 and hi = length arr in
    if not (gt ~compare (get arr (hi - 1)) elt)
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
