module Bests = Set.Make (Db.Entry)

type t =
  { size : int
  ; bests : Bests.t
  }

let empty = { size = 0; bests = Bests.empty }

type step =
  | Continue of t
  | Stop of t

let update_entry query entry =
  let extra_cost = Dynamic_cost.cost_of_entry query entry in
  Db.Entry.{ entry with cost = entry.cost + extra_cost }

let add ~query ~limit elt t =
  if t.size < limit
  then begin
    let elt = update_entry query elt in
    Continue { size = t.size + 1; bests = Bests.add elt t.bests }
  end
  else begin
    let worst = Bests.max_elt t.bests in
    if Db.Entry.(elt.cost > worst.cost)
    then Stop t
    else begin
      let elt = update_entry query elt in
      if Db.Entry.(elt.cost > worst.cost)
      then Continue t
      else Continue { t with bests = Bests.add elt @@ Bests.remove worst t.bests }
    end
  end

let max_seek = 500

let of_seq ~query ~limit seq =
  let rec go total_seen t seq =
    if total_seen >= limit + max_seek
    then t
    else begin
      match seq () with
      | Seq.Nil -> t
      | Cons (x, xs) -> begin
        match add ~query ~limit x t with
        | Stop t -> t
        | Continue t -> go (total_seen + 1) t xs
      end
    end
  in
  let t = go 0 empty seq in
  Bests.to_seq t.bests
