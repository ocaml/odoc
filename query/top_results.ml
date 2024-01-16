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
  let extra_cost = Dynamic_cost.score query entry in
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

let max_seek = 10

module Make (IO : Io.S) = struct
  module Seq = Io.Seq (IO)

  let of_seq ~query ~limit seq =
    let rec go total_seen t seq =
      if total_seen >= limit + max_seek
      then IO.return t
      else begin
        IO.bind (seq ())
        @@ function
        | Seq.Nil -> IO.return t
        | Cons (x, xs) -> begin
          match add ~query ~limit x t with
          | Stop t -> IO.return t
          | Continue t -> go (total_seen + 1) t xs
        end
      end
    in
    IO.map (go 0 empty seq) @@ fun t -> List.of_seq @@ Bests.to_seq t.bests
end
