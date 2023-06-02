module type S = sig
  type key
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val of_seq : (key * 'a) Seq.t -> 'a t
  val to_array : 'a t -> (key * 'a) array
  val find : key:key -> 'a t -> 'a option
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val fold : f:(key:key -> acc:'b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
end

module Make (Key : Map.OrderedType) : S with type key = Key.t = struct
  type key = Key.t
  type 'a t = (key * 'a) array

  let equal eq_a =
    Array.equal (fun (k, v) (k', v') -> Key.compare k k' = 0 && eq_a v v')

  let to_array arr = arr

  let of_seq seq =
    let arr = seq |> Array.of_seq in
    Array.fast_sort (fun (k, _) (k', _) -> Key.compare k k') arr ;
    arr

  let rec find ~key arr lo hi =
    if lo = hi
    then None
    else
      let mid = (lo + hi) / 2 in
      let key', v = arr.(mid) in
      let comp = Key.compare key key' in
      if comp = 0
      then Some v
      else if comp < 0
      then find ~key arr lo mid
      else find ~key arr mid hi

  let find ~key arr = find ~key arr 0 (Array.length arr)
  let map ~f arr = Array.map (fun (k, v) -> k, f v) arr

  let fold ~f ~init arr =
    Array.fold_left (fun acc (key, v) -> f ~key ~acc v) init arr
end
