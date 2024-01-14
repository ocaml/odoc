open Query.Private

let pkg = Db.Entry.Package.v ~name:"" ~version:""

let elt cost =
  Db.Entry.v ~cost ~name:"" ~kind:Db.Entry.Kind.Doc ~rhs:None ~doc_html:"" ~url:"" ~pkg ()

(** This module does the same thing as Succ, but its correctness is obvious
    and its performance terrible. *)
module Reference = struct
  include Set.Make (Db.Entry)

  let of_array arr = arr |> Array.to_seq |> of_seq
end

(** This module is used to construct a pair of a "set array" using [Reference]
    and a Succ that are exactly the same. *)
module Both = struct
  let empty = Reference.empty, Succ.empty
  let union (l, l') (r, r') = Reference.union l r, Succ.union l' r'
  let inter (l, l') (r, r') = Reference.inter l r, Succ.inter l' r'
  let of_array arr = Reference.of_array arr, Succ.of_array arr
end

(** This is a problematic exemple that was found randomly. It is saved here
    to check for regressions. *)
let extra_succ =
  let open Both in
  let of_array arr = Both.of_array (Array.map elt arr) in
  union
    (inter (of_array [| 0; 1 |]) (of_array [| 0; 1 |]))
    (inter (of_array [| 0; 2; 3 |]) (of_array [| 1; 3; 5; 7 |]))

let rec random_set ~empty ~union ~inter ~of_array size =
  let random_set = random_set ~empty ~union ~inter ~of_array in
  if size = 0
  then empty
  else (
    match Random.int 3 with
    | 0 ->
      let arr = Test_array.random_array size in
      let arr = Array.map elt arr in
      Array.sort Db.Entry.compare arr ;
      of_array arr
    | 1 -> inter (random_set (size / 2)) (random_set (size / 2))
    | 2 -> union (random_set (size / 2)) (random_set (size / 2))
    | _ -> assert false)

let to_costs lst = List.map (fun e -> e.Db.Entry.cost) (List.of_seq lst)

let test_to_seq tree () =
  let ref = fst tree |> Reference.to_seq |> to_costs in
  let real = snd tree |> Succ.to_seq |> to_costs in
  Alcotest.(check (list int)) "same int list" ref real

let tests_to_seq =
  [ Alcotest.test_case "Succ.to_seq extra" `Quick (test_to_seq extra_succ) ]
  @ List.init 50 (fun i ->
    let i = i * 7 in
    let succ = i |> Both.(random_set ~empty ~union ~inter ~of_array) in
    Alcotest.test_case (Printf.sprintf "Succ.to_seq size %i" i) `Quick (test_to_seq succ))
