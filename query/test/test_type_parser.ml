open Db.Typexpr

let random_elt arr = arr.(Random.int (Array.length arr))
let random_poly () = poly (random_elt [| "a"; "b"; "c"; "d"; "e" |])

let random_constr () =
  constr (random_elt [| "float"; "int"; "string"; "foo"; "bar"; "t" |]) []

let rec random_type size =
  match size with
  | 0 | 1 -> random_elt [| random_poly; random_constr; (fun () -> any) |] ()
  | (2 | 3 | 4) when Random.bool () -> random_constr_params size
  | _ when Random.int 100 < 20 ->
      let n = 2 + Random.int 3 in
      tuple (List.init n (fun _i -> random_type (size / n)))
  | _ ->
      let size = size / 2 in
      arrow (random_type size) (random_type size)

and random_constr_params n =
  constr
    (random_elt [| "list"; "option"; "t"; "result"; "array" |])
    (List.init (n-1) (fun i -> random_type i))

open Query.Private

let test_parser typ () =
  let str = Db.Typexpr.show typ in
  let typ' = Type_parser.of_string str in
  let str' = Result.map Db.Typexpr.show typ' in
  Alcotest.(check (result string string)) "same string" (Ok str) str'

let tests =
  List.init 50 (fun i ->
      let i = i in
      let typ = random_type i in
      Alcotest.test_case
        (Printf.sprintf "Type_parser size %i" i)
        `Quick (test_parser typ))
