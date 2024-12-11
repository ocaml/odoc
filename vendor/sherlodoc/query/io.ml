module type S = sig
  (* avoids a dependency on lwt for sherlodoc.js *)

  type 'a t

  val return : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Seq (Io : S) = struct
  type 'a t = unit -> 'a node Io.t

  and 'a node =
    | Nil
    | Cons of 'a * 'a t

  let rec of_seq s () =
    match s () with
    | Seq.Nil -> Io.return Nil
    | Cons (x, xs) -> Io.return (Cons (x, of_seq xs))

  let rec take n xs () =
    if n = 0
    then Io.return Nil
    else begin
      Io.map (xs ())
      @@ function
      | Nil -> Nil
      | Cons (x, xs) -> Cons (x, take (n - 1) xs)
    end

  let rec to_list acc s =
    Io.bind (s ())
    @@ function
    | Nil -> Io.return (List.rev acc)
    | Cons (x, xs) -> to_list (x :: acc) xs

  let to_list s = to_list [] s
end
