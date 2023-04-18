module type S = sig
  type t

  val copy : t -> t
end

module Make (Element : S) = struct
  module H = Hashtbl.Make (struct
    type t = Element.t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let cache = H.create 16
  let clear () = H.clear cache

  let memo str =
    try H.find cache str
    with Not_found ->
      let str = Element.copy str in
      H.add cache str str ;
      str
end
