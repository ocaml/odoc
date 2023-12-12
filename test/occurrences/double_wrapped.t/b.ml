module Y = A

module Z = C

let y = Y.x + A.x + Z.y + C.y

let (_ : A.t) = "string"

module M : A.M = struct end

module type Y = A.M

let _ =
  let open A in
  1 ||> 2
