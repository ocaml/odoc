module S = struct
  type t = int

  let x = 2
end

module R = A.F (S)
