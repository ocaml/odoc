include Stdlib.List

(* Same as Stdlib, except for the tmc annotation that does not make it slower
   but prevents stack overflows, notably on the browser. *)
let[@tail_mod_cons] rec map f = function
  | [] -> []
  | a :: l ->
      let r = f a in
      r :: map f l