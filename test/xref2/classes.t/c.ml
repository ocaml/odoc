class type d = object
  method e : B.t
  method f : #B.u -> unit
end
let f : B.t = assert false
let g : #B.u -> unit = fun _ -> ()
let h : B.u -> unit = fun _ -> ()
let i : < m : B.u > -> unit = fun _ -> ()

