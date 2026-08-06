include struct
  let g x = x

  let f x = Mylib.truc (g x)
end [@@merlin.hide]
