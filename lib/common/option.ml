include Stdlib.Option

module O = struct 

  let (let*) = bind
  let (let+) v f = map f v

end