module Foo = struct
    module A = struct
        type t = A
    end
    module B = struct
        type t = B
    end
end

open (Foo : module type of Foo with module A := Foo.A)

module B = B

open Set.Make(struct type t = Foo.A.t let compare = compare end)

type u = t
