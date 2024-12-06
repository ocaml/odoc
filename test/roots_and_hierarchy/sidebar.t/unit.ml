let x = 1

module X = struct
  module Y = struct
    type t = int
  end

  module Z = Y
end

module type Foo = sig end
