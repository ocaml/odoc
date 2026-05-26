let[@zero_alloc] add b x y = if b then x + y else x

module To_be_included = struct
  let[@zero_alloc] add b x y = if b then x + y else x
  (* [add] has a zero alloc annotation that it shouldn't loose *)
end

module Including = struct
  include To_be_included
end
