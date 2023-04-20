type writer = out_channel

let open_out = open_out
let close_out = close_out
let save ~db t =
  let str = Marshal.to_string t [] in
  let str = Base64.encode_string str in
  Printf.fprintf db "sherlodb=%S;\n%!" str

let load str =
  let str = Base64.decode_exn str in
  [Marshal.from_string str 0]
