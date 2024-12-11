type writer = out_channel

let open_out = open_out
let close_out = close_out
let save ~db t = Marshal.to_channel db t []

let load name =
  let file = open_in name in
  let t = Marshal.from_channel file in
  close_in file ;
  [ t ]
