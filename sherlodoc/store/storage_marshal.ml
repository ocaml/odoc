type writer = out_channel

(* marshal files are binary, need to open them in binary mode *)
let open_out = open_out_bin
let close_out = close_out
let save ~db t = Marshal.to_channel db t []

let load name =
  let file = open_in_bin name in
  Fun.protect ~finally:(fun () -> close_in file) (fun () -> [ Marshal.from_channel file ])
