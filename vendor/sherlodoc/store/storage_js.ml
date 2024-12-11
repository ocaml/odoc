type writer = out_channel

let open_out = open_out
let close_out = close_out

let deflate_string ?(level = 4) str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Zl.Higher.compress ~level ~dynamic:true ~w ~q ~refill ~flush i o ;
  Buffer.contents r

let save ~db t =
  let str = Marshal.to_string t [] in
  let str = deflate_string str in
  let str = Base64.encode_string str in
  Printf.fprintf db "function sherlodoc_db () { return %S; }\n%!" str

let load _ = failwith "js database format is unsupported"
