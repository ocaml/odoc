let print_error e =
  print_string
  @@ String.concat
       ""
       [ "Error : "
       ; Jstr.to_string @@ Jv.Error.name e
       ; " "
       ; Jstr.to_string @@ Jv.Error.message e
       ; "\n"
       ; Jstr.to_string @@ Jv.Error.stack e
       ]

let new_ cl = Jv.(new' (get global cl))

let stream_of_string str =
  let str = str |> Brr.Tarray.of_binary_jstr |> Result.get_ok |> Brr.Tarray.to_jv in
  let stream =
    new_
      "ReadableStream"
      Jv.
        [| obj
             [| ( "start"
                , callback ~arity:1 (fun controller ->
                    let _ = call controller "enqueue" [| str |] in
                    let _ = call controller "close" [||] in
                    ()) )
             |]
        |]
  in
  stream

module Decompress_browser = struct
  (** This module contains binding to the browser string compression api. It is
      much faster than using an OCaml library, and does not require sending code
      over the network. *)

  let string_of_stream stream =
    let buffer = Buffer.create 128 in
    let append str =
      Buffer.add_string buffer (str |> Brr.Tarray.of_jv |> Brr.Tarray.to_string)
    in
    let open Jv in
    let reader = call stream "getReader" [||] in
    let open Fut.Syntax in
    let rec read_step obj =
      let done_ = get obj "done" |> to_bool in
      let str = get obj "value" in
      if not done_
      then (
        append str ;
        read ())
      else Fut.return ()
    and read () : unit Fut.t =
      let read = call reader "read" [||] in
      let promise = Fut.of_promise ~ok:Fun.id read in
      Fut.bind promise (function
        | Ok v -> read_step v
        | Error e ->
          print_endline "error in string_of_stream" ;
          print_error e ;
          Fut.return ())
    in
    let+ () = read () in
    let r = Buffer.contents buffer in
    r

  let inflate str =
    let dekompressor = Jv.(new_ "DecompressionStream" [| of_string "deflate" |]) in
    let str = Jv.(call global "atob" [| str |]) |> Jv.to_jstr in
    let stream = stream_of_string str in
    let decompressed_stream = Jv.call stream "pipeThrough" [| dekompressor |] in
    string_of_stream decompressed_stream
end

let db =
  Jv.(Decompress_browser.inflate @@ call global "sherlodoc_db" [||])
  |> Fut.map (fun str -> [ Marshal.from_string str 0 ])

let string_of_kind =
  let open Db.Entry.Kind in
  let open Odoc_html_frontend in
  function
  | Db.Entry.Kind.Doc -> kind_doc
  | Type_decl _ -> kind_typedecl
  | Module -> kind_module
  | Exception _ -> kind_exception
  | Class_type -> kind_class_type
  | Method -> kind_method
  | Class -> kind_class
  | Type_extension -> kind_extension
  | Extension_constructor _ -> kind_extension_constructor
  | Module_type -> kind_module_type
  | Constructor _ -> kind_constructor
  | Field _ -> kind_field
  | Val _ -> kind_value

let search message db =
  let query = Jv.get message "data" in
  let query = query |> Jv.to_jstr |> Jstr.to_string in
  let results =
    Query.Blocking.search ~shards:db { Query.query; packages = []; limit = 50 }
  in
  let _ =
    Jv.(apply (get global "postMessage"))
      [| Jv.of_list
           (fun Db.Entry.{ name; rhs; doc_html; kind; url; _ } ->
             let typedecl_params =
               match kind with
               | Db.Entry.Kind.Type_decl args -> args
               | _ -> None
             in
             let prefix_name, name =
               match kind with
               | Db.Entry.Kind.Doc -> None, None
               | _ -> begin
                 match List.rev (String.split_on_char '.' name) with
                 | [] -> None, None
                 | [ hd ] -> None, Some hd
                 | hd :: tl -> Some (String.concat "." (List.rev tl)), Some hd
               end
             in
             let kind = string_of_kind kind in
             let html =
               Tyxml.Html.string_of_list
               @@ Odoc_html_frontend.of_strings
                    ~kind
                    ~prefix_name
                    ~name
                    ~typedecl_params
                    ~rhs
                    ~doc:doc_html
             in
             Jv.obj [| "html", Jv.of_string html; "url", Jv.of_string url |])
           results
      |]
  in
  ()

let don't_wait_for fut = Fut.await fut Fun.id

let search message =
  don't_wait_for
  @@
  let open Fut.Syntax in
  let+ db = db in
  search message db

let main () =
  let module J' = Jstr in
  let o = Jv.callback ~arity:1 search in
  Jv.(set global "onmessage" o)

let _ = main ()
