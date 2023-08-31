let print_error e =
  let open Jv.Error in
  Printf.eprintf "Error : %s %s\n%s%!"
    (Jstr.to_string @@ name e)
    (Jstr.to_string @@ message e)
    (Jstr.to_string @@ stack e)

let new_ cl = Jv.(new' (get global cl))

let stream_of_string str =
  let str =
    str |> Brr.Tarray.of_binary_jstr |> Result.get_ok |> Brr.Tarray.to_jv
  in
  let stream =
    new_ "ReadableStream"
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

let don't_wait_for fut = Fut.await fut Fun.id

let string_of_stream stream =
  print_endline "string_of_stream" ;
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
      | Ok v ->
          (* print_endline "Ok v" ; *)
          read_step v
      | Error e ->
          print_endline "error in string_of_stream" ;
          print_error e ;
          Fut.return ())
  in
  let+ () = read () in
  let r = Buffer.contents buffer in
  (* Printf.printf "Inflated to size %i\n%!" (String.length r) ; *)
  r

let inflate str =
  (* print_endline "inflating" ; *)
  let dekompressor =
    Jv.(new_ "DecompressionStream" [| of_string "deflate" |])
  in
  let str = Jv.(call global "atob" [| str |]) |> Jv.to_jstr in
  (* Printf.printf "String has size %i\n%!" (str |> Jstr.length) ; *)
  let stream = stream_of_string str in
  let decompressed_stream = Jv.call stream "pipeThrough" [| dekompressor |] in
  string_of_stream decompressed_stream

let db =
  Jv.(inflate @@ call global "sherlodoc_db" [||]) |> Fut.map Storage_js.load

let string_of_kind =
  let open Db.Elt.Kind in
  let open Odoc_search.Html in
  function
  | Db.Elt.Kind.Doc -> kind_doc
  | TypeDecl _ -> kind_typedecl
  | Module -> kind_module
  | Exception _ -> kind_exception
  | Class_type -> kind_class_type
  | Method -> kind_method
  | Class -> kind_class
  | TypeExtension -> kind_extension
  | ExtensionConstructor _ -> kind_extension_constructor
  | ModuleType -> kind_module_type
  | Constructor _ -> kind_constructor
  | Field _ -> kind_field
  | Val _ -> kind_value

let search message =
  don't_wait_for
  @@
  let open Fut.Syntax in
  let+ db = db in
  let query = Jv.get message "data" in
  let query = query |> Jv.to_jstr |> Jstr.to_string in
  let _pretty_query, results =
    Query.(api ~shards:db { query; packages = []; limit = 50 })
  in
  Printf.printf "Got %i results\n%!" (List.length results) ;
  let _ =
    Jv.(apply (get global "postMessage"))
      [| Jv.of_list
           (fun Db.Elt.{ name; rhs; doc_html; kind; url; _ } ->
             let typedecl_params =
               match kind with
               | Db.Elt.Kind.TypeDecl args -> args
               | _ -> None
             in
             let prefix_name, name =
               match kind with
               | Db.Elt.Kind.Doc -> None, None
               | _ ->
                   let rev_name =
                     name |> String.split_on_char '.' |> List.rev
                   in
                   ( rev_name |> List.tl |> List.rev |> String.concat "."
                     |> Option.some
                   , rev_name |> List.hd |> Option.some )
             in
             let kind = string_of_kind kind in

             let html =
               Odoc_search.Html.of_strings ~kind ~prefix_name ~name
                 ~typedecl_params ~rhs ~doc:doc_html
               |> List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ()))
               |> String.concat "\n"
             in
             Jv.obj [| "html", Jv.of_string html; "url", Jv.of_string url |])
           results
      |]
  in
  ()

let main () =
  let module J' = Jstr in
  let o = Jv.callback ~arity:1 search in
  Jv.(set global "onmessage" o)

let _ = main ()
