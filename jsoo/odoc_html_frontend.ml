(* copy-pasted from odoc/src/search/odoc_html_frontend.ml *)

let of_strings ~kind ~prefix_name ~name ~rhs ~typedecl_params ~doc =
  let open Tyxml.Html in
  let kind = code ~a:[ a_class [ "entry-kind" ] ] [ txt kind ]
  and typedecl_params =
    match typedecl_params with
    | None -> []
    | Some p ->
      [ span
          ~a:
            [ a_class
                [ (* the parameter of the typedecl are highlighted as if part of main entry name. *)
                  "entry-name"
                ]
            ]
          [ txt (p ^ " ") ]
      ]
  and prefix_name =
    match prefix_name with
    | None -> []
    | Some "" -> []
    | Some prefix_name ->
      [ span ~a:[ a_class [ "prefix-name" ] ] [ txt (prefix_name ^ ".") ] ]
  and name =
    match name with
    | Some name -> [ span ~a:[ a_class [ "entry-name" ] ] [ txt name ] ]
    | None -> []
  and rhs =
    match rhs with
    | None -> []
    | Some rhs -> [ code ~a:[ a_class [ "entry-rhs" ] ] [ txt rhs ] ]
  in
  [ kind
  ; code ~a:[ a_class [ "entry-title" ] ] (typedecl_params @ prefix_name @ name @ rhs)
  ; div ~a:[ a_class [ "entry-comment" ] ] [ Unsafe.data doc ]
  ]

let kind_doc = "doc"
let kind_typedecl = "type"
let kind_module = "mod"
let kind_exception = "exn"
let kind_class_type = "class"
let kind_class = "class"
let kind_method = "meth"
let kind_extension_constructor = "cons"
let kind_module_type = "sig"
let kind_constructor = "cons"
let kind_field = "field"
let kind_value = "val"
let kind_extension = "ext"
