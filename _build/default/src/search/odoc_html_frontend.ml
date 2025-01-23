module Html : sig
  type t

  val string_of_list : t list -> string

  type attr

  val a_class : string list -> attr
  val code : a:attr list -> t list -> t
  val span : a:attr list -> t list -> t
  val div : a:attr list -> t list -> t
  val txt : string -> t

  module Unsafe : sig
    val data : string -> t
  end
end = struct
  type t = Raw of string | Txt of string | Concat of t list

  let add_escape_string buf s =
    (* https://discuss.ocaml.org/t/html-encoding-of-string/4289/4 *)
    let add = Buffer.add_string buf in
    let len = String.length s in
    let max_idx = len - 1 in
    let flush start i =
      if start < len then Buffer.add_substring buf s start (i - start)
    in
    let rec loop start i =
      if i > max_idx then flush start i
      else
        match String.get s i with
        | '&' -> escape "&amp;" start i
        | '<' -> escape "&lt;" start i
        | '>' -> escape "&gt;" start i
        | '\'' -> escape "&apos;" start i
        | '"' -> escape "&quot;" start i
        | '@' -> escape "&commat;" start i
        | _ -> loop start (i + 1)
    and escape amperstr start i =
      flush start i;
      add amperstr;
      let next = i + 1 in
      loop next next
    in
    loop 0 0

  let to_string t =
    let buf = Buffer.create 16 in
    let rec go = function
      | Raw s -> Buffer.add_string buf s
      | Txt s -> add_escape_string buf s
      | Concat xs -> List.iter go xs
    in
    go t;
    Buffer.contents buf

  let string_of_list lst = to_string (Concat lst)

  type attr = t

  let a_class lst =
    Concat [ Raw "class=\""; Txt (String.concat " " lst); Raw "\"" ]

  let attrs = function [] -> Concat [] | xs -> Concat (Raw " " :: xs)

  let block name ~a body =
    let name = Raw name in
    Concat
      [ Raw "<"; name; attrs a; Raw ">"; Concat body; Raw "</"; name; Raw ">" ]

  let code = block "code"
  let span = block "span"
  let div = block "div"
  let txt s = Txt s

  module Unsafe = struct
    let data s = Raw s
  end
end

let of_strings ~kind ~prefix_name ~name ~rhs ~typedecl_params ~doc =
  let open Html in
  let kind = code ~a:[ a_class [ "entry-kind" ] ] [ txt kind ]
  and typedecl_params =
    match typedecl_params with
    | None -> []
    | Some p ->
        [
          span
            ~a:
              [
                a_class
                  [
                    (* the parameter of the typedecl are highlighted as if part of main entry name. *)
                    "entry-name";
                  ];
              ]
            [ txt (p ^ " ") ];
        ]
  and prefix_name =
    match prefix_name with
    | None -> []
    | Some "" -> []
    | Some prefix_name ->
        [ span ~a:[ a_class [ "prefix-name" ] ] [ txt prefix_name ] ]
  and name =
    match name with
    | Some name -> [ span ~a:[ a_class [ "entry-name" ] ] [ txt name ] ]
    | None -> []
  and rhs =
    match rhs with
    | None -> []
    | Some rhs -> [ code ~a:[ a_class [ "entry-rhs" ] ] [ txt rhs ] ]
  in
  Html.string_of_list
    [
      kind;
      code
        ~a:[ a_class [ "entry-title" ] ]
        (typedecl_params @ prefix_name @ name @ rhs);
      div ~a:[ a_class [ "entry-comment" ] ] [ Unsafe.data doc ];
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
