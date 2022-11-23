(* Shared utility functions *)

(* = Option.fold *)
let fold_option ~none ~some = function Some x -> some x | None -> none

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = list_concat_map ?sep ~f xs in
      match sep with None -> hd @ tl | Some sep -> hd @ (sep :: tl))

let optional_elt f ?a = function [] -> [] | l -> [ f ?a l ]

module Json = struct
  type json =
    [ `Null
    | `Bool of bool
    | `Float of float
    | `String of string
    | `Array of json list
    | `Object of (string * json) list ]

  let rec buffer_add_json b = function
    | `Null -> Buffer.add_string b "null"
    | `Bool bool -> Buffer.add_string b (if bool then "true" else "false")
    | `Float f -> Buffer.add_string b (Printf.sprintf "%.16g" f)
    | `String s -> buffer_add_json_string b s
    | `Array els -> (
        match els with
        | [] -> Buffer.add_string b "[]"
        | el :: els ->
            let add_sep_el b e =
              Buffer.add_char b ',';
              buffer_add_json b e
            in
            Buffer.add_char b '[';
            buffer_add_json b el;
            List.iter (add_sep_el b) els;
            Buffer.add_char b ']')
    | `Object mems -> (
        match mems with
        | [] -> Buffer.add_string b "{}"
        | mem :: mems ->
            let add_mem b (k, v) =
              buffer_add_json_string b k;
              Buffer.add_char b ':';
              buffer_add_json b v
            in
            let add_sep_mem b mem =
              Buffer.add_char b ',';
              add_mem b mem
            in
            Buffer.add_char b '{';
            add_mem b mem;
            List.iter (add_sep_mem b) mems;
            Buffer.add_char b '}')

  and buffer_add_json_string b s =
    let is_control = function
      | '\x00' .. '\x1F' | '\x7F' -> true
      | _ -> false
    in
    let len = String.length s in
    let max_idx = len - 1 in
    let flush b start i =
      if start < len then Buffer.add_substring b s start (i - start)
    in
    let rec loop start i =
      match i > max_idx with
      | true -> flush b start i
      | false -> (
          let next = i + 1 in
          match String.get s i with
          | '"' ->
              flush b start i;
              Buffer.add_string b "\\\"";
              loop next next
          | '\\' ->
              flush b start i;
              Buffer.add_string b "\\\\";
              loop next next
          | c when is_control c ->
              flush b start i;
              Buffer.add_string b (Printf.sprintf "\\u%04X" (Char.code c));
              loop next next
          | _c -> loop start next)
    in
    Buffer.add_char b '"';
    loop 0 0;
    Buffer.add_char b '"'

  let to_string json =
    let b = Buffer.create 1024 in
    buffer_add_json b json;
    Buffer.contents b
end
