module Html = Tyxml.Html

(* Shared utility functions *)

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep :: tl

let optional_code children =
  match children with
  | [] -> []
  | children -> [ Html.code children ]

(**
let configure_refmt () =
  let constructorLists = [] in
  Reason_pprint_ast.configure ~assumeExplicitArity:true ~constructorLists ~width:80
  (** This function helps configuring the printing of ambiguous arity syntax
      See https://reasonml.github.io/docs/en/convert-from-ocaml.html#constructor-syntax-fix for details **)
  (*TODO: Provide heuristics via ODOC_REFMT_HEURISTICS env *)

let reason_from_ocaml str =
  let _ = configure_refmt () in
  let ast = Lexing.from_string str |> Reason_toolchain.ML.implementation_with_comments in
  Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter ast;
  Format.flush_str_formatter ()

let ocaml_from_reason str =
  let _ = configure_refmt () in
  let ast = Lexing.from_string str |> Reason_toolchain.RE.implementation_with_comments in
  (*TODO: Use ocamlformat for better syntax? *)
  Reason_toolchain.ML.print_implementation_with_comments Format.str_formatter ast;
  Format.flush_str_formatter ()
**)
