(** Raw latex primitives:
    - macro definitions
    - env defitions
    - text escaping
*)

type pr = Format.formatter -> unit

type 'a with_options = ?options:pr list -> 'a

type ('a, 'b) tr = 'a Fmt.t -> 'b Fmt.t

type 'a t = ('a, 'a) tr

module Escape = struct
  let text ~code_hyphenation =
    let b = Buffer.create 17 in
    fun s ->
      for i = 0 to String.length s - 1 do
        match s.[i] with
        | '{' -> Buffer.add_string b "\\{"
        | '}' -> Buffer.add_string b "\\}"
        | '\\' -> Buffer.add_string b "\\textbackslash{}"
        | '%' -> Buffer.add_string b "\\%"
        | '~' -> Buffer.add_string b "\\textasciitilde{}"
        | '^' -> Buffer.add_string b "\\textasciicircum{}"
        | '_' ->
            if code_hyphenation then Buffer.add_string b {|\_\allowbreak{}|}
            else Buffer.add_string b {|\_|}
        | '.' when code_hyphenation -> Buffer.add_string b {|.\allowbreak{}|}
        | ';' when code_hyphenation -> Buffer.add_string b {|;\allowbreak{}|}
        | ',' when code_hyphenation -> Buffer.add_string b {|,\allowbreak{}|}
        | '&' -> Buffer.add_string b "\\&"
        | '#' -> Buffer.add_string b "\\#"
        | '$' -> Buffer.add_string b "\\$"
        | c -> Buffer.add_char b c
      done;
      let s = Buffer.contents b in
      Buffer.reset b;
      s

  let pp ~code_hyphenation ppf x =
    Format.pp_print_string ppf (text ~code_hyphenation x)

  let ref ppf s =
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '~' -> Fmt.pf ppf "+t+"
      | '&' -> Fmt.pf ppf "+a+"
      | '^' -> Fmt.pf ppf "+c+"
      | '%' -> Fmt.pf ppf "+p+"
      | '{' -> Fmt.pf ppf "+ob+"
      | '}' -> Fmt.pf ppf "+cb+"
      | '+' -> Fmt.pf ppf "+++"
      | c -> Fmt.pf ppf "%c" c
    done
end

let option ppf pp = Fmt.pf ppf "[%t]" pp

let create name ?(options = []) pp ppf content =
  Fmt.pf ppf {|\%s%a{%a}|} name (Fmt.list option) options pp content

let math name ppf = Fmt.pf ppf {|$\%s$|} name

let create2 name ?(options = []) pp_x pp_y ppf x y =
  Fmt.pf ppf {|\%s%a{%a}{%a}|} name (Fmt.list option) options pp_x x pp_y y

let bind pp x ppf = pp ppf x

let label ppf = create "label" Escape.ref ppf

let mbegin ?options = create "begin" ?options Fmt.string

let mend = create "end" Fmt.string

let code_fragment pp = create "ocamlcodefragment" pp

let break ppf level =
  let pre : _ format6 =
    match level with
    | Types.Aesthetic -> "%%"
    | Line -> {|\\|}
    | Separation -> {|\medbreak|}
    | _ -> ""
  in
  let post : _ format6 =
    match level with
    | Types.Line | Separation | Aesthetic | Simple -> ""
    | Paragraph -> "@,"
  in
  Fmt.pf ppf (pre ^^ "@," ^^ post)

let env name pp ?(with_break = false) ?(opts = []) ?(args = []) ppf content =
  mbegin ppf name;
  List.iter (Fmt.pf ppf "[%t]") opts;
  List.iter (Fmt.pf ppf "{%t}") args;
  pp ppf content;
  mend ppf name;
  break ppf (if with_break then Simple else Aesthetic)

let indent pp ppf x = env "ocamlindent" pp ppf x

let inline_code pp = create "ocamlinlinecode" pp

let verbatim ppf x = env "verbatim" Fmt.string ppf x

let pageref_star x = create "pageref*" Escape.ref x

let hyperref s = create "hyperref" ~options:[ bind Escape.ref s ]

let ref x = create "ref" Escape.ref x

let emph pp = create "emph" pp

let bold pp = create "bold" pp

let subscript pp = create "textsubscript" pp

let superscript pp = create "textsuperscript" pp

let code_block pp ppf x =
  let name = "ocamlcodeblock" in
  mbegin ppf name;
  Fmt.cut ppf ();
  pp ppf x;
  Fmt.cut ppf ();
  mend ppf name

let section pp = create "section" pp

let subsection pp = create "subsection" pp

let subsubsection pp = create "subsubsection" pp

let paragraph pp = create "paragraph" pp

let enumerate pp ppf x = env "enumerate" pp ppf x

let itemize pp ppf x = env "itemize" pp ppf x

let raw_description pp ppf x = env "description" pp ppf x

let href x pp ppf y =
  create2 "href" (Escape.pp ~code_hyphenation:false) pp ppf x y

let item ?options = create "item" ?options

let description pp ppf x =
  (* printing description inside a group make them more robust *)
  let group_printer d ppf = Fmt.pf ppf "{%a}" pp d in
  let elt ppf (d, elt) = item ~options:[ group_printer d ] pp ppf elt in
  let all ppf x =
    Fmt.pf ppf
      {|\kern-\topsep
\makeatletter\advance\%@topsepadd-\topsep\makeatother%% topsep is hardcoded
|};
    Fmt.list ~sep:(fun ppf () -> break ppf Aesthetic) elt ppf x
  in
  match x with
  | [] -> () (* empty description are not supported *)
  | _ :: _ -> raw_description all ppf x

let url ppf s =
  create "url" Fmt.string ppf (Escape.text ~code_hyphenation:false s)

let footnote x = create "footnote" url x

let rightarrow ppf = math "rightarrow" ppf

(** Latex uses forward slash even on Windows. *)
let latex_path ppf path =
  let path_s = String.concat "/" (Fpath.segs path) in
  Fmt.string ppf path_s

let input ppf x = create "input" latex_path ppf x

let ocamltabular ~column_desc pp ppf x =
  env "ocamltabular" ~args:[ column_desc ] pp ppf x

let small_table pp ppf (alignment, tbl) =
  let columns = match tbl with [] -> 1 | _ -> List.length (List.hd tbl) in
  let row ppf x =
    let ampersand ppf () = Fmt.pf ppf "& " in
    Fmt.list ~sep:ampersand pp ppf x;
    break ppf Line
  in
  let matrix ppf m = List.iter (row ppf) m in
  let column_desc =
    let pp_alignment ppf align =
      match align with
      | Odoc_document.Types.Table.Default -> Fmt.pf ppf "p"
      | Left -> Fmt.pf ppf "w{l}"
      | Right -> Fmt.pf ppf "w{r}"
      | Center -> Fmt.pf ppf "w{c}"
    in
    let cell ppf align =
      Fmt.pf ppf "%a{%.3f\\textwidth}" pp_alignment align
        (1.0 /. float_of_int columns)
    in
    match alignment with
    | None ->
        let rec repeat n s ppf =
          if n = 0 then () else Fmt.pf ppf "%t%t" s (repeat (n - 1) s)
        in
        repeat columns (fun ppf -> cell ppf Default)
    | Some alignment -> fun ppf -> List.iter (cell ppf) alignment
  in
  let table ppf tbl = ocamltabular ~column_desc matrix ppf tbl in
  (* we add line breaks to never insert tables between delimiters,
     to avoid rendering:
          | `A
       [  | `B   ]
          | `C
     or
       field_1: int;
     { field_2: int;     }
       field_3: int;
  *)
  break ppf Line;
  table ppf tbl;
  break ppf Line

let ocamltag tag pp ppf x = create2 "ocamltag" Fmt.string pp ppf tag x

let math ppf x = Fmt.pf ppf {|$%s$|} x

let equation ppf x =
  let name = "equation*" in
  mbegin ppf name;
  Fmt.cut ppf ();
  (* A blank line before \end{equation*} is a latex error,
     we trim on the right the user input to avoid any surprise *)
  let x = Astring.String.drop ~rev:true ~sat:Astring.Char.Ascii.is_white x in
  Fmt.string ppf x;
  Fmt.cut ppf ();
  mend ppf name
