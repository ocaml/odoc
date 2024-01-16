let empty_string = String.make 0 '_'

let non_empty_string s =
  (* to protect against `ancient` segfaulting on statically allocated values *)
  if s = "" then empty_string else s

module Kind = struct
  type t =
    | Doc
    | Module
    | Module_type
    | Class
    | Class_type
    | Method
    | Val of Typexpr.t
    | Type_decl of string option
    | Type_extension
    | Extension_constructor of Typexpr.t
    | Exception of Typexpr.t
    | Constructor of Typexpr.t
    | Field of Typexpr.t

  let equal = ( = )

  let get_type t =
    match t with
    | Val typ | Extension_constructor typ | Exception typ | Constructor typ | Field typ ->
      Some typ
    | Doc | Module | Module_type | Class | Class_type | Method | Type_decl _
    | Type_extension ->
      None
end

module Package = struct
  type t =
    { name : string
    ; version : string
    }

  let v ~name ~version =
    { name = non_empty_string name; version = non_empty_string version }

  let compare a b = String.compare a.name b.name
  let link { name; version } = "https://ocaml.org/p/" ^ name ^ "/" ^ version
end

type t =
  { name : string
  ; rhs : string option
  ; url : string
  ; kind : Kind.t
  ; cost : int
  ; doc_html : string
  ; pkg : Package.t
  }

let string_compare_shorter a b =
  match Int.compare (String.length a) (String.length b) with
  | 0 -> String.compare a b
  | c -> c

let structural_compare a b =
  match string_compare_shorter a.name b.name with
  | 0 -> begin
    match Package.compare a.pkg b.pkg with
    | 0 -> begin
      match Stdlib.compare a.kind b.kind with
      | 0 -> begin
        match string_compare_shorter a.doc_html b.doc_html with
        | 0 -> String.compare a.url b.url
        | c -> c
      end
      | c -> c
    end
    | c -> c
  end
  | c -> c

let compare a b =
  if a == b
  then 0
  else begin
    match Int.compare a.cost b.cost with
    | 0 -> structural_compare a b
    | cmp -> cmp
  end

let equal a b = compare a b = 0

let stdlib_link ~name t =
  let path, hashref =
    match List.rev name, String.index_opt t.url '#' with
    | _ :: path, Some idx ->
      let idx = idx + 1 in
      let tgt =
        match String.index_from_opt t.url idx '-' with
        | None -> String.sub t.url idx (String.length t.url - idx)
        | Some jdx ->
          let kind = String.sub t.url idx (jdx - idx) in
          let jdx = jdx + 1 in
          let target = String.sub t.url jdx (String.length t.url - jdx) in
          String.uppercase_ascii kind ^ target
      in
      path, "#" ^ tgt
    | path, _ -> path, ""
  in
  let path = String.concat "." (List.rev path) in
  "https://v2.ocaml.org/releases/5.1/api/" ^ path ^ ".html" ^ hashref

let link t =
  let fullname = String.split_on_char '.' t.name in
  match fullname with
  | "Stdlib" :: name -> stdlib_link ~name t
  | _ ->
    let pkg_link = Package.link t.pkg in
    let rec align n ys =
      match ys with
      | _ when n = 0 -> []
      | [] -> []
      | y :: ys -> y :: align (n - 1) ys
    in
    let length = List.length fullname in
    let length =
      match String.index_opt t.url '#' with
      | None -> length + 1
      | Some idx ->
        let tgt = String.sub t.url idx (String.length t.url - idx) in
        let count = ref 0 in
        String.iter
          (function
            | '.' -> incr count
            | _ -> ())
          tgt ;
        length - !count
    in
    let path = align length (List.rev (String.split_on_char '/' t.url)) in
    let path = String.concat "/" (List.rev path) in
    pkg_link ^ "/doc/" ^ path

let v ~name ~kind ~cost ~rhs ~doc_html ~url ~pkg () =
  { name = non_empty_string name
  ; kind
  ; url = non_empty_string url
  ; cost
  ; doc_html = non_empty_string doc_html
  ; pkg
  ; rhs = Option.map non_empty_string rhs
  }
