let empty_string = String.make 0 '_'
let non_empty_string s = if s = "" then empty_string else s

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
  let link { name; version } = Printf.sprintf "https://ocaml.org/p/%s/%s" name version
end

module T = struct
  type t =
    { name : string
    ; rhs : string option
    ; url : string
    ; kind : Kind.t
    ; cost : int
    ; doc_html : string
    ; pkg : Package.t
    ; is_from_module_type : bool
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
end

include T
module Set = Set.Make (T)

(** Array of elts. For use in functors that require a type [t] and not ['a t].*)
module Array = struct
  type elt = t
  type t = elt array option

  let is_empty = function
    | None -> true
    | Some arr ->
      assert (Array.length arr > 0) ;
      false

  let empty = None

  let minimum = function
    | None -> None
    | Some arr -> Some arr.(0)

  let of_list arr =
    let arr = Array.of_list arr in
    Array.sort compare arr ;
    if Array.length arr = 0 then empty else Some arr

  let equal_elt = T.equal
  let compare_elt = T.compare
end

let link t =
  let pkg_link = Package.link t.pkg in
  let name, path =
    match List.rev (String.split_on_char '.' t.name) with
    | name :: path -> name, String.concat "/" (List.rev path)
    | _ -> "", ""
  in
  pkg_link ^ "/doc/" ^ path ^ "/index.html#val-" ^ name

let v ~name ~kind ~cost ~rhs ~doc_html ~url ~is_from_module_type ~pkg () =
  { name = non_empty_string name
  ; kind
  ; url = non_empty_string url
  ; cost
  ; doc_html = non_empty_string doc_html
  ; pkg
  ; rhs = Option.map non_empty_string rhs
  ; is_from_module_type
  }
