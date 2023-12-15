module Kind = struct
  type 'a abstract =
    | Doc
    | TypeDecl of string option
    | Module
    | Exception of 'a
    | Class_type
    | Method
    | Class
    | TypeExtension
    | ExtensionConstructor of 'a
    | ModuleType
    | Constructor of 'a
    | Field of 'a
    | Val of 'a

  type t = Typexpr.t abstract

  let equal = ( = )
  let doc = Doc
  let type_decl args = TypeDecl args
  let module_ = Module
  let exception_ typ = Exception typ
  let class_type = Class_type
  let method_ = Method
  let class_ = Class
  let type_extension = TypeExtension
  let extension_constructor typ = ExtensionConstructor typ
  let module_type = ModuleType
  let constructor typ = Constructor typ
  let field typ = Field typ
  let val_ typ = Val typ
end

module Package = struct
  type t =
    { name : string
    ; version : string
    }

  let v ~name ~version = { name; version }
end

type package = Package.t =
  { name : string
  ; version : string
  }

type kind = Kind.t

module T = struct
  type t =
    { name : string
    ; rhs : string option
    ; url : string
    ; kind : Kind.t
    ; score : int
    ; doc_html : string
    ; pkg : Package.t option
    ; is_from_module_type : bool
    }

  let compare_pkg { name; version = _ } (b : package) =
    String.compare name b.name

  let structural_compare a b =
    begin
      match Int.compare (String.length a.name) (String.length b.name) with
      | 0 -> begin
          match String.compare a.name b.name with
          | 0 -> begin
              match Option.compare compare_pkg a.pkg b.pkg with
              | 0 -> begin
                  match Stdlib.compare a.kind b.kind with
                  | 0 -> Stdlib.compare a.url b.url
                  | c -> c
                end
              | c -> c
            end
          | c -> c
        end
      | c -> c
    end

  let compare a b =
    if a == b
    then 0
    else
      let cmp = Int.compare a.score b.score in
      if cmp = 0 then structural_compare a b else cmp
end

include T

let equal a b = compare a b = 0

module Set = Set.Make (T)

(** Array of elts. For use in functors that require a type [t] and not ['a t].*)
module Array = struct
  type elt = t
  type nonrec t = t array

  let is_empty arr = Array.length arr = 0

  let of_list arr =
    let arr = Array.of_list arr in
    Array.sort compare arr ;
    arr

  let equal_elt = equal
end

let pkg_link { pkg; _ } =
  match pkg with
  | None -> None
  | Some { name; version } ->
      Some (Printf.sprintf "https://ocaml.org/p/%s/%s" name version)

let link t =
  match pkg_link t with
  | None -> None
  | Some pkg_link ->
      let name, path =
        match List.rev (String.split_on_char '.' t.name) with
        | name :: path -> name, String.concat "/" (List.rev path)
        | _ -> "", ""
      in
      Some (pkg_link ^ "/doc/" ^ path ^ "/index.html#val-" ^ name)

let v ~name ~kind ~score ~rhs ~doc_html ~url ~is_from_module_type ?(pkg = None)
    () =
  { name; kind; url; score; doc_html; pkg; rhs; is_from_module_type }
