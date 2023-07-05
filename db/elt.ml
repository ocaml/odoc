type type_path = string list list

(** A type can viewed as a tree.
            [a -> b -> c * d] is the following tree :
            {[ ->
              |- a
              |- ->
                 |- b
                 |- *
                    |- c
                    |- d 
            ]} 
            {!type_paths} is the list of paths from root to leaf in the tree of 
            the type. There is an annotation to indicate the child's position.
            Here it would be :
            [ [["->";"0"; "a"];["->"; "1"; "->"; "0"; "b"]; ...] ]
            
            It is used to sort results. *)

module Kind = struct
  type 'a abstract =
    | Doc
    | TypeDecl
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

  type t = type_path abstract

  let equal = ( = )
  let doc = Doc
  let type_decl = TypeDecl
  let module_ = Module
  let exception_ type_path = Exception type_path
  let class_type = Class_type
  let method_ = Method
  let class_ = Class
  let type_extension = TypeExtension
  let extension_constructor type_path = ExtensionConstructor type_path
  let module_type = ModuleType
  let constructor type_path = Constructor type_path
  let field type_path = Field type_path
  let val_ type_path = Val type_path

  let to_string = function
    | Doc -> "doc"
    | TypeDecl -> "type"
    | Module -> "mod"
    | Exception _ -> "exn"
    | Class_type -> "class"
    | Method -> "meth"
    | Class -> "class"
    | TypeExtension -> "type"
    | ExtensionConstructor _ -> "cons"
    | ModuleType -> "sig"
    | Constructor _ -> "cons"
    | Field _ -> "field"
    | Val _ -> "val"
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
    }

  let compare_pkg { name; version = _ } (b : package) =
    String.compare name b.name

  let structural_compare a b =
    begin
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

let v ~name ~kind ~score ~rhs ~doc_html ~url ?(pkg = None) () =
  { name; kind; url; score; doc_html; pkg; rhs }
