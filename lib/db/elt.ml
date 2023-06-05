open Common

type displayable =
  { html : string
  ; txt : string
  }

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

type kind =
  | Doc
  | TypeDecl
  | Module
  | Exception
  | Class_type
  | Method
  | Class
  | TypeExtension
  | ExtensionConstructor
  | ModuleType
  | Constructor of type_path
  | Field of type_path
  | Val of type_path

type package =
  { name : string
  ; version : string
  }

module T = struct
  type t =
    { name : string
    ; kind : kind
    ; has_doc : bool
    ; pkg : package option
    ; json_display : string
    }

  let compare_pkg { name; version = _ } (b : package) =
    String.compare name b.name

  let generic_cost ~ignore_no_doc name has_doc =
    String.length name
    (* + (5 * List.length path) TODO : restore depth based ordering *)
    + (if ignore_no_doc || has_doc then 0 else 1000)
    + if String.starts_with ~prefix:"Stdlib." name then -100 else 0

  let type_cost paths =
    paths |> List.concat |> List.map String.length |> List.fold_left ( + ) 0

  let kind_cost (kind : kind) =
    match kind with
    | Constructor type_path | Field type_path | Val type_path ->
        type_cost type_path
    | Doc -> 400
    | TypeDecl | Module | Exception | Class_type | Method | Class
    | TypeExtension | ExtensionConstructor | ModuleType ->
        200

  let cost { name; kind; has_doc; pkg = _; json_display = _ } =
    let ignore_no_doc =
      match kind with
      | Module | ModuleType -> true
      | _ -> false
    in
    (* TODO : use entry cost *)
    generic_cost ~ignore_no_doc name has_doc + kind_cost kind

  let structural_compare a b =
    begin
      match String.compare a.name b.name with
      | 0 -> begin
          match Option.compare compare_pkg a.pkg b.pkg with
          | 0 -> Stdlib.compare a.kind b.kind
          | c -> c
        end
      | c -> c
    end

  let compare a b =
    if a == b
    then 0
    else
      let cost_a = cost a in
      let cost_b = cost b in
      let cmp = Int.compare cost_a cost_b in
      if cmp = 0 then structural_compare a b else cmp
end

include T

let equal a b = compare a b = 0
let ( = ) = equal
let ( < ) e e' = compare e e' < 0
let ( <= ) e e' = compare e e' <= 0
let ( > ) e e' = compare e e' > 0
let ( >= ) e e' = compare e e' >= 0
let hash : t -> int = Hashtbl.hash

module Set = Set.Make (T)

let pkg_link { pkg; _ } =
  let open Option.O in
  let+ { name; version } = pkg in
  Printf.sprintf "https://ocaml.org/p/%s/%s" name version

let link t =
  let open Option.O in
  let name, path =
    match List.rev (String.split_on_char '.' t.name) with
    | name :: path -> name, String.concat "/" (List.rev path)
    | _ -> "", ""
  in
  let+ pkg_link = pkg_link t in
  pkg_link ^ "/doc/" ^ path ^ "/index.html#val-" ^ name

module Kind = struct
  type t = kind

  let doc = Doc
  let type_decl = TypeDecl
  let module_ = Module
  let exception_ = Exception
  let class_type = Class_type
  let method_ = Method
  let class_ = Class
  let type_extension = TypeExtension
  let extension_constructor = ExtensionConstructor
  let module_type = ModuleType
  let constructor type_path = Constructor type_path
  let field type_path = Field type_path
  let val_ type_path = Val type_path
end

module Package = struct
  type t = package

  let v ~name ~version = let version = version in

                         { name; version }
end

let v ~name ~kind ~has_doc ?(pkg = None) ~json_display () =
  let json_display = json_display in
  { name; kind; has_doc; pkg; json_display }
