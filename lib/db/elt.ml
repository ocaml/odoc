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
  | TypeDecl of { type_decl : string }
  | Module
  | Exception
  | Class_type
  | Method
  | Class
  | TypeExtension
  | ExtensionConstructor
  | ModuleType
  | Constructor of
      { type_ : string
      ; type_paths : type_path
      }
  | Field of
      { type_ : string
      ; type_paths : type_path
      }
  | Val of
      { type_ : string
      ; type_paths : type_path
      }

type package =
  { name : string
  ; version : string
  }

module T = struct
  type t =
    { name : string
    ; kind : kind
    ; has_doc: bool
    ; pkg : package option
    ; json_output : string
    }

  let compare_pkg { name; version = _ } (b : package) =
    String.compare name b.name

  let compare a b =
    begin
      match String.compare a.name b.name with
      | 0 -> begin
          match Option.compare compare_pkg a.pkg b.pkg with
          | 0 -> Stdlib.compare a.kind b.kind
          | c -> c
        end
      | c -> c
    end

  let compare a b = if a == b then 0 else compare a b
end

include T

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

module Set = Set.Make (T)
