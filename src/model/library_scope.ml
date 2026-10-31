type t = Scoped of string list | First of string list | Any

let any = Any

let of_libraries = function
  | [] -> Any
  | libs -> Scoped (List.sort_uniq String.compare libs)

let first = function [] -> Any | libs -> First libs

let pp fmt t =
  let libs =
    Format.(pp_print_list ~pp_sep:(fun f () -> fprintf f "; ") pp_print_string)
  in
  match t with
  | Scoped l -> Format.fprintf fmt "Scoped [%a]" libs l
  | First l -> Format.fprintf fmt "First [%a]" libs l
  | Any -> Format.fprintf fmt "Any"
