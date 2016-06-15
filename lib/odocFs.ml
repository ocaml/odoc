type directory = string
type file = string

let (^/) = Printf.sprintf "%s/%s"

module Directory = struct
  type t = directory

  let create ~parent ~name =
    let path = parent ^/ name in
    begin if not (Sys.file_exists path) then
      Unix.mkdir path 0o755
    else if not (Sys.is_directory path) then
      invalid_arg "not a directory"
    end;
    path

  let of_string s = s
  let to_string t = t

  let ls t = Sys.readdir t |> Array.to_list |> List.map ~f:((^/) t)
end

module File = struct
  type t = file

  let create ~directory ~name = directory ^/ name

  let of_string s = s
  let to_string t = t
end
