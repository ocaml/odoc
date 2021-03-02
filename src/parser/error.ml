type t = { location : Location.span; message : string }

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let kmake k ?suggestion format =
  format
  |> kasprintf (fun message ->
         match suggestion with
         | None -> k message
         | Some suggestion -> k (message ^ "\nSuggestion: " ^ suggestion))

let make ?suggestion format =
  let k message location = { location; message } in
  kmake k ?suggestion format

type 'a with_warnings = { value : 'a; warnings : t list }

type warning_accumulator = t list ref

let accumulate_warnings f =
  let warnings = ref [] in
  let value = f warnings in
  { value; warnings = List.rev !warnings }

let warning accumulator error = accumulator := error :: !accumulator
