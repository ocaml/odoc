type t = int option
let rec f ?(optional : t) () = f ?optional ()
