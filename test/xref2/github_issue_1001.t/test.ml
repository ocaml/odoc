type easy = int option
let rec f ?(optional : easy) () = f ?optional ()

type 'a opt = 'a option
type hard = int opt
let rec g ?(optional : hard) () = g ?optional ()
