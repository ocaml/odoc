  $ ODOCLS=$(find ../docs/odoc/base/ -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  81 type 'a Base.list = 'a List.t
  93 type 'a Base.Export.list = 'a List.t
  101 type 'a Base.List.t = 'a list
  104 mod Base.List
  104 mod Caml.List
  108 val Base.List.rev : 'a t -> 'a t
  109 val Base.List.hd_exn : 'a t -> 'a
  109 val Base.List.return : 'a -> 'a t
  110 val Base.Bytes.to_list : t -> char list
  111 val Base.List.join : 'a t t -> 'a t
  111 val Base.List.tl_exn : 'a t -> 'a t
  111 val Base.Queue.of_list : 'a list -> 'a t
  111 val Base.Stack.of_list : 'a list -> 'a t
  113 val Base.List.concat : 'a t t -> 'a t
  113 mod Shadow_stdlib.List
  114 val Base.List.last : 'a t -> 'a option
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  125 mod Base.ListLabels
  125 mod Caml.ListLabels
  344 mod Base
  $ sherlodoc index --favoured-prefixes=Base $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  31 type 'a Base.list = 'a List.t
  43 type 'a Base.Export.list = 'a List.t
  51 type 'a Base.List.t = 'a list
  54 mod Base.List
  58 val Base.List.rev : 'a t -> 'a t
  59 val Base.List.hd_exn : 'a t -> 'a
  59 val Base.List.return : 'a -> 'a t
  60 val Base.Bytes.to_list : t -> char list
  61 val Base.List.join : 'a t t -> 'a t
  61 val Base.List.tl_exn : 'a t -> 'a t
  61 val Base.Queue.of_list : 'a list -> 'a t
  61 val Base.Stack.of_list : 'a list -> 'a t
  63 val Base.List.concat : 'a t t -> 'a t
  64 val Base.List.last : 'a t -> 'a option
  65 mod Base.List.Assoc
  65 mod Base.List.Infix
  65 cons Base.Sexp.t.List : t list -> t
  65 val Base.List.ignore_m : 'a t -> unit t
  66 val Base.List.drop : 'a t -> int -> 'a t
  66 val Base.List.take : 'a t -> int -> 'a t
  75 mod Base.ListLabels
  294 mod Base
  297 type Base.Nothing.t = 
  312 val Base.String.append : t -> t -> t
  314 val Base.Int.ascending : t -> t -> int
  $ sherlodoc index --favoured-prefixes=Caml $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  54 mod Caml.List
  75 mod Caml.ListLabels
  81 type 'a Base.list = 'a List.t
  93 type 'a Base.Export.list = 'a List.t
  101 type 'a Base.List.t = 'a list
  104 mod Base.List
  108 val Base.List.rev : 'a t -> 'a t
  109 val Base.List.hd_exn : 'a t -> 'a
  109 val Base.List.return : 'a -> 'a t
  110 val Base.Bytes.to_list : t -> char list
  111 val Base.List.join : 'a t t -> 'a t
  111 val Base.List.tl_exn : 'a t -> 'a t
  111 val Base.Queue.of_list : 'a list -> 'a t
  111 val Base.Stack.of_list : 'a list -> 'a t
  113 val Base.List.concat : 'a t t -> 'a t
  113 mod Shadow_stdlib.List
  114 val Base.List.last : 'a t -> 'a option
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  125 mod Base.ListLabels
  344 mod Base
  $ sherlodoc index --favoured-prefixes=Base,Caml $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  31 type 'a Base.list = 'a List.t
  43 type 'a Base.Export.list = 'a List.t
  51 type 'a Base.List.t = 'a list
  54 mod Base.List
  54 mod Caml.List
  58 val Base.List.rev : 'a t -> 'a t
  59 val Base.List.hd_exn : 'a t -> 'a
  59 val Base.List.return : 'a -> 'a t
  60 val Base.Bytes.to_list : t -> char list
  61 val Base.List.join : 'a t t -> 'a t
  61 val Base.List.tl_exn : 'a t -> 'a t
  61 val Base.Queue.of_list : 'a list -> 'a t
  61 val Base.Stack.of_list : 'a list -> 'a t
  63 val Base.List.concat : 'a t t -> 'a t
  64 val Base.List.last : 'a t -> 'a option
  65 mod Base.List.Assoc
  65 mod Base.List.Infix
  65 cons Base.Sexp.t.List : t list -> t
  65 val Base.List.ignore_m : 'a t -> unit t
  66 val Base.List.drop : 'a t -> int -> 'a t
  66 val Base.List.take : 'a t -> int -> 'a t
  75 mod Base.ListLabels
  75 mod Caml.ListLabels
  294 mod Base
  297 type Base.Nothing.t = 
  $ sherlodoc index $ODOCLS --favoured-prefixes "" > /dev/null
  $ sherlodoc search --print-cost "list"
  81 type 'a Base.list = 'a List.t
  93 type 'a Base.Export.list = 'a List.t
  101 type 'a Base.List.t = 'a list
  104 mod Base.List
  104 mod Caml.List
  108 val Base.List.rev : 'a t -> 'a t
  109 val Base.List.hd_exn : 'a t -> 'a
  109 val Base.List.return : 'a -> 'a t
  110 val Base.Bytes.to_list : t -> char list
  111 val Base.List.join : 'a t t -> 'a t
  111 val Base.List.tl_exn : 'a t -> 'a t
  111 val Base.Queue.of_list : 'a list -> 'a t
  111 val Base.Stack.of_list : 'a list -> 'a t
  113 val Base.List.concat : 'a t t -> 'a t
  113 mod Shadow_stdlib.List
  114 val Base.List.last : 'a t -> 'a option
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  125 mod Base.ListLabels
  125 mod Caml.ListLabels
  344 mod Base

Partial name search:
