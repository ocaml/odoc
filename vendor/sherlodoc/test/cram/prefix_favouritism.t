  $ ODOCLS=$(find ../docs/odoc/base/ -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  131 type 'a Base.list = 'a List.t
  143 type 'a Base.Export.list = 'a List.t
  151 type 'a Base.List.t = 'a list
  154 mod Base.List
  154 mod Caml.List
  158 val Base.List.rev : 'a t -> 'a t
  159 val Base.List.hd_exn : 'a t -> 'a
  159 val Base.List.return : 'a -> 'a t
  160 val Base.Bytes.to_list : t -> char list
  161 val Base.List.join : 'a t t -> 'a t
  161 val Base.List.tl_exn : 'a t -> 'a t
  161 val Base.Queue.of_list : 'a list -> 'a t
  161 val Base.Stack.of_list : 'a list -> 'a t
  163 val Base.List.concat : 'a t t -> 'a t
  163 mod Shadow_stdlib.List
  164 val Base.List.last : 'a t -> 'a option
  165 mod Base.List.Assoc
  165 mod Base.List.Infix
  165 cons Base.Sexp.t.List : t list -> t
  165 val Base.List.ignore_m : 'a t -> unit t
  166 val Base.List.drop : 'a t -> int -> 'a t
  166 val Base.List.take : 'a t -> int -> 'a t
  175 mod Caml.ListLabels
  394 mod Base
  397 type Base.Nothing.t = 
  $ sherlodoc index --favoured-prefixes=Base $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
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
  114 val Base.List.last : 'a t -> 'a option
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  344 mod Base
  347 type Base.Nothing.t = 
  362 val Base.String.append : t -> t -> t
  364 val Base.Int.ascending : t -> t -> int
  365 val Base.Bool.ascending : t -> t -> int
  $ sherlodoc index --favoured-prefixes=Caml $ODOCLS > /dev/null
  $ sherlodoc search --print-cost "list"
  104 mod Caml.List
  125 mod Caml.ListLabels
  131 type 'a Base.list = 'a List.t
  143 type 'a Base.Export.list = 'a List.t
  151 type 'a Base.List.t = 'a list
  154 mod Base.List
  158 val Base.List.rev : 'a t -> 'a t
  159 val Base.List.hd_exn : 'a t -> 'a
  159 val Base.List.return : 'a -> 'a t
  160 val Base.Bytes.to_list : t -> char list
  161 val Base.List.join : 'a t t -> 'a t
  161 val Base.List.tl_exn : 'a t -> 'a t
  161 val Base.Queue.of_list : 'a list -> 'a t
  161 val Base.Stack.of_list : 'a list -> 'a t
  163 val Base.List.concat : 'a t t -> 'a t
  163 mod Shadow_stdlib.List
  164 val Base.List.last : 'a t -> 'a option
  165 mod Base.List.Assoc
  165 mod Base.List.Infix
  165 cons Base.Sexp.t.List : t list -> t
  165 val Base.List.ignore_m : 'a t -> unit t
  166 val Base.List.drop : 'a t -> int -> 'a t
  166 val Base.List.take : 'a t -> int -> 'a t
  394 mod Base
  397 type Base.Nothing.t = 
  $ sherlodoc index --favoured-prefixes=Base,Caml $ODOCLS > /dev/null
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
  114 val Base.List.last : 'a t -> 'a option
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  125 mod Caml.ListLabels
  344 mod Base
  347 type Base.Nothing.t = 
  362 val Base.String.append : t -> t -> t
  $ sherlodoc index $ODOCLS --favoured-prefixes "" > /dev/null
  $ sherlodoc search --print-cost "list"
  131 type 'a Base.list = 'a List.t
  143 type 'a Base.Export.list = 'a List.t
  151 type 'a Base.List.t = 'a list
  154 mod Base.List
  154 mod Caml.List
  158 val Base.List.rev : 'a t -> 'a t
  159 val Base.List.hd_exn : 'a t -> 'a
  159 val Base.List.return : 'a -> 'a t
  160 val Base.Bytes.to_list : t -> char list
  161 val Base.List.join : 'a t t -> 'a t
  161 val Base.List.tl_exn : 'a t -> 'a t
  161 val Base.Queue.of_list : 'a list -> 'a t
  161 val Base.Stack.of_list : 'a list -> 'a t
  163 val Base.List.concat : 'a t t -> 'a t
  163 mod Shadow_stdlib.List
  164 val Base.List.last : 'a t -> 'a option
  165 mod Base.List.Assoc
  165 mod Base.List.Infix
  165 cons Base.Sexp.t.List : t list -> t
  165 val Base.List.ignore_m : 'a t -> unit t
  166 val Base.List.drop : 'a t -> int -> 'a t
  166 val Base.List.take : 'a t -> int -> 'a t
  175 mod Caml.ListLabels
  394 mod Base
  397 type Base.Nothing.t = 

Partial name search:
