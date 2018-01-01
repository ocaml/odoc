type point = {
  line : int;
  column : int;
}

type span = {
  start : point;
  end_ : point;
  file : string;
}

type 'a with_location = {
  location : span;
  value : 'a;
}

let value : 'a with_location -> 'a = fun {value; _} ->
  value

let map : ('a -> 'b) -> 'a with_location -> 'b with_location =
    fun f annotated ->
  {annotated with value = f annotated.value}

let same : _ with_location -> 'b -> 'b with_location = fun annotated value ->
  {annotated with value}
