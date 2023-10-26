val option_of_result : ('a, 'b) Result.result -> 'a option
val flatmap : ?sep:'a list -> f:('b -> 'a list) -> 'b list -> 'a list
val skip_until : p:('a -> bool) -> 'a list -> 'a list
val split_at : f:('a -> bool) -> 'a list -> 'a list * 'a list
val compute_length_source : Types.Source.t -> int
val compute_length_inline : Types.Inline.t -> int
