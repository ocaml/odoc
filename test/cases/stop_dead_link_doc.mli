(* This tests that references to hidden items (items in no documentation mode) don't get rendered *)

module Foo : sig
  type t
end

type foo = | Bar of Foo.t

type bar = | Bar of { field : Foo.t }

type foo_ = Bar_ of (int * Foo.t) * int
type bar_ = Bar__ of Foo.t option

(**/**)
module Another_Foo : sig
  type t
end
(**/**)

(* this should be rendered as `type another_foo` because it contains a reference to a hidden module*)
type another_foo = | Bar of Another_Foo.t

(* this should be rendered as `type another_bar` because it contains a reference to a hidden module*)
type another_bar = | Bar of { field : Another_Foo.t }

type another_foo_ = Bar_ of (int * Another_Foo.t) * int
type another_bar_ = Bar__ of Another_Foo.t option
