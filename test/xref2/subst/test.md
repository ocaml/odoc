```ocaml
let resolve_module_name sg name =
  let rec check = function
    | Component.Signature.Module (id, _r, _m) :: _rest
      when Ident.Name.module_ id = name ->
        id
    | _ :: rest -> check rest
    | [] -> failwith "Unknown"
  in
  check sg.Component.Signature.items

let module_substitution ~idents ~targets m test_data =
  let _, _, sg = Common.model_of_string test_data in

  let c = Component.Of_Lang.(signature empty sg) in

  let subst_idents_mod = resolve_module_name c idents in
  let subst_targets_mod = resolve_module_name c targets in

  let subst =
    let target = `Local (subst_targets_mod :> Ident.path_module) in
    Subst.add_module
      (subst_idents_mod :> Ident.path_module)
      (`Resolved target) target Subst.identity
  in

  let m =
    match Find.module_in_sig c "S" with
    | Some (`FModule (name, m)) -> m
    | None -> failwith "Error finding module!"
  in

  let m' = Subst.module_ subst m in

  let open Format in
  fprintf std_formatter "BEFORE\n======\n%!";
  fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m;
  fprintf std_formatter "AFTER \n======\n%!";
  fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m'
```

Module substitution test

This test substitutes one module for another. We substitute
SubTargets in place of SubstituteMe, so the result expected is that
the equations for t, u and v point to SubTargets rather than SubstituteMe

```ocaml
# module_substitution ~idents:"SubstituteMe" ~targets:"SubTargets" "S" {|
  module SubstituteMe : sig
      type t
      type u
      type v
  end

  module SubTargets : sig
      type t
      type u
      type v
  end

  module S : sig
      type tt = SubstituteMe.t
      type uu = SubstituteMe.u
      type vv = SubstituteMe.v
  end
  |}
BEFORE
======
S: sig
type tt/3 = local(SubstituteMe/2,false).t
type uu/4 = local(SubstituteMe/2,false).u
type vv/5 = local(SubstituteMe/2,false).v
 (removed=[])end

AFTER
======
S: sig
type tt/6 = r(SubTargets/1).t
type uu/7 = r(SubTargets/1).u
type vv/8 = r(SubTargets/1).v
 (removed=[])end

- : unit = ()
```

Now test by compiling signatures and printing the result:

```ocaml
(* Nicer output *)
#install_printer Component.Fmt.signature;;

let compile mli =
  let open Component in
  let id, docs, sg = Common.model_of_string mli in
  Odoc_xref2.Compile.signature Env.empty (id :> Odoc_model.Paths.Identifier.Signature.t) sg
  |> Of_Lang.signature Of_Lang.empty
```

```ocaml
# compile {|
  module type Monad = sig
    type 'a t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val join : 'a t t -> 'a t
  end

  (** Simplest case *)
  module SomeMonad : sig
    type 'a t

    include Monad with type 'a t := 'a t
  end

  (** Substitute with a more complex type *)
  module ComplexTypeExpr : sig
    type ('a, 'b) t

    include Monad with type 'a t := (int, 'a) t * ('a, int) t
  end

  (** No abstraction *)
  module Erase : sig
    include Monad with type 'a t := 'a
  end
  |}
- : Component.Signature.t =
module type Monad/33 = sig
  type t/34
  val map/35 : ([a] r(t/34)) -> ((a) -> b) -> [b] r(t/34)
  val join/36 : ([[a] r(t/34)] r(t/34)) -> [a] r(t/34)
   (removed=[])end
module SomeMonad/32 : sig
  type t/37
  include : r(Monad/33) with [r(root(Monad/33).t) = [a] r(t/37)] (sig =
    val map/38 : ([a] r(t/37)) -> ((a) -> b) -> [b] r(t/37)
    val join/39 : ([[a] r(t/37)] r(t/37)) -> [a] r(t/37)
     (removed=[]))
   (removed=[])end
module ComplexTypeExpr/30 : sig
  type t/40
  include : r(Monad/33) with [r(root(Monad/33).t) = ([r(int) * a] r(t/40) * [a * r(int)] r(t/40))] (sig =
    val map/41 : (([r(int) * a] r(t/40) * [a * r(int)] r(t/40))) -> ((a) -> b) -> ([r(int) * b] r(t/40) * [b * r(int)] r(t/40))
    val join/42 : (([r(int) * ([r(int) * a] r(t/40) * [a * r(int)] r(t/40))] r(t/40) * [([r(int) * a] r(t/40) * [a * r(int)] r(t/40)) * r(int)] r(t/40))) -> ([r(int) * a] r(t/40) * [a * r(int)] r(t/40))
     (removed=[]))
   (removed=[])end
module Erase/31 : sig
  include : r(Monad/33) with [r(root(Monad/33).t) = a] (sig = val map/43 : (a) -> ((a) -> b) -> b
                                                              val join/44 : (a) -> a
                                                               (removed=[]))
   (removed=[])end
 (removed=[])
```

More tests with two type variables:

```ocaml
# compile {|
  module type Monad_2 = sig
    type ('a, 'err) t
    val map : ('a, 'err) t -> f:('a -> 'b) -> ('b, 'err) t
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
  end

  module SwappedVars : sig
    type ('x, 'y) t
    include Monad_2 with type ('a, 'b) t := ('b, 'a) t
  end
  |}
- : Component.Signature.t =
module type Monad_2/61 = sig
  type t/62
  val map/63 : ([a * err] r(t/62)) -> f:((a) -> b) -> [b * err] r(t/62)
  val join/64 : ([[a * e] r(t/62) * e] r(t/62)) -> [a * e] r(t/62)
  val both/65 : ([a * e] r(t/62)) -> ([b * e] r(t/62)) -> [(a * b) * e] r(t/62)
   (removed=[])end
module SwappedVars/60 : sig
  type t/66
  include : r(Monad_2/61) with [r(root(Monad_2/61).t) = [b * a] r(t/66)] (sig =
    val map/67 : ([err * a] r(t/66)) -> f:((a) -> b) -> [err * b] r(t/66)
    val join/68 : ([e * [e * a] r(t/66)] r(t/66)) -> [e * a] r(t/66)
    val both/69 : ([e * a] r(t/66)) -> ([e * b] r(t/66)) -> [e * (a * b)] r(t/66)
     (removed=[]))
   (removed=[])end
 (removed=[])
```

Edge cases:

```ocaml
# compile {|
  module type S = sig
    type 'a t
    val map : 'a t -> ('a -> 'b) -> 'b t
  end

  module M : sig
    type 'a t
    include S with type 'a t := ([ `A of 'a * 'b ] as 'b) t
  end
  |}
- : Component.Signature.t =
module type S/78 = sig
  type t/79
  val map/80 : ([a] r(t/79)) -> ((a) -> b) -> [b] r(t/79)
   (removed=[])end
module M/77 : sig
  type t/81
  include : r(S/78) with [r(root(S/78).t) = [(alias (poly_var [ `A of (a * b) ]) b)] r(t/81)] (sig =
    val map/82 : ([(alias (poly_var [ `A of (a * b) ]) b)] r(t/81)) -> ((a) -> b) -> [(alias (poly_var [ `A of (b * b) ]) b)] r(t/81)
     (removed=[]))
   (removed=[])end
 (removed=[])
```
