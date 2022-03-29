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
  let _, sg, _ = Common.model_of_string test_data in

  let c = Component.Of_Lang.(signature (empty ()) sg) in

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
  |} ;;
BEFORE
======
S: sig
type tt/5 = local(SubstituteMe/2,false).t
type uu/4 = local(SubstituteMe/2,false).u
type vv/3 = local(SubstituteMe/2,false).v
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
  let id, sg, _ = Common.model_of_string mli in
  let env = Env.env_for_testing ~linking:false in
  Odoc_xref2.Compile.signature env (id :> Odoc_model.Paths.Identifier.Signature.t) sg
  |> Of_Lang.(signature (empty ()))
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
  |} ;;
- : Component.Signature.t =
module type Monad/59 = sig
  type t/60
  val map/61 : ([a] r(t/60)) -> ((a) -> b) -> [b] r(t/60)
  val join/62 : ([[a] r(t/60)] r(t/60)) -> [a] r(t/60)
   (removed=[])end
module SomeMonad/58 : sig
  type t/63
  include : r(Monad/59) with [r(root(Monad/59).t) = [a] r(t/63)] (sig =
    val map/64 : ([a] r(t/63)) -> ((a) -> b) -> [b] r(t/63)
    val join/65 : ([[a] r(t/63)] r(t/63)) -> [a] r(t/63)
     (removed=[]))
   (removed=[])end
module ComplexTypeExpr/57 : sig
  type t/66
  include : r(Monad/59) with [r(root(Monad/59).t) = ([r(int) * a] r(t/66) * [a * r(int)] r(t/66))] (sig =
    val map/67 : (([r(int) * a] r(t/66) * [a * r(int)] r(t/66))) -> ((a) -> b) -> ([r(int) * b] r(t/66) * [b * r(int)] r(t/66))
    val join/68 : (([r(int) * ([r(int) * a] r(t/66) * [a * r(int)] r(t/66))] r(t/66) * [([r(int) * a] r(t/66) * [a * r(int)] r(t/66)) * r(int)] r(t/66))) -> ([r(int) * a] r(t/66) * [a * r(int)] r(t/66))
     (removed=[]))
   (removed=[])end
module Erase/56 : sig
  include : r(Monad/59) with [r(root(Monad/59).t) = a] (sig = val map/69 : (a) -> ((a) -> b) -> b
                                                              val join/70 : (a) -> a
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
  |} ;;
- : Component.Signature.t =
module type Monad_2/105 = sig
  type t/106
  val map/107 : ([a * err] r(t/106)) -> f:((a) -> b) -> [b * err] r(t/106)
  val join/108 : ([[a * e] r(t/106) * e] r(t/106)) -> [a * e] r(t/106)
  val both/109 : ([a * e] r(t/106)) -> ([b * e] r(t/106)) -> [(a * b) * e] r(t/106)
   (removed=[])end
module SwappedVars/104 : sig
  type t/110
  include : r(Monad_2/105) with [r(root(Monad_2/105).t) = [b * a] r(t/110)] (sig =
    val map/111 : ([err * a] r(t/110)) -> f:((a) -> b) -> [err * b] r(t/110)
    val join/112 : ([e * [e * a] r(t/110)] r(t/110)) -> [e * a] r(t/110)
    val both/113 : ([e * a] r(t/110)) -> ([e * b] r(t/110)) -> [e * (a * b)] r(t/110)
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
  |} ;;
- : Component.Signature.t =
module type S/132 = sig
  type t/133
  val map/134 : ([a] r(t/133)) -> ((a) -> b) -> [b] r(t/133)
   (removed=[])end
module M/131 : sig
  type t/135
  include : r(S/132) with [r(root(S/132).t) = [(alias (poly_var [ `A of (a * b) ]) b)] r(t/135)] (sig =
    val map/136 : ([(alias (poly_var [ `A of (a * b) ]) b)] r(t/135)) -> ((a) -> b) -> [(alias (poly_var [ `A of (b * b) ]) b)] r(t/135)
     (removed=[]))
   (removed=[])end
 (removed=[])
```
