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
module type Monad/68 = sig
  type t/69
  val map/70 : ([a] r(t/69)) -> ((a) -> b) -> [b] r(t/69)
  val join/71 : ([[a] r(t/69)] r(t/69)) -> [a] r(t/69)
   (removed=[])end
module SomeMonad/67 : sig
  type t/72
  include : r(Monad/68) with [r(root(Monad/68).t) = [a] r(t/72)] (sig =
    val map/73 : ([a] r(t/72)) -> ((a) -> b) -> [b] r(t/72)
    val join/74 : ([[a] r(t/72)] r(t/72)) -> [a] r(t/72)
     (removed=[]))
   (removed=[])end
module ComplexTypeExpr/65 : sig
  type t/75
  include : r(Monad/68) with [r(root(Monad/68).t) = ([r(int) * a] r(t/75) * [a * r(int)] r(t/75))] (sig =
    val map/76 : (([r(int) * a] r(t/75) * [a * r(int)] r(t/75))) -> ((a) -> b) -> ([r(int) * b] r(t/75) * [b * r(int)] r(t/75))
    val join/77 : (([r(int) * ([r(int) * a] r(t/75) * [a * r(int)] r(t/75))] r(t/75) * [([r(int) * a] r(t/75) * [a * r(int)] r(t/75)) * r(int)] r(t/75))) -> ([r(int) * a] r(t/75) * [a * r(int)] r(t/75))
     (removed=[]))
   (removed=[])end
module Erase/66 : sig
  include : r(Monad/68) with [r(root(Monad/68).t) = a] (sig = val map/78 : (a) -> ((a) -> b) -> b
                                                              val join/79 : (a) -> a
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
module type Monad_2/121 = sig
  type t/122
  val map/123 : ([a * err] r(t/122)) -> f:((a) -> b) -> [b * err] r(t/122)
  val join/124 : ([[a * e] r(t/122) * e] r(t/122)) -> [a * e] r(t/122)
  val both/125 : ([a * e] r(t/122)) -> ([b * e] r(t/122)) -> [(a * b) * e] r(t/122)
   (removed=[])end
module SwappedVars/120 : sig
  type t/126
  include : r(Monad_2/121) with [r(root(Monad_2/121).t) = [b * a] r(t/126)] (sig =
    val map/127 : ([err * a] r(t/126)) -> f:((a) -> b) -> [err * b] r(t/126)
    val join/128 : ([e * [e * a] r(t/126)] r(t/126)) -> [e * a] r(t/126)
    val both/129 : ([e * a] r(t/126)) -> ([e * b] r(t/126)) -> [e * (a * b)] r(t/126)
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
module type S/151 = sig
  type t/152
  val map/153 : ([a] r(t/152)) -> ((a) -> b) -> [b] r(t/152)
   (removed=[])end
module M/150 : sig
  type t/154
  include : r(S/151) with [r(root(S/151).t) = [(alias (poly_var [ `A of (a * b) ]) b)] r(t/154)] (sig =
    val map/155 : ([(alias (poly_var [ `A of (a * b) ]) b)] r(t/154)) -> ((a) -> b) -> [(alias (poly_var [ `A of (b * b) ]) b)] r(t/154)
     (removed=[]))
   (removed=[])end
 (removed=[])
```
