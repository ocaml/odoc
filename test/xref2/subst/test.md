```ocaml
open Odoc_model.Names

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
    let name = ModuleName.make_std "S" in
    match Find.module_in_sig c name with
    | Some (`FModule (name, m)) -> m
    | None -> failwith "Error finding module!"
  in

  let m' = Subst.module_ subst m in

  let open Format in
  let cfg = Component.Fmt.default in
  fprintf std_formatter "BEFORE\n======\n%!";
  fprintf std_formatter "S%a\n\n%!" (Component.Fmt.module_ cfg) m;
  fprintf std_formatter "AFTER \n======\n%!";
  fprintf std_formatter "S%a\n\n%!" (Component.Fmt.module_ cfg) m'
```
```mdx-error
Line 22, characters 49-66:
Error: Unbound type constructor Ident.path_module
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
Line 1, characters 1-20:
Error: Unbound value module_substitution
```

Now test by compiling signatures and printing the result:

```ocaml
(* Nicer output *)
let sig_print = Component.Fmt.(signature default);;
#install_printer sig_print;;

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
module type Monad/21 =
  sig
    type t/22
    val map/23 : ([a] resolved(t/22)) -> ((a) -> b) -> [b] resolved(t/22)
    val join/24 : ([[a] resolved(t/22)] resolved(t/22)) -> [a] resolved(t/22)
  end
module SomeMonad/20 :
  sig
    type t/25
    include r(Monad/21) with [resolved(root(Monad/21).t) = [a] resolved(t/25)]
      (sig :
        val map/26 : ([a] resolved(t/25)) -> ((a) -> b) -> [b] resolved(t/25)
        val join/27 : ([[a] resolved(t/25)] resolved(t/25)) -> [a] resolved(t/25)
        (removed=type (a) t = ([a] local(t/25,false)))
       end)
  end (canonical=None)
module ComplexTypeExpr/19 :
  sig
    type t/28
    include r(Monad/21) with [resolved(root(Monad/21).t) = ([resolved(int) * a] resolved(t/28) * [a * resolved(int)] resolved(t/28))]
      (sig :
        val map/29 : (([resolved(int) * a] resolved(t/28) * [a * resolved(int)] resolved(t/28))) -> ((a) -> b) -> ([resolved(int) * b] resolved(t/28) * [b * resolved(int)] resolved(t/28))
        val join/30 : (([resolved(int) * ([resolved(int) * a] resolved(t/28) * [a * resolved(int)] resolved(t/28))] resolved(t/28) * [([resolved(int) * a] resolved(t/28) * [a * resolved(int)] resolved(t/28)) * resolved(int)] resolved(t/28))) -> ([resolved(int) * a] resolved(t/28) * [a * resolved(int)] resolved(t/28))
        (removed=type (a) t = (([resolved(int) * a] local(t/28,false) * [a * resolved(int)] local(t/28,false))))
       end)
  end (canonical=None)
module Erase/18 :
  sig
    include r(Monad/21) with [resolved(root(Monad/21).t) = a]
      (sig :
        val map/31 : (a) -> ((a) -> b) -> b
        val join/32 : (a) -> a
        (removed=type (a) t = (a))
       end)
  end (canonical=None)
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
module type Monad_2/45 =
  sig
    type t/46
    val map/47 : ([a * err] resolved(t/46)) -> f:((a) -> b) -> [b * err] resolved(t/46)
    val join/48 : ([[a * e] resolved(t/46) * e] resolved(t/46)) -> [a * e] resolved(t/46)
    val both/49 : ([a * e] resolved(t/46)) -> ([b * e] resolved(t/46)) -> [(a * b) * e] resolved(t/46)
  end
module SwappedVars/44 :
  sig
    type t/50
    include r(Monad_2/45) with [resolved(root(Monad_2/45).t) = [b * a] resolved(t/50)]
      (sig :
        val map/51 : ([err * a] resolved(t/50)) -> f:((a) -> b) -> [err * b] resolved(t/50)
        val join/52 : ([e * [e * a] resolved(t/50)] resolved(t/50)) -> [e * a] resolved(t/50)
        val both/53 : ([e * a] resolved(t/50)) -> ([e * b] resolved(t/50)) -> [e * (a * b)] resolved(t/50)
        (removed=type (a, b) t = ([b * a] local(t/50,false)))
       end)
  end (canonical=None)
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
module type S/60 =
  sig
    type t/61
    val map/62 : ([a] resolved(t/61)) -> ((a) -> b) -> [b] resolved(t/61)
  end
module M/59 :
  sig
    type t/63
    include r(S/60) with [resolved(root(S/60).t) = [(alias (poly_var [ `A of (a * b) ]) b)] resolved(t/63)]
      (sig :
        val map/64 : ([(alias (poly_var [ `A of (a * b) ]) b)] resolved(t/63)) -> ((a) -> b) -> [(alias (poly_var [ `A of (b * b) ]) b)] resolved(t/63)
        (removed=type (a) t = ([(alias (poly_var [ `A of (a * b) ]) b)] local(t/63,false)))
       end)
  end (canonical=None)
```
