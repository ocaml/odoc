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
type tt/6 = resolved(local(SubTargets/1)).t
type uu/7 = resolved(local(SubTargets/1)).u
type vv/8 = resolved(local(SubTargets/1)).v
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
module type Monad/30 = sig
  type t/31
  val map/32 : [a] resolved(t/31) -> a -> b -> [b] resolved(t/31)
  val join/33 : [[a] resolved(t/31)] resolved(t/31) -> [a] resolved(t/31)
   (removed=[])end
module SomeMonad/29 : sig
  type t/34
  include : resolved(Monad/30) with [resolved(root(Monad/30).t) = [a] resolved(t/34)] (sig =
    val map/35 : [a] resolved(t/34) -> a -> b -> [b] resolved(t/34)
    val join/36 : [[a] resolved(t/34)] resolved(t/34) -> [a] resolved(t/34)
     (removed=[]))
   (removed=[])end
module ComplexTypeExpr/27 : sig
  type t/37
  include : resolved(Monad/30) with [resolved(root(Monad/30).t) = ([resolved(identifier(int)) * a] resolved(t/37) * [a * resolved(identifier(int))] resolved(t/37))] (sig =
    val map/38 : ([resolved(identifier(int)) * a] resolved(t/37) * [a * resolved(identifier(int))] resolved(t/37)) -> a -> b -> ([resolved(identifier(int)) * b] resolved(t/37) * [b * resolved(identifier(int))] resolved(t/37))
    val join/39 : ([resolved(identifier(int)) * ([resolved(identifier(int)) * a] resolved(t/37) * [a * resolved(identifier(int))] resolved(t/37))] resolved(t/37) * [([resolved(identifier(int)) * a] resolved(t/37) * [a * resolved(identifier(int))] resolved(t/37)) * resolved(identifier(int))] resolved(t/37)) -> ([resolved(identifier(int)) * a] resolved(t/37) * [a * resolved(identifier(int))] resolved(t/37))
     (removed=[]))
   (removed=[])end
module Erase/28 : sig
  include : resolved(Monad/30) with [resolved(root(Monad/30).t) = a] (sig =
    val map/40 : a -> a -> b -> b
    val join/41 : a -> a
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
module type Monad_2/54 = sig
  type t/55
  val map/56 : [a * err] resolved(t/55) -> a -> b -> [b * err] resolved(t/55)
  val join/57 : [[a * e] resolved(t/55) * e] resolved(t/55) -> [a * e] resolved(t/55)
  val both/58 : [a * e] resolved(t/55) -> [b * e] resolved(t/55) -> [(a * b) * e] resolved(t/55)
   (removed=[])end
module SwappedVars/53 : sig
  type t/59
  include : resolved(Monad_2/54) with [resolved(root(Monad_2/54).t) = [b * a] resolved(t/59)] (sig =
    val map/60 : [err * a] resolved(t/59) -> a -> b -> [err * b] resolved(t/59)
    val join/61 : [e * [e * a] resolved(t/59)] resolved(t/59) -> [e * a] resolved(t/59)
    val both/62 : [e * a] resolved(t/59) -> [e * b] resolved(t/59) -> [e * (a * b)] resolved(t/59)
     (removed=[]))
   (removed=[])end
 (removed=[])
```
