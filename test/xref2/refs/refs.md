Testing resolution of references

## Env

Helpers:

```ocaml
let parse_ref ref_str =
  let open Odoc_model in
  let parse acc = Odoc_parser__Reference.parse acc (Location_.span []) ref_str in
  match Error.handle_errors_and_warnings ~warn_error:true (Error.accumulate_warnings parse) with
  | Ok ref -> ref
  | Error (`Msg e) -> failwith e

(* Shorten type for nicer output *)
type ref = Odoc_model.Paths_types.Resolved_reference.any

let resolve_ref' env ref_str : ref =
  let open Odoc_model in
  let unresolved = parse_ref ref_str in
  let resolve () = Ref_tools.resolve_reference env unresolved in
  match
    Lookup_failures.catch_failures resolve
    |> Lookup_failures.handle_failures ~warn_error:true ~filename:"tests"
  with
  | Error (`Msg e) -> failwith e
  | Ok None -> failwith "resolve_reference"
  | Ok (Some r) -> r

let resolve_ref_of_mli mli =
  let sg = Common.signature_of_mli_string mli in
  let env = Env.open_signature sg Env.empty in
  resolve_ref' env
```

## Resolving

Env:

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  (** First comment *)

  (** {1:L1 title} *)

  type t1 = C1
  type r1 = { rf1 : int }
  val f1 : unit -> unit
  module type T1 = sig end
  exception E1
  external e1 : unit -> unit = "e1"
  class c1 : object
    val v1 : unit
    method m1 : unit
  end
  class type ct1 = object
    val tv1 : unit
    method tm1 : unit
  end
  type x1 = ..
  type x1 += X1

  module M : sig

    (** {1:L2 title2} *)

    type t2 = C2
    type r2 = { rf2 : int }
    val f2 : unit -> unit
    module type T2 = sig end
    exception E2
    external e2 : unit -> unit = "e2"
    class c2 : object
      val v2 : unit
      method m2 : unit
    end
    class type ct2 = object
      val tv2 : unit
      method tm2 : unit
    end
    type x2 = ..
    type x2 += X2
    module N : sig end
  end

|}
```

Explicit, root:

```ocaml
# resolve_ref "module:M"
- : ref = `Identifier (`Module (`Root (Common.root, Root), M))
# resolve_ref "module:M.N"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), M)), N)
# resolve_ref "val:f1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), f1))
# resolve_ref "type:t1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), t1))
# resolve_ref "module-type:T1"
- : ref = `Identifier (`ModuleType (`Root (Common.root, Root), T1))
# resolve_ref "exception:E1"
- : ref = `Identifier (`Exception (`Root (Common.root, Root), E1))
# resolve_ref "constructor:C1"
- : ref =
`Identifier (`Constructor (`Type (`Root (Common.root, Root), t1), C1))
# resolve_ref "val:e1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), e1))
# resolve_ref "class:c1"
- : ref = `Identifier (`Class (`Root (Common.root, Root), c1))
# resolve_ref "class-type:ct1"
- : ref = `Identifier (`ClassType (`Root (Common.root, Root), ct1))
# resolve_ref "type:x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "constructor:X1" (* X1 is an extension constructor *)
Exception: Failure "resolve_reference".
# resolve_ref "extension:X1"
- : ref = `Identifier (`Extension (`Root (Common.root, Root), X1))
# resolve_ref "method:c1.m1"
- : ref = `Method (`Identifier (`Class (`Root (Common.root, Root), c1)), m1)
# resolve_ref "instance-variable:c1.v1"
- : ref =
`InstanceVariable (`Identifier (`Class (`Root (Common.root, Root), c1)), v1)
# resolve_ref "method:ct1.tm1" (* ct1 is a class type *)
- : ref =
`Method (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tm1)
# resolve_ref "instance-variable:ct1.tv1"
- : ref =
`InstanceVariable
  (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tv1)
# resolve_ref "field:rf1"
- : ref = `Identifier (`Field (`Type (`Root (Common.root, Root), r1), rf1))
# resolve_ref "field:r1.rf1"
- : ref = `Field (`Identifier (`Type (`Root (Common.root, Root), r1)), rf1)
# resolve_ref "section:L1"
- : ref = `Identifier (`Label (`Root (Common.root, Root), L1))
```

Explicit, in sig:

```ocaml
# resolve_ref "val:M.f2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), f2)
# resolve_ref "type:M.t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "module-type:M.T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "exception:M.E2"
- : ref =
`Exception (`Identifier (`Module (`Root (Common.root, Root), M)), E2)
# resolve_ref "constructor:M.C2" (* Not allowed by types *)
Exception: Failure "resolve_reference".
# resolve_ref "val:M.e2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), e2)
# resolve_ref "class:M.c2"
- : ref = `Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2)
# resolve_ref "class-type:M.ct2"
- : ref =
`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2)
# resolve_ref "type:M.x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "constructor:M.X2" (* X2 is an extension constructor *)
Exception: Failure "resolve_reference".
# resolve_ref "extension:M.X2"
- : ref =
`Extension (`Identifier (`Module (`Root (Common.root, Root), M)), X2)
# resolve_ref "method:M.c2.m2"
- : ref =
`Method
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), m2)
# resolve_ref "instance-variable:M.c2.v2"
- : ref =
`InstanceVariable
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), v2)
# resolve_ref "method:M.ct2.tm2" (* ct2 is a class type *)
- : ref =
`Method
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tm2)
# resolve_ref "instance-variable:M.ct2.tv2"
- : ref =
`InstanceVariable
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tv2)
# resolve_ref "field:M.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "field:M.r2.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "section:M.L2"
- : ref = `Label (`Identifier (`Module (`Root (Common.root, Root), M)), L2)
```

Implicit, root:

```ocaml
# resolve_ref "M"
- : ref = `Identifier (`Module (`Root (Common.root, Root), M))
# resolve_ref "M.N"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), M)), N)
# resolve_ref "f1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), f1))
# resolve_ref "t1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), t1))
# resolve_ref "T1"
- : ref = `Identifier (`ModuleType (`Root (Common.root, Root), T1))
# resolve_ref "E1"
- : ref = `Identifier (`Exception (`Root (Common.root, Root), E1))
# resolve_ref "C1"
- : ref =
`Identifier (`Constructor (`Type (`Root (Common.root, Root), t1), C1))
# resolve_ref "e1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), e1))
# resolve_ref "c1"
- : ref = `Identifier (`Class (`Root (Common.root, Root), c1))
# resolve_ref "ct1"
- : ref = `Identifier (`ClassType (`Root (Common.root, Root), ct1))
# resolve_ref "x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "X1"
- : ref = `Identifier (`Extension (`Root (Common.root, Root), X1))
# resolve_ref "c1.m1"
- : ref = `Method (`Identifier (`Class (`Root (Common.root, Root), c1)), m1)
# resolve_ref "c1.v1"
- : ref =
`InstanceVariable (`Identifier (`Class (`Root (Common.root, Root), c1)), v1)
# resolve_ref "ct1.tm1" (* ct1 is a class type *)
- : ref =
`Method (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tm1)
# resolve_ref "ct1.tv1"
- : ref =
`InstanceVariable
  (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tv1)
# resolve_ref "rf1"
- : ref = `Identifier (`Field (`Type (`Root (Common.root, Root), r1), rf1))
# resolve_ref "r1.rf1"
- : ref = `Field (`Identifier (`Type (`Root (Common.root, Root), r1)), rf1)
# resolve_ref "L1"
- : ref = `Identifier (`Label (`Root (Common.root, Root), L1))
```

Implicit, in sig:

```ocaml
# resolve_ref "M.f2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), f2)
# resolve_ref "M.t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "M.T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "M.E2"
- : ref =
`Exception (`Identifier (`Module (`Root (Common.root, Root), M)), E2)
# resolve_ref "M.C2"
- : ref =
`Constructor
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2), C2)
# resolve_ref "M.e2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), e2)
# resolve_ref "M.c2"
- : ref = `Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2)
# resolve_ref "M.ct2"
- : ref =
`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2)
# resolve_ref "M.x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "M.X2"
- : ref =
`Extension (`Identifier (`Module (`Root (Common.root, Root), M)), X2)
# resolve_ref "M.c2.m2"
- : ref =
`Method
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), m2)
# resolve_ref "M.c2.v2"
- : ref =
`InstanceVariable
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), v2)
# resolve_ref "M.ct2.tm2" (* ct2 is a class type *)
- : ref =
`Method
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tm2)
# resolve_ref "M.ct2.tv2"
- : ref =
`InstanceVariable
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tv2)
# resolve_ref "M.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.r2.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.L2"
- : ref = `Label (`Identifier (`Module (`Root (Common.root, Root), M)), L2)
```

Known kind:

```ocaml
# resolve_ref "module-M"
- : ref = `Identifier (`Module (`Root (Common.root, Root), M))
# resolve_ref "module-M.N"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), M)), N)
# resolve_ref "M.module-N"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), M)), N)
# resolve_ref "module-M.module-N"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), M)), N)
# resolve_ref "type-t1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), t1))
# resolve_ref "module-type-T1"
- : ref = `Identifier (`ModuleType (`Root (Common.root, Root), T1))
# resolve_ref "class-c1"
- : ref = `Identifier (`Class (`Root (Common.root, Root), c1))
# resolve_ref "class-type-ct1"
- : ref = `Identifier (`ClassType (`Root (Common.root, Root), ct1))
# resolve_ref "type-x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "class-c1.m1"
- : ref = `Method (`Identifier (`Class (`Root (Common.root, Root), c1)), m1)
# resolve_ref "class-c1.v1"
- : ref =
`InstanceVariable (`Identifier (`Class (`Root (Common.root, Root), c1)), v1)
# resolve_ref "class-type-ct1.tm1"
- : ref =
`Method (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tm1)
# resolve_ref "class-type-ct1.tv1"
- : ref =
`InstanceVariable
  (`Identifier (`ClassType (`Root (Common.root, Root), ct1)), tv1)
# resolve_ref "field-rf1"
- : ref = `Identifier (`Field (`Type (`Root (Common.root, Root), r1), rf1))
# resolve_ref "type-r1.rf1"
- : ref = `Field (`Identifier (`Type (`Root (Common.root, Root), r1)), rf1)
# resolve_ref "type-r1.field-rf1"
- : ref = `Field (`Identifier (`Type (`Root (Common.root, Root), r1)), rf1)
# resolve_ref "section-L1"
- : ref = `Identifier (`Label (`Root (Common.root, Root), L1))
# resolve_ref "M.type-t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "M.module-type-T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "M.class-c2"
- : ref = `Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2)
# resolve_ref "M.class-type-ct2"
- : ref =
`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2)
# resolve_ref "M.type-x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "M.class-c2.m2"
- : ref =
`Method
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), m2)
# resolve_ref "M.class-c2.v2"
- : ref =
`InstanceVariable
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), v2)
# resolve_ref "M.class-type-ct2.tm2"
- : ref =
`Method
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tm2)
# resolve_ref "M.class-type-ct2.tv2"
- : ref =
`InstanceVariable
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tv2)
# resolve_ref "M.type-r2.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.r2.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.type-r2.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "M.section-L2"
- : ref = `Label (`Identifier (`Module (`Root (Common.root, Root), M)), L2)
# resolve_ref "module-M.type-t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "module-M.module-type-T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "module-M.class-c2"
- : ref = `Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2)
# resolve_ref "module-M.class-type-ct2"
- : ref =
`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2)
# resolve_ref "module-M.type-x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "module-M.class-c2.m2"
- : ref =
`Method
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), m2)
# resolve_ref "module-M.class-c2.v2"
- : ref =
`InstanceVariable
  (`Class (`Identifier (`Module (`Root (Common.root, Root), M)), c2), v2)
# resolve_ref "module-M.class-type-ct2.tm2"
- : ref =
`Method
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tm2)
# resolve_ref "module-M.class-type-ct2.tv2"
- : ref =
`InstanceVariable
  (`ClassType (`Identifier (`Module (`Root (Common.root, Root), M)), ct2),
   tv2)
# resolve_ref "module-M.type-r2.rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "module-M.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
# resolve_ref "module-M.section-L2"
- : ref = `Label (`Identifier (`Module (`Root (Common.root, Root), M)), L2)
# resolve_ref "module-M.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
```

Class and class type as type:

```ocaml
# resolve_ref "type:c1"
- : ref = `Identifier (`Class (`Root (Common.root, Root), c1))
# resolve_ref "type:ct1"
- : ref = `Identifier (`ClassType (`Root (Common.root, Root), ct1))
```

Constructors in type parent:

```ocaml
# resolve_ref "t1.C1"
- : ref =
`Constructor (`Identifier (`Type (`Root (Common.root, Root), t1)), C1)
# resolve_ref "constructor:t1.C1"
- : ref =
`Constructor (`Identifier (`Type (`Root (Common.root, Root), t1)), C1)
# resolve_ref "t1.constructor-C1"
- : ref =
`Constructor (`Identifier (`Type (`Root (Common.root, Root), t1)), C1)
# resolve_ref "constructor:type-t1.C1"
- : ref =
`Constructor (`Identifier (`Type (`Root (Common.root, Root), t1)), C1)
# resolve_ref "M.t2.C2"
- : ref =
`Constructor
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2), C2)
# resolve_ref "constructor:M.t2.C2"
- : ref =
`Constructor
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2), C2)
# resolve_ref "M.t2.constructor-C2"
- : ref =
`Constructor
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2), C2)
# resolve_ref "constructor:M.type-t2.C2"
- : ref =
`Constructor
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2), C2)
```

Signature parent:

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    module N : sig
      type t
    end
    module type T = sig
      type t
    end
  end
  module type MT = sig
    type t
  end
|}
```

```ocaml
# resolve_ref "M.module-N.t"
- : ref =
`Type (`Module (`Identifier (`Module (`Root (Common.root, Root), M)), N), t)
# resolve_ref "M.module-type-T.t"
- : ref =
`Type
  (`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T), t)
# resolve_ref "M.N.type-t"
- : ref =
`Type (`Module (`Identifier (`Module (`Root (Common.root, Root), M)), N), t)
# resolve_ref "M.T.type-t"
- : ref =
`Type
  (`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T), t)
# resolve_ref "type:M.N.t"
- : ref =
`Type (`Module (`Identifier (`Module (`Root (Common.root, Root), M)), N), t)
# resolve_ref "type:MT.t"
- : ref =
`Type (`Identifier (`ModuleType (`Root (Common.root, Root), MT)), t)
```

Substitutions are only available in 4.08 onwards:

<!-- $MDX version>=4.08 -->
```ocaml
let resolve_ref = resolve_ref_of_mli {|
  type r1 = { rf1 : int }
  type s1 := r1

  module M : sig
    type r2 = { rf2 : int }
    type s2 := r2
  end
|}
```

<!-- $MDX version>=4.08 -->
```ocaml
# resolve_ref "s1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), s1))
# resolve_ref "s1.rf1"
Exception: Failure "resolve_reference".
# resolve_ref "M.s2"
Exception: Failure "resolve_reference".
# resolve_ref "M.s2.rf2"
Exception: Failure "resolve_reference".
```

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module type T1 = sig
    type t
  end

  module type T2 = sig
    module N : T1
  end

  module A : T1
  module B = A
  module C : module type of A
  module D : T1 with type t = A.t
  module E : T2 with module N = A
|}
```

```ocaml
# resolve_ref "A.t"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), A)), t)
# resolve_ref "B.t" (* get_module_path_modifiers is [`Aliased] *)
- : ref =
`Type
  (`SubstAlias
     (`Identifier (`Module (`Root (Common.root, Root), A)),
      `Identifier (`Module (`Root (Common.root, Root), B))),
   t)
# resolve_ref "C.t"
Exception: Failure "resolve_reference".
# resolve_ref "D.t"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), D)), t)
# resolve_ref "E.N.t"
- : ref =
`Type
  (`SubstAlias
     (`Identifier (`Module (`Root (Common.root, Root), A)),
      `Module (`Identifier (`Module (`Root (Common.root, Root), E)), N)),
   t)
```

## Failures

Test some error paths.

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    type t = C
    type u = { f : t }
    type obj_sig = < m : int >
  end

  type obj_env = < m : int >
|}
```

```ocaml
# (* Lookup a field but find a constructor *)
  resolve_ref "M.field-C"
Exception: Failure "resolve_reference".
# resolve_ref "M.t.field-C"
Exception: Failure "resolve_reference".
# (* Lookup a class but find a type *)
  resolve_ref "M.class-t"
Exception: Failure "resolve_reference".
# (* Lookup a constructor but find a field *)
  resolve_ref "M.constructor-f"
Exception: Failure "resolve_reference".
# resolve_ref "M.u.constructor-f"
Exception: Failure "resolve_reference".
```

Lookup classes but get types

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    type t = < m : int >
  end
  type u = < m : int >
  class c : object end
|}
```

```ocaml
# resolve_ref "m" (* in env *)
Exception: Failure "resolve_reference".
# resolve_ref "method-m"
Exception: Failure "resolve_reference".
# resolve_ref "u.method-m" (* Parent is type in env *)
Exception: Failure "resolve_reference".
# resolve_ref "M.method-m" (* Parent is sig *)
Exception: Failure "resolve_reference".
# resolve_ref "M.t.method-m"
Exception: Failure "resolve_reference".
# resolve_ref "c.constructor-C" (* Type in env but find class (parent of constructor is "datatype") *)
Exception: Failure "resolve_reference".
# resolve_ref "c.field-f" (* Field in class (parent of field is "label_parent") *)
Exception: Failure "resolve_reference".
```

## Ambiguous references

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  type t = X
  val t : t

  module X : sig
    type u = Y
    val u : u

    module Y : sig end
  end

  module Everything_ambiguous_in_sig : sig
    include sig type t end
    (* include sig class r : object end end *) (* assertion failure in the compiler *)
    (* include sig class type r = object end end *)
    include sig module type t end
    (** {1:t Label} *)
    include sig type r = { t : t } end
    include sig val t : t end
    include sig external t : t -> t = "t" end
    include sig type t := t end

    include sig module T : sig end end
    include sig module T := X end
    include sig exception T end
    include sig type r = .. type r += T end
  end


|}
```

Ambiguous in env:

```ocaml
# resolve_ref "t"
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 't' is ambiguous. Please specify its kind: val-t, type-t.
# resolve_ref "X"
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 'X' is ambiguous. Please specify its kind: module-X, constructor-X.
```

Ambiguous in sig:

```ocaml
# resolve_ref "X.u"
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 'u' is ambiguous. Please specify its kind: type-u, val-u.
# resolve_ref "X.Y"
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 'Y' is ambiguous. Please specify its kind: constructor-Y, module-Y.
# resolve_ref "Everything_ambiguous_in_sig.t" (* Some kinds are missing: label, type subst (would be "type-") *)
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 't' is ambiguous. Please specify its kind: type-t, module-type-t, field-t, val-t, val-t.
# resolve_ref "Everything_ambiguous_in_sig.T" (* Missing kind: module subst (would be "module-") *)
Exception: Failure "Warnings have been generated.".
File "tests":
Reference to 'T' is ambiguous. Please specify its kind: module-T, exception-T, extension-T.
```

Unambiguous:

```ocaml
# resolve_ref "type-t"
- : ref = `Identifier (`Type (`Root (Common.root, Root), t))
# resolve_ref "val-t"
- : ref = `Identifier (`Value (`Root (Common.root, Root), t))
# resolve_ref "constructor-X"
- : ref =
`Identifier (`Constructor (`Type (`Root (Common.root, Root), t), X))
# resolve_ref "module-X"
- : ref = `Identifier (`Module (`Root (Common.root, Root), X))
# resolve_ref "X.type-u"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), X)), u)
# resolve_ref "X.val-u"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), X)), u)
# resolve_ref "X.constructor-Y"
Exception: Failure "resolve_reference".
# resolve_ref "X.module-Y"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), X)), Y)
```

Unambiguous 2:

```ocaml
# resolve_ref "type:t"
- : ref = `Identifier (`Type (`Root (Common.root, Root), t))
# resolve_ref "val:t"
- : ref = `Identifier (`Value (`Root (Common.root, Root), t))
# resolve_ref "constructor:X"
- : ref =
`Identifier (`Constructor (`Type (`Root (Common.root, Root), t), X))
# resolve_ref "module:X"
- : ref = `Identifier (`Module (`Root (Common.root, Root), X))
# resolve_ref "type:X.u"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), X)), u)
# resolve_ref "val:X.u"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), X)), u)
# resolve_ref "constructor:X.Y"
Exception: Failure "resolve_reference".
# resolve_ref "module:X.Y"
- : ref = `Module (`Identifier (`Module (`Root (Common.root, Root), X)), Y)
```
