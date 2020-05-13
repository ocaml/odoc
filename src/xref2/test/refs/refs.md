Testing resolution of references

## Env

Prelude:

```ocaml
(* Prelude *)
#require "odoc.xref2";;
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;

#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.UnitName.fmt;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ModuleTypeName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ClassName.fmt;;
#install_printer Odoc_model.Names.ClassTypeName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Odoc_model.Names.ExceptionName.fmt;;
#install_printer Odoc_model.Names.FieldName.fmt;;
#install_printer Odoc_model.Names.ConstructorName.fmt;;
```

Test data:

```ocaml
let test_mli = {|

  type t1 = C1
  type r1 = { f1 : int }
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
    type t2 = C2
    type r2 = { f2 : int }
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
let sg = Common.signature_of_mli_string test_mli
let env = Env.open_signature sg Env.empty
```

Helpers:

```ocaml
let parse_ref ref_str =
  let open Odoc_model in
  Error.set_warn_error true;
  let parse acc = Odoc_parser__Reference.parse acc (Location_.span []) ref_str in
  match Error.shed_warnings (Error.accumulate_warnings parse) with
  | Ok ref -> ref
  | Error e -> failwith (Error.to_string e)

type ref = Odoc_model.Paths_types.Resolved_reference.any

let resolve_ref ref_str : ref =
  match Ref_tools.resolve_reference env (parse_ref ref_str) with
  | None -> failwith "resolve_reference"
  | Some r -> r
```

## Resolving

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
Exception: Failure "resolve_reference".
# resolve_ref "constructor:C1"
Exception: Failure "resolve_reference".
# resolve_ref "val:e1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), e1))
# resolve_ref "class:c1"
Exception: Failure "resolve_reference".
# resolve_ref "class-type:ct1"
Exception: Failure "resolve_reference".
# resolve_ref "type:x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "constructor:X1" (* X1 is an extension constructor *)
Exception: Failure "resolve_reference".
# resolve_ref "extension:X1"
Exception: Failure "resolve_reference".
# resolve_ref "method:c1.m1"
Exception: Failure "resolve_reference".
# resolve_ref "instance-variable:c1.v1"
Exception: Failure "resolve_reference".
# resolve_ref "method:ct1.tm1" (* ct1 is a class type *)
Exception: Failure "resolve_reference".
# resolve_ref "instance-variable:ct1.tv1"
Exception: Failure "resolve_reference".
# resolve_ref "field:t1.f1"
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
# resolve_ref "constructor:M.C2"
Exception: Failure "resolve_reference".
# resolve_ref "val:M.e2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), e2)
# resolve_ref "class:M.c2"
Exception: Failure "resolve_reference".
# resolve_ref "class-type:M.ct2"
Exception: Failure "resolve_reference".
# resolve_ref "type:M.x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "constructor:M.X2" (* X2 is an extension constructor *)
Exception: Failure "resolve_reference".
# resolve_ref "extension:M.X2"
Exception: Failure "resolve_reference".
# resolve_ref "method:M.c2.m2"
Exception: Failure "resolve_reference".
# resolve_ref "instance-variable:M.c2.v2"
Exception: Failure "resolve_reference".
# resolve_ref "method:M.ct2.tm2" (* ct2 is a class type *)
Exception: Failure "resolve_reference".
# resolve_ref "instance-variable:M.ct2.tv2"
Exception: Failure "resolve_reference".
# resolve_ref "field:M.t2.f2"
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
# resolve_ref "C1"
Exception: Failure "resolve_reference".
# resolve_ref "e1"
- : ref = `Identifier (`Value (`Root (Common.root, Root), e1))
# resolve_ref "c1"
- : ref = `Identifier (`Class (`Root (Common.root, Root), c1))
# resolve_ref "ct1"
- : ref = `Identifier (`ClassType (`Root (Common.root, Root), ct1))
# resolve_ref "x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "X1"
Exception: Failure "resolve_reference".
# resolve_ref "c1.m1"
Exception: Failure "resolve_reference".
# resolve_ref "c1.v1"
Exception: Failure "resolve_reference".
# resolve_ref "ct1.tm1" (* ct1 is a class type *)
Exception: Failure "resolve_reference".
# resolve_ref "ct1.tv1"
Exception: Failure "resolve_reference".
# resolve_ref "t1.f1"
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
# resolve_ref "M.C2"
Exception: Failure "resolve_reference".
# resolve_ref "M.e2"
- : ref = `Value (`Identifier (`Module (`Root (Common.root, Root), M)), e2)
# resolve_ref "M.c2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), c2)
# resolve_ref "M.ct2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), ct2)
# resolve_ref "M.x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "M.X2"
Exception: Failure "resolve_reference".
# resolve_ref "M.c2.m2"
Exception: Failure "resolve_reference".
# resolve_ref "M.c2.v2"
Exception: Failure "resolve_reference".
# resolve_ref "M.ct2.tm2" (* ct2 is a class type *)
Exception: Failure "resolve_reference".
# resolve_ref "M.ct2.tv2"
Exception: Failure "resolve_reference".
# resolve_ref "M.t2.f2"
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
# resolve_ref "class-type-ct1"
Exception: Failure "resolve_reference".
# resolve_ref "type-x1"
- : ref = `Identifier (`Type (`Root (Common.root, Root), x1))
# resolve_ref "class-c1.m1"
Exception: Failure "resolve_reference".
# resolve_ref "class-c1.v1"
Exception: Failure "resolve_reference".
# resolve_ref "class-type-ct1.tm1"
Exception: Failure "resolve_reference".
# resolve_ref "class-type-ct1.tv1"
Exception: Failure "resolve_reference".
# resolve_ref "type-t1.f1"
Exception: Failure "resolve_reference".
# resolve_ref "M.type-t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "M.module-type-T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "M.class-c2"
Exception: Failure "resolve_reference".
# resolve_ref "M.class-type-ct2"
Exception: Failure "resolve_reference".
# resolve_ref "M.type-x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "M.class-c2.m2"
Exception: Failure "resolve_reference".
# resolve_ref "M.class-c2.v2"
Exception: Failure "resolve_reference".
# resolve_ref "M.class-type-ct2.tm2"
Exception: Failure "resolve_reference".
# resolve_ref "M.class-type-ct2.tv2"
Exception: Failure "resolve_reference".
# resolve_ref "M.type-t2.f2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.type-t2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), t2)
# resolve_ref "module-M.module-type-T2"
- : ref =
`ModuleType (`Identifier (`Module (`Root (Common.root, Root), M)), T2)
# resolve_ref "module-M.class-c2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.class-type-ct2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.type-x2"
- : ref = `Type (`Identifier (`Module (`Root (Common.root, Root), M)), x2)
# resolve_ref "module-M.class-c2.m2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.class-c2.v2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.class-type-ct2.tm2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.class-type-ct2.tv2"
Exception: Failure "resolve_reference".
# resolve_ref "module-M.type-t2.f2"
Exception: Failure "resolve_reference".
```
