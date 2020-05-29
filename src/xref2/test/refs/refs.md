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
#install_printer Odoc_model.Names.ExtensionName.fmt;;
#install_printer Odoc_model.Names.MethodName.fmt;;
#install_printer Odoc_model.Names.InstanceVariableName.fmt;;
```

Test data:

```ocaml
let test_mli = {|

  (** {1:L1} *)
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
  type s1 := r1

  module M : sig
    (** {1:L2} *)
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
    type s2 := r2
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
  let unresolved = parse_ref ref_str in
  match Ref_tools.resolve_reference env unresolved with
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
- : ref = `Identifier (`Exception (`Root (Common.root, Root), E1))
# resolve_ref "constructor:C1"
- : ref =
`Identifier (`Constructor (`Type (`Root (Common.root, Root), t1), C1))
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
- : ref =
`Exception (`Identifier (`Module (`Root (Common.root, Root), M)), E2)
# resolve_ref "constructor:M.C2"
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
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
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
Exception: Failure "resolve_reference".
# resolve_ref "module-M.field-rf2"
- : ref =
`Field
  (`Type (`Identifier (`Module (`Root (Common.root, Root), M)), r2), rf2)
```

Substitutions:

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
