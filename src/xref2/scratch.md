```ocaml require=odoc.xref_test,env=e1
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 65536;;
```

```ocaml env=e1
let test_data = {|
module type Applicative_infix = sig
  type 'a t
end

module type Let_syntax = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S
  end

  module Let_syntax : sig
    include Applicative_infix with type 'a t := 'a t

    module Let_syntax : sig
      module Open_on_rhs : Open_on_rhs_intf.S
    end
  end
end

module type For_let_syntax = sig
  type 'a t

  include Applicative_infix with type 'a t := 'a t
end

module type Applicative = sig
  module Make_let_syntax
      (X : For_let_syntax)
      (Intf : sig module type S end)
      (Impl : Intf.S) :
    Let_syntax with type 'a t := 'a X.t with module Open_on_rhs_intf := Intf
end
|};;
let cmti = Common.cmti_of_string test_data;;
let _, _, sg = Odoc_loader__Cmti.read_interface Common.root "Root" cmti;;
let resolved = Resolve.signature Env.empty sg;;
let env = Env.open_signature resolved Env.empty;;
let applicative = Common.LangUtils.Lens.(get (Signature.module_type "Applicative") resolved);;
let sg2 = Common.LangUtils.Lens.(get (ModuleType.expr |-~ option |-~ ModuleType.expr_signature) applicative);;
let env = Env.open_signature sg2 env;;
let m2 = Common.LangUtils.Lens.(get(Signature.module_ "Make_let_syntax") sg2);;
let env = Env.add_functor_args (m2.Odoc_model.Lang.Module.id :> Odoc_model.Paths.Identifier.Signature.t) env;;
let type_ = Expand.module_decl env m2.Odoc_model.Lang.Module.id m2.Odoc_model.Lang.Module.type_;;
let id = m2.Odoc_model.Lang.Module.id;;
let m' = Env.lookup_module id env;;
let expr = match m'.Component.Module.type_ with | Component.Module.ModuleType e -> e | _ -> failwith "bah";;
let expanded = 
  try 
    ignore (Expand.expansion_of_module_type_expr (id :> Odoc_model.Paths.Identifier.Signature.t) env expr);
    "OK"
  with _ ->
    Printexc.get_backtrace ()
```

```ocaml env=e1
# #install_printer Common.Ident.print_with_scope;;
# m';
- : Component.Module.t =
{Odoc_xref2.Component.Module.doc = [];
 type_ =
  Odoc_xref2.Component.Module.ModuleType
   (Odoc_xref2.Component.ModuleType.Functor
     (Some
       {Odoc_xref2.Component.FunctorArgument.id = `LParameter ("X", 107);
        expr =
         Odoc_xref2.Component.ModuleType.Path
          (`Resolved
             (`Identifier
                (`ModuleType (`Root (Common.root, "Root"), "For_let_syntax"))));
        expansion = None},
     Odoc_xref2.Component.ModuleType.Functor
      (Some
        {Odoc_xref2.Component.FunctorArgument.id = `LParameter ("Intf", 108);
         expr =
          Odoc_xref2.Component.ModuleType.Signature
           {Odoc_xref2.Component.Signature.items =
             [Odoc_xref2.Component.Signature.ModuleType
               (`LModuleType ("S", 109),
               {Odoc_xref2.Component.Delayed.v =
                 Some
                  {Odoc_xref2.Component.ModuleType.doc = []; expr = None;
                   expansion = None};
                get = <fun>})];
            removed = []};
         expansion = Some Odoc_xref2.Component.Module.AlreadyASig},
      Odoc_xref2.Component.ModuleType.Functor
       (Some
         {Odoc_xref2.Component.FunctorArgument.id = `LParameter ("Impl", 110);
          expr =
           Odoc_xref2.Component.ModuleType.Path
            (`Resolved
               (`ModuleType (`Local (`LParameter ("Intf", 108)), "S")));
          expansion = None},
       Odoc_xref2.Component.ModuleType.With
        (Odoc_xref2.Component.ModuleType.With
          (Odoc_xref2.Component.ModuleType.Path
            (`Resolved
               (`Identifier
                  (`ModuleType (`Root (Common.root, "Root"), "Let_syntax")))),
          [Odoc_xref2.Component.ModuleType.TypeSubst
            (`Resolved (`Type (`Root, "t")),
            {Odoc_xref2.Component.TypeDecl.Equation.params =
              [(Odoc_model.Lang.TypeDecl.Var "a", None)];
             private_ = false;
             manifest =
              Some
               (Odoc_xref2.Component.TypeExpr.Constr
                 (`Resolved (`Type (`Local (`LParameter ("X", 107)), "t")),
                 []));
             constraints = []})]),
        [Odoc_xref2.Component.ModuleType.ModuleSubst
          (`Resolved (`Module (`Root, "Open_on_rhs_intf")),
          `Resolved (`Local (`LParameter ("Intf", 108))))])))));
 canonical = None; hidden = false; display_type = None; expansion = None}
```


```ocaml env=e1
let test_data = {|
module With10 : sig
  module type T = sig
    module M : sig
      module type S
    end
    module N : M.S
  end
  (** {!With10.T} is a submodule type. *)
end
|};;
let cmti = Common.cmti_of_string test_data;;
let _, _, sg = Odoc_loader__Cmti.read_interface Common.root "Root" cmti;;
let resolved = Resolve2.signature Env.empty sg;;
let env = Env.open_signature resolved Env.empty;;
let with10 = Common.LangUtils.Lens.(get (Signature.module_ "With10") resolved);;
let id = with10.Odoc_model.Lang.Module.id;;
let m' = Env.lookup_module id env;;
let expr = match m'.Component.Module.type_ with | Component.Module.ModuleType e -> e | _ -> failwith "bah";;

(*let expanded = 
    Expand.expansion_of_module_type_expr (id :> Odoc_model.Paths.Identifier.Signature.t) env expr;;*)
```

```ocaml env=e1
# #install_printer Common.Ident.print_with_scope;;
# m';;
- : Component.Module.t =
{Odoc_xref2.Component.Module.doc = [];
 type_ =
  Odoc_xref2.Component.Module.ModuleType
   (Odoc_xref2.Component.ModuleType.Signature
     {Odoc_xref2.Component.Signature.items =
       [Odoc_xref2.Component.Signature.ModuleType (`LModuleType ("T", 147),
         {Odoc_xref2.Component.Delayed.v =
           Some
            {Odoc_xref2.Component.ModuleType.doc =
              [`Paragraph
                 [`Reference
                    (`Resolved
                       (`ModuleType (`Local (`LModule ("With10", 146)), "T")),
                     []);
                  `Space; `Word "is"; `Space; `Word "a"; `Space;
                  `Word "submodule"; `Space; `Word "type."]];
             expr =
              Some
               (Odoc_xref2.Component.ModuleType.Signature
                 {Odoc_xref2.Component.Signature.items =
                   [Odoc_xref2.Component.Signature.Module
                     (`LModule ("M", 148),
                     Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_xref2.Component.Delayed.v =
                       Some
                        {Odoc_xref2.Component.Module.doc = [];
                         type_ =
                          Odoc_xref2.Component.Module.ModuleType
                           (Odoc_xref2.Component.ModuleType.Signature
                             {Odoc_xref2.Component.Signature.items =
                               [Odoc_xref2.Component.Signature.ModuleType
                                 (`LModuleType ("S", 150),
                                 {Odoc_xref2.Component.Delayed.v =
                                   Some
                                    {Odoc_xref2.Component.ModuleType.doc = [];
                                     expr = None; expansion = None};
                                  get = <fun>})];
                              removed = []});
                         canonical = None; hidden = false;
                         display_type = None;
                         expansion =
                          Some Odoc_xref2.Component.Module.AlreadyASig};
                      get = <fun>});
                    Odoc_xref2.Component.Signature.Module
                     (`LModule ("N", 149),
                     Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_xref2.Component.Delayed.v =
                       Some
                        {Odoc_xref2.Component.Module.doc = [];
                         type_ =
                          Odoc_xref2.Component.Module.ModuleType
                           (Odoc_xref2.Component.ModuleType.Path
                             (`Resolved
                                (`ModuleType
                                   (`Local (`LModule ("M", 148)), "S"))));
                         canonical = None; hidden = false;
                         display_type = None; expansion = None};
                      get = <fun>})];
                  removed = []});
             expansion = Some Odoc_xref2.Component.Module.AlreadyASig};
          get = <fun>})];
      removed = []});
 canonical = None; hidden = false; display_type = None;
 expansion = Some Odoc_xref2.Component.Module.AlreadyASig}
```
