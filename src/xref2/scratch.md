```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 6000;;
#print_depth 20;;
```

```ocaml env=e1
let test_data = {|
module type RESULT = sig type u type v end
module Make (S : sig type t end)
  : RESULT
      with type u := S.t
      with type v = S.t
|}
let sg = Common.signature_of_mli_string test_data;;
```

```ocaml env=e1
# sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.ModuleType
  {Odoc_model.Lang.ModuleType.id =
    `ModuleType (`Root (Common.root, "Root"), "RESULT");
   doc = [];
   expr =
    Some
     (Odoc_model.Lang.ModuleType.Signature
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`ModuleType (`Root (Common.root, "Root"), "RESULT"), "u");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None});
        Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`ModuleType (`Root (Common.root, "Root"), "RESULT"), "v");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None})]);
   display_expr = None; expansion = Some Odoc_model.Lang.Module.AlreadyASig};
 Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.Module.id = `Module (`Root (Common.root, "Root"), "Make");
   doc = [];
   type_ =
    Odoc_model.Lang.Module.ModuleType
     (Odoc_model.Lang.ModuleType.Functor
       (Some
         {Odoc_model.Lang.FunctorArgument.id =
           `Parameter (`Module (`Root (Common.root, "Root"), "Make"), "S");
          expr =
           Odoc_model.Lang.ModuleType.Signature
            [Odoc_model.Lang.Signature.Type
              (Odoc_model.Lang.Signature.Ordinary,
              {Odoc_model.Lang.TypeDecl.id =
                `Type
                  (`Parameter
                     (`Module (`Root (Common.root, "Root"), "Make"), "S"),
                   "t");
               doc = [];
               equation =
                {Odoc_model.Lang.TypeDecl.Equation.params = [];
                 private_ = false; manifest = None; constraints = []};
               representation = None})];
          expansion = Some Odoc_model.Lang.Module.AlreadyASig},
       Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.With
          (Odoc_model.Lang.ModuleType.Path
            (`Resolved
               (`Identifier
                  (`ModuleType (`Root (Common.root, "Root"), "RESULT")))),
          [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Root, "u"),
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier (`Parameter (`Module (`Root ...), "S"))),
                       "t"),
                    []));
              constraints = []})]),
          [Odoc_model.Lang.ModuleType.TypeEq (`Dot (`Root, "v"),
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier
                          (`Parameter (`Module (`Root (...), "Make"), "S"))),
                     "t"),
                 []));
             constraints = []})])));
    canonical = None; hidden = false; display_type = None; expansion = None})]
```
