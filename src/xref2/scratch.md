```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_xref2.Component.Fmt.model_identifier;;
#install_printer Common.value_name_pp;;
#install_printer Common.type_name_pp;;
#install_printer Common.unit_name_pp;;
#print_length 60000;;
#print_depth 200;;

let mt_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.ModuleType.t :> Odoc_model.Paths.Identifier.t);;
#install_printer mt_printer;;
let m_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.Module.t :> Odoc_model.Paths.Identifier.t);;
#install_printer m_printer;;
let t_printer fmt t = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (t : Odoc_model.Paths.Identifier.Type.t :> Odoc_model.Paths.Identifier.t);;
#install_printer t_printer;;
let doc_printer fmt (doc : Odoc_model.Comment.docs) = Format.fprintf fmt "<<docs>>";;
#install_printer doc_printer;;
#install_printer Ident.print_with_scope;;
#install_printer Ident.print;;
```

```ocaml env=e1
let test_data = {|
module M
  (S : sig
     module E : sig
       type 'a t
     end
  end)
  (D : sig type element end)
= struct
  include S
  module E = struct

    include E
    include struct
      open D
      type 'a t = element E.t
      let foo : unit -> element E.t = fun () -> failwith "foo"
    end
  end
end
|}
let sg = Common.cmt_of_string test_data;;
```

```ocaml env=e1
let test_data = {|
module M : sig
  type t
  val id : t -> t  
end

module Mextended : sig
  include module type of struct include M end
  type t
  val ignore : t -> unit
end
|};;
let sg = Common.model_of_string test_data;;
```

```ocaml env=e1
# Common.load_cmt "/Users/jon/.opam/4.09.0/lib/base/base__Container_intf.cmt";;
- : (Odoc_model.Lang.Compilation_unit.t, Odoc_model.Error.t) result =
Result.Ok
 {Odoc_model.Lang.Compilation_unit.id = (root Base__Container_intf);
  doc = <<docs>>; digest = "óuºÅX‚Õ@j°mADß^”";
  imports =
   [Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__",
     Some "p(N)ÀËœ\021µ\007;QdˆœX");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Hash",
     Some "ˇP\127E–Pó%}∫¿v©j\011");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Hash_intf",
     Some "ªnÕ\006≈1cEüÏ\024'˜“åc");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Import",
     Some ",V\012\tÈ>3ÿ\012!z∞ÛjI“");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Import0",
     Some "íEL·»NXØ f3Ã.5∂€");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Monad",
     Some "/GKIA˚öµ \023øï¿ë‹•");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Monad_intf",
     Some "\022K]∑{¥\027=Kÿ\1277\127s«L");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Poly0",
     Some "rµ8)ı◊bX\029¨\023ìÕ±ß˙");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved
     ("Base__Ppx_compare_lib", Some "©˛xÄ€4ùP∂vÍ\019Â\027Ö");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Ppx_hash_lib",
     Some "Âúø \005D_2!Obb∏µ=é");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved
     ("Base__Ppx_sexp_conv_lib", Some "˛ï˜è\011c\002:≤°\019ˇ'KN");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Result",
     Some "ì\006/ÎÓŒâıë¥\015\\à@@Z");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Sexp",
     Some "'Á¸√\025Ñ8Xn\029:n:LGŸ");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Sexpable",
     Some "ƒÙ\031d)\005)\006‡Ô≥Ûñ#%_");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Sexplib",
     Some "!‘¬>Ìæ0ì\014;≥¢}%Ä*");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Base__Stringable",
     Some "ÅX@RæŸ5\003mò\025∂mD§æ");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved
     ("Base_internalhash_types", Some "E¸\016ÄôW<\021¸∑¡Â:Ê˜Û");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Caml",
     Some "˙B˛π¶M˝\nß\023\017j–,Te");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved
     ("CamlinternalFormatBasics", Some "ñ—jÆbUÎ\002ßàÓñ®≠∏\022");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Sexplib0",
     Some "Ñ\020ª˘r¯›Çá∆ßŒÙ&∑\025");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Sexplib0__Sexp",
     Some "ªp'â§EÁN.nl ﬂÙz.");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved
     ("Sexplib0__Sexp_conv", Some "«íMV·ÎÂá€ç2\t*RM\020");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Shadow_stdlib",
     Some "yı”\bY\"g‹q\007}˜VR‚‰");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib",
     Some "M\030#∑‚\027Í¸~5Æç\003\023Sc");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__buffer",
     Some "‘<™°ﬂ5B@˚à)\027´∞uA");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__format",
     Some "1íUøº£Õ∂\tû\018æ÷¿Îù");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__hashtbl",
     Some "∑î±Ü¶\025\018Ö\001œ(\011ëÔd");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__int32",
     Some "cI¡G&IY¨ø\n…\"ég0Y");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__int64",
     Some "0@ê8®c\025 Í\b.mœ@\021¥");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__map",
     Some "-∂‰D\019]2¶újSZÏ∑\002û");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__moreLabels",
     Some "6s\025∏kÓÍÎ\015˘gÖ c‘@");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__nativeint",
     Some "ä\017Îÿ5S¸–ó+K≥ƒ¥oÆ");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__obj",
     Some "h_J7´\127J\018Àœ\001ga~◊¯");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__pervasives",
     Some "\025˘†‰É™[{N∞ì\023—‚»ÿ");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__seq",
     Some "\"µ&≤!Pö‚Dÿg\029é\023¶ó");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__set",
     Some "≥tô%+Pa=•ﬂ6⁄°Ô˙\029");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__stdLabels",
     Some "¥T<,'†lRq‘£ç\006∫\tN");
    Odoc_model.Lang.Compilation_unit.Import.Unresolved ("Stdlib__uchar",
     Some "\027˛¥ÍÖÙ\019bî÷mÆ!Œô|")];
  source =
   Some
    {Odoc_model.Lang.Compilation_unit.Source.file = "src/container_intf.ml";
     build_dir =
      "/Users/jon/.opam/4.09.0/.opam-switch/build/base.v0.13.0/_build/default";
     digest = "\016DßmÙ\127k“ƒÖÍYF—≤\b"};
  interface = true; hidden = true;
  content =
   Odoc_model.Lang.Compilation_unit.Module
    [Odoc_model.Lang.Signature.Open {Odoc_model.Lang.Open.expansion = []};
     Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
      {Odoc_model.Lang.Module.id = (root Base__Container_intf).Export;
       doc = <<docs>>;
       type_ =
        Odoc_model.Lang.Module.ModuleType
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Module
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.Module.id =
               (root Base__Container_intf).Export.Continue_or_stop;
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.Signature
                  [Odoc_model.Lang.Signature.Type
                    (Odoc_model.Lang.Signature.Ordinary,
                    {Odoc_model.Lang.TypeDecl.id =
                      (root Base__Container_intf).Export.Continue_or_stop.t;
                     doc = <<docs>>;
                     equation =
                      {Odoc_model.Lang.TypeDecl.Equation.params =
                        [(Odoc_model.Lang.TypeDecl.Var "a", None);
                         (Odoc_model.Lang.TypeDecl.Var "b", None)];
                       private_ = false; manifest = None; constraints = []};
                     representation =
                      Some
                       (Odoc_model.Lang.TypeDecl.Representation.Variant
                         [{Odoc_model.Lang.TypeDecl.Constructor.id =
                            `Constructor
                              ((root Base__Container_intf).Export.Continue_or_stop.t,
                               <abstr>);
                           doc = <<docs>>;
                           args =
                            Odoc_model.Lang.TypeDecl.Constructor.Tuple
                             [Odoc_model.Lang.TypeExpr.Var "a"];
                           res = None};
                          {Odoc_model.Lang.TypeDecl.Constructor.id =
                            `Constructor
                              ((root Base__Container_intf).Export.Continue_or_stop.t,
                               <abstr>);
                           doc = <<docs>>;
                           args =
                            Odoc_model.Lang.TypeDecl.Constructor.Tuple
                             [Odoc_model.Lang.TypeExpr.Var "b"];
                           res = None}])})]);
              canonical = None; hidden = false; display_type = None;
              expansion = Some Odoc_model.Lang.Module.AlreadyASig});
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Export.Summable;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Signature
                  [Odoc_model.Lang.Signature.Type
                    (Odoc_model.Lang.Signature.Ordinary,
                    {Odoc_model.Lang.TypeDecl.id =
                      (root Base__Container_intf).Export.Summable.t;
                     doc = <<docs>>;
                     equation =
                      {Odoc_model.Lang.TypeDecl.Equation.params = [];
                       private_ = false; manifest = None; constraints = []};
                     representation = None});
                   Odoc_model.Lang.Signature.Value
                    {Odoc_model.Lang.Value.id =
                      `Value
                        (`ModuleType
                           (`Module (`Root (Common.root, <abstr>), Export),
                            <abstr>),
                         zero);
                     doc = <<docs>>;
                     type_ =
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Module
                                      (`Root (Common.root, <abstr>), Export),
                                    <abstr>),
                                 t))),
                       [])};
                   Odoc_model.Lang.Signature.Value
                    {Odoc_model.Lang.Value.id =
                      `Value
                        (`ModuleType
                           (`Module (`Root (Common.root, <abstr>), Export),
                            <abstr>),
                         (+));
                     doc = <<docs>>;
                     type_ =
                      Odoc_model.Lang.TypeExpr.Arrow (None,
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved
                           (`Identifier
                              (`Type
                                 (`ModuleType
                                    (`Module
                                       (`Root (Common.root, <abstr>), Export),
                                     <abstr>),
                                  t))),
                        []),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Module
                                        (`Root (Common.root, <abstr>),
                                         Export),
                                      <abstr>),
                                   t))),
                         []),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Module
                                        (`Root (Common.root, <abstr>),
                                         Export),
                                      <abstr>),
                                   t))),
                         [])))}]);
              display_expr = None;
              expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
       canonical =
        Some
         (`Dot (`Root "Base", "Container"),
          `Dot (`Root (<abstr>, `TUnknown), "Container"));
       hidden = false; display_type = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig});
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).S0;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id = (root Base__Container_intf).S0.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params = [];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).S0.elt;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params = [];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), mem);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            elt))),
                  []),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  []),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).S0_phantom;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).S0_phantom.elt;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params = [];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).S0_phantom.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), mem);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            elt))),
                  []),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  [Odoc_model.Lang.TypeExpr.Any]),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   []),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [])])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).S1;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id = (root Base__Container_intf).S1.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), mem);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "a",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "equal"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType bool)), []))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), []))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  [Odoc_model.Lang.TypeExpr.Var "a"]),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "b"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "b"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Var "a"]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Var "a"]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id =
        (root Base__Container_intf).S1_phantom_invariant;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).S1_phantom_invariant.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None);
                  (Odoc_model.Lang.TypeDecl.Var "phantom", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), mem);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "a",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "equal"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType bool)), []))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), []))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any; Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any; Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  [Odoc_model.Lang.TypeExpr.Var "a";
                   Odoc_model.Lang.TypeExpr.Any]),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "b"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "b"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Var "a"]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Var "a"]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "a",
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).S1_phantom;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).S1_phantom.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None);
                  (Odoc_model.Lang.TypeDecl.Var "phantom",
                   Some Odoc_model.Lang.TypeDecl.Pos)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Include
             {Odoc_model.Lang.Include.parent =
               `ModuleType (`Root (Common.root, <abstr>), <abstr>);
              doc = <<docs>>;
              decl =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.With
                  (Odoc_model.Lang.ModuleType.Path
                    (`Resolved
                       (`Identifier
                          (root Base__Container_intf).S1_phantom_invariant)),
                  [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Root, "t"),
                    {Odoc_model.Lang.TypeDecl.Equation.params =
                      [(Odoc_model.Lang.TypeDecl.Var "a", None);
                       (Odoc_model.Lang.TypeDecl.Var "phantom", None)];
                     private_ = false;
                     manifest =
                      Some
                       (Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Root (Common.root, <abstr>), <abstr>),
                                   t))),
                         [Odoc_model.Lang.TypeExpr.Var "a";
                          Odoc_model.Lang.TypeExpr.Var "phantom"]));
                     constraints = []})]));
              inline = false;
              expansion =
               {Odoc_model.Lang.Include.resolved = false;
                content =
                 [Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        mem);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow (None,
                       Odoc_model.Lang.TypeExpr.Var "a",
                       Odoc_model.Lang.TypeExpr.Arrow
                        (Some (Odoc_model.Lang.TypeExpr.Label "equal"),
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "a",
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Var "a",
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved (`Identifier (`CoreType bool)),
                           []))),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), []))))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        length);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType int)), []))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        is_empty);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType bool)), []))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        iter);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType unit)), [])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType unit)), [])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        fold);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                       Odoc_model.Lang.TypeExpr.Var "accum",
                       Odoc_model.Lang.TypeExpr.Arrow
                        (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "accum",
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Var "a",
                          Odoc_model.Lang.TypeExpr.Var "accum")),
                        Odoc_model.Lang.TypeExpr.Var "accum")))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        fold_result);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                       Odoc_model.Lang.TypeExpr.Var "accum",
                       Odoc_model.Lang.TypeExpr.Arrow
                        (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "accum",
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Var "a",
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                           [Odoc_model.Lang.TypeExpr.Var "accum";
                            Odoc_model.Lang.TypeExpr.Var "e"]))),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                         [Odoc_model.Lang.TypeExpr.Var "accum";
                          Odoc_model.Lang.TypeExpr.Var "e"]))))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        fold_until);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                       Odoc_model.Lang.TypeExpr.Var "accum",
                       Odoc_model.Lang.TypeExpr.Arrow
                        (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "accum",
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Var "a",
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Dot
                              (`Dot
                                 (`Resolved
                                    (`Identifier
                                       (root Base__Container_intf).Export),
                                  "Continue_or_stop"),
                               "t"),
                           [Odoc_model.Lang.TypeExpr.Var "accum";
                            Odoc_model.Lang.TypeExpr.Var "final"]))),
                        Odoc_model.Lang.TypeExpr.Arrow
                         (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Var "accum",
                          Odoc_model.Lang.TypeExpr.Var "final"),
                         Odoc_model.Lang.TypeExpr.Var "final"))))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        exists);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), [])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType bool)), [])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        for_all);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), [])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType bool)), [])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        count);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), [])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType int)), [])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        sum);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Package
                       {Odoc_model.Lang.TypeExpr.Package.path =
                         `Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Summable");
                        substitutions =
                         [(`Dot (`Root, "t"),
                           Odoc_model.Lang.TypeExpr.Var "sum")]},
                      Odoc_model.Lang.TypeExpr.Arrow (None,
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved
                           (`Identifier
                              (`Type
                                 (`ModuleType
                                    (`Root (Common.root, <abstr>), <abstr>),
                                  t))),
                        [Odoc_model.Lang.TypeExpr.Var "a";
                         Odoc_model.Lang.TypeExpr.Var "b"]),
                       Odoc_model.Lang.TypeExpr.Arrow
                        (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "a",
                         Odoc_model.Lang.TypeExpr.Var "sum"),
                        Odoc_model.Lang.TypeExpr.Var "sum")))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        find);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), [])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType option)),
                        [Odoc_model.Lang.TypeExpr.Var "a"])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        find_map);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "c"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType option)),
                         [Odoc_model.Lang.TypeExpr.Var "b"])),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType option)),
                        [Odoc_model.Lang.TypeExpr.Var "b"])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        to_list);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType list)),
                       [Odoc_model.Lang.TypeExpr.Var "a"]))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        to_array);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType array)),
                       [Odoc_model.Lang.TypeExpr.Var "a"]))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        min_elt);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "a",
                         Odoc_model.Lang.TypeExpr.Constr
                          (`Resolved (`Identifier (`CoreType int)), []))),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType option)),
                        [Odoc_model.Lang.TypeExpr.Var "a"])))};
                  Odoc_model.Lang.Signature.Value
                   {Odoc_model.Lang.Value.id =
                     `Value
                       (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                        max_elt);
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved
                          (`Identifier
                             (`Type
                                (`ModuleType
                                   (`Root (Common.root, <abstr>), <abstr>),
                                 t))),
                       [Odoc_model.Lang.TypeExpr.Var "a";
                        Odoc_model.Lang.TypeExpr.Var "b"]),
                      Odoc_model.Lang.TypeExpr.Arrow
                       (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Arrow (None,
                         Odoc_model.Lang.TypeExpr.Var "a",
                         Odoc_model.Lang.TypeExpr.Constr
                          (`Resolved (`Identifier (`CoreType int)), []))),
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved (`Identifier (`CoreType option)),
                        [Odoc_model.Lang.TypeExpr.Var "a"])))}]}}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).Generic;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Generic.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Generic.elt;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  [Odoc_model.Lang.TypeExpr.Var "a"]),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "b"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "b"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id =
        (root Base__Container_intf).Generic_phantom;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Generic_phantom.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None);
                  (Odoc_model.Lang.TypeDecl.Var "phantom", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Generic_phantom.elt;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any; Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType int)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Any; Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType bool)), []))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType unit)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                   [Odoc_model.Lang.TypeExpr.Var "accum";
                    Odoc_model.Lang.TypeExpr.Var "e"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "accum";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "accum",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Var "final"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Package
                 {Odoc_model.Lang.TypeExpr.Package.path =
                   `Dot
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Export),
                      "Summable");
                  substitutions =
                   [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            t))),
                  [Odoc_model.Lang.TypeExpr.Var "a";
                   Odoc_model.Lang.TypeExpr.Any]),
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Var "sum"),
                  Odoc_model.Lang.TypeExpr.Var "sum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), [])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "b"])),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Var "b"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType list)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType array)),
                 [Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Any]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved
                      (`Identifier
                         (`Type
                            (`ModuleType
                               (`Root (Common.root, <abstr>), <abstr>),
                             elt))),
                   [Odoc_model.Lang.TypeExpr.Var "a"]),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"]),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType int)), []))),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType option)),
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`ModuleType
                                (`Root (Common.root, <abstr>), <abstr>),
                              elt))),
                    [Odoc_model.Lang.TypeExpr.Var "a"])])))}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id =
        (root Base__Container_intf).Make_gen_arg;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Make_gen_arg.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Make_gen_arg.elt;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 [Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`ModuleType
                                 (`Root (Common.root, <abstr>), <abstr>),
                               elt))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Polymorphic_variant
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                  Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                 elements =
                  [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Define_using_fold";
                     constant = true; arguments = []; doc = <<docs>>};
                   Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Custom";
                     constant = false;
                     arguments =
                      [Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Root (Common.root, <abstr>), <abstr>),
                                   t))),
                         [Odoc_model.Lang.TypeExpr.Var "a"]),
                        Odoc_model.Lang.TypeExpr.Arrow
                         (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved
                              (`Identifier
                                 (`Type
                                    (`ModuleType
                                       (`Root (Common.root, <abstr>),
                                        <abstr>),
                                     elt))),
                           [Odoc_model.Lang.TypeExpr.Var "a"]),
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved (`Identifier (`CoreType unit)),
                           [])),
                         Odoc_model.Lang.TypeExpr.Constr
                          (`Resolved (`Identifier (`CoreType unit)),
                          [])))];
                     doc = <<docs>>}]}};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Polymorphic_variant
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                  Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                 elements =
                  [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Define_using_fold";
                     constant = true; arguments = []; doc = <<docs>>};
                   Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Custom";
                     constant = false;
                     arguments =
                      [Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Root (Common.root, <abstr>), <abstr>),
                                   t))),
                         [Odoc_model.Lang.TypeExpr.Var "a"]),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType int)), []))];
                     doc = <<docs>>}]}}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).Make_arg;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.With
           (Odoc_model.Lang.ModuleType.Path
             (`Resolved
                (`Identifier (root Base__Container_intf).Make_gen_arg)),
           [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Root, "elt"),
             {Odoc_model.Lang.TypeDecl.Equation.params =
               [(Odoc_model.Lang.TypeDecl.Var "a", None)];
              private_ = false;
              manifest =
               Some
                (Odoc_model.Lang.TypeExpr.Constr
                  (`Dot (`Dot (`Dot (`Root "Base__", "Monad"), "Ident"), "t"),
                  [Odoc_model.Lang.TypeExpr.Var "a"]));
              constraints = []})]));
       display_expr = None; expansion = None};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).Make0_arg;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Module
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.Module.id =
               (root Base__Container_intf).Make0_arg.Elt;
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.Signature
                  [Odoc_model.Lang.Signature.Type
                    (Odoc_model.Lang.Signature.Ordinary,
                    {Odoc_model.Lang.TypeDecl.id =
                      (root Base__Container_intf).Make0_arg.Elt.t;
                     doc = <<docs>>;
                     equation =
                      {Odoc_model.Lang.TypeDecl.Equation.params = [];
                       private_ = false; manifest = None; constraints = []};
                     representation = None});
                   Odoc_model.Lang.Signature.Value
                    {Odoc_model.Lang.Value.id =
                      `Value
                        (`Module
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            Elt),
                         equal);
                     doc = <<docs>>;
                     type_ =
                      Odoc_model.Lang.TypeExpr.Arrow (None,
                       Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved
                           (`Identifier
                              (`Type
                                 (`Module
                                    (`ModuleType
                                       (`Root (Common.root, <abstr>),
                                        <abstr>),
                                     Elt),
                                  t))),
                        []),
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`Module
                                     (`ModuleType
                                        (`Root (Common.root, <abstr>),
                                         <abstr>),
                                      Elt),
                                   t))),
                         []),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType bool)), [])))}]);
              canonical = None; hidden = false; display_type = None;
              expansion = Some Odoc_model.Lang.Module.AlreadyASig});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Make0_arg.t;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params = [];
                private_ = false; manifest = None; constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), fold);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           t))),
                 []),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "accum",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "accum",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Resolved
                           (`Identifier
                              (root Base__Container_intf).Make0_arg.Elt),
                         "t"),
                     []),
                    Odoc_model.Lang.TypeExpr.Var "accum")),
                  Odoc_model.Lang.TypeExpr.Var "accum")))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Polymorphic_variant
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                  Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                 elements =
                  [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Define_using_fold";
                     constant = true; arguments = []; doc = <<docs>>};
                   Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Custom";
                     constant = false;
                     arguments =
                      [Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Root (Common.root, <abstr>), <abstr>),
                                   t))),
                         []),
                        Odoc_model.Lang.TypeExpr.Arrow
                         (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                         Odoc_model.Lang.TypeExpr.Arrow (None,
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Dot
                              (`Resolved
                                 (`Identifier
                                    (root Base__Container_intf).Make0_arg.Elt),
                               "t"),
                           []),
                          Odoc_model.Lang.TypeExpr.Constr
                           (`Resolved (`Identifier (`CoreType unit)),
                           [])),
                         Odoc_model.Lang.TypeExpr.Constr
                          (`Resolved (`Identifier (`CoreType unit)),
                          [])))];
                     doc = <<docs>>}]}};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Polymorphic_variant
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                  Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                 elements =
                  [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Define_using_fold";
                     constant = true; arguments = []; doc = <<docs>>};
                   Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                    {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                      = "Custom";
                     constant = false;
                     arguments =
                      [Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved
                            (`Identifier
                               (`Type
                                  (`ModuleType
                                     (`Root (Common.root, <abstr>), <abstr>),
                                   t))),
                         []),
                        Odoc_model.Lang.TypeExpr.Constr
                         (`Resolved (`Identifier (`CoreType int)), []))];
                     doc = <<docs>>}]}}]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig};
     Odoc_model.Lang.Signature.ModuleType
      {Odoc_model.Lang.ModuleType.id = (root Base__Container_intf).Container;
       doc = <<docs>>;
       expr =
        Some
         (Odoc_model.Lang.ModuleType.Signature
           [Odoc_model.Lang.Signature.Include
             {Odoc_model.Lang.Include.parent =
               `ModuleType (`Root (Common.root, <abstr>), <abstr>);
              doc = <<docs>>;
              decl =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.TypeOf
                  (Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.Include
                        {Odoc_model.Lang.Include.parent =
                          `ModuleType (`Root (Common.root, <abstr>), <abstr>);
                         doc = <<docs>>;
                         decl =
                          Odoc_model.Lang.Module.Alias
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export));
                         inline = false;
                         expansion =
                          {Odoc_model.Lang.Include.resolved = false;
                           content =
                            [Odoc_model.Lang.Signature.Module
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.Module.id =
                                (root Base__Container_intf).Container.Continue_or_stop;
                               doc = <<docs>>;
                               type_ =
                                Odoc_model.Lang.Module.Alias
                                 (`Dot
                                    (`Resolved
                                       (`Identifier
                                          (root Base__Container_intf).Export),
                                     "Continue_or_stop"));
                               canonical = None; hidden = false;
                               display_type = None; expansion = None});
                             Odoc_model.Lang.Signature.ModuleType
                              {Odoc_model.Lang.ModuleType.id =
                                (root Base__Container_intf).Container.$Summable;
                               doc = <<docs>>;
                               expr =
                                Some
                                 (Odoc_model.Lang.ModuleType.Signature
                                   [Odoc_model.Lang.Signature.Type
                                     (Odoc_model.Lang.Signature.Ordinary,
                                     {Odoc_model.Lang.TypeDecl.id =
                                       (root Base__Container_intf).Container.$Summable.t;
                                      doc = <<docs>>;
                                      equation =
                                       {Odoc_model.Lang.TypeDecl.Equation.params
                                         = [];
                                        private_ = false; manifest = None;
                                        constraints = []};
                                      representation = None});
                                    Odoc_model.Lang.Signature.Value
                                     {Odoc_model.Lang.Value.id =
                                       `Value
                                         (`ModuleType
                                            (`ModuleType
                                               (`Root (Common.root, <abstr>),
                                                <abstr>),
                                             <abstr>),
                                          zero);
                                      doc = <<docs>>;
                                      type_ =
                                       Odoc_model.Lang.TypeExpr.Constr
                                        (`Resolved
                                           (`Identifier
                                              (`Type
                                                 (`ModuleType
                                                    (`ModuleType
                                                       (`Root
                                                          (Common.root,
                                                           <abstr>),
                                                        <abstr>),
                                                     <abstr>),
                                                  t))),
                                        [])};
                                    Odoc_model.Lang.Signature.Value
                                     {Odoc_model.Lang.Value.id =
                                       `Value
                                         (`ModuleType
                                            (`ModuleType
                                               (`Root (Common.root, <abstr>),
                                                <abstr>),
                                             <abstr>),
                                          (+));
                                      doc = <<docs>>;
                                      type_ =
                                       Odoc_model.Lang.TypeExpr.Arrow (None,
                                        Odoc_model.Lang.TypeExpr.Constr
                                         (`Resolved
                                            (`Identifier
                                               (`Type
                                                  (`ModuleType
                                                     (`ModuleType
                                                        (`Root
                                                           (Common.root,
                                                            <abstr>),
                                                         <abstr>),
                                                      <abstr>),
                                                   t))),
                                         []),
                                        Odoc_model.Lang.TypeExpr.Arrow (None,
                                         Odoc_model.Lang.TypeExpr.Constr
                                          (`Resolved
                                             (`Identifier
                                                (`Type
                                                   (`ModuleType
                                                      (`ModuleType
                                                         (`Root
                                                            (Common.root,
                                                             <abstr>),
                                                          <abstr>),
                                                       <abstr>),
                                                    t))),
                                          []),
                                         Odoc_model.Lang.TypeExpr.Constr
                                          (`Resolved
                                             (`Identifier
                                                (`Type
                                                   (`ModuleType
                                                      (`ModuleType
                                                         (`Root
                                                            (Common.root,
                                                             <abstr>),
                                                          <abstr>),
                                                       <abstr>),
                                                    t))),
                                          [])))}]);
                               display_expr = None;
                               expansion =
                                Some Odoc_model.Lang.Module.AlreadyASig}]}}])));
              inline = false;
              expansion =
               {Odoc_model.Lang.Include.resolved = false;
                content =
                 [Odoc_model.Lang.Signature.Module
                   (Odoc_model.Lang.Signature.Ordinary,
                   {Odoc_model.Lang.Module.id =
                     (root Base__Container_intf).Container.Continue_or_stop;
                    doc = <<docs>>;
                    type_ =
                     Odoc_model.Lang.Module.Alias
                      (`Dot
                         (`Resolved
                            (`Identifier (root Base__Container_intf).Export),
                          "Continue_or_stop"));
                    canonical = None; hidden = false; display_type = None;
                    expansion = None});
                  Odoc_model.Lang.Signature.ModuleType
                   {Odoc_model.Lang.ModuleType.id =
                     (root Base__Container_intf).Container.$Summable;
                    doc = <<docs>>;
                    expr =
                     Some
                      (Odoc_model.Lang.ModuleType.Signature
                        [Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            (root Base__Container_intf).Container.$Summable.t;
                           doc = <<docs>>;
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params = [];
                             private_ = false; manifest = None;
                             constraints = []};
                           representation = None});
                         Odoc_model.Lang.Signature.Value
                          {Odoc_model.Lang.Value.id =
                            `Value
                              (`ModuleType
                                 (`ModuleType
                                    (`Root (Common.root, <abstr>), <abstr>),
                                  <abstr>),
                               zero);
                           doc = <<docs>>;
                           type_ =
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved
                                (`Identifier
                                   (`Type
                                      (`ModuleType
                                         (`ModuleType
                                            (`Root (Common.root, <abstr>),
                                             <abstr>),
                                          <abstr>),
                                       t))),
                             [])};
                         Odoc_model.Lang.Signature.Value
                          {Odoc_model.Lang.Value.id =
                            `Value
                              (`ModuleType
                                 (`ModuleType
                                    (`Root (Common.root, <abstr>), <abstr>),
                                  <abstr>),
                               (+));
                           doc = <<docs>>;
                           type_ =
                            Odoc_model.Lang.TypeExpr.Arrow (None,
                             Odoc_model.Lang.TypeExpr.Constr
                              (`Resolved
                                 (`Identifier
                                    (`Type
                                       (`ModuleType
                                          (`ModuleType
                                             (`Root (Common.root, <abstr>),
                                              <abstr>),
                                           <abstr>),
                                        t))),
                              []),
                             Odoc_model.Lang.TypeExpr.Arrow (None,
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`ModuleType
                                           (`ModuleType
                                              (`Root (Common.root, <abstr>),
                                               <abstr>),
                                            <abstr>),
                                         t))),
                               []),
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`ModuleType
                                           (`ModuleType
                                              (`Root (Common.root, <abstr>),
                                               <abstr>),
                                            <abstr>),
                                         t))),
                               [])))}]);
                    display_expr = None;
                    expansion = Some Odoc_model.Lang.Module.AlreadyASig}]}};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.S0;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved (`Identifier (root Base__Container_intf).S0)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.S0_phantom;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier (root Base__Container_intf).S0_phantom)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.S1;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved (`Identifier (root Base__Container_intf).S1)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.S1_phantom_invariant;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier
                        (root Base__Container_intf).S1_phantom_invariant)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.S1_phantom;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier (root Base__Container_intf).S1_phantom)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.Generic;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier (root Base__Container_intf).Generic)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.Generic_phantom;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier (root Base__Container_intf).Generic_phantom)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.ModuleType
             {Odoc_model.Lang.ModuleType.id =
               (root Base__Container_intf).Container.Summable;
              doc = <<docs>>;
              expr =
               Some
                (Odoc_model.Lang.ModuleType.Path
                  (`Resolved
                     (`Identifier
                        (root Base__Container_intf).Container.$Summable)));
              display_expr = None; expansion = None};
            Odoc_model.Lang.Signature.Comment (`Docs <<docs>>);
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Container.fold;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "t", None);
                  (Odoc_model.Lang.TypeDecl.Var "a", None);
                  (Odoc_model.Lang.TypeDecl.Var "accum", None)];
                private_ = false;
                manifest =
                 Some
                  (Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "t",
                    Odoc_model.Lang.TypeExpr.Arrow
                     (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                     Odoc_model.Lang.TypeExpr.Var "accum",
                     Odoc_model.Lang.TypeExpr.Arrow
                      (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                      Odoc_model.Lang.TypeExpr.Arrow (None,
                       Odoc_model.Lang.TypeExpr.Var "accum",
                       Odoc_model.Lang.TypeExpr.Arrow (None,
                        Odoc_model.Lang.TypeExpr.Var "a",
                        Odoc_model.Lang.TypeExpr.Var "accum")),
                      Odoc_model.Lang.TypeExpr.Var "accum"))));
                constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Container.iter;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "t", None);
                  (Odoc_model.Lang.TypeDecl.Var "a", None)];
                private_ = false;
                manifest =
                 Some
                  (Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "t",
                    Odoc_model.Lang.TypeExpr.Arrow
                     (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                     Odoc_model.Lang.TypeExpr.Arrow (None,
                      Odoc_model.Lang.TypeExpr.Var "a",
                      Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType unit)), [])),
                     Odoc_model.Lang.TypeExpr.Constr
                      (`Resolved (`Identifier (`CoreType unit)), []))));
                constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Type
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.TypeDecl.id =
               (root Base__Container_intf).Container.length;
              doc = <<docs>>;
              equation =
               {Odoc_model.Lang.TypeDecl.Equation.params =
                 [(Odoc_model.Lang.TypeDecl.Var "t", None)];
                private_ = false;
                manifest =
                 Some
                  (Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "t",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType int)), [])));
                constraints = []};
              representation = None});
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), iter);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType unit)), [])]),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), count);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType int)), [])]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType bool)), [])),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType int)), []))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  min_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType int)), []))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  max_elt);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "compare"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType int)), []))),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  length);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Any;
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType int)), [])]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType int)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_list);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType list)),
                   [Odoc_model.Lang.TypeExpr.Var "a"])]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType list)),
                  [Odoc_model.Lang.TypeExpr.Var "a"])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), sum);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Var "sum"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Package
                  {Odoc_model.Lang.TypeExpr.Package.path =
                    `Resolved
                      (`Identifier
                         (root Base__Container_intf).Container.Summable);
                   substitutions =
                    [(`Dot (`Root, "t"), Odoc_model.Lang.TypeExpr.Var "sum")]},
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "t",
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Var "sum"),
                   Odoc_model.Lang.TypeExpr.Var "sum"))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_result);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Var "b"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "b",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "b",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                     [Odoc_model.Lang.TypeExpr.Var "b";
                      Odoc_model.Lang.TypeExpr.Var "e"]))),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "t",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Dot (`Dot (`Root "Base__", "Result"), "t"),
                    [Odoc_model.Lang.TypeExpr.Var "b";
                     Odoc_model.Lang.TypeExpr.Var "e"])))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  fold_until);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "fold"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           fold))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a";
                  Odoc_model.Lang.TypeExpr.Var "b"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "init"),
                 Odoc_model.Lang.TypeExpr.Var "b",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "b",
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "a",
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Identifier (root Base__Container_intf).Export),
                            "Continue_or_stop"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "b";
                      Odoc_model.Lang.TypeExpr.Var "final"]))),
                  Odoc_model.Lang.TypeExpr.Arrow
                   (Some (Odoc_model.Lang.TypeExpr.Label "finish"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "b",
                    Odoc_model.Lang.TypeExpr.Var "final"),
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Var "t",
                    Odoc_model.Lang.TypeExpr.Var "final")))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  is_empty);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType bool)), [])))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  exists);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType bool)), [])),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), []))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  for_all);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType bool)), [])),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType bool)), []))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>), find);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType bool)), [])),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "a"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  find_map);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           iter))),
                 [Odoc_model.Lang.TypeExpr.Var "t";
                  Odoc_model.Lang.TypeExpr.Var "a"]),
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "t",
                 Odoc_model.Lang.TypeExpr.Arrow
                  (Some (Odoc_model.Lang.TypeExpr.Label "f"),
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Var "a",
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType option)),
                    [Odoc_model.Lang.TypeExpr.Var "b"])),
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType option)),
                   [Odoc_model.Lang.TypeExpr.Var "b"]))))};
            Odoc_model.Lang.Signature.Value
             {Odoc_model.Lang.Value.id =
               `Value
                 (`ModuleType (`Root (Common.root, <abstr>), <abstr>),
                  to_array);
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.TypeExpr.Arrow
                (Some (Odoc_model.Lang.TypeExpr.Label "length"),
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type
                          (`ModuleType
                             (`Root (Common.root, <abstr>), <abstr>),
                           length))),
                 [Odoc_model.Lang.TypeExpr.Var "t"]),
                Odoc_model.Lang.TypeExpr.Arrow
                 (Some (Odoc_model.Lang.TypeExpr.Label "iter"),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved
                     (`Identifier
                        (`Type
                           (`ModuleType
                              (`Root (Common.root, <abstr>), <abstr>),
                            iter))),
                  [Odoc_model.Lang.TypeExpr.Var "t";
                   Odoc_model.Lang.TypeExpr.Var "a"]),
                 Odoc_model.Lang.TypeExpr.Arrow (None,
                  Odoc_model.Lang.TypeExpr.Var "t",
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType array)),
                   [Odoc_model.Lang.TypeExpr.Var "a"]))))};
            Odoc_model.Lang.Signature.Module
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.Module.id =
               (root Base__Container_intf).Container.Make;
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.Functor
                  (Odoc_model.Lang.FunctorParameter.Named
                    {Odoc_model.Lang.FunctorParameter.id =
                      (param (root Base__Container_intf).Container.Make T);
                     expr =
                      Odoc_model.Lang.ModuleType.Path
                       (`Resolved
                          (`Identifier (root Base__Container_intf).Make_arg));
                     display_expr = None; expansion = None},
                  Odoc_model.Lang.ModuleType.With
                   (Odoc_model.Lang.ModuleType.Path
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Container.S1)),
                   [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Root, "t"),
                     {Odoc_model.Lang.TypeDecl.Equation.params =
                       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                      private_ = false;
                      manifest =
                       Some
                        (Odoc_model.Lang.TypeExpr.Constr
                          (`Dot
                             (`Resolved
                                (`Identifier
                                   (param (root Base__Container_intf).Container.Make T)),
                              "t"),
                          [Odoc_model.Lang.TypeExpr.Var "a"]));
                      constraints = []})])));
              canonical = None; hidden = false; display_type = None;
              expansion = None});
            Odoc_model.Lang.Signature.Module
             (Odoc_model.Lang.Signature.Ordinary,
             {Odoc_model.Lang.Module.id =
               (root Base__Container_intf).Container.Make0;
              doc = <<docs>>;
              type_ =
               Odoc_model.Lang.Module.ModuleType
                (Odoc_model.Lang.ModuleType.Functor
                  (Odoc_model.Lang.FunctorParameter.Named
                    {Odoc_model.Lang.FunctorParameter.id =
                      (param (root Base__Container_intf).Container.Make0 T);
                     expr =
                      Odoc_model.Lang.ModuleType.Path
                       (`Resolved
                          (`Identifier (root Base__Container_intf).Make0_arg));
                     display_expr = None; expansion = None},
                  Odoc_model.Lang.ModuleType.With
                   (Odoc_model.Lang.ModuleType.Path
                     (`Resolved
                        (`Identifier (root Base__Container_intf).Container.S0)),
                   [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Root, "t"),
                     {Odoc_model.Lang.TypeDecl.Equation.params = [];
                      private_ = false;
                      manifest =
                       Some
                        (Odoc_model.Lang.TypeExpr.Constr
                          (`Dot
                             (`Resolved
                                (`Identifier
                                   (param (root Base__Container_intf).Container.Make0 T)),
                              "t"),
                          []));
                      constraints = []});
                    Odoc_model.Lang.ModuleType.TypeSubst
                     (`Dot (`Root, "elt"),
                     {Odoc_model.Lang.TypeDecl.Equation.params = [];
                      private_ = false;
                      manifest =
                       Some
                        (Odoc_model.Lang.TypeExpr.Constr
                          (`Dot
                             (`Dot
                                (`Resolved
                                   (`Identifier
                                      (param (root Base__Container_intf).Container.Make0 T)),
                                 "Elt"),
                              "t"),
                          []));
                      constraints = []})])));
              canonical = None; hidden = false; display_type = None;
              expansion = None})]);
       display_expr = None;
       expansion = Some Odoc_model.Lang.Module.AlreadyASig}];
  expansion = None}
```

```ocaml env=e1
# sg;;
- : Odoc_model.Paths_types.Identifier.reference_module *
    Odoc_model.Comment.docs * Odoc_model.Lang.Signature.t
=
((root Root), <<docs>>,
 [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).M; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).M.t; doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value (`Module (`Root (Common.root, <abstr>), M), id);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).Mextended; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `Module (`Root (Common.root, <abstr>), Mextended);
           doc = <<docs>>;
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.ModuleType
                 (Odoc_model.Lang.ModuleType.Signature
                   [Odoc_model.Lang.Signature.Include
                     {Odoc_model.Lang.Include.parent =
                       `Module (`Root (Common.root, <abstr>), Mextended);
                      doc = <<docs>>;
                      decl =
                       Odoc_model.Lang.Module.Alias
                        (`Resolved (`Identifier (root Root).M));
                      inline = false;
                      expansion =
                       {Odoc_model.Lang.Include.resolved = false;
                        content =
                         [Odoc_model.Lang.Signature.Type
                           (Odoc_model.Lang.Signature.Ordinary,
                           {Odoc_model.Lang.TypeDecl.id =
                             (root Root).Mextended.$t;
                            doc = <<docs>>;
                            equation =
                             {Odoc_model.Lang.TypeDecl.Equation.params = [];
                              private_ = false;
                              manifest =
                               Some
                                (Odoc_model.Lang.TypeExpr.Constr
                                  (`Dot
                                     (`Resolved (`Identifier (root Root).M),
                                      "t"),
                                  []));
                              constraints = []};
                            representation = None});
                          Odoc_model.Lang.Signature.Value
                           {Odoc_model.Lang.Value.id =
                             `Value
                               (`Module
                                  (`Root (Common.root, <abstr>), Mextended),
                                id);
                            doc = <<docs>>;
                            type_ =
                             Odoc_model.Lang.TypeExpr.Arrow (None,
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         $t))),
                               []),
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         $t))),
                               []))}]}}])));
           inline = false;
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             content =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.$t;
                 doc = <<docs>>;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false;
                   manifest =
                    Some
                     (Odoc_model.Lang.TypeExpr.Constr
                       (`Dot (`Resolved (`Identifier (root Root).M), "t"),
                       []));
                   constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Value
                {Odoc_model.Lang.Value.id =
                  `Value
                    (`Module (`Root (Common.root, <abstr>), Mextended), id);
                 doc = <<docs>>;
                 type_ =
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t))),
                    []))}]}};
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.t;
           doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value
              (`Module (`Root (Common.root, <abstr>), Mextended), ignore);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type
                       (`Module (`Root (Common.root, <abstr>), Mextended), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType unit)), []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig})])
```
