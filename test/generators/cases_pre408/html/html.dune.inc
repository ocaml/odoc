(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../alias.odocl})
   (with-stdout-to
    Alias.index.html.gen
    (progn
     (system "cat 'html.gen/test/Alias/index.html'")))
   (with-stdout-to
    Alias.Foo__X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Alias/Foo__X/index.html'")))
   (with-stdout-to
    Alias.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Alias/X/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Alias.index.html Alias.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Alias.Foo__X.index.html Alias.Foo__X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Alias.X.index.html Alias.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../bugs.odocl})
   (with-stdout-to
    Bugs.index.html.gen
    (progn
     (system "cat 'html.gen/test/Bugs/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs.index.html Bugs.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../bugs_pre_410.odocl})
   (with-stdout-to
    Bugs_pre_410.index.html.gen
    (progn
     (system "cat 'html.gen/test/Bugs_pre_410/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs_pre_410.index.html Bugs_pre_410.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../class.odocl})
   (with-stdout-to
    Class.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/index.html'")))
   (with-stdout-to
    Class.class-type-empty.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-type-empty/index.html'")))
   (with-stdout-to
    Class.class-type-mutually.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-type-mutually/index.html'")))
   (with-stdout-to
    Class.class-type-recursive.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-type-recursive/index.html'")))
   (with-stdout-to
    Class.class-mutually'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-mutually'\\''/index.html'")))
   (with-stdout-to
    Class.class-recursive'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-recursive'\\''/index.html'")))
   (with-stdout-to
    Class.class-type-empty_virtual.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-type-empty_virtual/index.html'")))
   (with-stdout-to
    Class.class-empty_virtual'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-empty_virtual'\\''/index.html'")))
   (with-stdout-to
    Class.class-type-polymorphic.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-type-polymorphic/index.html'")))
   (with-stdout-to
    Class.class-polymorphic'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Class/class-polymorphic'\\''/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Class.index.html Class.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-type-empty.index.html
   Class.class-type-empty.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-type-mutually.index.html
   Class.class-type-mutually.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-type-recursive.index.html
   Class.class-type-recursive.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-mutually'.index.html
   Class.class-mutually'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-recursive'.index.html
   Class.class-recursive'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-type-empty_virtual.index.html
   Class.class-type-empty_virtual.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-empty_virtual'.index.html
   Class.class-empty_virtual'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-type-polymorphic.index.html
   Class.class-type-polymorphic.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Class.class-polymorphic'.index.html
   Class.class-polymorphic'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../external.odocl})
   (with-stdout-to
    External.index.html.gen
    (progn
     (system "cat 'html.gen/test/External/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff External.index.html External.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../functor.odocl})
   (with-stdout-to
    Functor.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/index.html'")))
   (with-stdout-to
    Functor.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/module-type-S/index.html'")))
   (with-stdout-to
    Functor.module-type-S1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/module-type-S1/index.html'")))
   (with-stdout-to
    Functor.module-type-S1.argument-1-_.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Functor/module-type-S1/argument-1-_/index.html'")))
   (with-stdout-to
    Functor.F1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F1/index.html'")))
   (with-stdout-to
    Functor.F1.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F1/argument-1-Arg/index.html'")))
   (with-stdout-to
    Functor.F2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F2/index.html'")))
   (with-stdout-to
    Functor.F2.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F2/argument-1-Arg/index.html'")))
   (with-stdout-to
    Functor.F3.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F3/index.html'")))
   (with-stdout-to
    Functor.F3.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F3/argument-1-Arg/index.html'")))
   (with-stdout-to
    Functor.F4.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F4/index.html'")))
   (with-stdout-to
    Functor.F4.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F4/argument-1-Arg/index.html'")))
   (with-stdout-to
    Functor.F5.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor/F5/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor.index.html Functor.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.module-type-S.index.html
   Functor.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.module-type-S1.index.html
   Functor.module-type-S1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.module-type-S1.argument-1-_.index.html
   Functor.module-type-S1.argument-1-_.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F1.index.html Functor.F1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.F1.argument-1-Arg.index.html
   Functor.F1.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F2.index.html Functor.F2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.F2.argument-1-Arg.index.html
   Functor.F2.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F3.index.html Functor.F3.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.F3.argument-1-Arg.index.html
   Functor.F3.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F4.index.html Functor.F4.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor.F4.argument-1-Arg.index.html
   Functor.F4.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F5.index.html Functor.F5.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../functor2.odocl})
   (with-stdout-to
    Functor2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/index.html'")))
   (with-stdout-to
    Functor2.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/module-type-S/index.html'")))
   (with-stdout-to
    Functor2.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/X/index.html'")))
   (with-stdout-to
    Functor2.X.argument-1-Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/X/argument-1-Y/index.html'")))
   (with-stdout-to
    Functor2.X.argument-2-Z.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/X/argument-2-Z/index.html'")))
   (with-stdout-to
    Functor2.module-type-XF.index.html.gen
    (progn
     (system "cat 'html.gen/test/Functor2/module-type-XF/index.html'")))
   (with-stdout-to
    Functor2.module-type-XF.argument-1-Y.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Functor2/module-type-XF/argument-1-Y/index.html'")))
   (with-stdout-to
    Functor2.module-type-XF.argument-2-Z.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Functor2/module-type-XF/argument-2-Z/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor2.index.html Functor2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.module-type-S.index.html
   Functor2.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor2.X.index.html Functor2.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.X.argument-1-Y.index.html
   Functor2.X.argument-1-Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.X.argument-2-Z.index.html
   Functor2.X.argument-2-Z.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.module-type-XF.index.html
   Functor2.module-type-XF.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.module-type-XF.argument-1-Y.index.html
   Functor2.module-type-XF.argument-1-Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Functor2.module-type-XF.argument-2-Z.index.html
   Functor2.module-type-XF.argument-2-Z.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../include.odocl})
   (with-stdout-to
    Include.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include/index.html'")))
   (with-stdout-to
    Include.module-type-Not_inlined.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include/module-type-Not_inlined/index.html'")))
   (with-stdout-to
    Include.module-type-Inlined.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include/module-type-Inlined/index.html'")))
   (with-stdout-to
    Include.module-type-Not_inlined_and_closed.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include/module-type-Not_inlined_and_closed/index.html'")))
   (with-stdout-to
    Include.module-type-Not_inlined_and_opened.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include/module-type-Not_inlined_and_opened/index.html'")))
   (with-stdout-to
    Include.module-type-Inherent_Module.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include/module-type-Inherent_Module/index.html'")))
   (with-stdout-to
    Include.module-type-Dorminant_Module.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include/module-type-Dorminant_Module/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Include.index.html Include.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Not_inlined.index.html
   Include.module-type-Not_inlined.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Inlined.index.html
   Include.module-type-Inlined.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Not_inlined_and_closed.index.html
   Include.module-type-Not_inlined_and_closed.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Not_inlined_and_opened.index.html
   Include.module-type-Not_inlined_and_opened.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Inherent_Module.index.html
   Include.module-type-Inherent_Module.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include.module-type-Dorminant_Module.index.html
   Include.module-type-Dorminant_Module.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../include2.odocl})
   (with-stdout-to
    Include2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include2/index.html'")))
   (with-stdout-to
    Include2.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include2/X/index.html'")))
   (with-stdout-to
    Include2.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include2/Y/index.html'")))
   (with-stdout-to
    Include2.Y_include_synopsis.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include2/Y_include_synopsis/index.html'")))
   (with-stdout-to
    Include2.Y_include_doc.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include2/Y_include_doc/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Include2.index.html Include2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.X.index.html Include2.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.Y.index.html Include2.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include2.Y_include_synopsis.index.html
   Include2.Y_include_synopsis.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include2.Y_include_doc.index.html
   Include2.Y_include_doc.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run
    odoc
    html-generate
    --indent
    -o
    html.gen
    %{dep:../include_sections.odocl})
   (with-stdout-to
    Include_sections.index.html.gen
    (progn
     (system "cat 'html.gen/test/Include_sections/index.html'")))
   (with-stdout-to
    Include_sections.module-type-Something.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Include_sections/module-type-Something/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Include_sections.index.html Include_sections.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Include_sections.module-type-Something.index.html
   Include_sections.module-type-Something.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../interlude.odocl})
   (with-stdout-to
    Interlude.index.html.gen
    (progn
     (system "cat 'html.gen/test/Interlude/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Interlude.index.html Interlude.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../markup.odocl})
   (with-stdout-to
    Markup.index.html.gen
    (progn
     (system "cat 'html.gen/test/Markup/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Markup.index.html Markup.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../page-mld.odocl})
   (with-stdout-to
    mld.html.gen
    (progn
     (system "cat 'html.gen/test/mld.html'"))))))

(rule
 (alias runtest)
 (action
  (diff mld.html mld.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../module.odocl})
   (with-stdout-to
    Module.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/index.html'")))
   (with-stdout-to
    Module.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S/index.html'")))
   (with-stdout-to
    Module.module-type-S.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S/M/index.html'")))
   (with-stdout-to
    Module.module-type-S2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S2/index.html'")))
   (with-stdout-to
    Module.module-type-S2.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S2/M/index.html'")))
   (with-stdout-to
    Module.module-type-S3.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S3/index.html'")))
   (with-stdout-to
    Module.module-type-S3.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S3/M/index.html'")))
   (with-stdout-to
    Module.module-type-S4.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S4/index.html'")))
   (with-stdout-to
    Module.module-type-S4.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S4/M/index.html'")))
   (with-stdout-to
    Module.module-type-S5.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S5/index.html'")))
   (with-stdout-to
    Module.module-type-S5.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S5/M/index.html'")))
   (with-stdout-to
    Module.module-type-S6.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S6/index.html'")))
   (with-stdout-to
    Module.module-type-S6.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S6/M/index.html'")))
   (with-stdout-to
    Module.M'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/M'\\''/index.html'")))
   (with-stdout-to
    Module.module-type-S7.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S7/index.html'")))
   (with-stdout-to
    Module.module-type-S8.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S8/index.html'")))
   (with-stdout-to
    Module.module-type-S9.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/module-type-S9/index.html'")))
   (with-stdout-to
    Module.Mutually.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/Mutually/index.html'")))
   (with-stdout-to
    Module.Recursive.index.html.gen
    (progn
     (system "cat 'html.gen/test/Module/Recursive/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Module.index.html Module.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.module-type-S.index.html Module.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S.M.index.html
   Module.module-type-S.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S2.index.html
   Module.module-type-S2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S2.M.index.html
   Module.module-type-S2.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S3.index.html
   Module.module-type-S3.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S3.M.index.html
   Module.module-type-S3.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S4.index.html
   Module.module-type-S4.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S4.M.index.html
   Module.module-type-S4.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S5.index.html
   Module.module-type-S5.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S5.M.index.html
   Module.module-type-S5.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S6.index.html
   Module.module-type-S6.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S6.M.index.html
   Module.module-type-S6.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.M'.index.html Module.M'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S7.index.html
   Module.module-type-S7.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S8.index.html
   Module.module-type-S8.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Module.module-type-S9.index.html
   Module.module-type-S9.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.Mutually.index.html Module.Mutually.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.Recursive.index.html Module.Recursive.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../nested.odocl})
   (with-stdout-to
    Nested.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/index.html'")))
   (with-stdout-to
    Nested.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/X/index.html'")))
   (with-stdout-to
    Nested.module-type-Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/module-type-Y/index.html'")))
   (with-stdout-to
    Nested.F.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/F/index.html'")))
   (with-stdout-to
    Nested.F.argument-1-Arg1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/F/argument-1-Arg1/index.html'")))
   (with-stdout-to
    Nested.F.argument-2-Arg2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/F/argument-2-Arg2/index.html'")))
   (with-stdout-to
    Nested.class-z.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/class-z/index.html'")))
   (with-stdout-to
    Nested.class-inherits.index.html.gen
    (progn
     (system "cat 'html.gen/test/Nested/class-inherits/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Nested.index.html Nested.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.X.index.html Nested.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.module-type-Y.index.html Nested.module-type-Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.F.index.html Nested.F.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Nested.F.argument-1-Arg1.index.html
   Nested.F.argument-1-Arg1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Nested.F.argument-2-Arg2.index.html
   Nested.F.argument-2-Arg2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.class-z.index.html Nested.class-z.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Nested.class-inherits.index.html
   Nested.class-inherits.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../ocamlary.odocl})
   (with-stdout-to
    Ocamlary.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/index.html'")))
   (with-stdout-to
    Ocamlary.Empty.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Empty/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-Empty.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-Empty/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MissingComment.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-MissingComment/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-EmptySig.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-EmptySig/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-EmptySigAlias.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-EmptySigAlias/index.html'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignature.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/ModuleWithSignature/index.html'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignatureAlias.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/ModuleWithSignatureAlias/index.html'")))
   (with-stdout-to
    Ocamlary.One.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/One/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SigForMod.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-SigForMod/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SigForMod.Inner.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SigForMod/Inner/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SigForMod.Inner.module-type-Empty.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SigForMod/Inner/module-type-Empty/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-SuperSig/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-SubSigA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-SubSigA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-SubSigA.SubSigAMod.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-SubSigA/SubSigAMod/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-SubSigB.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-SubSigB/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-EmptySig.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-EmptySig/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-One.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-One/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-SuperSig.module-type-SuperSig.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-SuperSig/module-type-SuperSig/index.html'")))
   (with-stdout-to
    Ocamlary.Buffer.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Buffer/index.html'")))
   (with-stdout-to
    Ocamlary.CollectionModule.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/CollectionModule/index.html'")))
   (with-stdout-to
    Ocamlary.CollectionModule.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CollectionModule/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CollectionModule/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.CollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CollectionModule/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.CollectionModule.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CollectionModule/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-COLLECTION.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-COLLECTION/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-COLLECTION.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-COLLECTION/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-COLLECTION.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-COLLECTION/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-COLLECTION.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-COLLECTION/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-COLLECTION.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-COLLECTION/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Recollection/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.argument-1-C.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/argument-1-C/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.argument-1-C.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/argument-1-C/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.argument-1-C.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/argument-1-C/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.argument-1-C.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/argument-1-C/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.argument-1-C.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/argument-1-C/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.Recollection.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Recollection/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-MMM/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.C.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-MMM/C/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.C.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-MMM/C/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.C.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-MMM/C/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.C.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-MMM/C/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-MMM.C.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-MMM/C/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RECOLLECTION.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RECOLLECTION/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RecollectionModule.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RecollectionModule/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RecollectionModule.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RecollectionModule/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RecollectionModule.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RecollectionModule/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RecollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RecollectionModule/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-RecollectionModule.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-RecollectionModule/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-A/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.Q.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-A/Q/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.Q.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-A/Q/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.Q.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-A/Q/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-A/Q/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-A.Q.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-A/Q/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-B/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.Q.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-B/Q/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.Q.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-B/Q/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.Q.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-B/Q/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-B/Q/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-B.Q.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-B/Q/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-C/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.Q.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-C/Q/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.Q.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-C/Q/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.Q.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-C/Q/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-C/Q/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-C.Q.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-C/Q/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/FunctorTypeOf/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.argument-1-Collection.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/FunctorTypeOf/argument-1-Collection/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/FunctorTypeOf/argument-1-Collection/InnerModuleA/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.InnerModuleA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/FunctorTypeOf/argument-1-Collection/InnerModuleA/InnerModuleA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/FunctorTypeOf/argument-1-Collection/InnerModuleA/module-type-InnerModuleTypeA'\\''/index.html'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.argument-1-Collection.module-type-InnerModuleTypeA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/FunctorTypeOf/argument-1-Collection/module-type-InnerModuleTypeA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-IncludeModuleType.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-IncludeModuleType/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-ToInclude.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-ToInclude/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-ToInclude.IncludedA.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-ToInclude/IncludedA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-ToInclude.module-type-IncludedB.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-ToInclude/module-type-IncludedB/index.html'")))
   (with-stdout-to
    Ocamlary.IncludedA.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/IncludedA/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-IncludedB.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-IncludedB/index.html'")))
   (with-stdout-to
    Ocamlary.ExtMod.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/ExtMod/index.html'")))
   (with-stdout-to
    Ocamlary.class-empty_class.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/class-empty_class/index.html'")))
   (with-stdout-to
    Ocamlary.class-one_method_class.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/class-one_method_class/index.html'")))
   (with-stdout-to
    Ocamlary.class-two_method_class.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/class-two_method_class/index.html'")))
   (with-stdout-to
    Ocamlary.class-param_class.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/class-param_class/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep1/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep1/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.module-type-S.class-c.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep1/module-type-S/class-c/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep1/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.X.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep1/X/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Dep1.X.Y.class-c.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep1/X/Y/class-c/index.html'")))
   (with-stdout-to
    Ocamlary.Dep2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep2/index.html'")))
   (with-stdout-to
    Ocamlary.Dep2.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep2/argument-1-Arg/index.html'")))
   (with-stdout-to
    Ocamlary.Dep2.argument-1-Arg.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep2/argument-1-Arg/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep2.A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep2/A/index.html'")))
   (with-stdout-to
    Ocamlary.Dep3.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep3/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.module-type-T.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.module-type-S.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/module-type-S/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.module-type-S.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/module-type-S/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Dep4.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep4/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep5.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep5/index.html'")))
   (with-stdout-to
    Ocamlary.Dep5.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep5/argument-1-Arg/index.html'")))
   (with-stdout-to
    Ocamlary.Dep5.argument-1-Arg.module-type-S.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep5/argument-1-Arg/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.Dep5.argument-1-Arg.module-type-S.Y.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep5/argument-1-Arg/module-type-S/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Dep5.Z.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep5/Z/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.module-type-T.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.module-type-T.module-type-R.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep6/module-type-T/module-type-R/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.module-type-T.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/module-type-T/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.X.module-type-R.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/X/module-type-R/index.html'")))
   (with-stdout-to
    Ocamlary.Dep6.X.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep6/X/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Dep7.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep7/index.html'")))
   (with-stdout-to
    Ocamlary.Dep7.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep7/argument-1-Arg/index.html'")))
   (with-stdout-to
    Ocamlary.Dep7.argument-1-Arg.module-type-T.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep7/argument-1-Arg/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.Dep7.argument-1-Arg.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep7/argument-1-Arg/X/index.html'")))
   (with-stdout-to
    Ocamlary.Dep7.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep7/M/index.html'")))
   (with-stdout-to
    Ocamlary.Dep8.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep8/index.html'")))
   (with-stdout-to
    Ocamlary.Dep8.module-type-T.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep8/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.Dep9.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep9/index.html'")))
   (with-stdout-to
    Ocamlary.Dep9.argument-1-X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep9/argument-1-X/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-Dep10.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-Dep10/index.html'")))
   (with-stdout-to
    Ocamlary.Dep11.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep11/index.html'")))
   (with-stdout-to
    Ocamlary.Dep11.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep11/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.Dep11.module-type-S.class-c.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/Dep11/module-type-S/class-c/index.html'")))
   (with-stdout-to
    Ocamlary.Dep12.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep12/index.html'")))
   (with-stdout-to
    Ocamlary.Dep12.argument-1-Arg.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep12/argument-1-Arg/index.html'")))
   (with-stdout-to
    Ocamlary.Dep13.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep13/index.html'")))
   (with-stdout-to
    Ocamlary.Dep13.class-c.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Dep13/class-c/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With1/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With1.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With1/M/index.html'")))
   (with-stdout-to
    Ocamlary.With2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With2/index.html'")))
   (with-stdout-to
    Ocamlary.With2.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With2/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.With3.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With3/index.html'")))
   (with-stdout-to
    Ocamlary.With3.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With3/N/index.html'")))
   (with-stdout-to
    Ocamlary.With4.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With4/index.html'")))
   (with-stdout-to
    Ocamlary.With4.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With4/N/index.html'")))
   (with-stdout-to
    Ocamlary.With5.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With5/index.html'")))
   (with-stdout-to
    Ocamlary.With5.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With5/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.With5.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With5/N/index.html'")))
   (with-stdout-to
    Ocamlary.With6.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With6/index.html'")))
   (with-stdout-to
    Ocamlary.With6.module-type-T.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With6/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.With6.module-type-T.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With6/module-type-T/M/index.html'")))
   (with-stdout-to
    Ocamlary.With7.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With7/index.html'")))
   (with-stdout-to
    Ocamlary.With7.argument-1-X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With7/argument-1-X/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With8.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With8/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With8.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With8/M/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With8.M.module-type-S.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-With8/M/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With8.M.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With8/M/N/index.html'")))
   (with-stdout-to
    Ocamlary.With9.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With9/index.html'")))
   (with-stdout-to
    Ocamlary.With9.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With9/module-type-S/index.html'")))
   (with-stdout-to
    Ocamlary.With10.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With10/index.html'")))
   (with-stdout-to
    Ocamlary.With10.module-type-T.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/With10/module-type-T/index.html'")))
   (with-stdout-to
    Ocamlary.With10.module-type-T.M.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/With10/module-type-T/M/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With11.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With11/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-With11.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-With11/N/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-NestedInclude1.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-NestedInclude1/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-NestedInclude1.module-type-NestedInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-NestedInclude1/module-type-NestedInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-NestedInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-NestedInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.DoubleInclude1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/DoubleInclude1/index.html'")))
   (with-stdout-to
    Ocamlary.DoubleInclude1.DoubleInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/DoubleInclude1/DoubleInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.DoubleInclude3.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/DoubleInclude3/index.html'")))
   (with-stdout-to
    Ocamlary.DoubleInclude3.DoubleInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/DoubleInclude3/DoubleInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.IncludeInclude1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/IncludeInclude1/index.html'")))
   (with-stdout-to
    Ocamlary.IncludeInclude1.module-type-IncludeInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/IncludeInclude1/module-type-IncludeInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-IncludeInclude2.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-IncludeInclude2/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/CanonicalTest/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__List.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CanonicalTest/Base__List/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/CanonicalTest/Base__/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/CanonicalTest/Base/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base.List.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CanonicalTest/Base/List/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__Tests.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CanonicalTest/Base__Tests/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__Tests.C.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CanonicalTest/Base__Tests/C/index.html'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.List_modif.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/CanonicalTest/List_modif/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__A/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__B.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__B/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__C.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__C/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__D.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__D/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__E.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__E/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo__/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/A/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.B.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/B/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.C.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/C/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.D.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/D/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.E.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Foo/E/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.Std.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/Std/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.E.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/E/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.P1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/P1/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.P1.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/P1/Y/index.html'")))
   (with-stdout-to
    Ocamlary.Aliases.P2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Aliases/P2/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-M/index.html'")))
   (with-stdout-to
    Ocamlary.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/M/index.html'")))
   (with-stdout-to
    Ocamlary.Only_a_module.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/Only_a_module/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-TypeExt.index.html.gen
    (progn
     (system "cat 'html.gen/test/Ocamlary/module-type-TypeExt/index.html'")))
   (with-stdout-to
    Ocamlary.module-type-TypeExtPruned.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Ocamlary/module-type-TypeExtPruned/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.index.html Ocamlary.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Empty.index.html Ocamlary.Empty.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-Empty.index.html
   Ocamlary.module-type-Empty.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MissingComment.index.html
   Ocamlary.module-type-MissingComment.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-EmptySig.index.html
   Ocamlary.module-type-EmptySig.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-EmptySigAlias.index.html
   Ocamlary.module-type-EmptySigAlias.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.ModuleWithSignature.index.html
   Ocamlary.ModuleWithSignature.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.ModuleWithSignatureAlias.index.html
   Ocamlary.ModuleWithSignatureAlias.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.One.index.html Ocamlary.One.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SigForMod.index.html
   Ocamlary.module-type-SigForMod.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SigForMod.Inner.index.html
   Ocamlary.module-type-SigForMod.Inner.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SigForMod.Inner.module-type-Empty.index.html
   Ocamlary.module-type-SigForMod.Inner.module-type-Empty.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.index.html
   Ocamlary.module-type-SuperSig.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-SubSigA.index.html
   Ocamlary.module-type-SuperSig.module-type-SubSigA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-SubSigA.SubSigAMod.index.html
   Ocamlary.module-type-SuperSig.module-type-SubSigA.SubSigAMod.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-SubSigB.index.html
   Ocamlary.module-type-SuperSig.module-type-SubSigB.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-EmptySig.index.html
   Ocamlary.module-type-SuperSig.module-type-EmptySig.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-One.index.html
   Ocamlary.module-type-SuperSig.module-type-One.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-SuperSig.module-type-SuperSig.index.html
   Ocamlary.module-type-SuperSig.module-type-SuperSig.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Buffer.index.html Ocamlary.Buffer.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.index.html
   Ocamlary.CollectionModule.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.InnerModuleA.index.html
   Ocamlary.CollectionModule.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.CollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.module-type-InnerModuleTypeA.index.html
   Ocamlary.CollectionModule.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-COLLECTION.index.html
   Ocamlary.module-type-COLLECTION.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-COLLECTION.InnerModuleA.index.html
   Ocamlary.module-type-COLLECTION.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-COLLECTION.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-COLLECTION.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-COLLECTION.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-COLLECTION.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-COLLECTION.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-COLLECTION.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.index.html
   Ocamlary.Recollection.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.argument-1-C.index.html
   Ocamlary.Recollection.argument-1-C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.argument-1-C.InnerModuleA.index.html
   Ocamlary.Recollection.argument-1-C.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.argument-1-C.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.Recollection.argument-1-C.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.argument-1-C.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.Recollection.argument-1-C.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.argument-1-C.module-type-InnerModuleTypeA.index.html
   Ocamlary.Recollection.argument-1-C.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.InnerModuleA.index.html
   Ocamlary.Recollection.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.Recollection.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.Recollection.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.module-type-InnerModuleTypeA.index.html
   Ocamlary.Recollection.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.index.html
   Ocamlary.module-type-MMM.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.C.index.html
   Ocamlary.module-type-MMM.C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.C.InnerModuleA.index.html
   Ocamlary.module-type-MMM.C.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.C.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-MMM.C.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.C.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-MMM.C.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-MMM.C.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-MMM.C.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RECOLLECTION.index.html
   Ocamlary.module-type-RECOLLECTION.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RecollectionModule.index.html
   Ocamlary.module-type-RecollectionModule.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RecollectionModule.InnerModuleA.index.html
   Ocamlary.module-type-RecollectionModule.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RecollectionModule.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-RecollectionModule.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RecollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-RecollectionModule.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-RecollectionModule.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-RecollectionModule.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.index.html
   Ocamlary.module-type-A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.Q.index.html
   Ocamlary.module-type-A.Q.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.Q.InnerModuleA.index.html
   Ocamlary.module-type-A.Q.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.Q.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-A.Q.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-A.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-A.Q.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-A.Q.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.index.html
   Ocamlary.module-type-B.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.Q.index.html
   Ocamlary.module-type-B.Q.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.Q.InnerModuleA.index.html
   Ocamlary.module-type-B.Q.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.Q.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-B.Q.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-B.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-B.Q.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-B.Q.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.index.html
   Ocamlary.module-type-C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.Q.index.html
   Ocamlary.module-type-C.Q.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.Q.InnerModuleA.index.html
   Ocamlary.module-type-C.Q.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.Q.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.module-type-C.Q.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.module-type-C.Q.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-C.Q.module-type-InnerModuleTypeA.index.html
   Ocamlary.module-type-C.Q.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.index.html
   Ocamlary.FunctorTypeOf.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.argument-1-Collection.index.html
   Ocamlary.FunctorTypeOf.argument-1-Collection.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.index.html
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.InnerModuleA'.index.html
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.InnerModuleA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.module-type-InnerModuleTypeA'.index.html
   Ocamlary.FunctorTypeOf.argument-1-Collection.InnerModuleA.module-type-InnerModuleTypeA'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.FunctorTypeOf.argument-1-Collection.module-type-InnerModuleTypeA.index.html
   Ocamlary.FunctorTypeOf.argument-1-Collection.module-type-InnerModuleTypeA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-IncludeModuleType.index.html
   Ocamlary.module-type-IncludeModuleType.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-ToInclude.index.html
   Ocamlary.module-type-ToInclude.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-ToInclude.IncludedA.index.html
   Ocamlary.module-type-ToInclude.IncludedA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-ToInclude.module-type-IncludedB.index.html
   Ocamlary.module-type-ToInclude.module-type-IncludedB.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.IncludedA.index.html Ocamlary.IncludedA.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-IncludedB.index.html
   Ocamlary.module-type-IncludedB.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.ExtMod.index.html Ocamlary.ExtMod.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.class-empty_class.index.html
   Ocamlary.class-empty_class.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.class-one_method_class.index.html
   Ocamlary.class-one_method_class.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.class-two_method_class.index.html
   Ocamlary.class-two_method_class.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.class-param_class.index.html
   Ocamlary.class-param_class.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.index.html Ocamlary.Dep1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep1.module-type-S.index.html
   Ocamlary.Dep1.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep1.module-type-S.class-c.index.html
   Ocamlary.Dep1.module-type-S.class-c.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.X.index.html Ocamlary.Dep1.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.X.Y.index.html Ocamlary.Dep1.X.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep1.X.Y.class-c.index.html
   Ocamlary.Dep1.X.Y.class-c.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep2.index.html Ocamlary.Dep2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep2.argument-1-Arg.index.html
   Ocamlary.Dep2.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep2.argument-1-Arg.X.index.html
   Ocamlary.Dep2.argument-1-Arg.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep2.A.index.html Ocamlary.Dep2.A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep3.index.html Ocamlary.Dep3.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep4.index.html Ocamlary.Dep4.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep4.module-type-T.index.html
   Ocamlary.Dep4.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep4.module-type-S.index.html
   Ocamlary.Dep4.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep4.module-type-S.X.index.html
   Ocamlary.Dep4.module-type-S.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep4.module-type-S.Y.index.html
   Ocamlary.Dep4.module-type-S.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep4.X.index.html Ocamlary.Dep4.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.index.html Ocamlary.Dep5.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep5.argument-1-Arg.index.html
   Ocamlary.Dep5.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep5.argument-1-Arg.module-type-S.index.html
   Ocamlary.Dep5.argument-1-Arg.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep5.argument-1-Arg.module-type-S.Y.index.html
   Ocamlary.Dep5.argument-1-Arg.module-type-S.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.Z.index.html Ocamlary.Dep5.Z.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.index.html Ocamlary.Dep6.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep6.module-type-S.index.html
   Ocamlary.Dep6.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep6.module-type-T.index.html
   Ocamlary.Dep6.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep6.module-type-T.module-type-R.index.html
   Ocamlary.Dep6.module-type-T.module-type-R.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep6.module-type-T.Y.index.html
   Ocamlary.Dep6.module-type-T.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.X.index.html Ocamlary.Dep6.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep6.X.module-type-R.index.html
   Ocamlary.Dep6.X.module-type-R.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.X.Y.index.html Ocamlary.Dep6.X.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.index.html Ocamlary.Dep7.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep7.argument-1-Arg.index.html
   Ocamlary.Dep7.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep7.argument-1-Arg.module-type-T.index.html
   Ocamlary.Dep7.argument-1-Arg.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep7.argument-1-Arg.X.index.html
   Ocamlary.Dep7.argument-1-Arg.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.M.index.html Ocamlary.Dep7.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep8.index.html Ocamlary.Dep8.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep8.module-type-T.index.html
   Ocamlary.Dep8.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep9.index.html Ocamlary.Dep9.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep9.argument-1-X.index.html
   Ocamlary.Dep9.argument-1-X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-Dep10.index.html
   Ocamlary.module-type-Dep10.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep11.index.html Ocamlary.Dep11.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep11.module-type-S.index.html
   Ocamlary.Dep11.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep11.module-type-S.class-c.index.html
   Ocamlary.Dep11.module-type-S.class-c.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep12.index.html Ocamlary.Dep12.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep12.argument-1-Arg.index.html
   Ocamlary.Dep12.argument-1-Arg.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep13.index.html Ocamlary.Dep13.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Dep13.class-c.index.html
   Ocamlary.Dep13.class-c.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With1.index.html
   Ocamlary.module-type-With1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With1.M.index.html
   Ocamlary.module-type-With1.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With2.index.html Ocamlary.With2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With2.module-type-S.index.html
   Ocamlary.With2.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.index.html Ocamlary.With3.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.N.index.html Ocamlary.With3.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.index.html Ocamlary.With4.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.N.index.html Ocamlary.With4.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With5.index.html Ocamlary.With5.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With5.module-type-S.index.html
   Ocamlary.With5.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With5.N.index.html Ocamlary.With5.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With6.index.html Ocamlary.With6.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With6.module-type-T.index.html
   Ocamlary.With6.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With6.module-type-T.M.index.html
   Ocamlary.With6.module-type-T.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With7.index.html Ocamlary.With7.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With7.argument-1-X.index.html
   Ocamlary.With7.argument-1-X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With8.index.html
   Ocamlary.module-type-With8.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With8.M.index.html
   Ocamlary.module-type-With8.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With8.M.module-type-S.index.html
   Ocamlary.module-type-With8.M.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With8.M.N.index.html
   Ocamlary.module-type-With8.M.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With9.index.html Ocamlary.With9.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With9.module-type-S.index.html
   Ocamlary.With9.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With10.index.html Ocamlary.With10.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With10.module-type-T.index.html
   Ocamlary.With10.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.With10.module-type-T.M.index.html
   Ocamlary.With10.module-type-T.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With11.index.html
   Ocamlary.module-type-With11.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-With11.N.index.html
   Ocamlary.module-type-With11.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-NestedInclude1.index.html
   Ocamlary.module-type-NestedInclude1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-NestedInclude1.module-type-NestedInclude2.index.html
   Ocamlary.module-type-NestedInclude1.module-type-NestedInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-NestedInclude2.index.html
   Ocamlary.module-type-NestedInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude1.index.html
   Ocamlary.DoubleInclude1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude1.DoubleInclude2.index.html
   Ocamlary.DoubleInclude1.DoubleInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude3.index.html
   Ocamlary.DoubleInclude3.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude3.DoubleInclude2.index.html
   Ocamlary.DoubleInclude3.DoubleInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.IncludeInclude1.index.html
   Ocamlary.IncludeInclude1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.IncludeInclude1.module-type-IncludeInclude2.index.html
   Ocamlary.IncludeInclude1.module-type-IncludeInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-IncludeInclude2.index.html
   Ocamlary.module-type-IncludeInclude2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.index.html
   Ocamlary.CanonicalTest.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__List.index.html
   Ocamlary.CanonicalTest.Base__List.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__.index.html
   Ocamlary.CanonicalTest.Base__.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base.index.html
   Ocamlary.CanonicalTest.Base.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base.List.index.html
   Ocamlary.CanonicalTest.Base.List.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__Tests.index.html
   Ocamlary.CanonicalTest.Base__Tests.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__Tests.C.index.html
   Ocamlary.CanonicalTest.Base__Tests.C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.List_modif.index.html
   Ocamlary.CanonicalTest.List_modif.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.index.html Ocamlary.Aliases.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__A.index.html
   Ocamlary.Aliases.Foo__A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__B.index.html
   Ocamlary.Aliases.Foo__B.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__C.index.html
   Ocamlary.Aliases.Foo__C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__D.index.html
   Ocamlary.Aliases.Foo__D.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__E.index.html
   Ocamlary.Aliases.Foo__E.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo__.index.html
   Ocamlary.Aliases.Foo__.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.index.html Ocamlary.Aliases.Foo.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo.A.index.html
   Ocamlary.Aliases.Foo.A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo.B.index.html
   Ocamlary.Aliases.Foo.B.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo.C.index.html
   Ocamlary.Aliases.Foo.C.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo.D.index.html
   Ocamlary.Aliases.Foo.D.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.Foo.E.index.html
   Ocamlary.Aliases.Foo.E.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Std.index.html Ocamlary.Aliases.Std.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.E.index.html Ocamlary.Aliases.E.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.P1.index.html Ocamlary.Aliases.P1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Aliases.P1.Y.index.html
   Ocamlary.Aliases.P1.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.P2.index.html Ocamlary.Aliases.P2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-M.index.html
   Ocamlary.module-type-M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.M.index.html Ocamlary.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Only_a_module.index.html
   Ocamlary.Only_a_module.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-TypeExt.index.html
   Ocamlary.module-type-TypeExt.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.module-type-TypeExtPruned.index.html
   Ocamlary.module-type-TypeExtPruned.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../section.odocl})
   (with-stdout-to
    Section.index.html.gen
    (progn
     (system "cat 'html.gen/test/Section/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Section.index.html Section.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../stop.odocl})
   (with-stdout-to
    Stop.index.html.gen
    (progn
     (system "cat 'html.gen/test/Stop/index.html'")))
   (with-stdout-to
    Stop.N.index.html.gen
    (progn
     (system "cat 'html.gen/test/Stop/N/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop.index.html Stop.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Stop.N.index.html Stop.N.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run
    odoc
    html-generate
    --indent
    -o
    html.gen
    %{dep:../stop_dead_link_doc.odocl})
   (with-stdout-to
    Stop_dead_link_doc.index.html.gen
    (progn
     (system "cat 'html.gen/test/Stop_dead_link_doc/index.html'")))
   (with-stdout-to
    Stop_dead_link_doc.Foo.index.html.gen
    (progn
     (system "cat 'html.gen/test/Stop_dead_link_doc/Foo/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop_dead_link_doc.index.html Stop_dead_link_doc.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Stop_dead_link_doc.Foo.index.html
   Stop_dead_link_doc.Foo.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run
    odoc
    html-generate
    --indent
    -o
    html.gen
    %{dep:../toplevel_comments.odocl})
   (with-stdout-to
    Toplevel_comments.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/index.html'")))
   (with-stdout-to
    Toplevel_comments.module-type-T.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/module-type-T/index.html'")))
   (with-stdout-to
    Toplevel_comments.Include_inline.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/Include_inline/index.html'")))
   (with-stdout-to
    Toplevel_comments.Include_inline'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/Include_inline'\\''/index.html'")))
   (with-stdout-to
    Toplevel_comments.module-type-Include_inline_T.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/module-type-Include_inline_T/index.html'")))
   (with-stdout-to
    Toplevel_comments.module-type-Include_inline_T'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/module-type-Include_inline_T'\\''/index.html'")))
   (with-stdout-to
    Toplevel_comments.M.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/M/index.html'")))
   (with-stdout-to
    Toplevel_comments.M'.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/M'\\''/index.html'")))
   (with-stdout-to
    Toplevel_comments.M''.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/M'\\'''\\''/index.html'")))
   (with-stdout-to
    Toplevel_comments.Alias.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/Alias/index.html'")))
   (with-stdout-to
    Toplevel_comments.class-c1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/class-c1/index.html'")))
   (with-stdout-to
    Toplevel_comments.class-type-ct.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Toplevel_comments/class-type-ct/index.html'")))
   (with-stdout-to
    Toplevel_comments.class-c2.index.html.gen
    (progn
     (system "cat 'html.gen/test/Toplevel_comments/class-c2/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.index.html Toplevel_comments.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.module-type-T.index.html
   Toplevel_comments.module-type-T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.Include_inline.index.html
   Toplevel_comments.Include_inline.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.Include_inline'.index.html
   Toplevel_comments.Include_inline'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.module-type-Include_inline_T.index.html
   Toplevel_comments.module-type-Include_inline_T.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.module-type-Include_inline_T'.index.html
   Toplevel_comments.module-type-Include_inline_T'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.M.index.html Toplevel_comments.M.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.M'.index.html Toplevel_comments.M'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.M''.index.html
   Toplevel_comments.M''.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.Alias.index.html
   Toplevel_comments.Alias.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.class-c1.index.html
   Toplevel_comments.class-c1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.class-type-ct.index.html
   Toplevel_comments.class-type-ct.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.class-c2.index.html
   Toplevel_comments.class-c2.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../type.odocl})
   (with-stdout-to
    Type.index.html.gen
    (progn
     (system "cat 'html.gen/test/Type/index.html'")))
   (with-stdout-to
    Type.module-type-X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Type/module-type-X/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Type.index.html Type.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Type.module-type-X.index.html Type.module-type-X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../val.odocl})
   (with-stdout-to
    Val.index.html.gen
    (progn
     (system "cat 'html.gen/test/Val/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Val.index.html Val.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))
