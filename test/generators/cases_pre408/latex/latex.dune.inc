(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../alias.odocl})
   (with-stdout-to
    Alias.tex.gen
    (progn
     (system "cat 'latex.gen/test/Alias.tex'")))
   (with-stdout-to
    Alias.X.tex.gen
    (progn
     (system "cat 'latex.gen/test/Alias.X.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Alias.tex Alias.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Alias.X.tex Alias.X.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../bugs.odocl})
   (with-stdout-to
    Bugs.tex.gen
    (progn
     (system "cat 'latex.gen/test/Bugs.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs.tex Bugs.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../bugs_pre_410.odocl})
   (with-stdout-to
    Bugs_pre_410.tex.gen
    (progn
     (system "cat 'latex.gen/test/Bugs_pre_410.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs_pre_410.tex Bugs_pre_410.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../class.odocl})
   (with-stdout-to
    Class.tex.gen
    (progn
     (system "cat 'latex.gen/test/Class.tex'")))
   (with-stdout-to
    Class.mutually'.tex.gen
    (progn
     (system "cat 'latex.gen/test/Class.mutually'\\''.tex'")))
   (with-stdout-to
    Class.recursive'.tex.gen
    (progn
     (system "cat 'latex.gen/test/Class.recursive'\\''.tex'")))
   (with-stdout-to
    Class.empty_virtual'.tex.gen
    (progn
     (system "cat 'latex.gen/test/Class.empty_virtual'\\''.tex'")))
   (with-stdout-to
    Class.polymorphic'.tex.gen
    (progn
     (system "cat 'latex.gen/test/Class.polymorphic'\\''.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Class.tex Class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.mutually'.tex Class.mutually'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.recursive'.tex Class.recursive'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.empty_virtual'.tex Class.empty_virtual'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.polymorphic'.tex Class.polymorphic'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../external.odocl})
   (with-stdout-to
    External.tex.gen
    (progn
     (system "cat 'latex.gen/test/External.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff External.tex External.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../functor.odocl})
   (with-stdout-to
    Functor.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.tex'")))
   (with-stdout-to
    Functor.F1.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.F1.tex'")))
   (with-stdout-to
    Functor.F2.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.F2.tex'")))
   (with-stdout-to
    Functor.F3.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.F3.tex'")))
   (with-stdout-to
    Functor.F4.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.F4.tex'")))
   (with-stdout-to
    Functor.F5.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor.F5.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor.tex Functor.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F1.tex Functor.F1.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F2.tex Functor.F2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F3.tex Functor.F3.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F4.tex Functor.F4.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F5.tex Functor.F5.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../functor2.odocl})
   (with-stdout-to
    Functor2.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor2.tex'")))
   (with-stdout-to
    Functor2.X.tex.gen
    (progn
     (system "cat 'latex.gen/test/Functor2.X.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor2.tex Functor2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor2.X.tex Functor2.X.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../include.odocl})
   (with-stdout-to
    Include.tex.gen
    (progn
     (system "cat 'latex.gen/test/Include.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Include.tex Include.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../include2.odocl})
   (with-stdout-to
    Include2.tex.gen
    (progn
     (system "cat 'latex.gen/test/Include2.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Include2.tex Include2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../include_sections.odocl})
   (with-stdout-to
    Include_sections.tex.gen
    (progn
     (system "cat 'latex.gen/test/Include_sections.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Include_sections.tex Include_sections.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../interlude.odocl})
   (with-stdout-to
    Interlude.tex.gen
    (progn
     (system "cat 'latex.gen/test/Interlude.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Interlude.tex Interlude.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../markup.odocl})
   (with-stdout-to
    Markup.tex.gen
    (progn
     (system "cat 'latex.gen/test/Markup.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Markup.tex Markup.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../page-mld.odocl})
   (with-stdout-to
    mld.tex.gen
    (progn
     (system "cat 'latex.gen/test/mld.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff mld.tex mld.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../module.odocl})
   (with-stdout-to
    Module.tex.gen
    (progn
     (system "cat 'latex.gen/test/Module.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Module.tex Module.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../nested.odocl})
   (with-stdout-to
    Nested.tex.gen
    (progn
     (system "cat 'latex.gen/test/Nested.tex'")))
   (with-stdout-to
    Nested.F.tex.gen
    (progn
     (system "cat 'latex.gen/test/Nested.F.tex'")))
   (with-stdout-to
    Nested.z.tex.gen
    (progn
     (system "cat 'latex.gen/test/Nested.z.tex'")))
   (with-stdout-to
    Nested.inherits.tex.gen
    (progn
     (system "cat 'latex.gen/test/Nested.inherits.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Nested.tex Nested.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.F.tex Nested.F.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.z.tex Nested.z.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.inherits.tex Nested.inherits.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../ocamlary.odocl})
   (with-stdout-to
    Ocamlary.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.tex'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignature.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.ModuleWithSignature.tex'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignatureAlias.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.ModuleWithSignatureAlias.tex'")))
   (with-stdout-to
    Ocamlary.Recollection.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Recollection.tex'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.FunctorTypeOf.tex'")))
   (with-stdout-to
    Ocamlary.empty_class.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.empty_class.tex'")))
   (with-stdout-to
    Ocamlary.one_method_class.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.one_method_class.tex'")))
   (with-stdout-to
    Ocamlary.two_method_class.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.two_method_class.tex'")))
   (with-stdout-to
    Ocamlary.param_class.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.param_class.tex'")))
   (with-stdout-to
    Ocamlary.Dep2.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep2.tex'")))
   (with-stdout-to
    Ocamlary.Dep5.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep5.tex'")))
   (with-stdout-to
    Ocamlary.Dep5.Z.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep5.Z.tex'")))
   (with-stdout-to
    Ocamlary.Dep7.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep7.tex'")))
   (with-stdout-to
    Ocamlary.Dep7.M.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep7.M.tex'")))
   (with-stdout-to
    Ocamlary.Dep9.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep9.tex'")))
   (with-stdout-to
    Ocamlary.Dep12.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep12.tex'")))
   (with-stdout-to
    Ocamlary.Dep13.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep13.tex'")))
   (with-stdout-to
    Ocamlary.Dep13.c.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.Dep13.c.tex'")))
   (with-stdout-to
    Ocamlary.With3.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.With3.tex'")))
   (with-stdout-to
    Ocamlary.With3.N.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.With3.N.tex'")))
   (with-stdout-to
    Ocamlary.With4.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.With4.tex'")))
   (with-stdout-to
    Ocamlary.With4.N.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.With4.N.tex'")))
   (with-stdout-to
    Ocamlary.With7.tex.gen
    (progn
     (system "cat 'latex.gen/test/Ocamlary.With7.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.tex Ocamlary.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.ModuleWithSignature.tex
   Ocamlary.ModuleWithSignature.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.ModuleWithSignatureAlias.tex
   Ocamlary.ModuleWithSignatureAlias.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Recollection.tex Ocamlary.Recollection.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.FunctorTypeOf.tex Ocamlary.FunctorTypeOf.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.empty_class.tex Ocamlary.empty_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.one_method_class.tex Ocamlary.one_method_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.two_method_class.tex Ocamlary.two_method_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.param_class.tex Ocamlary.param_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep2.tex Ocamlary.Dep2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.tex Ocamlary.Dep5.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.Z.tex Ocamlary.Dep5.Z.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.tex Ocamlary.Dep7.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.M.tex Ocamlary.Dep7.M.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep9.tex Ocamlary.Dep9.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep12.tex Ocamlary.Dep12.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep13.tex Ocamlary.Dep13.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep13.c.tex Ocamlary.Dep13.c.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.tex Ocamlary.With3.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.N.tex Ocamlary.With3.N.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.tex Ocamlary.With4.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.N.tex Ocamlary.With4.N.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With7.tex Ocamlary.With7.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../section.odocl})
   (with-stdout-to
    Section.tex.gen
    (progn
     (system "cat 'latex.gen/test/Section.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Section.tex Section.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../stop.odocl})
   (with-stdout-to
    Stop.tex.gen
    (progn
     (system "cat 'latex.gen/test/Stop.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop.tex Stop.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../stop_dead_link_doc.odocl})
   (with-stdout-to
    Stop_dead_link_doc.tex.gen
    (progn
     (system "cat 'latex.gen/test/Stop_dead_link_doc.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop_dead_link_doc.tex Stop_dead_link_doc.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../toplevel_comments.odocl})
   (with-stdout-to
    Toplevel_comments.tex.gen
    (progn
     (system "cat 'latex.gen/test/Toplevel_comments.tex'")))
   (with-stdout-to
    Toplevel_comments.Alias.tex.gen
    (progn
     (system "cat 'latex.gen/test/Toplevel_comments.Alias.tex'")))
   (with-stdout-to
    Toplevel_comments.c1.tex.gen
    (progn
     (system "cat 'latex.gen/test/Toplevel_comments.c1.tex'")))
   (with-stdout-to
    Toplevel_comments.c2.tex.gen
    (progn
     (system "cat 'latex.gen/test/Toplevel_comments.c2.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.tex Toplevel_comments.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.Alias.tex Toplevel_comments.Alias.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.c1.tex Toplevel_comments.c1.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.c2.tex Toplevel_comments.c2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../type.odocl})
   (with-stdout-to
    Type.tex.gen
    (progn
     (system "cat 'latex.gen/test/Type.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Type.tex Type.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../val.odocl})
   (with-stdout-to
    Val.tex.gen
    (progn
     (system "cat 'latex.gen/test/Val.tex'"))))))

(rule
 (alias runtest)
 (action
  (diff Val.tex Val.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))
