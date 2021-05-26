(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../alias.odocl})
   (with-stdout-to
    Alias.3o.gen
    (progn
     (system "cat 'man.gen/test/Alias.3o'")))
   (with-stdout-to
    Alias.Foo__X.3o.gen
    (progn
     (system "cat 'man.gen/test/Alias.Foo__X.3o'")))
   (with-stdout-to
    Alias.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Alias.X.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Alias.3o Alias.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Alias.Foo__X.3o Alias.Foo__X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Alias.X.3o Alias.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../bugs.odocl})
   (with-stdout-to
    Bugs.3o.gen
    (progn
     (system "cat 'man.gen/test/Bugs.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs.3o Bugs.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../bugs_pre_410.odocl})
   (with-stdout-to
    Bugs_pre_410.3o.gen
    (progn
     (system "cat 'man.gen/test/Bugs_pre_410.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs_pre_410.3o Bugs_pre_410.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../class.odocl})
   (with-stdout-to
    Class.3o.gen
    (progn
     (system "cat 'man.gen/test/Class.3o'")))
   (with-stdout-to
    Class.mutually'.3o.gen
    (progn
     (system "cat 'man.gen/test/Class.mutually'\\''.3o'")))
   (with-stdout-to
    Class.recursive'.3o.gen
    (progn
     (system "cat 'man.gen/test/Class.recursive'\\''.3o'")))
   (with-stdout-to
    Class.empty_virtual'.3o.gen
    (progn
     (system "cat 'man.gen/test/Class.empty_virtual'\\''.3o'")))
   (with-stdout-to
    Class.polymorphic'.3o.gen
    (progn
     (system "cat 'man.gen/test/Class.polymorphic'\\''.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Class.3o Class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.mutually'.3o Class.mutually'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.recursive'.3o Class.recursive'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.empty_virtual'.3o Class.empty_virtual'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Class.polymorphic'.3o Class.polymorphic'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../external.odocl})
   (with-stdout-to
    External.3o.gen
    (progn
     (system "cat 'man.gen/test/External.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff External.3o External.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../functor.odocl})
   (with-stdout-to
    Functor.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.3o'")))
   (with-stdout-to
    Functor.F1.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.F1.3o'")))
   (with-stdout-to
    Functor.F2.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.F2.3o'")))
   (with-stdout-to
    Functor.F3.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.F3.3o'")))
   (with-stdout-to
    Functor.F4.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.F4.3o'")))
   (with-stdout-to
    Functor.F5.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor.F5.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor.3o Functor.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F1.3o Functor.F1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F2.3o Functor.F2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F3.3o Functor.F3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F4.3o Functor.F4.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor.F5.3o Functor.F5.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../functor2.odocl})
   (with-stdout-to
    Functor2.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor2.3o'")))
   (with-stdout-to
    Functor2.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Functor2.X.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Functor2.3o Functor2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Functor2.X.3o Functor2.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../include.odocl})
   (with-stdout-to
    Include.3o.gen
    (progn
     (system "cat 'man.gen/test/Include.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Include.3o Include.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../include2.odocl})
   (with-stdout-to
    Include2.3o.gen
    (progn
     (system "cat 'man.gen/test/Include2.3o'")))
   (with-stdout-to
    Include2.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Include2.X.3o'")))
   (with-stdout-to
    Include2.Y.3o.gen
    (progn
     (system "cat 'man.gen/test/Include2.Y.3o'")))
   (with-stdout-to
    Include2.Y_include_synopsis.3o.gen
    (progn
     (system "cat 'man.gen/test/Include2.Y_include_synopsis.3o'")))
   (with-stdout-to
    Include2.Y_include_doc.3o.gen
    (progn
     (system "cat 'man.gen/test/Include2.Y_include_doc.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Include2.3o Include2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.X.3o Include2.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.Y.3o Include2.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.Y_include_synopsis.3o Include2.Y_include_synopsis.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Include2.Y_include_doc.3o Include2.Y_include_doc.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../include_sections.odocl})
   (with-stdout-to
    Include_sections.3o.gen
    (progn
     (system "cat 'man.gen/test/Include_sections.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Include_sections.3o Include_sections.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../interlude.odocl})
   (with-stdout-to
    Interlude.3o.gen
    (progn
     (system "cat 'man.gen/test/Interlude.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Interlude.3o Interlude.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../markup.odocl})
   (with-stdout-to
    Markup.3o.gen
    (progn
     (system "cat 'man.gen/test/Markup.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Markup.3o Markup.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../page-mld.odocl})
   (with-stdout-to
    mld.3o.gen
    (progn
     (system "cat 'man.gen/test/mld.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff mld.3o mld.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../module.odocl})
   (with-stdout-to
    Module.3o.gen
    (progn
     (system "cat 'man.gen/test/Module.3o'")))
   (with-stdout-to
    Module.M'.3o.gen
    (progn
     (system "cat 'man.gen/test/Module.M'\\''.3o'")))
   (with-stdout-to
    Module.Mutually.3o.gen
    (progn
     (system "cat 'man.gen/test/Module.Mutually.3o'")))
   (with-stdout-to
    Module.Recursive.3o.gen
    (progn
     (system "cat 'man.gen/test/Module.Recursive.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Module.3o Module.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.M'.3o Module.M'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.Mutually.3o Module.Mutually.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Module.Recursive.3o Module.Recursive.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../nested.odocl})
   (with-stdout-to
    Nested.3o.gen
    (progn
     (system "cat 'man.gen/test/Nested.3o'")))
   (with-stdout-to
    Nested.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Nested.X.3o'")))
   (with-stdout-to
    Nested.F.3o.gen
    (progn
     (system "cat 'man.gen/test/Nested.F.3o'")))
   (with-stdout-to
    Nested.z.3o.gen
    (progn
     (system "cat 'man.gen/test/Nested.z.3o'")))
   (with-stdout-to
    Nested.inherits.3o.gen
    (progn
     (system "cat 'man.gen/test/Nested.inherits.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Nested.3o Nested.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.X.3o Nested.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.F.3o Nested.F.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.z.3o Nested.z.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Nested.inherits.3o Nested.inherits.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../ocamlary.odocl})
   (with-stdout-to
    Ocamlary.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.3o'")))
   (with-stdout-to
    Ocamlary.Empty.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Empty.3o'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignature.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.ModuleWithSignature.3o'")))
   (with-stdout-to
    Ocamlary.ModuleWithSignatureAlias.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.ModuleWithSignatureAlias.3o'")))
   (with-stdout-to
    Ocamlary.One.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.One.3o'")))
   (with-stdout-to
    Ocamlary.Buffer.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Buffer.3o'")))
   (with-stdout-to
    Ocamlary.CollectionModule.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CollectionModule.3o'")))
   (with-stdout-to
    Ocamlary.CollectionModule.InnerModuleA.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CollectionModule.InnerModuleA.3o'")))
   (with-stdout-to
    Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o.gen
    (progn
     (system
      "cat 'man.gen/test/Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'\\''.3o'")))
   (with-stdout-to
    Ocamlary.Recollection.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Recollection.3o'")))
   (with-stdout-to
    Ocamlary.Recollection.InnerModuleA.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Recollection.InnerModuleA.3o'")))
   (with-stdout-to
    Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o.gen
    (progn
     (system
      "cat 'man.gen/test/Ocamlary.Recollection.InnerModuleA.InnerModuleA'\\''.3o'")))
   (with-stdout-to
    Ocamlary.FunctorTypeOf.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.FunctorTypeOf.3o'")))
   (with-stdout-to
    Ocamlary.IncludedA.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.IncludedA.3o'")))
   (with-stdout-to
    Ocamlary.ExtMod.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.ExtMod.3o'")))
   (with-stdout-to
    Ocamlary.empty_class.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.empty_class.3o'")))
   (with-stdout-to
    Ocamlary.one_method_class.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.one_method_class.3o'")))
   (with-stdout-to
    Ocamlary.two_method_class.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.two_method_class.3o'")))
   (with-stdout-to
    Ocamlary.param_class.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.param_class.3o'")))
   (with-stdout-to
    Ocamlary.Dep1.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep1.3o'")))
   (with-stdout-to
    Ocamlary.Dep1.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep1.X.3o'")))
   (with-stdout-to
    Ocamlary.Dep1.X.Y.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep1.X.Y.3o'")))
   (with-stdout-to
    Ocamlary.Dep1.X.Y.c.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep1.X.Y.c.3o'")))
   (with-stdout-to
    Ocamlary.Dep2.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep2.3o'")))
   (with-stdout-to
    Ocamlary.Dep2.A.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep2.A.3o'")))
   (with-stdout-to
    Ocamlary.Dep3.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep3.3o'")))
   (with-stdout-to
    Ocamlary.Dep4.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep4.3o'")))
   (with-stdout-to
    Ocamlary.Dep4.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep4.X.3o'")))
   (with-stdout-to
    Ocamlary.Dep5.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep5.3o'")))
   (with-stdout-to
    Ocamlary.Dep5.Z.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep5.Z.3o'")))
   (with-stdout-to
    Ocamlary.Dep6.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep6.3o'")))
   (with-stdout-to
    Ocamlary.Dep6.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep6.X.3o'")))
   (with-stdout-to
    Ocamlary.Dep6.X.Y.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep6.X.Y.3o'")))
   (with-stdout-to
    Ocamlary.Dep7.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep7.3o'")))
   (with-stdout-to
    Ocamlary.Dep7.M.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep7.M.3o'")))
   (with-stdout-to
    Ocamlary.Dep8.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep8.3o'")))
   (with-stdout-to
    Ocamlary.Dep9.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep9.3o'")))
   (with-stdout-to
    Ocamlary.Dep11.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep11.3o'")))
   (with-stdout-to
    Ocamlary.Dep12.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep12.3o'")))
   (with-stdout-to
    Ocamlary.Dep13.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep13.3o'")))
   (with-stdout-to
    Ocamlary.Dep13.c.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Dep13.c.3o'")))
   (with-stdout-to
    Ocamlary.With2.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With2.3o'")))
   (with-stdout-to
    Ocamlary.With3.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With3.3o'")))
   (with-stdout-to
    Ocamlary.With3.N.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With3.N.3o'")))
   (with-stdout-to
    Ocamlary.With4.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With4.3o'")))
   (with-stdout-to
    Ocamlary.With4.N.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With4.N.3o'")))
   (with-stdout-to
    Ocamlary.With5.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With5.3o'")))
   (with-stdout-to
    Ocamlary.With5.N.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With5.N.3o'")))
   (with-stdout-to
    Ocamlary.With6.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With6.3o'")))
   (with-stdout-to
    Ocamlary.With7.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With7.3o'")))
   (with-stdout-to
    Ocamlary.With9.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With9.3o'")))
   (with-stdout-to
    Ocamlary.With10.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.With10.3o'")))
   (with-stdout-to
    Ocamlary.DoubleInclude1.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.DoubleInclude1.3o'")))
   (with-stdout-to
    Ocamlary.DoubleInclude1.DoubleInclude2.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.DoubleInclude1.DoubleInclude2.3o'")))
   (with-stdout-to
    Ocamlary.DoubleInclude3.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.DoubleInclude3.3o'")))
   (with-stdout-to
    Ocamlary.DoubleInclude3.DoubleInclude2.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.DoubleInclude3.DoubleInclude2.3o'")))
   (with-stdout-to
    Ocamlary.IncludeInclude1.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.IncludeInclude1.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__List.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base__List.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base__.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base.List.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base.List.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__Tests.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base__Tests.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.Base__Tests.C.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.Base__Tests.C.3o'")))
   (with-stdout-to
    Ocamlary.CanonicalTest.List_modif.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.CanonicalTest.List_modif.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__A.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__A.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__B.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__B.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__C.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__C.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__D.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__D.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__E.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__E.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo__.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo__.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.A.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.A.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.B.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.B.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.C.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.C.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.D.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.D.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Foo.E.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Foo.E.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.Std.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.Std.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.E.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.E.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.P1.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.P1.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.P1.Y.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.P1.Y.3o'")))
   (with-stdout-to
    Ocamlary.Aliases.P2.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Aliases.P2.3o'")))
   (with-stdout-to
    Ocamlary.M.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.M.3o'")))
   (with-stdout-to
    Ocamlary.Only_a_module.3o.gen
    (progn
     (system "cat 'man.gen/test/Ocamlary.Only_a_module.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.3o Ocamlary.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Empty.3o Ocamlary.Empty.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.ModuleWithSignature.3o Ocamlary.ModuleWithSignature.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.ModuleWithSignatureAlias.3o
   Ocamlary.ModuleWithSignatureAlias.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.One.3o Ocamlary.One.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Buffer.3o Ocamlary.Buffer.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.CollectionModule.3o Ocamlary.CollectionModule.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.InnerModuleA.3o
   Ocamlary.CollectionModule.InnerModuleA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o
   Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Recollection.3o Ocamlary.Recollection.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.InnerModuleA.3o
   Ocamlary.Recollection.InnerModuleA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o
   Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.FunctorTypeOf.3o Ocamlary.FunctorTypeOf.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.IncludedA.3o Ocamlary.IncludedA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.ExtMod.3o Ocamlary.ExtMod.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.empty_class.3o Ocamlary.empty_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.one_method_class.3o Ocamlary.one_method_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.two_method_class.3o Ocamlary.two_method_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.param_class.3o Ocamlary.param_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.3o Ocamlary.Dep1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.X.3o Ocamlary.Dep1.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.X.Y.3o Ocamlary.Dep1.X.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep1.X.Y.c.3o Ocamlary.Dep1.X.Y.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep2.3o Ocamlary.Dep2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep2.A.3o Ocamlary.Dep2.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep3.3o Ocamlary.Dep3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep4.3o Ocamlary.Dep4.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep4.X.3o Ocamlary.Dep4.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.3o Ocamlary.Dep5.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep5.Z.3o Ocamlary.Dep5.Z.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.3o Ocamlary.Dep6.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.X.3o Ocamlary.Dep6.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep6.X.Y.3o Ocamlary.Dep6.X.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.3o Ocamlary.Dep7.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep7.M.3o Ocamlary.Dep7.M.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep8.3o Ocamlary.Dep8.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep9.3o Ocamlary.Dep9.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep11.3o Ocamlary.Dep11.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep12.3o Ocamlary.Dep12.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep13.3o Ocamlary.Dep13.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Dep13.c.3o Ocamlary.Dep13.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With2.3o Ocamlary.With2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.3o Ocamlary.With3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With3.N.3o Ocamlary.With3.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.3o Ocamlary.With4.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With4.N.3o Ocamlary.With4.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With5.3o Ocamlary.With5.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With5.N.3o Ocamlary.With5.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With6.3o Ocamlary.With6.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With7.3o Ocamlary.With7.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With9.3o Ocamlary.With9.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.With10.3o Ocamlary.With10.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.DoubleInclude1.3o Ocamlary.DoubleInclude1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude1.DoubleInclude2.3o
   Ocamlary.DoubleInclude1.DoubleInclude2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.DoubleInclude3.3o Ocamlary.DoubleInclude3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.DoubleInclude3.DoubleInclude2.3o
   Ocamlary.DoubleInclude3.DoubleInclude2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.IncludeInclude1.3o Ocamlary.IncludeInclude1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.CanonicalTest.3o Ocamlary.CanonicalTest.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__List.3o
   Ocamlary.CanonicalTest.Base__List.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__.3o
   Ocamlary.CanonicalTest.Base__.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.CanonicalTest.Base.3o Ocamlary.CanonicalTest.Base.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base.List.3o
   Ocamlary.CanonicalTest.Base.List.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__Tests.3o
   Ocamlary.CanonicalTest.Base__Tests.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.Base__Tests.C.3o
   Ocamlary.CanonicalTest.Base__Tests.C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Ocamlary.CanonicalTest.List_modif.3o
   Ocamlary.CanonicalTest.List_modif.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.3o Ocamlary.Aliases.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__A.3o Ocamlary.Aliases.Foo__A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__B.3o Ocamlary.Aliases.Foo__B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__C.3o Ocamlary.Aliases.Foo__C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__D.3o Ocamlary.Aliases.Foo__D.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__E.3o Ocamlary.Aliases.Foo__E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo__.3o Ocamlary.Aliases.Foo__.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.3o Ocamlary.Aliases.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.A.3o Ocamlary.Aliases.Foo.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.B.3o Ocamlary.Aliases.Foo.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.C.3o Ocamlary.Aliases.Foo.C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.D.3o Ocamlary.Aliases.Foo.D.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Foo.E.3o Ocamlary.Aliases.Foo.E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.Std.3o Ocamlary.Aliases.Std.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.E.3o Ocamlary.Aliases.E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.P1.3o Ocamlary.Aliases.P1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.P1.Y.3o Ocamlary.Aliases.P1.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Aliases.P2.3o Ocamlary.Aliases.P2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.M.3o Ocamlary.M.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Ocamlary.Only_a_module.3o Ocamlary.Only_a_module.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../section.odocl})
   (with-stdout-to
    Section.3o.gen
    (progn
     (system "cat 'man.gen/test/Section.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Section.3o Section.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../stop.odocl})
   (with-stdout-to
    Stop.3o.gen
    (progn
     (system "cat 'man.gen/test/Stop.3o'")))
   (with-stdout-to
    Stop.N.3o.gen
    (progn
     (system "cat 'man.gen/test/Stop.N.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop.3o Stop.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Stop.N.3o Stop.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../stop_dead_link_doc.odocl})
   (with-stdout-to
    Stop_dead_link_doc.3o.gen
    (progn
     (system "cat 'man.gen/test/Stop_dead_link_doc.3o'")))
   (with-stdout-to
    Stop_dead_link_doc.Foo.3o.gen
    (progn
     (system "cat 'man.gen/test/Stop_dead_link_doc.Foo.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Stop_dead_link_doc.3o Stop_dead_link_doc.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Stop_dead_link_doc.Foo.3o Stop_dead_link_doc.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../toplevel_comments.odocl})
   (with-stdout-to
    Toplevel_comments.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.3o'")))
   (with-stdout-to
    Toplevel_comments.Include_inline.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.Include_inline.3o'")))
   (with-stdout-to
    Toplevel_comments.Include_inline'.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.Include_inline'\\''.3o'")))
   (with-stdout-to
    Toplevel_comments.M.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.M.3o'")))
   (with-stdout-to
    Toplevel_comments.M'.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.M'\\''.3o'")))
   (with-stdout-to
    Toplevel_comments.M''.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.M'\\'''\\''.3o'")))
   (with-stdout-to
    Toplevel_comments.Alias.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.Alias.3o'")))
   (with-stdout-to
    Toplevel_comments.c1.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.c1.3o'")))
   (with-stdout-to
    Toplevel_comments.c2.3o.gen
    (progn
     (system "cat 'man.gen/test/Toplevel_comments.c2.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.3o Toplevel_comments.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.Include_inline.3o
   Toplevel_comments.Include_inline.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff
   Toplevel_comments.Include_inline'.3o
   Toplevel_comments.Include_inline'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.M.3o Toplevel_comments.M.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.M'.3o Toplevel_comments.M'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.M''.3o Toplevel_comments.M''.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.Alias.3o Toplevel_comments.Alias.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.c1.3o Toplevel_comments.c1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff Toplevel_comments.c2.3o Toplevel_comments.c2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../type.odocl})
   (with-stdout-to
    Type.3o.gen
    (progn
     (system "cat 'man.gen/test/Type.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Type.3o Type.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../val.odocl})
   (with-stdout-to
    Val.3o.gen
    (progn
     (system "cat 'man.gen/test/Val.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Val.3o Val.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))
