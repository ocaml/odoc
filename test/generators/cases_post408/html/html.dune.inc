(rule
 (action
  (progn
   (run
    odoc
    html-generate
    --indent
    -o
    html.gen
    %{dep:../bugs_post_406.odocl})
   (with-stdout-to
    Bugs_post_406.index.html.gen
    (progn
     (system "cat 'html.gen/test/Bugs_post_406/index.html'")))
   (with-stdout-to
    Bugs_post_406.class-type-let_open.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Bugs_post_406/class-type-let_open/index.html'")))
   (with-stdout-to
    Bugs_post_406.class-let_open'.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Bugs_post_406/class-let_open'\\''/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs_post_406.index.html Bugs_post_406.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Bugs_post_406.class-type-let_open.index.html
   Bugs_post_406.class-type-let_open.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Bugs_post_406.class-let_open'.index.html
   Bugs_post_406.class-let_open'.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../labels.odocl})
   (with-stdout-to
    Labels.index.html.gen
    (progn
     (system "cat 'html.gen/test/Labels/index.html'")))
   (with-stdout-to
    Labels.A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Labels/A/index.html'")))
   (with-stdout-to
    Labels.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Labels/module-type-S/index.html'")))
   (with-stdout-to
    Labels.class-c.index.html.gen
    (progn
     (system "cat 'html.gen/test/Labels/class-c/index.html'")))
   (with-stdout-to
    Labels.class-type-cs.index.html.gen
    (progn
     (system "cat 'html.gen/test/Labels/class-type-cs/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Labels.index.html Labels.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.A.index.html Labels.A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.module-type-S.index.html Labels.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.class-c.index.html Labels.class-c.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.class-type-cs.index.html Labels.class-type-cs.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../recent.odocl})
   (with-stdout-to
    Recent.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/index.html'")))
   (with-stdout-to
    Recent.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/module-type-S/index.html'")))
   (with-stdout-to
    Recent.module-type-S1.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/module-type-S1/index.html'")))
   (with-stdout-to
    Recent.module-type-S1.argument-1-_.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Recent/module-type-S1/argument-1-_/index.html'")))
   (with-stdout-to
    Recent.Z.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/Z/index.html'")))
   (with-stdout-to
    Recent.Z.Y.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/Z/Y/index.html'")))
   (with-stdout-to
    Recent.Z.Y.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/Z/Y/X/index.html'")))
   (with-stdout-to
    Recent.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/X/index.html'")))
   (with-stdout-to
    Recent.module-type-PolyS.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent/module-type-PolyS/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Recent.index.html Recent.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.module-type-S.index.html Recent.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent.module-type-S1.index.html
   Recent.module-type-S1.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent.module-type-S1.argument-1-_.index.html
   Recent.module-type-S1.argument-1-_.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.index.html Recent.Z.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.Y.index.html Recent.Z.Y.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.Y.X.index.html Recent.Z.Y.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.X.index.html Recent.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent.module-type-PolyS.index.html
   Recent.module-type-PolyS.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc html-generate --indent -o html.gen %{dep:../recent_impl.odocl})
   (with-stdout-to
    Recent_impl.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/index.html'")))
   (with-stdout-to
    Recent_impl.Foo.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/Foo/index.html'")))
   (with-stdout-to
    Recent_impl.Foo.A.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/Foo/A/index.html'")))
   (with-stdout-to
    Recent_impl.Foo.B.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/Foo/B/index.html'")))
   (with-stdout-to
    Recent_impl.B.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/B/index.html'")))
   (with-stdout-to
    Recent_impl.module-type-S.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/module-type-S/index.html'")))
   (with-stdout-to
    Recent_impl.module-type-S.F.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/module-type-S/F/index.html'")))
   (with-stdout-to
    Recent_impl.module-type-S.F.argument-1-_.index.html.gen
    (progn
     (system
      "cat 'html.gen/test/Recent_impl/module-type-S/F/argument-1-_/index.html'")))
   (with-stdout-to
    Recent_impl.module-type-S.X.index.html.gen
    (progn
     (system "cat 'html.gen/test/Recent_impl/module-type-S/X/index.html'"))))))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.index.html Recent_impl.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.index.html Recent_impl.Foo.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.A.index.html Recent_impl.Foo.A.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.B.index.html Recent_impl.Foo.B.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.B.index.html Recent_impl.B.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent_impl.module-type-S.index.html
   Recent_impl.module-type-S.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent_impl.module-type-S.F.index.html
   Recent_impl.module-type-S.F.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent_impl.module-type-S.F.argument-1-_.index.html
   Recent_impl.module-type-S.F.argument-1-_.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   Recent_impl.module-type-S.X.index.html
   Recent_impl.module-type-S.X.index.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))
