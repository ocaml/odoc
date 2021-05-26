(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../bugs_post_406.odocl})
   (with-stdout-to
    Bugs_post_406.3o.gen
    (progn
     (system "cat 'man.gen/test/Bugs_post_406.3o'")))
   (with-stdout-to
    Bugs_post_406.let_open'.3o.gen
    (progn
     (system "cat 'man.gen/test/Bugs_post_406.let_open'\\''.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Bugs_post_406.3o Bugs_post_406.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Bugs_post_406.let_open'.3o Bugs_post_406.let_open'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../labels.odocl})
   (with-stdout-to
    Labels.3o.gen
    (progn
     (system "cat 'man.gen/test/Labels.3o'")))
   (with-stdout-to
    Labels.A.3o.gen
    (progn
     (system "cat 'man.gen/test/Labels.A.3o'")))
   (with-stdout-to
    Labels.c.3o.gen
    (progn
     (system "cat 'man.gen/test/Labels.c.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Labels.3o Labels.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.A.3o Labels.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.c.3o Labels.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../recent.odocl})
   (with-stdout-to
    Recent.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent.3o'")))
   (with-stdout-to
    Recent.Z.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent.Z.3o'")))
   (with-stdout-to
    Recent.Z.Y.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent.Z.Y.3o'")))
   (with-stdout-to
    Recent.Z.Y.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent.Z.Y.X.3o'")))
   (with-stdout-to
    Recent.X.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent.X.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Recent.3o Recent.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.3o Recent.Z.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.Y.3o Recent.Z.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.Z.Y.X.3o Recent.Z.Y.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.X.3o Recent.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc man-generate -o man.gen %{dep:../recent_impl.odocl})
   (with-stdout-to
    Recent_impl.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent_impl.3o'")))
   (with-stdout-to
    Recent_impl.Foo.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent_impl.Foo.3o'")))
   (with-stdout-to
    Recent_impl.Foo.A.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent_impl.Foo.A.3o'")))
   (with-stdout-to
    Recent_impl.Foo.B.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent_impl.Foo.B.3o'")))
   (with-stdout-to
    Recent_impl.B.3o.gen
    (progn
     (system "cat 'man.gen/test/Recent_impl.B.3o'"))))))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.3o Recent_impl.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.3o Recent_impl.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.A.3o Recent_impl.Foo.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.Foo.B.3o Recent_impl.Foo.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.B.3o Recent_impl.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))
