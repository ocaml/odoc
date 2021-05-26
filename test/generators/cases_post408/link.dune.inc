(rule
 (target bugs_post_406.cmti)
 (deps cases/bugs_post_406.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target bugs_post_406.odoc)
 (deps bugs_post_406.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target bugs_post_406.odocl)
 (deps bugs_post_406.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target labels.cmti)
 (deps cases/labels.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target labels.odoc)
 (deps labels.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target labels.odocl)
 (deps labels.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target recent.cmti)
 (deps cases/recent.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target recent.odoc)
 (deps recent.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target recent.odocl)
 (deps recent.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target recent_impl.cmt)
 (deps cases/recent_impl.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target recent_impl.odoc)
 (deps recent_impl.cmt)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target recent_impl.odocl)
 (deps recent_impl.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(subdir
 html
 (rule
  (with-stdout-to
   html.dune.inc.gen
   (pipe-stdout
    (run
     gen_html/gen_html.exe
     %{dep:../bugs_post_406.odocl}
     %{dep:../labels.odocl}
     %{dep:../recent.odocl}
     %{dep:../recent_impl.odocl})
    (run dune format-dune-file)))))

(subdir
 latex
 (rule
  (with-stdout-to
   latex.dune.inc.gen
   (pipe-stdout
    (run
     gen_latex/gen_latex.exe
     %{dep:../bugs_post_406.odocl}
     %{dep:../labels.odocl}
     %{dep:../recent.odocl}
     %{dep:../recent_impl.odocl})
    (run dune format-dune-file)))))

(subdir
 man
 (rule
  (with-stdout-to
   man.dune.inc.gen
   (pipe-stdout
    (run
     gen_man/gen_man.exe
     %{dep:../bugs_post_406.odocl}
     %{dep:../labels.odocl}
     %{dep:../recent.odocl}
     %{dep:../recent_impl.odocl})
    (run dune format-dune-file)))))

(rule
 (alias runtest)
 (action
  (diff html/html.dune.inc html/html.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff latex/latex.dune.inc latex/latex.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff man/man.dune.inc man/man.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))
