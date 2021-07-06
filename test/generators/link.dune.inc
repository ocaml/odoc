(rule
 (target alias.cmt)
 (deps cases/alias.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target alias.odoc)
 (deps alias.cmt)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target alias.odocl)
 (deps alias.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target bugs.cmt)
 (deps cases/bugs.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target bugs.odoc)
 (deps bugs.cmt)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target bugs.odocl)
 (deps bugs.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target bugs_post_406.cmti)
 (deps cases/bugs_post_406.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_post_406.odoc)
 (deps bugs_post_406.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_post_406.odocl)
 (deps bugs_post_406.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_pre_410.cmt)
 (deps cases/bugs_pre_410.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target bugs_pre_410.odoc)
 (deps bugs_pre_410.cmt)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target bugs_pre_410.odocl)
 (deps bugs_pre_410.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target class.cmti)
 (deps cases/class.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target class.odoc)
 (deps class.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target class.odocl)
 (deps class.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target external.cmti)
 (deps cases/external.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target external.odoc)
 (deps external.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target external.odocl)
 (deps external.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target functor.cmti)
 (deps cases/functor.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target functor.odoc)
 (deps functor.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target functor.odocl)
 (deps functor.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target functor2.cmti)
 (deps cases/functor2.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target functor2.odoc)
 (deps functor2.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target functor2.odocl)
 (deps functor2.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target include.cmti)
 (deps cases/include.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target include.odoc)
 (deps include.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target include.odocl)
 (deps include.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target include2.cmt)
 (deps cases/include2.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target include2.odoc)
 (deps include2.cmt)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target include2.odocl)
 (deps include2.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target include_sections.cmti)
 (deps cases/include_sections.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target include_sections.odoc)
 (deps include_sections.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target include_sections.odocl)
 (deps include_sections.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target interlude.cmti)
 (deps cases/interlude.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target interlude.odoc)
 (deps interlude.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target interlude.odocl)
 (deps interlude.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target labels.cmti)
 (deps cases/labels.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target labels.odoc)
 (deps labels.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target labels.odocl)
 (deps labels.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target markup.cmti)
 (deps cases/markup.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target markup.odoc)
 (deps markup.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target markup.odocl)
 (deps markup.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target page-mld.odoc)
 (deps cases/mld.mld)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target page-mld.odocl)
 (deps page-mld.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target module.cmti)
 (deps cases/module.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target module.odoc)
 (deps module.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target module.odocl)
 (deps module.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target module_type_subst.cmti)
 (deps cases/module_type_subst.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target module_type_subst.odoc)
 (deps module_type_subst.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target module_type_subst.odocl)
 (deps module_type_subst.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target nested.cmti)
 (deps cases/nested.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target nested.odoc)
 (deps nested.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target nested.odocl)
 (deps nested.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target ocamlary.cmti)
 (deps cases/ocamlary.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target ocamlary.odoc)
 (deps ocamlary.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target ocamlary.odocl)
 (deps ocamlary.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target recent.cmti)
 (deps cases/recent.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent.odoc)
 (deps recent.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent.odocl)
 (deps recent.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.cmt)
 (deps cases/recent_impl.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.odoc)
 (deps recent_impl.cmt)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.odocl)
 (deps recent_impl.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target section.cmti)
 (deps cases/section.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target section.odoc)
 (deps section.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target section.odocl)
 (deps section.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target stop.cmti)
 (deps cases/stop.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target stop.odoc)
 (deps stop.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target stop.odocl)
 (deps stop.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target stop_dead_link_doc.cmti)
 (deps cases/stop_dead_link_doc.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target stop_dead_link_doc.odoc)
 (deps stop_dead_link_doc.cmti)
 (action
  (run odoc compile -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target stop_dead_link_doc.odocl)
 (deps stop_dead_link_doc.odoc)
 (action
  (run odoc link -o %{target} %{deps}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target toplevel_comments.cmti)
 (deps cases/toplevel_comments.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target toplevel_comments.odoc)
 (deps toplevel_comments.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target toplevel_comments.odocl)
 (deps toplevel_comments.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target type.cmti)
 (deps cases/type.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target type.odoc)
 (deps type.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target type.odocl)
 (deps type.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target val.cmti)
 (deps cases/val.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target val.odoc)
 (deps val.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target val.odocl)
 (deps val.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(subdir
 html
 (rule
  (targets Alias.html.gen Alias-Foo__X.html.gen Alias-X.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../alias.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Alias.html html/Alias.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Alias-Foo__X.html html/Alias-Foo__X.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Alias-X.html html/Alias-X.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    alias.gen
    (run odoc html-targets -o . %{dep:../alias.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/alias.targets html/alias.gen)))

(subdir
 latex
 (rule
  (targets Alias.tex.gen Alias.X.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../alias.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Alias.tex latex/Alias.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Alias.X.tex latex/Alias.X.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    alias.gen
    (run odoc latex-targets -o . %{dep:../alias.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/alias.targets latex/alias.gen)))

(subdir
 man
 (rule
  (targets Alias.3o.gen Alias.Foo__X.3o.gen Alias.X.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../alias.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Alias.3o man/Alias.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Alias.Foo__X.3o man/Alias.Foo__X.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Alias.X.3o man/Alias.X.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    alias.gen
    (run odoc man-targets -o . %{dep:../alias.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/alias.targets man/alias.gen)))

(subdir
 html
 (rule
  (targets Bugs.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../bugs.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Bugs.html html/Bugs.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs.gen
    (run odoc html-targets -o . %{dep:../bugs.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/bugs.targets html/bugs.gen)))

(subdir
 latex
 (rule
  (targets Bugs.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../bugs.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Bugs.tex latex/Bugs.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs.gen
    (run odoc latex-targets -o . %{dep:../bugs.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/bugs.targets latex/bugs.gen)))

(subdir
 man
 (rule
  (targets Bugs.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../bugs.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Bugs.3o man/Bugs.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs.gen
    (run odoc man-targets -o . %{dep:../bugs.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/bugs.targets man/bugs.gen)))

(subdir
 html
 (rule
  (targets
   Bugs_post_406.html.gen
   Bugs_post_406-class-type-let_open.html.gen
   Bugs_post_406-class-let_open'.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff html/Bugs_post_406.html html/Bugs_post_406.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (alias runtest)
 (action
  (diff
   html/Bugs_post_406-class-type-let_open.html
   html/Bugs_post_406-class-type-let_open.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (alias runtest)
 (action
  (diff
   html/Bugs_post_406-class-let_open'.html
   html/Bugs_post_406-class-let_open'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc html-targets -o . %{dep:../bugs_post_406.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff html/bugs_post_406.targets html/bugs_post_406.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 latex
 (rule
  (targets Bugs_post_406.tex.gen Bugs_post_406.let_open'.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff latex/Bugs_post_406.tex latex/Bugs_post_406.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Bugs_post_406.let_open'.tex
   latex/Bugs_post_406.let_open'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc latex-targets -o . %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff latex/bugs_post_406.targets latex/bugs_post_406.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 man
 (rule
  (targets Bugs_post_406.3o.gen Bugs_post_406.let_open'.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff man/Bugs_post_406.3o man/Bugs_post_406.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (alias runtest)
 (action
  (diff man/Bugs_post_406.let_open'.3o man/Bugs_post_406.let_open'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc man-targets -o . %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(rule
 (alias runtest)
 (action
  (diff man/bugs_post_406.targets man/bugs_post_406.gen))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(subdir
 html
 (rule
  (targets Bugs_pre_410.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/Bugs_pre_410.html html/Bugs_pre_410.html.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.gen
    (run odoc html-targets -o . %{dep:../bugs_pre_410.odocl} --flat)))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/bugs_pre_410.targets html/bugs_pre_410.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (targets Bugs_pre_410.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/Bugs_pre_410.tex latex/Bugs_pre_410.tex.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.gen
    (run odoc latex-targets -o . %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/bugs_pre_410.targets latex/bugs_pre_410.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (targets Bugs_pre_410.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/Bugs_pre_410.3o man/Bugs_pre_410.3o.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.gen
    (run odoc man-targets -o . %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/bugs_pre_410.targets man/bugs_pre_410.gen))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (targets
   Class.html.gen
   Class-class-type-empty.html.gen
   Class-class-type-mutually.html.gen
   Class-class-type-recursive.html.gen
   Class-class-mutually'.html.gen
   Class-class-recursive'.html.gen
   Class-class-type-empty_virtual.html.gen
   Class-class-empty_virtual'.html.gen
   Class-class-type-polymorphic.html.gen
   Class-class-polymorphic'.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../class.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Class.html html/Class.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-type-empty.html
   html/Class-class-type-empty.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-type-mutually.html
   html/Class-class-type-mutually.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-type-recursive.html
   html/Class-class-type-recursive.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Class-class-mutually'.html html/Class-class-mutually'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-recursive'.html
   html/Class-class-recursive'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-type-empty_virtual.html
   html/Class-class-type-empty_virtual.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-empty_virtual'.html
   html/Class-class-empty_virtual'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-type-polymorphic.html
   html/Class-class-type-polymorphic.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Class-class-polymorphic'.html
   html/Class-class-polymorphic'.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    class.gen
    (run odoc html-targets -o . %{dep:../class.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/class.targets html/class.gen)))

(subdir
 latex
 (rule
  (targets
   Class.tex.gen
   Class.mutually'.tex.gen
   Class.recursive'.tex.gen
   Class.empty_virtual'.tex.gen
   Class.polymorphic'.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../class.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Class.tex latex/Class.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Class.mutually'.tex latex/Class.mutually'.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Class.recursive'.tex latex/Class.recursive'.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Class.empty_virtual'.tex latex/Class.empty_virtual'.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Class.polymorphic'.tex latex/Class.polymorphic'.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    class.gen
    (run odoc latex-targets -o . %{dep:../class.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/class.targets latex/class.gen)))

(subdir
 man
 (rule
  (targets
   Class.3o.gen
   Class.mutually'.3o.gen
   Class.recursive'.3o.gen
   Class.empty_virtual'.3o.gen
   Class.polymorphic'.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../class.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Class.3o man/Class.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Class.mutually'.3o man/Class.mutually'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Class.recursive'.3o man/Class.recursive'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Class.empty_virtual'.3o man/Class.empty_virtual'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Class.polymorphic'.3o man/Class.polymorphic'.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    class.gen
    (run odoc man-targets -o . %{dep:../class.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/class.targets man/class.gen)))

(subdir
 html
 (rule
  (targets External.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../external.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/External.html html/External.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    external.gen
    (run odoc html-targets -o . %{dep:../external.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/external.targets html/external.gen)))

(subdir
 latex
 (rule
  (targets External.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../external.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/External.tex latex/External.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    external.gen
    (run odoc latex-targets -o . %{dep:../external.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/external.targets latex/external.gen)))

(subdir
 man
 (rule
  (targets External.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../external.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/External.3o man/External.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    external.gen
    (run odoc man-targets -o . %{dep:../external.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/external.targets man/external.gen)))

(subdir
 html
 (rule
  (targets
   Functor.html.gen
   Functor-module-type-S.html.gen
   Functor-module-type-S1.html.gen
   Functor-module-type-S1-argument-1-_.html.gen
   Functor-F1.html.gen
   Functor-F1-argument-1-Arg.html.gen
   Functor-F2.html.gen
   Functor-F2-argument-1-Arg.html.gen
   Functor-F3.html.gen
   Functor-F3-argument-1-Arg.html.gen
   Functor-F4.html.gen
   Functor-F4-argument-1-Arg.html.gen
   Functor-F5.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../functor.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Functor.html html/Functor.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-module-type-S.html html/Functor-module-type-S.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-module-type-S1.html
   html/Functor-module-type-S1.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-module-type-S1-argument-1-_.html
   html/Functor-module-type-S1-argument-1-_.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-F1.html html/Functor-F1.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-F1-argument-1-Arg.html
   html/Functor-F1-argument-1-Arg.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-F2.html html/Functor-F2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-F2-argument-1-Arg.html
   html/Functor-F2-argument-1-Arg.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-F3.html html/Functor-F3.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-F3-argument-1-Arg.html
   html/Functor-F3-argument-1-Arg.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-F4.html html/Functor-F4.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor-F4-argument-1-Arg.html
   html/Functor-F4-argument-1-Arg.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor-F5.html html/Functor-F5.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    functor.gen
    (run odoc html-targets -o . %{dep:../functor.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/functor.targets html/functor.gen)))

(subdir
 latex
 (rule
  (targets
   Functor.tex.gen
   Functor.F1.tex.gen
   Functor.F2.tex.gen
   Functor.F3.tex.gen
   Functor.F4.tex.gen
   Functor.F5.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../functor.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.tex latex/Functor.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.F1.tex latex/Functor.F1.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.F2.tex latex/Functor.F2.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.F3.tex latex/Functor.F3.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.F4.tex latex/Functor.F4.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor.F5.tex latex/Functor.F5.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    functor.gen
    (run odoc latex-targets -o . %{dep:../functor.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/functor.targets latex/functor.gen)))

(subdir
 man
 (rule
  (targets
   Functor.3o.gen
   Functor.F1.3o.gen
   Functor.F2.3o.gen
   Functor.F3.3o.gen
   Functor.F4.3o.gen
   Functor.F5.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../functor.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Functor.3o man/Functor.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor.F1.3o man/Functor.F1.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor.F2.3o man/Functor.F2.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor.F3.3o man/Functor.F3.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor.F4.3o man/Functor.F4.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor.F5.3o man/Functor.F5.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    functor.gen
    (run odoc man-targets -o . %{dep:../functor.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/functor.targets man/functor.gen)))

(subdir
 html
 (rule
  (targets
   Functor2.html.gen
   Functor2-module-type-S.html.gen
   Functor2-X.html.gen
   Functor2-X-argument-1-Y.html.gen
   Functor2-X-argument-2-Z.html.gen
   Functor2-module-type-XF.html.gen
   Functor2-module-type-XF-argument-1-Y.html.gen
   Functor2-module-type-XF-argument-2-Z.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../functor2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Functor2.html html/Functor2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-module-type-S.html
   html/Functor2-module-type-S.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Functor2-X.html html/Functor2-X.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-X-argument-1-Y.html
   html/Functor2-X-argument-1-Y.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-X-argument-2-Z.html
   html/Functor2-X-argument-2-Z.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-module-type-XF.html
   html/Functor2-module-type-XF.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-module-type-XF-argument-1-Y.html
   html/Functor2-module-type-XF-argument-1-Y.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Functor2-module-type-XF-argument-2-Z.html
   html/Functor2-module-type-XF-argument-2-Z.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    functor2.gen
    (run odoc html-targets -o . %{dep:../functor2.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/functor2.targets html/functor2.gen)))

(subdir
 latex
 (rule
  (targets Functor2.tex.gen Functor2.X.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../functor2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Functor2.tex latex/Functor2.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Functor2.X.tex latex/Functor2.X.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    functor2.gen
    (run odoc latex-targets -o . %{dep:../functor2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/functor2.targets latex/functor2.gen)))

(subdir
 man
 (rule
  (targets Functor2.3o.gen Functor2.X.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../functor2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Functor2.3o man/Functor2.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Functor2.X.3o man/Functor2.X.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    functor2.gen
    (run odoc man-targets -o . %{dep:../functor2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/functor2.targets man/functor2.gen)))

(subdir
 html
 (rule
  (targets
   Include.html.gen
   Include-module-type-Not_inlined.html.gen
   Include-module-type-Inlined.html.gen
   Include-module-type-Not_inlined_and_closed.html.gen
   Include-module-type-Not_inlined_and_opened.html.gen
   Include-module-type-Inherent_Module.html.gen
   Include-module-type-Dorminant_Module.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../include.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Include.html html/Include.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Not_inlined.html
   html/Include-module-type-Not_inlined.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Inlined.html
   html/Include-module-type-Inlined.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Not_inlined_and_closed.html
   html/Include-module-type-Not_inlined_and_closed.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Not_inlined_and_opened.html
   html/Include-module-type-Not_inlined_and_opened.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Inherent_Module.html
   html/Include-module-type-Inherent_Module.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include-module-type-Dorminant_Module.html
   html/Include-module-type-Dorminant_Module.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include.gen
    (run odoc html-targets -o . %{dep:../include.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/include.targets html/include.gen)))

(subdir
 latex
 (rule
  (targets Include.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../include.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Include.tex latex/Include.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include.gen
    (run odoc latex-targets -o . %{dep:../include.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/include.targets latex/include.gen)))

(subdir
 man
 (rule
  (targets Include.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../include.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Include.3o man/Include.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include.gen
    (run odoc man-targets -o . %{dep:../include.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/include.targets man/include.gen)))

(subdir
 html
 (rule
  (targets
   Include2.html.gen
   Include2-X.html.gen
   Include2-Y.html.gen
   Include2-Y_include_synopsis.html.gen
   Include2-Y_include_doc.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../include2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Include2.html html/Include2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Include2-X.html html/Include2-X.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Include2-Y.html html/Include2-Y.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include2-Y_include_synopsis.html
   html/Include2-Y_include_synopsis.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include2-Y_include_doc.html
   html/Include2-Y_include_doc.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include2.gen
    (run odoc html-targets -o . %{dep:../include2.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/include2.targets html/include2.gen)))

(subdir
 latex
 (rule
  (targets Include2.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../include2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Include2.tex latex/Include2.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include2.gen
    (run odoc latex-targets -o . %{dep:../include2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/include2.targets latex/include2.gen)))

(subdir
 man
 (rule
  (targets
   Include2.3o.gen
   Include2.X.3o.gen
   Include2.Y.3o.gen
   Include2.Y_include_synopsis.3o.gen
   Include2.Y_include_doc.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../include2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Include2.3o man/Include2.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Include2.X.3o man/Include2.X.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Include2.Y.3o man/Include2.Y.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff
   man/Include2.Y_include_synopsis.3o
   man/Include2.Y_include_synopsis.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Include2.Y_include_doc.3o man/Include2.Y_include_doc.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include2.gen
    (run odoc man-targets -o . %{dep:../include2.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/include2.targets man/include2.gen)))

(subdir
 html
 (rule
  (targets
   Include_sections.html.gen
   Include_sections-module-type-Something.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../include_sections.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Include_sections.html html/Include_sections.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Include_sections-module-type-Something.html
   html/Include_sections-module-type-Something.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include_sections.gen
    (run odoc html-targets -o . %{dep:../include_sections.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/include_sections.targets html/include_sections.gen)))

(subdir
 latex
 (rule
  (targets Include_sections.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../include_sections.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Include_sections.tex latex/Include_sections.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include_sections.gen
    (run odoc latex-targets -o . %{dep:../include_sections.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/include_sections.targets latex/include_sections.gen)))

(subdir
 man
 (rule
  (targets Include_sections.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../include_sections.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Include_sections.3o man/Include_sections.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include_sections.gen
    (run odoc man-targets -o . %{dep:../include_sections.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/include_sections.targets man/include_sections.gen)))

(subdir
 html
 (rule
  (targets Interlude.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../interlude.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Interlude.html html/Interlude.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    interlude.gen
    (run odoc html-targets -o . %{dep:../interlude.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/interlude.targets html/interlude.gen)))

(subdir
 latex
 (rule
  (targets Interlude.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../interlude.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Interlude.tex latex/Interlude.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    interlude.gen
    (run odoc latex-targets -o . %{dep:../interlude.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/interlude.targets latex/interlude.gen)))

(subdir
 man
 (rule
  (targets Interlude.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../interlude.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Interlude.3o man/Interlude.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    interlude.gen
    (run odoc man-targets -o . %{dep:../interlude.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/interlude.targets man/interlude.gen)))

(subdir
 html
 (rule
  (targets
   Labels.html.gen
   Labels-A.html.gen
   Labels-module-type-S.html.gen
   Labels-class-c.html.gen
   Labels-class-type-cs.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/Labels.html html/Labels.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Labels-A.html html/Labels-A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Labels-module-type-S.html html/Labels-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Labels-class-c.html html/Labels-class-c.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Labels-class-type-cs.html html/Labels-class-type-cs.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    labels.gen
    (run odoc html-targets -o . %{dep:../labels.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/labels.targets html/labels.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (targets Labels.tex.gen Labels.c.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/Labels.tex latex/Labels.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff latex/Labels.c.tex latex/Labels.c.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    labels.gen
    (run odoc latex-targets -o . %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/labels.targets latex/labels.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (targets Labels.3o.gen Labels.A.3o.gen Labels.c.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/Labels.3o man/Labels.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Labels.A.3o man/Labels.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Labels.c.3o man/Labels.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    labels.gen
    (run odoc man-targets -o . %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/labels.targets man/labels.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (targets Markup.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../markup.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Markup.html html/Markup.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    markup.gen
    (run odoc html-targets -o . %{dep:../markup.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/markup.targets html/markup.gen)))

(subdir
 latex
 (rule
  (targets Markup.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../markup.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Markup.tex latex/Markup.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    markup.gen
    (run odoc latex-targets -o . %{dep:../markup.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/markup.targets latex/markup.gen)))

(subdir
 man
 (rule
  (targets Markup.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../markup.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Markup.3o man/Markup.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    markup.gen
    (run odoc man-targets -o . %{dep:../markup.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/markup.targets man/markup.gen)))

(subdir
 html
 (rule
  (targets mld.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../page-mld.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/mld.html html/mld.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    page-mld.gen
    (run odoc html-targets -o . %{dep:../page-mld.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/page-mld.targets html/page-mld.gen)))

(subdir
 latex
 (rule
  (targets mld.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../page-mld.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/mld.tex latex/mld.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    page-mld.gen
    (run odoc latex-targets -o . %{dep:../page-mld.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/page-mld.targets latex/page-mld.gen)))

(subdir
 man
 (rule
  (targets mld.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../page-mld.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/mld.3o man/mld.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    page-mld.gen
    (run odoc man-targets -o . %{dep:../page-mld.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/page-mld.targets man/page-mld.gen)))

(subdir
 html
 (rule
  (targets
   Module.html.gen
   Module-module-type-S.html.gen
   Module-module-type-S-M.html.gen
   Module-module-type-S2.html.gen
   Module-module-type-S2-M.html.gen
   Module-module-type-S3.html.gen
   Module-module-type-S3-M.html.gen
   Module-module-type-S4.html.gen
   Module-module-type-S4-M.html.gen
   Module-module-type-S5.html.gen
   Module-module-type-S5-M.html.gen
   Module-module-type-S6.html.gen
   Module-module-type-S6-M.html.gen
   Module-M'.html.gen
   Module-module-type-S7.html.gen
   Module-module-type-S8.html.gen
   Module-module-type-S9.html.gen
   Module-Mutually.html.gen
   Module-Recursive.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../module.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Module.html html/Module.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S.html html/Module-module-type-S.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S-M.html
   html/Module-module-type-S-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S2.html html/Module-module-type-S2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S2-M.html
   html/Module-module-type-S2-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S3.html html/Module-module-type-S3.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S3-M.html
   html/Module-module-type-S3-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S4.html html/Module-module-type-S4.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S4-M.html
   html/Module-module-type-S4-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S5.html html/Module-module-type-S5.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S5-M.html
   html/Module-module-type-S5-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S6.html html/Module-module-type-S6.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Module-module-type-S6-M.html
   html/Module-module-type-S6-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-M'.html html/Module-M'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S7.html html/Module-module-type-S7.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S8.html html/Module-module-type-S8.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-module-type-S9.html html/Module-module-type-S9.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-Mutually.html html/Module-Mutually.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Module-Recursive.html html/Module-Recursive.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    module.gen
    (run odoc html-targets -o . %{dep:../module.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/module.targets html/module.gen)))

(subdir
 latex
 (rule
  (targets Module.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../module.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Module.tex latex/Module.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    module.gen
    (run odoc latex-targets -o . %{dep:../module.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/module.targets latex/module.gen)))

(subdir
 man
 (rule
  (targets
   Module.3o.gen
   Module.M'.3o.gen
   Module.Mutually.3o.gen
   Module.Recursive.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../module.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Module.3o man/Module.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Module.M'.3o man/Module.M'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Module.Mutually.3o man/Module.Mutually.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Module.Recursive.3o man/Module.Recursive.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    module.gen
    (run odoc man-targets -o . %{dep:../module.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/module.targets man/module.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    module_type_subst.gen
    (run odoc html-targets -o . %{dep:../module_type_subst.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(rule
 (alias runtest)
 (action
  (diff html/module_type_subst.targets html/module_type_subst.gen))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    module_type_subst.gen
    (run odoc latex-targets -o . %{dep:../module_type_subst.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(rule
 (alias runtest)
 (action
  (diff latex/module_type_subst.targets latex/module_type_subst.gen))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    module_type_subst.gen
    (run odoc man-targets -o . %{dep:../module_type_subst.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(rule
 (alias runtest)
 (action
  (diff man/module_type_subst.targets man/module_type_subst.gen))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(subdir
 html
 (rule
  (targets
   Nested.html.gen
   Nested-X.html.gen
   Nested-module-type-Y.html.gen
   Nested-F.html.gen
   Nested-F-argument-1-Arg1.html.gen
   Nested-F-argument-2-Arg2.html.gen
   Nested-class-z.html.gen
   Nested-class-inherits.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../nested.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Nested.html html/Nested.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Nested-X.html html/Nested-X.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Nested-module-type-Y.html html/Nested-module-type-Y.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Nested-F.html html/Nested-F.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Nested-F-argument-1-Arg1.html
   html/Nested-F-argument-1-Arg1.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Nested-F-argument-2-Arg2.html
   html/Nested-F-argument-2-Arg2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Nested-class-z.html html/Nested-class-z.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Nested-class-inherits.html html/Nested-class-inherits.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    nested.gen
    (run odoc html-targets -o . %{dep:../nested.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/nested.targets html/nested.gen)))

(subdir
 latex
 (rule
  (targets
   Nested.tex.gen
   Nested.F.tex.gen
   Nested.z.tex.gen
   Nested.inherits.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../nested.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Nested.tex latex/Nested.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Nested.F.tex latex/Nested.F.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Nested.z.tex latex/Nested.z.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Nested.inherits.tex latex/Nested.inherits.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    nested.gen
    (run odoc latex-targets -o . %{dep:../nested.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/nested.targets latex/nested.gen)))

(subdir
 man
 (rule
  (targets
   Nested.3o.gen
   Nested.X.3o.gen
   Nested.F.3o.gen
   Nested.z.3o.gen
   Nested.inherits.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../nested.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Nested.3o man/Nested.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Nested.X.3o man/Nested.X.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Nested.F.3o man/Nested.F.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Nested.z.3o man/Nested.z.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Nested.inherits.3o man/Nested.inherits.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    nested.gen
    (run odoc man-targets -o . %{dep:../nested.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/nested.targets man/nested.gen)))

(subdir
 html
 (rule
  (targets
   Ocamlary.html.gen
   Ocamlary-Empty.html.gen
   Ocamlary-module-type-Empty.html.gen
   Ocamlary-module-type-MissingComment.html.gen
   Ocamlary-module-type-EmptySig.html.gen
   Ocamlary-module-type-EmptySigAlias.html.gen
   Ocamlary-ModuleWithSignature.html.gen
   Ocamlary-ModuleWithSignatureAlias.html.gen
   Ocamlary-One.html.gen
   Ocamlary-module-type-SigForMod.html.gen
   Ocamlary-module-type-SigForMod-Inner.html.gen
   Ocamlary-module-type-SigForMod-Inner-module-type-Empty.html.gen
   Ocamlary-module-type-SuperSig.html.gen
   Ocamlary-module-type-SuperSig-module-type-SubSigA.html.gen
   Ocamlary-module-type-SuperSig-module-type-SubSigA-SubSigAMod.html.gen
   Ocamlary-module-type-SuperSig-module-type-SubSigB.html.gen
   Ocamlary-module-type-SuperSig-module-type-EmptySig.html.gen
   Ocamlary-module-type-SuperSig-module-type-One.html.gen
   Ocamlary-module-type-SuperSig-module-type-SuperSig.html.gen
   Ocamlary-Buffer.html.gen
   Ocamlary-CollectionModule.html.gen
   Ocamlary-CollectionModule-InnerModuleA.html.gen
   Ocamlary-CollectionModule-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-CollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-CollectionModule-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-COLLECTION.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-COLLECTION-module-type-InnerModuleTypeA.html.gen
   Ocamlary-Recollection.html.gen
   Ocamlary-Recollection-argument-1-C.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-Recollection-argument-1-C-module-type-InnerModuleTypeA.html.gen
   Ocamlary-Recollection-InnerModuleA.html.gen
   Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-Recollection-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-MMM.html.gen
   Ocamlary-module-type-MMM-C.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-MMM-C-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-RECOLLECTION.html.gen
   Ocamlary-module-type-RecollectionModule.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-RecollectionModule-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-A.html.gen
   Ocamlary-module-type-A-Q.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-A-Q-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-B.html.gen
   Ocamlary-module-type-B-Q.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-B-Q-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-C.html.gen
   Ocamlary-module-type-C-Q.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-C-Q-module-type-InnerModuleTypeA.html.gen
   Ocamlary-FunctorTypeOf.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-module-type-InnerModuleTypeA.html.gen
   Ocamlary-module-type-IncludeModuleType.html.gen
   Ocamlary-module-type-ToInclude.html.gen
   Ocamlary-module-type-ToInclude-IncludedA.html.gen
   Ocamlary-module-type-ToInclude-module-type-IncludedB.html.gen
   Ocamlary-IncludedA.html.gen
   Ocamlary-module-type-IncludedB.html.gen
   Ocamlary-ExtMod.html.gen
   Ocamlary-class-empty_class.html.gen
   Ocamlary-class-one_method_class.html.gen
   Ocamlary-class-two_method_class.html.gen
   Ocamlary-class-param_class.html.gen
   Ocamlary-Dep1.html.gen
   Ocamlary-Dep1-module-type-S.html.gen
   Ocamlary-Dep1-module-type-S-class-c.html.gen
   Ocamlary-Dep1-X.html.gen
   Ocamlary-Dep1-X-Y.html.gen
   Ocamlary-Dep1-X-Y-class-c.html.gen
   Ocamlary-Dep2.html.gen
   Ocamlary-Dep2-argument-1-Arg.html.gen
   Ocamlary-Dep2-argument-1-Arg-X.html.gen
   Ocamlary-Dep2-A.html.gen
   Ocamlary-Dep3.html.gen
   Ocamlary-Dep4.html.gen
   Ocamlary-Dep4-module-type-T.html.gen
   Ocamlary-Dep4-module-type-S.html.gen
   Ocamlary-Dep4-module-type-S-X.html.gen
   Ocamlary-Dep4-module-type-S-Y.html.gen
   Ocamlary-Dep4-X.html.gen
   Ocamlary-Dep5.html.gen
   Ocamlary-Dep5-argument-1-Arg.html.gen
   Ocamlary-Dep5-argument-1-Arg-module-type-S.html.gen
   Ocamlary-Dep5-argument-1-Arg-module-type-S-Y.html.gen
   Ocamlary-Dep5-Z.html.gen
   Ocamlary-Dep6.html.gen
   Ocamlary-Dep6-module-type-S.html.gen
   Ocamlary-Dep6-module-type-T.html.gen
   Ocamlary-Dep6-module-type-T-module-type-R.html.gen
   Ocamlary-Dep6-module-type-T-Y.html.gen
   Ocamlary-Dep6-X.html.gen
   Ocamlary-Dep6-X-module-type-R.html.gen
   Ocamlary-Dep6-X-Y.html.gen
   Ocamlary-Dep7.html.gen
   Ocamlary-Dep7-argument-1-Arg.html.gen
   Ocamlary-Dep7-argument-1-Arg-module-type-T.html.gen
   Ocamlary-Dep7-argument-1-Arg-X.html.gen
   Ocamlary-Dep7-M.html.gen
   Ocamlary-Dep8.html.gen
   Ocamlary-Dep8-module-type-T.html.gen
   Ocamlary-Dep9.html.gen
   Ocamlary-Dep9-argument-1-X.html.gen
   Ocamlary-module-type-Dep10.html.gen
   Ocamlary-Dep11.html.gen
   Ocamlary-Dep11-module-type-S.html.gen
   Ocamlary-Dep11-module-type-S-class-c.html.gen
   Ocamlary-Dep12.html.gen
   Ocamlary-Dep12-argument-1-Arg.html.gen
   Ocamlary-Dep13.html.gen
   Ocamlary-Dep13-class-c.html.gen
   Ocamlary-module-type-With1.html.gen
   Ocamlary-module-type-With1-M.html.gen
   Ocamlary-With2.html.gen
   Ocamlary-With2-module-type-S.html.gen
   Ocamlary-With3.html.gen
   Ocamlary-With3-N.html.gen
   Ocamlary-With4.html.gen
   Ocamlary-With4-N.html.gen
   Ocamlary-With5.html.gen
   Ocamlary-With5-module-type-S.html.gen
   Ocamlary-With5-N.html.gen
   Ocamlary-With6.html.gen
   Ocamlary-With6-module-type-T.html.gen
   Ocamlary-With6-module-type-T-M.html.gen
   Ocamlary-With7.html.gen
   Ocamlary-With7-argument-1-X.html.gen
   Ocamlary-module-type-With8.html.gen
   Ocamlary-module-type-With8-M.html.gen
   Ocamlary-module-type-With8-M-module-type-S.html.gen
   Ocamlary-module-type-With8-M-N.html.gen
   Ocamlary-With9.html.gen
   Ocamlary-With9-module-type-S.html.gen
   Ocamlary-With10.html.gen
   Ocamlary-With10-module-type-T.html.gen
   Ocamlary-With10-module-type-T-M.html.gen
   Ocamlary-module-type-With11.html.gen
   Ocamlary-module-type-With11-N.html.gen
   Ocamlary-module-type-NestedInclude1.html.gen
   Ocamlary-module-type-NestedInclude1-module-type-NestedInclude2.html.gen
   Ocamlary-module-type-NestedInclude2.html.gen
   Ocamlary-DoubleInclude1.html.gen
   Ocamlary-DoubleInclude1-DoubleInclude2.html.gen
   Ocamlary-DoubleInclude3.html.gen
   Ocamlary-DoubleInclude3-DoubleInclude2.html.gen
   Ocamlary-IncludeInclude1.html.gen
   Ocamlary-IncludeInclude1-module-type-IncludeInclude2.html.gen
   Ocamlary-module-type-IncludeInclude2.html.gen
   Ocamlary-CanonicalTest.html.gen
   Ocamlary-CanonicalTest-Base__List.html.gen
   Ocamlary-CanonicalTest-Base__.html.gen
   Ocamlary-CanonicalTest-Base.html.gen
   Ocamlary-CanonicalTest-Base-List.html.gen
   Ocamlary-CanonicalTest-Base__Tests.html.gen
   Ocamlary-CanonicalTest-Base__Tests-C.html.gen
   Ocamlary-CanonicalTest-List_modif.html.gen
   Ocamlary-Aliases.html.gen
   Ocamlary-Aliases-Foo__A.html.gen
   Ocamlary-Aliases-Foo__B.html.gen
   Ocamlary-Aliases-Foo__C.html.gen
   Ocamlary-Aliases-Foo__D.html.gen
   Ocamlary-Aliases-Foo__E.html.gen
   Ocamlary-Aliases-Foo__.html.gen
   Ocamlary-Aliases-Foo.html.gen
   Ocamlary-Aliases-Foo-A.html.gen
   Ocamlary-Aliases-Foo-B.html.gen
   Ocamlary-Aliases-Foo-C.html.gen
   Ocamlary-Aliases-Foo-D.html.gen
   Ocamlary-Aliases-Foo-E.html.gen
   Ocamlary-Aliases-Std.html.gen
   Ocamlary-Aliases-E.html.gen
   Ocamlary-Aliases-P1.html.gen
   Ocamlary-Aliases-P1-Y.html.gen
   Ocamlary-Aliases-P2.html.gen
   Ocamlary-module-type-M.html.gen
   Ocamlary-M.html.gen
   Ocamlary-Only_a_module.html.gen
   Ocamlary-module-type-TypeExt.html.gen
   Ocamlary-module-type-TypeExtPruned.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary.html html/Ocamlary.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Empty.html html/Ocamlary-Empty.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-Empty.html
   html/Ocamlary-module-type-Empty.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MissingComment.html
   html/Ocamlary-module-type-MissingComment.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-EmptySig.html
   html/Ocamlary-module-type-EmptySig.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-EmptySigAlias.html
   html/Ocamlary-module-type-EmptySigAlias.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-ModuleWithSignature.html
   html/Ocamlary-ModuleWithSignature.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-ModuleWithSignatureAlias.html
   html/Ocamlary-ModuleWithSignatureAlias.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-One.html html/Ocamlary-One.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SigForMod.html
   html/Ocamlary-module-type-SigForMod.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SigForMod-Inner.html
   html/Ocamlary-module-type-SigForMod-Inner.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SigForMod-Inner-module-type-Empty.html
   html/Ocamlary-module-type-SigForMod-Inner-module-type-Empty.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig.html
   html/Ocamlary-module-type-SuperSig.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-SubSigA.html
   html/Ocamlary-module-type-SuperSig-module-type-SubSigA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-SubSigA-SubSigAMod.html
   html/Ocamlary-module-type-SuperSig-module-type-SubSigA-SubSigAMod.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-SubSigB.html
   html/Ocamlary-module-type-SuperSig-module-type-SubSigB.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-EmptySig.html
   html/Ocamlary-module-type-SuperSig-module-type-EmptySig.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-One.html
   html/Ocamlary-module-type-SuperSig-module-type-One.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-SuperSig-module-type-SuperSig.html
   html/Ocamlary-module-type-SuperSig-module-type-SuperSig.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Buffer.html html/Ocamlary-Buffer.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CollectionModule.html
   html/Ocamlary-CollectionModule.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CollectionModule-InnerModuleA.html
   html/Ocamlary-CollectionModule-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CollectionModule-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-CollectionModule-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-CollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CollectionModule-module-type-InnerModuleTypeA.html
   html/Ocamlary-CollectionModule-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-COLLECTION.html
   html/Ocamlary-module-type-COLLECTION.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-COLLECTION-InnerModuleA.html
   html/Ocamlary-module-type-COLLECTION-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-COLLECTION-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-COLLECTION-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Recollection.html html/Ocamlary-Recollection.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-argument-1-C.html
   html/Ocamlary-Recollection-argument-1-C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA.html
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-argument-1-C-module-type-InnerModuleTypeA.html
   html/Ocamlary-Recollection-argument-1-C-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-InnerModuleA.html
   html/Ocamlary-Recollection-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Recollection-module-type-InnerModuleTypeA.html
   html/Ocamlary-Recollection-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM.html
   html/Ocamlary-module-type-MMM.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM-C.html
   html/Ocamlary-module-type-MMM-C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM-C-InnerModuleA.html
   html/Ocamlary-module-type-MMM-C-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-MMM-C-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-MMM-C-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RECOLLECTION.html
   html/Ocamlary-module-type-RECOLLECTION.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RecollectionModule.html
   html/Ocamlary-module-type-RecollectionModule.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA.html
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-RecollectionModule-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-RecollectionModule-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A.html
   html/Ocamlary-module-type-A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A-Q.html
   html/Ocamlary-module-type-A-Q.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A-Q-InnerModuleA.html
   html/Ocamlary-module-type-A-Q-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-A-Q-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-A-Q-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B.html
   html/Ocamlary-module-type-B.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B-Q.html
   html/Ocamlary-module-type-B-Q.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B-Q-InnerModuleA.html
   html/Ocamlary-module-type-B-Q-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-B-Q-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-B-Q-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C.html
   html/Ocamlary-module-type-C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C-Q.html
   html/Ocamlary-module-type-C-Q.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C-Q-InnerModuleA.html
   html/Ocamlary-module-type-C-Q-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-C-Q-module-type-InnerModuleTypeA.html
   html/Ocamlary-module-type-C-Q-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf.html
   html/Ocamlary-FunctorTypeOf.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf-argument-1-Collection.html
   html/Ocamlary-FunctorTypeOf-argument-1-Collection.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-module-type-InnerModuleTypeA.html
   html/Ocamlary-FunctorTypeOf-argument-1-Collection-module-type-InnerModuleTypeA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-IncludeModuleType.html
   html/Ocamlary-module-type-IncludeModuleType.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-ToInclude.html
   html/Ocamlary-module-type-ToInclude.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-ToInclude-IncludedA.html
   html/Ocamlary-module-type-ToInclude-IncludedA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-ToInclude-module-type-IncludedB.html
   html/Ocamlary-module-type-ToInclude-module-type-IncludedB.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-IncludedA.html html/Ocamlary-IncludedA.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-IncludedB.html
   html/Ocamlary-module-type-IncludedB.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-ExtMod.html html/Ocamlary-ExtMod.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-class-empty_class.html
   html/Ocamlary-class-empty_class.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-class-one_method_class.html
   html/Ocamlary-class-one_method_class.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-class-two_method_class.html
   html/Ocamlary-class-two_method_class.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-class-param_class.html
   html/Ocamlary-class-param_class.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep1.html html/Ocamlary-Dep1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep1-module-type-S.html
   html/Ocamlary-Dep1-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep1-module-type-S-class-c.html
   html/Ocamlary-Dep1-module-type-S-class-c.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep1-X.html html/Ocamlary-Dep1-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep1-X-Y.html html/Ocamlary-Dep1-X-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep1-X-Y-class-c.html
   html/Ocamlary-Dep1-X-Y-class-c.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep2.html html/Ocamlary-Dep2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep2-argument-1-Arg.html
   html/Ocamlary-Dep2-argument-1-Arg.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep2-argument-1-Arg-X.html
   html/Ocamlary-Dep2-argument-1-Arg-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep2-A.html html/Ocamlary-Dep2-A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep3.html html/Ocamlary-Dep3.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep4.html html/Ocamlary-Dep4.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep4-module-type-T.html
   html/Ocamlary-Dep4-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep4-module-type-S.html
   html/Ocamlary-Dep4-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep4-module-type-S-X.html
   html/Ocamlary-Dep4-module-type-S-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep4-module-type-S-Y.html
   html/Ocamlary-Dep4-module-type-S-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep4-X.html html/Ocamlary-Dep4-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep5.html html/Ocamlary-Dep5.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep5-argument-1-Arg.html
   html/Ocamlary-Dep5-argument-1-Arg.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep5-argument-1-Arg-module-type-S.html
   html/Ocamlary-Dep5-argument-1-Arg-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep5-argument-1-Arg-module-type-S-Y.html
   html/Ocamlary-Dep5-argument-1-Arg-module-type-S-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep5-Z.html html/Ocamlary-Dep5-Z.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep6.html html/Ocamlary-Dep6.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep6-module-type-S.html
   html/Ocamlary-Dep6-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep6-module-type-T.html
   html/Ocamlary-Dep6-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep6-module-type-T-module-type-R.html
   html/Ocamlary-Dep6-module-type-T-module-type-R.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep6-module-type-T-Y.html
   html/Ocamlary-Dep6-module-type-T-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep6-X.html html/Ocamlary-Dep6-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep6-X-module-type-R.html
   html/Ocamlary-Dep6-X-module-type-R.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep6-X-Y.html html/Ocamlary-Dep6-X-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep7.html html/Ocamlary-Dep7.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep7-argument-1-Arg.html
   html/Ocamlary-Dep7-argument-1-Arg.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep7-argument-1-Arg-module-type-T.html
   html/Ocamlary-Dep7-argument-1-Arg-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep7-argument-1-Arg-X.html
   html/Ocamlary-Dep7-argument-1-Arg-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep7-M.html html/Ocamlary-Dep7-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep8.html html/Ocamlary-Dep8.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep8-module-type-T.html
   html/Ocamlary-Dep8-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep9.html html/Ocamlary-Dep9.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep9-argument-1-X.html
   html/Ocamlary-Dep9-argument-1-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-Dep10.html
   html/Ocamlary-module-type-Dep10.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep11.html html/Ocamlary-Dep11.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep11-module-type-S.html
   html/Ocamlary-Dep11-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep11-module-type-S-class-c.html
   html/Ocamlary-Dep11-module-type-S-class-c.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep12.html html/Ocamlary-Dep12.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep12-argument-1-Arg.html
   html/Ocamlary-Dep12-argument-1-Arg.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Dep13.html html/Ocamlary-Dep13.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Dep13-class-c.html
   html/Ocamlary-Dep13-class-c.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With1.html
   html/Ocamlary-module-type-With1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With1-M.html
   html/Ocamlary-module-type-With1-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With2.html html/Ocamlary-With2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With2-module-type-S.html
   html/Ocamlary-With2-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With3.html html/Ocamlary-With3.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With3-N.html html/Ocamlary-With3-N.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With4.html html/Ocamlary-With4.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With4-N.html html/Ocamlary-With4-N.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With5.html html/Ocamlary-With5.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With5-module-type-S.html
   html/Ocamlary-With5-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With5-N.html html/Ocamlary-With5-N.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With6.html html/Ocamlary-With6.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With6-module-type-T.html
   html/Ocamlary-With6-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With6-module-type-T-M.html
   html/Ocamlary-With6-module-type-T-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With7.html html/Ocamlary-With7.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With7-argument-1-X.html
   html/Ocamlary-With7-argument-1-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With8.html
   html/Ocamlary-module-type-With8.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With8-M.html
   html/Ocamlary-module-type-With8-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With8-M-module-type-S.html
   html/Ocamlary-module-type-With8-M-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With8-M-N.html
   html/Ocamlary-module-type-With8-M-N.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With9.html html/Ocamlary-With9.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With9-module-type-S.html
   html/Ocamlary-With9-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-With10.html html/Ocamlary-With10.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With10-module-type-T.html
   html/Ocamlary-With10-module-type-T.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-With10-module-type-T-M.html
   html/Ocamlary-With10-module-type-T-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With11.html
   html/Ocamlary-module-type-With11.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-With11-N.html
   html/Ocamlary-module-type-With11-N.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-NestedInclude1.html
   html/Ocamlary-module-type-NestedInclude1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-NestedInclude1-module-type-NestedInclude2.html
   html/Ocamlary-module-type-NestedInclude1-module-type-NestedInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-NestedInclude2.html
   html/Ocamlary-module-type-NestedInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-DoubleInclude1.html
   html/Ocamlary-DoubleInclude1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-DoubleInclude1-DoubleInclude2.html
   html/Ocamlary-DoubleInclude1-DoubleInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-DoubleInclude3.html
   html/Ocamlary-DoubleInclude3.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-DoubleInclude3-DoubleInclude2.html
   html/Ocamlary-DoubleInclude3-DoubleInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-IncludeInclude1.html
   html/Ocamlary-IncludeInclude1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-IncludeInclude1-module-type-IncludeInclude2.html
   html/Ocamlary-IncludeInclude1-module-type-IncludeInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-IncludeInclude2.html
   html/Ocamlary-module-type-IncludeInclude2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest.html
   html/Ocamlary-CanonicalTest.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base__List.html
   html/Ocamlary-CanonicalTest-Base__List.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base__.html
   html/Ocamlary-CanonicalTest-Base__.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base.html
   html/Ocamlary-CanonicalTest-Base.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base-List.html
   html/Ocamlary-CanonicalTest-Base-List.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base__Tests.html
   html/Ocamlary-CanonicalTest-Base__Tests.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-Base__Tests-C.html
   html/Ocamlary-CanonicalTest-Base__Tests-C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-CanonicalTest-List_modif.html
   html/Ocamlary-CanonicalTest-List_modif.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases.html html/Ocamlary-Aliases.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__A.html
   html/Ocamlary-Aliases-Foo__A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__B.html
   html/Ocamlary-Aliases-Foo__B.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__C.html
   html/Ocamlary-Aliases-Foo__C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__D.html
   html/Ocamlary-Aliases-Foo__D.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__E.html
   html/Ocamlary-Aliases-Foo__E.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo__.html
   html/Ocamlary-Aliases-Foo__.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-Foo.html html/Ocamlary-Aliases-Foo.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo-A.html
   html/Ocamlary-Aliases-Foo-A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo-B.html
   html/Ocamlary-Aliases-Foo-B.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo-C.html
   html/Ocamlary-Aliases-Foo-C.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo-D.html
   html/Ocamlary-Aliases-Foo-D.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Aliases-Foo-E.html
   html/Ocamlary-Aliases-Foo-E.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-Std.html html/Ocamlary-Aliases-Std.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-E.html html/Ocamlary-Aliases-E.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-P1.html html/Ocamlary-Aliases-P1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-P1-Y.html html/Ocamlary-Aliases-P1-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-Aliases-P2.html html/Ocamlary-Aliases-P2.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-M.html
   html/Ocamlary-module-type-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff html/Ocamlary-M.html html/Ocamlary-M.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-Only_a_module.html
   html/Ocamlary-Only_a_module.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-TypeExt.html
   html/Ocamlary-module-type-TypeExt.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   html/Ocamlary-module-type-TypeExtPruned.html
   html/Ocamlary-module-type-TypeExtPruned.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    ocamlary.gen
    (run odoc html-targets -o . %{dep:../ocamlary.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff html/ocamlary.targets html/ocamlary.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 latex
 (rule
  (targets
   Ocamlary.tex.gen
   Ocamlary.ModuleWithSignature.tex.gen
   Ocamlary.ModuleWithSignatureAlias.tex.gen
   Ocamlary.Recollection.tex.gen
   Ocamlary.FunctorTypeOf.tex.gen
   Ocamlary.empty_class.tex.gen
   Ocamlary.one_method_class.tex.gen
   Ocamlary.two_method_class.tex.gen
   Ocamlary.param_class.tex.gen
   Ocamlary.Dep2.tex.gen
   Ocamlary.Dep5.tex.gen
   Ocamlary.Dep5.Z.tex.gen
   Ocamlary.Dep7.tex.gen
   Ocamlary.Dep7.M.tex.gen
   Ocamlary.Dep9.tex.gen
   Ocamlary.Dep12.tex.gen
   Ocamlary.Dep13.tex.gen
   Ocamlary.Dep13.c.tex.gen
   Ocamlary.With3.tex.gen
   Ocamlary.With3.N.tex.gen
   Ocamlary.With4.tex.gen
   Ocamlary.With4.N.tex.gen
   Ocamlary.With7.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.tex latex/Ocamlary.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Ocamlary.ModuleWithSignature.tex
   latex/Ocamlary.ModuleWithSignature.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Ocamlary.ModuleWithSignatureAlias.tex
   latex/Ocamlary.ModuleWithSignatureAlias.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Recollection.tex latex/Ocamlary.Recollection.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Ocamlary.FunctorTypeOf.tex
   latex/Ocamlary.FunctorTypeOf.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.empty_class.tex latex/Ocamlary.empty_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Ocamlary.one_method_class.tex
   latex/Ocamlary.one_method_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Ocamlary.two_method_class.tex
   latex/Ocamlary.two_method_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.param_class.tex latex/Ocamlary.param_class.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep2.tex latex/Ocamlary.Dep2.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep5.tex latex/Ocamlary.Dep5.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep5.Z.tex latex/Ocamlary.Dep5.Z.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep7.tex latex/Ocamlary.Dep7.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep7.M.tex latex/Ocamlary.Dep7.M.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep9.tex latex/Ocamlary.Dep9.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep12.tex latex/Ocamlary.Dep12.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep13.tex latex/Ocamlary.Dep13.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.Dep13.c.tex latex/Ocamlary.Dep13.c.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.With3.tex latex/Ocamlary.With3.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.With3.N.tex latex/Ocamlary.With3.N.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.With4.tex latex/Ocamlary.With4.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.With4.N.tex latex/Ocamlary.With4.N.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff latex/Ocamlary.With7.tex latex/Ocamlary.With7.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    ocamlary.gen
    (run odoc latex-targets -o . %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff latex/ocamlary.targets latex/ocamlary.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 man
 (rule
  (targets
   Ocamlary.3o.gen
   Ocamlary.Empty.3o.gen
   Ocamlary.ModuleWithSignature.3o.gen
   Ocamlary.ModuleWithSignatureAlias.3o.gen
   Ocamlary.One.3o.gen
   Ocamlary.Buffer.3o.gen
   Ocamlary.CollectionModule.3o.gen
   Ocamlary.CollectionModule.InnerModuleA.3o.gen
   Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o.gen
   Ocamlary.Recollection.3o.gen
   Ocamlary.Recollection.InnerModuleA.3o.gen
   Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o.gen
   Ocamlary.FunctorTypeOf.3o.gen
   Ocamlary.IncludedA.3o.gen
   Ocamlary.ExtMod.3o.gen
   Ocamlary.empty_class.3o.gen
   Ocamlary.one_method_class.3o.gen
   Ocamlary.two_method_class.3o.gen
   Ocamlary.param_class.3o.gen
   Ocamlary.Dep1.3o.gen
   Ocamlary.Dep1.X.3o.gen
   Ocamlary.Dep1.X.Y.3o.gen
   Ocamlary.Dep1.X.Y.c.3o.gen
   Ocamlary.Dep2.3o.gen
   Ocamlary.Dep2.A.3o.gen
   Ocamlary.Dep3.3o.gen
   Ocamlary.Dep4.3o.gen
   Ocamlary.Dep4.X.3o.gen
   Ocamlary.Dep5.3o.gen
   Ocamlary.Dep5.Z.3o.gen
   Ocamlary.Dep6.3o.gen
   Ocamlary.Dep6.X.3o.gen
   Ocamlary.Dep6.X.Y.3o.gen
   Ocamlary.Dep7.3o.gen
   Ocamlary.Dep7.M.3o.gen
   Ocamlary.Dep8.3o.gen
   Ocamlary.Dep9.3o.gen
   Ocamlary.Dep11.3o.gen
   Ocamlary.Dep12.3o.gen
   Ocamlary.Dep13.3o.gen
   Ocamlary.Dep13.c.3o.gen
   Ocamlary.With2.3o.gen
   Ocamlary.With3.3o.gen
   Ocamlary.With3.N.3o.gen
   Ocamlary.With4.3o.gen
   Ocamlary.With4.N.3o.gen
   Ocamlary.With5.3o.gen
   Ocamlary.With5.N.3o.gen
   Ocamlary.With6.3o.gen
   Ocamlary.With7.3o.gen
   Ocamlary.With9.3o.gen
   Ocamlary.With10.3o.gen
   Ocamlary.DoubleInclude1.3o.gen
   Ocamlary.DoubleInclude1.DoubleInclude2.3o.gen
   Ocamlary.DoubleInclude3.3o.gen
   Ocamlary.DoubleInclude3.DoubleInclude2.3o.gen
   Ocamlary.IncludeInclude1.3o.gen
   Ocamlary.CanonicalTest.3o.gen
   Ocamlary.CanonicalTest.Base__List.3o.gen
   Ocamlary.CanonicalTest.Base__.3o.gen
   Ocamlary.CanonicalTest.Base.3o.gen
   Ocamlary.CanonicalTest.Base.List.3o.gen
   Ocamlary.CanonicalTest.Base__Tests.3o.gen
   Ocamlary.CanonicalTest.Base__Tests.C.3o.gen
   Ocamlary.CanonicalTest.List_modif.3o.gen
   Ocamlary.Aliases.3o.gen
   Ocamlary.Aliases.Foo__A.3o.gen
   Ocamlary.Aliases.Foo__B.3o.gen
   Ocamlary.Aliases.Foo__C.3o.gen
   Ocamlary.Aliases.Foo__D.3o.gen
   Ocamlary.Aliases.Foo__E.3o.gen
   Ocamlary.Aliases.Foo__.3o.gen
   Ocamlary.Aliases.Foo.3o.gen
   Ocamlary.Aliases.Foo.A.3o.gen
   Ocamlary.Aliases.Foo.B.3o.gen
   Ocamlary.Aliases.Foo.C.3o.gen
   Ocamlary.Aliases.Foo.D.3o.gen
   Ocamlary.Aliases.Foo.E.3o.gen
   Ocamlary.Aliases.Std.3o.gen
   Ocamlary.Aliases.E.3o.gen
   Ocamlary.Aliases.P1.3o.gen
   Ocamlary.Aliases.P1.Y.3o.gen
   Ocamlary.Aliases.P2.3o.gen
   Ocamlary.M.3o.gen
   Ocamlary.Only_a_module.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.3o man/Ocamlary.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Empty.3o man/Ocamlary.Empty.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.ModuleWithSignature.3o
   man/Ocamlary.ModuleWithSignature.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.ModuleWithSignatureAlias.3o
   man/Ocamlary.ModuleWithSignatureAlias.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.One.3o man/Ocamlary.One.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Buffer.3o man/Ocamlary.Buffer.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CollectionModule.3o
   man/Ocamlary.CollectionModule.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CollectionModule.InnerModuleA.3o
   man/Ocamlary.CollectionModule.InnerModuleA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o
   man/Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Recollection.3o man/Ocamlary.Recollection.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.Recollection.InnerModuleA.3o
   man/Ocamlary.Recollection.InnerModuleA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o
   man/Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.FunctorTypeOf.3o man/Ocamlary.FunctorTypeOf.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.IncludedA.3o man/Ocamlary.IncludedA.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.ExtMod.3o man/Ocamlary.ExtMod.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.empty_class.3o man/Ocamlary.empty_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.one_method_class.3o
   man/Ocamlary.one_method_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.two_method_class.3o
   man/Ocamlary.two_method_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.param_class.3o man/Ocamlary.param_class.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep1.3o man/Ocamlary.Dep1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep1.X.3o man/Ocamlary.Dep1.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep1.X.Y.3o man/Ocamlary.Dep1.X.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep1.X.Y.c.3o man/Ocamlary.Dep1.X.Y.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep2.3o man/Ocamlary.Dep2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep2.A.3o man/Ocamlary.Dep2.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep3.3o man/Ocamlary.Dep3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep4.3o man/Ocamlary.Dep4.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep4.X.3o man/Ocamlary.Dep4.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep5.3o man/Ocamlary.Dep5.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep5.Z.3o man/Ocamlary.Dep5.Z.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep6.3o man/Ocamlary.Dep6.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep6.X.3o man/Ocamlary.Dep6.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep6.X.Y.3o man/Ocamlary.Dep6.X.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep7.3o man/Ocamlary.Dep7.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep7.M.3o man/Ocamlary.Dep7.M.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep8.3o man/Ocamlary.Dep8.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep9.3o man/Ocamlary.Dep9.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep11.3o man/Ocamlary.Dep11.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep12.3o man/Ocamlary.Dep12.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep13.3o man/Ocamlary.Dep13.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Dep13.c.3o man/Ocamlary.Dep13.c.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With2.3o man/Ocamlary.With2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With3.3o man/Ocamlary.With3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With3.N.3o man/Ocamlary.With3.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With4.3o man/Ocamlary.With4.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With4.N.3o man/Ocamlary.With4.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With5.3o man/Ocamlary.With5.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With5.N.3o man/Ocamlary.With5.N.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With6.3o man/Ocamlary.With6.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With7.3o man/Ocamlary.With7.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With9.3o man/Ocamlary.With9.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.With10.3o man/Ocamlary.With10.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.DoubleInclude1.3o man/Ocamlary.DoubleInclude1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.DoubleInclude1.DoubleInclude2.3o
   man/Ocamlary.DoubleInclude1.DoubleInclude2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.DoubleInclude3.3o man/Ocamlary.DoubleInclude3.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.DoubleInclude3.DoubleInclude2.3o
   man/Ocamlary.DoubleInclude3.DoubleInclude2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.IncludeInclude1.3o man/Ocamlary.IncludeInclude1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.CanonicalTest.3o man/Ocamlary.CanonicalTest.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base__List.3o
   man/Ocamlary.CanonicalTest.Base__List.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base__.3o
   man/Ocamlary.CanonicalTest.Base__.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base.3o
   man/Ocamlary.CanonicalTest.Base.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base.List.3o
   man/Ocamlary.CanonicalTest.Base.List.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base__Tests.3o
   man/Ocamlary.CanonicalTest.Base__Tests.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.Base__Tests.C.3o
   man/Ocamlary.CanonicalTest.Base__Tests.C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff
   man/Ocamlary.CanonicalTest.List_modif.3o
   man/Ocamlary.CanonicalTest.List_modif.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.3o man/Ocamlary.Aliases.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__A.3o man/Ocamlary.Aliases.Foo__A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__B.3o man/Ocamlary.Aliases.Foo__B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__C.3o man/Ocamlary.Aliases.Foo__C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__D.3o man/Ocamlary.Aliases.Foo__D.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__E.3o man/Ocamlary.Aliases.Foo__E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo__.3o man/Ocamlary.Aliases.Foo__.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.3o man/Ocamlary.Aliases.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.A.3o man/Ocamlary.Aliases.Foo.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.B.3o man/Ocamlary.Aliases.Foo.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.C.3o man/Ocamlary.Aliases.Foo.C.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.D.3o man/Ocamlary.Aliases.Foo.D.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Foo.E.3o man/Ocamlary.Aliases.Foo.E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.Std.3o man/Ocamlary.Aliases.Std.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.E.3o man/Ocamlary.Aliases.E.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.P1.3o man/Ocamlary.Aliases.P1.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.P1.Y.3o man/Ocamlary.Aliases.P1.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Aliases.P2.3o man/Ocamlary.Aliases.P2.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.M.3o man/Ocamlary.M.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (alias runtest)
 (action
  (diff man/Ocamlary.Only_a_module.3o man/Ocamlary.Only_a_module.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    ocamlary.gen
    (run odoc man-targets -o . %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(rule
 (alias runtest)
 (action
  (diff man/ocamlary.targets man/ocamlary.gen))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(subdir
 html
 (rule
  (targets
   Recent.html.gen
   Recent-module-type-S.html.gen
   Recent-module-type-S1.html.gen
   Recent-module-type-S1-argument-1-_.html.gen
   Recent-Z.html.gen
   Recent-Z-Y.html.gen
   Recent-Z-Y-X.html.gen
   Recent-X.html.gen
   Recent-module-type-PolyS.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/Recent.html html/Recent.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-module-type-S.html html/Recent-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-module-type-S1.html html/Recent-module-type-S1.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent-module-type-S1-argument-1-_.html
   html/Recent-module-type-S1-argument-1-_.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-Z.html html/Recent-Z.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-Z-Y.html html/Recent-Z-Y.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-Z-Y-X.html html/Recent-Z-Y-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent-X.html html/Recent-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent-module-type-PolyS.html
   html/Recent-module-type-PolyS.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent.gen
    (run odoc html-targets -o . %{dep:../recent.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/recent.targets html/recent.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (targets Recent.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/Recent.tex latex/Recent.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    recent.gen
    (run odoc latex-targets -o . %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/recent.targets latex/recent.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (targets
   Recent.3o.gen
   Recent.Z.3o.gen
   Recent.Z.Y.3o.gen
   Recent.Z.Y.X.3o.gen
   Recent.X.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/Recent.3o man/Recent.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent.Z.3o man/Recent.Z.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent.Z.Y.3o man/Recent.Z.Y.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent.Z.Y.X.3o man/Recent.Z.Y.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent.X.3o man/Recent.X.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    recent.gen
    (run odoc man-targets -o . %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/recent.targets man/recent.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (targets
   Recent_impl.html.gen
   Recent_impl-Foo.html.gen
   Recent_impl-Foo-A.html.gen
   Recent_impl-Foo-B.html.gen
   Recent_impl-B.html.gen
   Recent_impl-module-type-S.html.gen
   Recent_impl-module-type-S-F.html.gen
   Recent_impl-module-type-S-F-argument-1-_.html.gen
   Recent_impl-module-type-S-X.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/Recent_impl.html html/Recent_impl.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent_impl-Foo.html html/Recent_impl-Foo.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent_impl-Foo-A.html html/Recent_impl-Foo-A.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent_impl-Foo-B.html html/Recent_impl-Foo-B.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff html/Recent_impl-B.html html/Recent_impl-B.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent_impl-module-type-S.html
   html/Recent_impl-module-type-S.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent_impl-module-type-S-F.html
   html/Recent_impl-module-type-S-F.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent_impl-module-type-S-F-argument-1-_.html
   html/Recent_impl-module-type-S-F-argument-1-_.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff
   html/Recent_impl-module-type-S-X.html
   html/Recent_impl-module-type-S-X.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent_impl.gen
    (run odoc html-targets -o . %{dep:../recent_impl.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff html/recent_impl.targets html/recent_impl.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (targets Recent_impl.tex.gen Recent_impl.B.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/Recent_impl.tex latex/Recent_impl.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff latex/Recent_impl.B.tex latex/Recent_impl.B.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    recent_impl.gen
    (run odoc latex-targets -o . %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff latex/recent_impl.targets latex/recent_impl.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (targets
   Recent_impl.3o.gen
   Recent_impl.Foo.3o.gen
   Recent_impl.Foo.A.3o.gen
   Recent_impl.Foo.B.3o.gen
   Recent_impl.B.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/Recent_impl.3o man/Recent_impl.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent_impl.Foo.3o man/Recent_impl.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent_impl.Foo.A.3o man/Recent_impl.Foo.A.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent_impl.Foo.B.3o man/Recent_impl.Foo.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (alias runtest)
 (action
  (diff man/Recent_impl.B.3o man/Recent_impl.B.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    recent_impl.gen
    (run odoc man-targets -o . %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(rule
 (alias runtest)
 (action
  (diff man/recent_impl.targets man/recent_impl.gen))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(subdir
 html
 (rule
  (targets Section.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../section.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Section.html html/Section.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    section.gen
    (run odoc html-targets -o . %{dep:../section.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/section.targets html/section.gen)))

(subdir
 latex
 (rule
  (targets Section.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../section.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Section.tex latex/Section.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    section.gen
    (run odoc latex-targets -o . %{dep:../section.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/section.targets latex/section.gen)))

(subdir
 man
 (rule
  (targets Section.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../section.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Section.3o man/Section.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    section.gen
    (run odoc man-targets -o . %{dep:../section.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/section.targets man/section.gen)))

(subdir
 html
 (rule
  (targets Stop.html.gen Stop-N.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../stop.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Stop.html html/Stop.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Stop-N.html html/Stop-N.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    stop.gen
    (run odoc html-targets -o . %{dep:../stop.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/stop.targets html/stop.gen)))

(subdir
 latex
 (rule
  (targets Stop.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../stop.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Stop.tex latex/Stop.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    stop.gen
    (run odoc latex-targets -o . %{dep:../stop.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/stop.targets latex/stop.gen)))

(subdir
 man
 (rule
  (targets Stop.3o.gen Stop.N.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../stop.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Stop.3o man/Stop.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Stop.N.3o man/Stop.N.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    stop.gen
    (run odoc man-targets -o . %{dep:../stop.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/stop.targets man/stop.gen)))

(subdir
 html
 (rule
  (targets Stop_dead_link_doc.html.gen Stop_dead_link_doc-Foo.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff html/Stop_dead_link_doc.html html/Stop_dead_link_doc.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (alias runtest)
 (action
  (diff
   html/Stop_dead_link_doc-Foo.html
   html/Stop_dead_link_doc-Foo.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.gen
    (run odoc html-targets -o . %{dep:../stop_dead_link_doc.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff html/stop_dead_link_doc.targets html/stop_dead_link_doc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 latex
 (rule
  (targets Stop_dead_link_doc.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff latex/Stop_dead_link_doc.tex latex/Stop_dead_link_doc.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.gen
    (run odoc latex-targets -o . %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff latex/stop_dead_link_doc.targets latex/stop_dead_link_doc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 man
 (rule
  (targets Stop_dead_link_doc.3o.gen Stop_dead_link_doc.Foo.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff man/Stop_dead_link_doc.3o man/Stop_dead_link_doc.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (alias runtest)
 (action
  (diff man/Stop_dead_link_doc.Foo.3o man/Stop_dead_link_doc.Foo.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.gen
    (run odoc man-targets -o . %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(rule
 (alias runtest)
 (action
  (diff man/stop_dead_link_doc.targets man/stop_dead_link_doc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(subdir
 html
 (rule
  (targets
   Toplevel_comments.html.gen
   Toplevel_comments-module-type-T.html.gen
   Toplevel_comments-Include_inline.html.gen
   Toplevel_comments-Include_inline'.html.gen
   Toplevel_comments-module-type-Include_inline_T.html.gen
   Toplevel_comments-module-type-Include_inline_T'.html.gen
   Toplevel_comments-M.html.gen
   Toplevel_comments-M'.html.gen
   Toplevel_comments-M''.html.gen
   Toplevel_comments-Alias.html.gen
   Toplevel_comments-class-c1.html.gen
   Toplevel_comments-class-type-ct.html.gen
   Toplevel_comments-class-c2.html.gen
   Toplevel_comments-Ref_in_synopsis.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../toplevel_comments.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Toplevel_comments.html html/Toplevel_comments.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-module-type-T.html
   html/Toplevel_comments-module-type-T.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-Include_inline.html
   html/Toplevel_comments-Include_inline.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-Include_inline'.html
   html/Toplevel_comments-Include_inline'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-module-type-Include_inline_T.html
   html/Toplevel_comments-module-type-Include_inline_T.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-module-type-Include_inline_T'.html
   html/Toplevel_comments-module-type-Include_inline_T'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Toplevel_comments-M.html html/Toplevel_comments-M.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Toplevel_comments-M'.html html/Toplevel_comments-M'.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Toplevel_comments-M''.html html/Toplevel_comments-M''.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-Alias.html
   html/Toplevel_comments-Alias.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-class-c1.html
   html/Toplevel_comments-class-c1.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-class-type-ct.html
   html/Toplevel_comments-class-type-ct.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-class-c2.html
   html/Toplevel_comments-class-c2.html.gen)))

(rule
 (alias runtest)
 (action
  (diff
   html/Toplevel_comments-Ref_in_synopsis.html
   html/Toplevel_comments-Ref_in_synopsis.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    toplevel_comments.gen
    (run odoc html-targets -o . %{dep:../toplevel_comments.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/toplevel_comments.targets html/toplevel_comments.gen)))

(subdir
 latex
 (rule
  (targets
   Toplevel_comments.tex.gen
   Toplevel_comments.Alias.tex.gen
   Toplevel_comments.c1.tex.gen
   Toplevel_comments.c2.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../toplevel_comments.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Toplevel_comments.tex latex/Toplevel_comments.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff
   latex/Toplevel_comments.Alias.tex
   latex/Toplevel_comments.Alias.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Toplevel_comments.c1.tex latex/Toplevel_comments.c1.tex.gen)))

(rule
 (alias runtest)
 (action
  (diff latex/Toplevel_comments.c2.tex latex/Toplevel_comments.c2.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    toplevel_comments.gen
    (run odoc latex-targets -o . %{dep:../toplevel_comments.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/toplevel_comments.targets latex/toplevel_comments.gen)))

(subdir
 man
 (rule
  (targets
   Toplevel_comments.3o.gen
   Toplevel_comments.Include_inline.3o.gen
   Toplevel_comments.Include_inline'.3o.gen
   Toplevel_comments.M.3o.gen
   Toplevel_comments.M'.3o.gen
   Toplevel_comments.M''.3o.gen
   Toplevel_comments.Alias.3o.gen
   Toplevel_comments.c1.3o.gen
   Toplevel_comments.c2.3o.gen
   Toplevel_comments.Ref_in_synopsis.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --extra-suffix
     gen
     %{dep:../toplevel_comments.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.3o man/Toplevel_comments.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff
   man/Toplevel_comments.Include_inline.3o
   man/Toplevel_comments.Include_inline.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff
   man/Toplevel_comments.Include_inline'.3o
   man/Toplevel_comments.Include_inline'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.M.3o man/Toplevel_comments.M.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.M'.3o man/Toplevel_comments.M'.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.M''.3o man/Toplevel_comments.M''.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.Alias.3o man/Toplevel_comments.Alias.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.c1.3o man/Toplevel_comments.c1.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff man/Toplevel_comments.c2.3o man/Toplevel_comments.c2.3o.gen)))

(rule
 (alias runtest)
 (action
  (diff
   man/Toplevel_comments.Ref_in_synopsis.3o
   man/Toplevel_comments.Ref_in_synopsis.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    toplevel_comments.gen
    (run odoc man-targets -o . %{dep:../toplevel_comments.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/toplevel_comments.targets man/toplevel_comments.gen)))

(subdir
 html
 (rule
  (targets Type.html.gen Type-module-type-X.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../type.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Type.html html/Type.html.gen)))

(rule
 (alias runtest)
 (action
  (diff html/Type-module-type-X.html html/Type-module-type-X.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    type.gen
    (run odoc html-targets -o . %{dep:../type.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/type.targets html/type.gen)))

(subdir
 latex
 (rule
  (targets Type.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../type.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Type.tex latex/Type.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    type.gen
    (run odoc latex-targets -o . %{dep:../type.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/type.targets latex/type.gen)))

(subdir
 man
 (rule
  (targets Type.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../type.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Type.3o man/Type.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    type.gen
    (run odoc man-targets -o . %{dep:../type.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/type.targets man/type.gen)))

(subdir
 html
 (rule
  (targets Val.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../val.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/Val.html html/Val.html.gen)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    val.gen
    (run odoc html-targets -o . %{dep:../val.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/val.targets html/val.gen)))

(subdir
 latex
 (rule
  (targets Val.tex.gen)
  (action
   (progn
    (run odoc latex-generate -o . --extra-suffix gen %{dep:../val.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/Val.tex latex/Val.tex.gen)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    val.gen
    (run odoc latex-targets -o . %{dep:../val.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/val.targets latex/val.gen)))

(subdir
 man
 (rule
  (targets Val.3o.gen)
  (action
   (progn
    (run odoc man-generate -o . --extra-suffix gen %{dep:../val.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/Val.3o man/Val.3o.gen)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    val.gen
    (run odoc man-targets -o . %{dep:../val.odocl})))))

(rule
 (alias runtest)
 (action
  (diff man/val.targets man/val.gen)))
