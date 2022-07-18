(rule
 (target alerts.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/alerts.mli})))

(rule
 (target alerts.odoc)
 (action
  (run odoc compile -o %{target} %{dep:alerts.cmti})))

(rule
 (target alerts.odocl)
 (action
  (run odoc link -o %{target} %{dep:alerts.odoc})))

(rule
 (target alias.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/alias.ml})))

(rule
 (target alias.odoc)
 (action
  (run odoc compile -o %{target} %{dep:alias.cmt})))

(rule
 (target alias.odocl)
 (action
  (run odoc link -o %{target} %{dep:alias.odoc})))

(rule
 (target bugs.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/bugs.ml})))

(rule
 (target bugs.odoc)
 (action
  (run odoc compile -o %{target} %{dep:bugs.cmt})))

(rule
 (target bugs.odocl)
 (action
  (run odoc link -o %{target} %{dep:bugs.odoc})))

(rule
 (target bugs_post_406.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/bugs_post_406.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_post_406.odoc)
 (action
  (run odoc compile -o %{target} %{dep:bugs_post_406.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_post_406.odocl)
 (action
  (run odoc link -o %{target} %{dep:bugs_post_406.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.06)))

(rule
 (target bugs_pre_410.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/bugs_pre_410.ml}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target bugs_pre_410.odoc)
 (action
  (run odoc compile -o %{target} %{dep:bugs_pre_410.cmt}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target bugs_pre_410.odocl)
 (action
  (run odoc link -o %{target} %{dep:bugs_pre_410.odoc}))
 (enabled_if
  (<= %{ocaml_version} 4.09)))

(rule
 (target class.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/class.mli})))

(rule
 (target class.odoc)
 (action
  (run odoc compile -o %{target} %{dep:class.cmti})))

(rule
 (target class.odocl)
 (action
  (run odoc link -o %{target} %{dep:class.odoc})))

(rule
 (target class_comments.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/class_comments.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (target class_comments.odoc)
 (action
  (run odoc compile -o %{target} %{dep:class_comments.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (target class_comments.odocl)
 (action
  (run odoc link -o %{target} %{dep:class_comments.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (target external.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/external.mli})))

(rule
 (target external.odoc)
 (action
  (run odoc compile -o %{target} %{dep:external.cmti})))

(rule
 (target external.odocl)
 (action
  (run odoc link -o %{target} %{dep:external.odoc})))

(rule
 (target functor.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/functor.mli})))

(rule
 (target functor.odoc)
 (action
  (run odoc compile -o %{target} %{dep:functor.cmti})))

(rule
 (target functor.odocl)
 (action
  (run odoc link -o %{target} %{dep:functor.odoc})))

(rule
 (target functor2.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/functor2.mli})))

(rule
 (target functor2.odoc)
 (action
  (run odoc compile -o %{target} %{dep:functor2.cmti})))

(rule
 (target functor2.odocl)
 (action
  (run odoc link -o %{target} %{dep:functor2.odoc})))

(rule
 (target include.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/include.mli})))

(rule
 (target include.odoc)
 (action
  (run odoc compile -o %{target} %{dep:include.cmti})))

(rule
 (target include.odocl)
 (action
  (run odoc link -o %{target} %{dep:include.odoc})))

(rule
 (target include2.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/include2.ml})))

(rule
 (target include2.odoc)
 (action
  (run odoc compile -o %{target} %{dep:include2.cmt})))

(rule
 (target include2.odocl)
 (action
  (run odoc link -o %{target} %{dep:include2.odoc})))

(rule
 (target include_sections.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/include_sections.mli})))

(rule
 (target include_sections.odoc)
 (action
  (run odoc compile -o %{target} %{dep:include_sections.cmti})))

(rule
 (target include_sections.odocl)
 (action
  (run odoc link -o %{target} %{dep:include_sections.odoc})))

(rule
 (target interlude.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/interlude.mli})))

(rule
 (target interlude.odoc)
 (action
  (run odoc compile -o %{target} %{dep:interlude.cmti})))

(rule
 (target interlude.odocl)
 (action
  (run odoc link -o %{target} %{dep:interlude.odoc})))

(rule
 (target labels.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/labels.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target labels.odoc)
 (action
  (run odoc compile -o %{target} %{dep:labels.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target labels.odocl)
 (action
  (run odoc link -o %{target} %{dep:labels.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target markup.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/markup.mli})))

(rule
 (target markup.odoc)
 (action
  (run odoc compile -o %{target} %{dep:markup.cmti})))

(rule
 (target markup.odocl)
 (action
  (run odoc link -o %{target} %{dep:markup.odoc})))

(rule
 (target page-mld.odoc)
 (action
  (run odoc compile -o %{target} %{dep:cases/mld.mld})))

(rule
 (target page-mld.odocl)
 (action
  (run odoc link -o %{target} %{dep:page-mld.odoc})))

(rule
 (target module.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/module.mli})))

(rule
 (target module.odoc)
 (action
  (run odoc compile -o %{target} %{dep:module.cmti})))

(rule
 (target module.odocl)
 (action
  (run odoc link -o %{target} %{dep:module.odoc})))

(rule
 (target module_type_alias.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/module_type_alias.mli})))

(rule
 (target module_type_alias.odoc)
 (action
  (run odoc compile -o %{target} %{dep:module_type_alias.cmti})))

(rule
 (target module_type_alias.odocl)
 (action
  (run odoc link -o %{target} %{dep:module_type_alias.odoc})))

(rule
 (target module_type_subst.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/module_type_subst.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target module_type_subst.odoc)
 (action
  (run odoc compile -o %{target} %{dep:module_type_subst.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target module_type_subst.odocl)
 (action
  (run odoc link -o %{target} %{dep:module_type_subst.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.13)))

(rule
 (target nested.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/nested.mli})))

(rule
 (target nested.odoc)
 (action
  (run odoc compile -o %{target} %{dep:nested.cmti})))

(rule
 (target nested.odocl)
 (action
  (run odoc link -o %{target} %{dep:nested.odoc})))

(rule
 (target ocamlary.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/ocamlary.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target ocamlary.odoc)
 (action
  (run odoc compile -o %{target} %{dep:ocamlary.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target ocamlary.odocl)
 (action
  (run odoc link -o %{target} %{dep:ocamlary.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.07)))

(rule
 (target recent.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/recent.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent.odoc)
 (action
  (run odoc compile -o %{target} %{dep:recent.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent.odocl)
 (action
  (run odoc link -o %{target} %{dep:recent.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/recent_impl.ml}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.odoc)
 (action
  (run odoc compile -o %{target} %{dep:recent_impl.cmt}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target recent_impl.odocl)
 (action
  (run odoc link -o %{target} %{dep:recent_impl.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.09)))

(rule
 (target section.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/section.mli})))

(rule
 (target section.odoc)
 (action
  (run odoc compile -o %{target} %{dep:section.cmti})))

(rule
 (target section.odocl)
 (action
  (run odoc link -o %{target} %{dep:section.odoc})))

(rule
 (target stop.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/stop.mli})))

(rule
 (target stop.odoc)
 (action
  (run odoc compile -o %{target} %{dep:stop.cmti})))

(rule
 (target stop.odocl)
 (action
  (run odoc link -o %{target} %{dep:stop.odoc})))

(rule
 (target stop_dead_link_doc.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/stop_dead_link_doc.mli}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target stop_dead_link_doc.odoc)
 (action
  (run odoc compile -o %{target} %{dep:stop_dead_link_doc.cmti}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target stop_dead_link_doc.odocl)
 (action
  (run odoc link -o %{target} %{dep:stop_dead_link_doc.odoc}))
 (enabled_if
  (>= %{ocaml_version} 4.04)))

(rule
 (target stop_first_comment.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/stop_first_comment.mli})))

(rule
 (target stop_first_comment.odoc)
 (action
  (run odoc compile -o %{target} %{dep:stop_first_comment.cmti})))

(rule
 (target stop_first_comment.odocl)
 (action
  (run odoc link -o %{target} %{dep:stop_first_comment.odoc})))

(rule
 (target tag_link.cmt)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/tag_link.ml})))

(rule
 (target tag_link.odoc)
 (action
  (run odoc compile -o %{target} %{dep:tag_link.cmt})))

(rule
 (target tag_link.odocl)
 (action
  (run odoc link -o %{target} %{dep:tag_link.odoc})))

(rule
 (target toplevel_comments.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/toplevel_comments.mli})))

(rule
 (target toplevel_comments.odoc)
 (action
  (run odoc compile -o %{target} %{dep:toplevel_comments.cmti})))

(rule
 (target toplevel_comments.odocl)
 (action
  (run odoc link -o %{target} %{dep:toplevel_comments.odoc})))

(rule
 (target type.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/type.mli})))

(rule
 (target type.odoc)
 (action
  (run odoc compile -o %{target} %{dep:type.cmti})))

(rule
 (target type.odocl)
 (action
  (run odoc link -o %{target} %{dep:type.odoc})))

(rule
 (target val.cmti)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{dep:cases/val.mli})))

(rule
 (target val.odoc)
 (action
  (run odoc compile -o %{target} %{dep:val.cmti})))

(rule
 (target val.odocl)
 (action
  (run odoc link -o %{target} %{dep:val.odoc})))

(subdir
 html
 (rule
  (targets Alerts.html.gen Alerts-Top1.html.gen Alerts-Top2.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../alerts.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alerts.html Alerts.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alerts-Top1.html Alerts-Top1.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alerts-Top2.html Alerts-Top2.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    alerts.targets.gen
    (run odoc html-targets -o . %{dep:../alerts.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff alerts.targets alerts.targets.gen))))

(subdir
 latex
 (rule
  (targets Alerts.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../alerts.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alerts.tex Alerts.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    alerts.targets.gen
    (run odoc latex-targets -o . %{dep:../alerts.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff alerts.targets alerts.targets.gen))))

(subdir
 man
 (rule
  (targets Alerts.3o.gen Alerts.Top1.3o.gen Alerts.Top2.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../alerts.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alerts.3o Alerts.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alerts.Top1.3o Alerts.Top1.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alerts.Top2.3o Alerts.Top2.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    alerts.targets.gen
    (run odoc man-targets -o . %{dep:../alerts.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff alerts.targets alerts.targets.gen))))

(subdir
 html
 (rule
  (targets Alias.html.gen Alias-X.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alias.html Alias.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alias-X.html Alias-X.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    alias.targets.gen
    (run odoc html-targets -o . %{dep:../alias.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff alias.targets alias.targets.gen))))

(subdir
 latex
 (rule
  (targets Alias.tex.gen Alias.X.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alias.tex Alias.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alias.X.tex Alias.X.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    alias.targets.gen
    (run odoc latex-targets -o . %{dep:../alias.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff alias.targets alias.targets.gen))))

(subdir
 man
 (rule
  (targets Alias.3o.gen Alias.X.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Alias.3o Alias.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Alias.X.3o Alias.X.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    alias.targets.gen
    (run odoc man-targets -o . %{dep:../alias.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff alias.targets alias.targets.gen))))

(subdir
 html
 (rule
  (targets Bugs.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../bugs.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Bugs.html Bugs.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs.targets.gen
    (run odoc html-targets -o . %{dep:../bugs.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff bugs.targets bugs.targets.gen))))

(subdir
 latex
 (rule
  (targets Bugs.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../bugs.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Bugs.tex Bugs.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs.targets.gen
    (run odoc latex-targets -o . %{dep:../bugs.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff bugs.targets bugs.targets.gen))))

(subdir
 man
 (rule
  (targets Bugs.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../bugs.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Bugs.3o Bugs.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs.targets.gen
    (run odoc man-targets -o . %{dep:../bugs.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff bugs.targets bugs.targets.gen))))

(subdir
 html
 (rule
  (targets
   Bugs_post_406.html.gen
   Bugs_post_406-class-type-let_open.html.gen
   Bugs_post_406-class-let_open'.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../bugs_post_406.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_post_406.html Bugs_post_406.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff
    Bugs_post_406-class-type-let_open.html
    Bugs_post_406-class-type-let_open.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff
    Bugs_post_406-class-let_open'.html
    Bugs_post_406-class-let_open'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs_post_406.targets.gen
    (run odoc html-targets -o . %{dep:../bugs_post_406.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff bugs_post_406.targets bugs_post_406.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 latex
 (rule
  (targets Bugs_post_406.tex.gen Bugs_post_406.let_open'.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../bugs_post_406.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_post_406.tex Bugs_post_406.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_post_406.let_open'.tex Bugs_post_406.let_open'.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs_post_406.targets.gen
    (run odoc latex-targets -o . %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff bugs_post_406.targets bugs_post_406.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 man
 (rule
  (targets Bugs_post_406.3o.gen Bugs_post_406.let_open'.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../bugs_post_406.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_post_406.3o Bugs_post_406.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_post_406.let_open'.3o Bugs_post_406.let_open'.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs_post_406.targets.gen
    (run odoc man-targets -o . %{dep:../bugs_post_406.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.06)))
 (rule
  (alias runtest)
  (action
   (diff bugs_post_406.targets bugs_post_406.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.06))))

(subdir
 html
 (rule
  (targets Bugs_pre_410.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../bugs_pre_410.odocl}))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_pre_410.html Bugs_pre_410.html.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.targets.gen
    (run odoc html-targets -o . %{dep:../bugs_pre_410.odocl} --flat)))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff bugs_pre_410.targets bugs_pre_410.targets.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (targets Bugs_pre_410.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../bugs_pre_410.odocl}))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_pre_410.tex Bugs_pre_410.tex.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.targets.gen
    (run odoc latex-targets -o . %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff bugs_pre_410.targets bugs_pre_410.targets.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (targets Bugs_pre_410.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../bugs_pre_410.odocl}))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Bugs_pre_410.3o Bugs_pre_410.3o.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs_pre_410.targets.gen
    (run odoc man-targets -o . %{dep:../bugs_pre_410.odocl})))
  (enabled_if
   (<= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff bugs_pre_410.targets bugs_pre_410.targets.gen))
  (enabled_if
   (<= %{ocaml_version} 4.09))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../class.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Class.html Class.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-type-empty.html Class-class-type-empty.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-type-mutually.html Class-class-type-mutually.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-type-recursive.html Class-class-type-recursive.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-mutually'.html Class-class-mutually'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-recursive'.html Class-class-recursive'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Class-class-type-empty_virtual.html
    Class-class-type-empty_virtual.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-empty_virtual'.html Class-class-empty_virtual'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Class-class-type-polymorphic.html
    Class-class-type-polymorphic.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class-class-polymorphic'.html Class-class-polymorphic'.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    class.targets.gen
    (run odoc html-targets -o . %{dep:../class.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff class.targets class.targets.gen))))

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
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../class.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Class.tex Class.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.mutually'.tex Class.mutually'.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.recursive'.tex Class.recursive'.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.empty_virtual'.tex Class.empty_virtual'.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.polymorphic'.tex Class.polymorphic'.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    class.targets.gen
    (run odoc latex-targets -o . %{dep:../class.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff class.targets class.targets.gen))))

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
   (run odoc man-generate -o . --extra-suffix gen %{dep:../class.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Class.3o Class.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.mutually'.3o Class.mutually'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.recursive'.3o Class.recursive'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.empty_virtual'.3o Class.empty_virtual'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Class.polymorphic'.3o Class.polymorphic'.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    class.targets.gen
    (run odoc man-targets -o . %{dep:../class.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff class.targets class.targets.gen))))

(subdir
 html
 (rule
  (targets
   Class_comments.html.gen
   Class_comments-class-x.html.gen
   Class_comments-class-c.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../class_comments.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.html Class_comments.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments-class-x.html Class_comments-class-x.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments-class-c.html Class_comments-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    class_comments.targets.gen
    (run odoc html-targets -o . %{dep:../class_comments.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff class_comments.targets class_comments.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 latex
 (rule
  (targets
   Class_comments.tex.gen
   Class_comments.x.tex.gen
   Class_comments.c.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../class_comments.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.tex Class_comments.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.x.tex Class_comments.x.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.c.tex Class_comments.c.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    class_comments.targets.gen
    (run odoc latex-targets -o . %{dep:../class_comments.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff class_comments.targets class_comments.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 man
 (rule
  (targets
   Class_comments.3o.gen
   Class_comments.x.3o.gen
   Class_comments.c.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../class_comments.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.3o Class_comments.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.x.3o Class_comments.x.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff Class_comments.c.3o Class_comments.c.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    class_comments.targets.gen
    (run odoc man-targets -o . %{dep:../class_comments.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.08)))
 (rule
  (alias runtest)
  (action
   (diff class_comments.targets class_comments.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.08))))

(subdir
 html
 (rule
  (targets External.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../external.odocl})))
 (rule
  (alias runtest)
  (action
   (diff External.html External.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    external.targets.gen
    (run odoc html-targets -o . %{dep:../external.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff external.targets external.targets.gen))))

(subdir
 latex
 (rule
  (targets External.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../external.odocl})))
 (rule
  (alias runtest)
  (action
   (diff External.tex External.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    external.targets.gen
    (run odoc latex-targets -o . %{dep:../external.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff external.targets external.targets.gen))))

(subdir
 man
 (rule
  (targets External.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../external.odocl})))
 (rule
  (alias runtest)
  (action
   (diff External.3o External.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    external.targets.gen
    (run odoc man-targets -o . %{dep:../external.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff external.targets external.targets.gen))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../functor.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor.html Functor.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-module-type-S.html Functor-module-type-S.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-module-type-S1.html Functor-module-type-S1.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Functor-module-type-S1-argument-1-_.html
    Functor-module-type-S1-argument-1-_.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F1.html Functor-F1.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F1-argument-1-Arg.html Functor-F1-argument-1-Arg.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F2.html Functor-F2.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F2-argument-1-Arg.html Functor-F2-argument-1-Arg.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F3.html Functor-F3.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F3-argument-1-Arg.html Functor-F3-argument-1-Arg.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F4.html Functor-F4.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F4-argument-1-Arg.html Functor-F4-argument-1-Arg.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor-F5.html Functor-F5.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    functor.targets.gen
    (run odoc html-targets -o . %{dep:../functor.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff functor.targets functor.targets.gen))))

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
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../functor.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor.tex Functor.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F1.tex Functor.F1.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F2.tex Functor.F2.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F3.tex Functor.F3.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F4.tex Functor.F4.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F5.tex Functor.F5.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    functor.targets.gen
    (run odoc latex-targets -o . %{dep:../functor.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff functor.targets functor.targets.gen))))

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
   (run odoc man-generate -o . --extra-suffix gen %{dep:../functor.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor.3o Functor.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F1.3o Functor.F1.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F2.3o Functor.F2.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F3.3o Functor.F3.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F4.3o Functor.F4.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor.F5.3o Functor.F5.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    functor.targets.gen
    (run odoc man-targets -o . %{dep:../functor.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff functor.targets functor.targets.gen))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../functor2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor2.html Functor2.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2-module-type-S.html Functor2-module-type-S.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2-X.html Functor2-X.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2-X-argument-1-Y.html Functor2-X-argument-1-Y.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2-X-argument-2-Z.html Functor2-X-argument-2-Z.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2-module-type-XF.html Functor2-module-type-XF.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Functor2-module-type-XF-argument-1-Y.html
    Functor2-module-type-XF-argument-1-Y.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Functor2-module-type-XF-argument-2-Z.html
    Functor2-module-type-XF-argument-2-Z.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    functor2.targets.gen
    (run odoc html-targets -o . %{dep:../functor2.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff functor2.targets functor2.targets.gen))))

(subdir
 latex
 (rule
  (targets Functor2.tex.gen Functor2.X.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../functor2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor2.tex Functor2.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2.X.tex Functor2.X.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    functor2.targets.gen
    (run odoc latex-targets -o . %{dep:../functor2.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff functor2.targets functor2.targets.gen))))

(subdir
 man
 (rule
  (targets Functor2.3o.gen Functor2.X.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../functor2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Functor2.3o Functor2.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Functor2.X.3o Functor2.X.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    functor2.targets.gen
    (run odoc man-targets -o . %{dep:../functor2.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff functor2.targets functor2.targets.gen))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../include.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include.html Include.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Not_inlined.html
    Include-module-type-Not_inlined.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Inlined.html
    Include-module-type-Inlined.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Not_inlined_and_closed.html
    Include-module-type-Not_inlined_and_closed.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Not_inlined_and_opened.html
    Include-module-type-Not_inlined_and_opened.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Inherent_Module.html
    Include-module-type-Inherent_Module.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include-module-type-Dorminant_Module.html
    Include-module-type-Dorminant_Module.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include.targets.gen
    (run odoc html-targets -o . %{dep:../include.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff include.targets include.targets.gen))))

(subdir
 latex
 (rule
  (targets Include.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../include.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include.tex Include.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include.targets.gen
    (run odoc latex-targets -o . %{dep:../include.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include.targets include.targets.gen))))

(subdir
 man
 (rule
  (targets Include.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../include.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include.3o Include.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include.targets.gen
    (run odoc man-targets -o . %{dep:../include.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include.targets include.targets.gen))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../include2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include2.html Include2.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2-X.html Include2-X.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2-Y.html Include2-Y.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include2-Y_include_synopsis.html
    Include2-Y_include_synopsis.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2-Y_include_doc.html Include2-Y_include_doc.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include2.targets.gen
    (run odoc html-targets -o . %{dep:../include2.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff include2.targets include2.targets.gen))))

(subdir
 latex
 (rule
  (targets Include2.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../include2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include2.tex Include2.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include2.targets.gen
    (run odoc latex-targets -o . %{dep:../include2.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include2.targets include2.targets.gen))))

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
   (run odoc man-generate -o . --extra-suffix gen %{dep:../include2.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include2.3o Include2.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2.X.3o Include2.X.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2.Y.3o Include2.Y.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2.Y_include_synopsis.3o Include2.Y_include_synopsis.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Include2.Y_include_doc.3o Include2.Y_include_doc.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include2.targets.gen
    (run odoc man-targets -o . %{dep:../include2.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include2.targets include2.targets.gen))))

(subdir
 html
 (rule
  (targets
   Include_sections.html.gen
   Include_sections-module-type-Something.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../include_sections.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include_sections.html Include_sections.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Include_sections-module-type-Something.html
    Include_sections-module-type-Something.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    include_sections.targets.gen
    (run odoc html-targets -o . %{dep:../include_sections.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff include_sections.targets include_sections.targets.gen))))

(subdir
 latex
 (rule
  (targets Include_sections.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../include_sections.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include_sections.tex Include_sections.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    include_sections.targets.gen
    (run odoc latex-targets -o . %{dep:../include_sections.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include_sections.targets include_sections.targets.gen))))

(subdir
 man
 (rule
  (targets Include_sections.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../include_sections.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Include_sections.3o Include_sections.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    include_sections.targets.gen
    (run odoc man-targets -o . %{dep:../include_sections.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff include_sections.targets include_sections.targets.gen))))

(subdir
 html
 (rule
  (targets Interlude.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../interlude.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Interlude.html Interlude.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    interlude.targets.gen
    (run odoc html-targets -o . %{dep:../interlude.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff interlude.targets interlude.targets.gen))))

(subdir
 latex
 (rule
  (targets Interlude.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../interlude.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Interlude.tex Interlude.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    interlude.targets.gen
    (run odoc latex-targets -o . %{dep:../interlude.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff interlude.targets interlude.targets.gen))))

(subdir
 man
 (rule
  (targets Interlude.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../interlude.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Interlude.3o Interlude.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    interlude.targets.gen
    (run odoc man-targets -o . %{dep:../interlude.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff interlude.targets interlude.targets.gen))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../labels.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.html Labels.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels-A.html Labels-A.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels-module-type-S.html Labels-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels-class-c.html Labels-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels-class-type-cs.html Labels-class-type-cs.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    labels.targets.gen
    (run odoc html-targets -o . %{dep:../labels.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff labels.targets labels.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (targets Labels.tex.gen Labels.c.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../labels.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.tex Labels.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.c.tex Labels.c.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    labels.targets.gen
    (run odoc latex-targets -o . %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff labels.targets labels.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (targets Labels.3o.gen Labels.A.3o.gen Labels.c.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../labels.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.3o Labels.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.A.3o Labels.A.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Labels.c.3o Labels.c.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    labels.targets.gen
    (run odoc man-targets -o . %{dep:../labels.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff labels.targets labels.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (targets Markup.html.gen Markup-X.html.gen Markup-Y.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../markup.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Markup.html Markup.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Markup-X.html Markup-X.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Markup-Y.html Markup-Y.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    markup.targets.gen
    (run odoc html-targets -o . %{dep:../markup.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff markup.targets markup.targets.gen))))

(subdir
 latex
 (rule
  (targets Markup.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../markup.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Markup.tex Markup.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    markup.targets.gen
    (run odoc latex-targets -o . %{dep:../markup.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff markup.targets markup.targets.gen))))

(subdir
 man
 (rule
  (targets Markup.3o.gen Markup.X.3o.gen Markup.Y.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../markup.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Markup.3o Markup.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Markup.X.3o Markup.X.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Markup.Y.3o Markup.Y.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    markup.targets.gen
    (run odoc man-targets -o . %{dep:../markup.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff markup.targets markup.targets.gen))))

(subdir
 html
 (rule
  (targets mld.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../page-mld.odocl})))
 (rule
  (alias runtest)
  (action
   (diff mld.html mld.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    page-mld.targets.gen
    (run odoc html-targets -o . %{dep:../page-mld.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff page-mld.targets page-mld.targets.gen))))

(subdir
 latex
 (rule
  (targets mld.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../page-mld.odocl})))
 (rule
  (alias runtest)
  (action
   (diff mld.tex mld.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    page-mld.targets.gen
    (run odoc latex-targets -o . %{dep:../page-mld.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff page-mld.targets page-mld.targets.gen))))

(subdir
 man
 (rule
  (targets mld.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../page-mld.odocl})))
 (rule
  (alias runtest)
  (action
   (diff mld.3o mld.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    page-mld.targets.gen
    (run odoc man-targets -o . %{dep:../page-mld.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff page-mld.targets page-mld.targets.gen))))

(subdir
 html
 (rule
  (targets
   Module.html.gen
   Module-module-type-S.html.gen
   Module-module-type-S-M.html.gen
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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../module.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module.html Module.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S.html Module-module-type-S.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S-M.html Module-module-type-S-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S3.html Module-module-type-S3.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S3-M.html Module-module-type-S3-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S4.html Module-module-type-S4.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S4-M.html Module-module-type-S4-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S5.html Module-module-type-S5.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S5-M.html Module-module-type-S5-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S6.html Module-module-type-S6.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S6-M.html Module-module-type-S6-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-M'.html Module-M'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S7.html Module-module-type-S7.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S8.html Module-module-type-S8.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-module-type-S9.html Module-module-type-S9.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-Mutually.html Module-Mutually.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module-Recursive.html Module-Recursive.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    module.targets.gen
    (run odoc html-targets -o . %{dep:../module.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff module.targets module.targets.gen))))

(subdir
 latex
 (rule
  (targets Module.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../module.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module.tex Module.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    module.targets.gen
    (run odoc latex-targets -o . %{dep:../module.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff module.targets module.targets.gen))))

(subdir
 man
 (rule
  (targets
   Module.3o.gen
   Module.M'.3o.gen
   Module.Mutually.3o.gen
   Module.Recursive.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../module.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module.3o Module.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module.M'.3o Module.M'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module.Mutually.3o Module.Mutually.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Module.Recursive.3o Module.Recursive.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    module.targets.gen
    (run odoc man-targets -o . %{dep:../module.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff module.targets module.targets.gen))))

(subdir
 html
 (rule
  (targets
   Module_type_alias.html.gen
   Module_type_alias-module-type-A.html.gen
   Module_type_alias-module-type-B.html.gen
   Module_type_alias-module-type-B-argument-1-C.html.gen
   Module_type_alias-module-type-E.html.gen
   Module_type_alias-module-type-E-argument-1-F.html.gen
   Module_type_alias-module-type-E-argument-2-C.html.gen
   Module_type_alias-module-type-G.html.gen
   Module_type_alias-module-type-G-argument-1-H.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../module_type_alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module_type_alias.html Module_type_alias.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-A.html
    Module_type_alias-module-type-A.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-B.html
    Module_type_alias-module-type-B.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-B-argument-1-C.html
    Module_type_alias-module-type-B-argument-1-C.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-E.html
    Module_type_alias-module-type-E.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-E-argument-1-F.html
    Module_type_alias-module-type-E-argument-1-F.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-E-argument-2-C.html
    Module_type_alias-module-type-E-argument-2-C.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-G.html
    Module_type_alias-module-type-G.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_alias-module-type-G-argument-1-H.html
    Module_type_alias-module-type-G-argument-1-H.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    module_type_alias.targets.gen
    (run odoc html-targets -o . %{dep:../module_type_alias.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff module_type_alias.targets module_type_alias.targets.gen))))

(subdir
 latex
 (rule
  (targets Module_type_alias.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../module_type_alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module_type_alias.tex Module_type_alias.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    module_type_alias.targets.gen
    (run odoc latex-targets -o . %{dep:../module_type_alias.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff module_type_alias.targets module_type_alias.targets.gen))))

(subdir
 man
 (rule
  (targets Module_type_alias.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../module_type_alias.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Module_type_alias.3o Module_type_alias.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    module_type_alias.targets.gen
    (run odoc man-targets -o . %{dep:../module_type_alias.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff module_type_alias.targets module_type_alias.targets.gen))))

(subdir
 html
 (rule
  (targets
   Module_type_subst.html.gen
   Module_type_subst-Local.html.gen
   Module_type_subst-Local-module-type-local.html.gen
   Module_type_subst-Local-module-type-s.html.gen
   Module_type_subst-module-type-s.html.gen
   Module_type_subst-Basic.html.gen
   Module_type_subst-Basic-module-type-u.html.gen
   Module_type_subst-Basic-module-type-u-module-type-T.html.gen
   Module_type_subst-Basic-module-type-with_.html.gen
   Module_type_subst-Basic-module-type-u2.html.gen
   Module_type_subst-Basic-module-type-u2-module-type-T.html.gen
   Module_type_subst-Basic-module-type-u2-M.html.gen
   Module_type_subst-Basic-module-type-with_2.html.gen
   Module_type_subst-Basic-module-type-with_2-module-type-T.html.gen
   Module_type_subst-Basic-module-type-with_2-M.html.gen
   Module_type_subst-Basic-module-type-a.html.gen
   Module_type_subst-Basic-module-type-a-M.html.gen
   Module_type_subst-Basic-module-type-c.html.gen
   Module_type_subst-Basic-module-type-c-M.html.gen
   Module_type_subst-Nested.html.gen
   Module_type_subst-Nested-module-type-nested.html.gen
   Module_type_subst-Nested-module-type-nested-N.html.gen
   Module_type_subst-Nested-module-type-nested-N-module-type-t.html.gen
   Module_type_subst-Nested-module-type-with_.html.gen
   Module_type_subst-Nested-module-type-with_-N.html.gen
   Module_type_subst-Nested-module-type-with_subst.html.gen
   Module_type_subst-Nested-module-type-with_subst-N.html.gen
   Module_type_subst-Structural.html.gen
   Module_type_subst-Structural-module-type-u.html.gen
   Module_type_subst-Structural-module-type-u-module-type-a.html.gen
   Module_type_subst-Structural-module-type-u-module-type-a-module-type-b.html.gen
   Module_type_subst-Structural-module-type-u-module-type-a-module-type-b-module-type-c.html.gen
   Module_type_subst-Structural-module-type-w.html.gen
   Module_type_subst-Structural-module-type-w-module-type-a.html.gen
   Module_type_subst-Structural-module-type-w-module-type-a-module-type-b.html.gen
   Module_type_subst-Structural-module-type-w-module-type-a-module-type-b-module-type-c.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../module_type_subst.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.html Module_type_subst.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst-Local.html Module_type_subst-Local.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Local-module-type-local.html
    Module_type_subst-Local-module-type-local.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Local-module-type-s.html
    Module_type_subst-Local-module-type-s.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-module-type-s.html
    Module_type_subst-module-type-s.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst-Basic.html Module_type_subst-Basic.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-u.html
    Module_type_subst-Basic-module-type-u.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-u-module-type-T.html
    Module_type_subst-Basic-module-type-u-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-with_.html
    Module_type_subst-Basic-module-type-with_.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-u2.html
    Module_type_subst-Basic-module-type-u2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-u2-module-type-T.html
    Module_type_subst-Basic-module-type-u2-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-u2-M.html
    Module_type_subst-Basic-module-type-u2-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-with_2.html
    Module_type_subst-Basic-module-type-with_2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-with_2-module-type-T.html
    Module_type_subst-Basic-module-type-with_2-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-with_2-M.html
    Module_type_subst-Basic-module-type-with_2-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-a.html
    Module_type_subst-Basic-module-type-a.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-a-M.html
    Module_type_subst-Basic-module-type-a-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-c.html
    Module_type_subst-Basic-module-type-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Basic-module-type-c-M.html
    Module_type_subst-Basic-module-type-c-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst-Nested.html Module_type_subst-Nested.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-nested.html
    Module_type_subst-Nested-module-type-nested.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-nested-N.html
    Module_type_subst-Nested-module-type-nested-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-nested-N-module-type-t.html
    Module_type_subst-Nested-module-type-nested-N-module-type-t.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-with_.html
    Module_type_subst-Nested-module-type-with_.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-with_-N.html
    Module_type_subst-Nested-module-type-with_-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-with_subst.html
    Module_type_subst-Nested-module-type-with_subst.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Nested-module-type-with_subst-N.html
    Module_type_subst-Nested-module-type-with_subst-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural.html
    Module_type_subst-Structural.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-u.html
    Module_type_subst-Structural-module-type-u.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-u-module-type-a.html
    Module_type_subst-Structural-module-type-u-module-type-a.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-u-module-type-a-module-type-b.html
    Module_type_subst-Structural-module-type-u-module-type-a-module-type-b.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-u-module-type-a-module-type-b-module-type-c.html
    Module_type_subst-Structural-module-type-u-module-type-a-module-type-b-module-type-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-w.html
    Module_type_subst-Structural-module-type-w.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-w-module-type-a.html
    Module_type_subst-Structural-module-type-w-module-type-a.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-w-module-type-a-module-type-b.html
    Module_type_subst-Structural-module-type-w-module-type-a-module-type-b.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff
    Module_type_subst-Structural-module-type-w-module-type-a-module-type-b-module-type-c.html
    Module_type_subst-Structural-module-type-w-module-type-a-module-type-b-module-type-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    module_type_subst.targets.gen
    (run odoc html-targets -o . %{dep:../module_type_subst.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff module_type_subst.targets module_type_subst.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(subdir
 latex
 (rule
  (targets Module_type_subst.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../module_type_subst.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.tex Module_type_subst.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    module_type_subst.targets.gen
    (run odoc latex-targets -o . %{dep:../module_type_subst.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff module_type_subst.targets module_type_subst.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(subdir
 man
 (rule
  (targets
   Module_type_subst.3o.gen
   Module_type_subst.Local.3o.gen
   Module_type_subst.Basic.3o.gen
   Module_type_subst.Nested.3o.gen
   Module_type_subst.Structural.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../module_type_subst.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.3o Module_type_subst.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.Local.3o Module_type_subst.Local.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.Basic.3o Module_type_subst.Basic.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.Nested.3o Module_type_subst.Nested.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff Module_type_subst.Structural.3o Module_type_subst.Structural.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    module_type_subst.targets.gen
    (run odoc man-targets -o . %{dep:../module_type_subst.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.13)))
 (rule
  (alias runtest)
  (action
   (diff module_type_subst.targets module_type_subst.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.13))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../nested.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Nested.html Nested.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-X.html Nested-X.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-module-type-Y.html Nested-module-type-Y.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-F.html Nested-F.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-F-argument-1-Arg1.html Nested-F-argument-1-Arg1.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-F-argument-2-Arg2.html Nested-F-argument-2-Arg2.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-class-z.html Nested-class-z.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested-class-inherits.html Nested-class-inherits.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    nested.targets.gen
    (run odoc html-targets -o . %{dep:../nested.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff nested.targets nested.targets.gen))))

(subdir
 latex
 (rule
  (targets
   Nested.tex.gen
   Nested.F.tex.gen
   Nested.z.tex.gen
   Nested.inherits.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../nested.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Nested.tex Nested.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.F.tex Nested.F.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.z.tex Nested.z.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.inherits.tex Nested.inherits.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    nested.targets.gen
    (run odoc latex-targets -o . %{dep:../nested.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff nested.targets nested.targets.gen))))

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
   (run odoc man-generate -o . --extra-suffix gen %{dep:../nested.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Nested.3o Nested.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.X.3o Nested.X.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.F.3o Nested.F.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.z.3o Nested.z.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Nested.inherits.3o Nested.inherits.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    nested.targets.gen
    (run odoc man-targets -o . %{dep:../nested.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff nested.targets nested.targets.gen))))

(subdir
 html
 (rule
  (targets
   Ocamlary.html.gen
   Ocamlary-Empty.html.gen
   Ocamlary-module-type-Empty.html.gen
   Ocamlary-module-type-MissingComment.html.gen
   Ocamlary-module-type-EmptySig.html.gen
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
   Ocamlary-module-type-COLLECTION.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-Recollection.html.gen
   Ocamlary-Recollection-argument-1-C.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-Recollection-InnerModuleA.html.gen
   Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-MMM.html.gen
   Ocamlary-module-type-MMM-C.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-RECOLLECTION.html.gen
   Ocamlary-module-type-RecollectionModule.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-A.html.gen
   Ocamlary-module-type-A-Q.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-B.html.gen
   Ocamlary-module-type-B-Q.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-module-type-C.html.gen
   Ocamlary-module-type-C-Q.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
   Ocamlary-FunctorTypeOf.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html.gen
   Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen
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
   Ocamlary-Dep6-module-type-T-Y.html.gen
   Ocamlary-Dep6-X.html.gen
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
   Ocamlary-IncludeInclude1-IncludeInclude2_M.html.gen
   Ocamlary-module-type-IncludeInclude2.html.gen
   Ocamlary-IncludeInclude2_M.html.gen
   Ocamlary-CanonicalTest.html.gen
   Ocamlary-CanonicalTest-Base.html.gen
   Ocamlary-CanonicalTest-Base-List.html.gen
   Ocamlary-CanonicalTest-Base_Tests.html.gen
   Ocamlary-CanonicalTest-Base_Tests-C.html.gen
   Ocamlary-CanonicalTest-List_modif.html.gen
   Ocamlary-Aliases.html.gen
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
   Ocamlary-Aliases-P2-Z.html.gen
   Ocamlary-module-type-M.html.gen
   Ocamlary-M.html.gen
   Ocamlary-Only_a_module.html.gen
   Ocamlary-module-type-TypeExt.html.gen
   Ocamlary-module-type-TypeExtPruned.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../ocamlary.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.html Ocamlary.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Empty.html Ocamlary-Empty.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-Empty.html Ocamlary-module-type-Empty.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-MissingComment.html
    Ocamlary-module-type-MissingComment.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-EmptySig.html
    Ocamlary-module-type-EmptySig.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-ModuleWithSignature.html
    Ocamlary-ModuleWithSignature.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-ModuleWithSignatureAlias.html
    Ocamlary-ModuleWithSignatureAlias.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-One.html Ocamlary-One.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SigForMod.html
    Ocamlary-module-type-SigForMod.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SigForMod-Inner.html
    Ocamlary-module-type-SigForMod-Inner.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SigForMod-Inner-module-type-Empty.html
    Ocamlary-module-type-SigForMod-Inner-module-type-Empty.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig.html
    Ocamlary-module-type-SuperSig.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-SubSigA.html
    Ocamlary-module-type-SuperSig-module-type-SubSigA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-SubSigA-SubSigAMod.html
    Ocamlary-module-type-SuperSig-module-type-SubSigA-SubSigAMod.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-SubSigB.html
    Ocamlary-module-type-SuperSig-module-type-SubSigB.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-EmptySig.html
    Ocamlary-module-type-SuperSig-module-type-EmptySig.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-One.html
    Ocamlary-module-type-SuperSig-module-type-One.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-SuperSig-module-type-SuperSig.html
    Ocamlary-module-type-SuperSig-module-type-SuperSig.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Buffer.html Ocamlary-Buffer.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-CollectionModule.html Ocamlary-CollectionModule.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CollectionModule-InnerModuleA.html
    Ocamlary-CollectionModule-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CollectionModule-InnerModuleA-InnerModuleA'.html
    Ocamlary-CollectionModule-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-CollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-COLLECTION.html
    Ocamlary-module-type-COLLECTION.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-COLLECTION-InnerModuleA.html
    Ocamlary-module-type-COLLECTION-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-COLLECTION-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-COLLECTION-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Recollection.html Ocamlary-Recollection.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-argument-1-C.html
    Ocamlary-Recollection-argument-1-C.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-argument-1-C-InnerModuleA.html
    Ocamlary-Recollection-argument-1-C-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html
    Ocamlary-Recollection-argument-1-C-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-Recollection-argument-1-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-InnerModuleA.html
    Ocamlary-Recollection-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html
    Ocamlary-Recollection-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-Recollection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-MMM.html Ocamlary-module-type-MMM.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-MMM-C.html Ocamlary-module-type-MMM-C.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-MMM-C-InnerModuleA.html
    Ocamlary-module-type-MMM-C-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-MMM-C-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-MMM-C-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-RECOLLECTION.html
    Ocamlary-module-type-RECOLLECTION.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-RecollectionModule.html
    Ocamlary-module-type-RecollectionModule.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-RecollectionModule-InnerModuleA.html
    Ocamlary-module-type-RecollectionModule-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-RecollectionModule-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-RecollectionModule-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-A.html Ocamlary-module-type-A.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-A-Q.html Ocamlary-module-type-A-Q.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-A-Q-InnerModuleA.html
    Ocamlary-module-type-A-Q-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-A-Q-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-A-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-B.html Ocamlary-module-type-B.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-B-Q.html Ocamlary-module-type-B-Q.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-B-Q-InnerModuleA.html
    Ocamlary-module-type-B-Q-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-B-Q-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-B-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-C.html Ocamlary-module-type-C.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-C-Q.html Ocamlary-module-type-C-Q.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-C-Q-InnerModuleA.html
    Ocamlary-module-type-C-Q-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html
    Ocamlary-module-type-C-Q-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-module-type-C-Q-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-FunctorTypeOf.html Ocamlary-FunctorTypeOf.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-FunctorTypeOf-argument-1-Collection.html
    Ocamlary-FunctorTypeOf-argument-1-Collection.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-InnerModuleA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html
    Ocamlary-FunctorTypeOf-argument-1-Collection-InnerModuleA-module-type-InnerModuleTypeA'.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-IncludeModuleType.html
    Ocamlary-module-type-IncludeModuleType.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-ToInclude.html
    Ocamlary-module-type-ToInclude.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-ToInclude-IncludedA.html
    Ocamlary-module-type-ToInclude-IncludedA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-ToInclude-module-type-IncludedB.html
    Ocamlary-module-type-ToInclude-module-type-IncludedB.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-IncludedA.html Ocamlary-IncludedA.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-IncludedB.html
    Ocamlary-module-type-IncludedB.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-ExtMod.html Ocamlary-ExtMod.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-class-empty_class.html Ocamlary-class-empty_class.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-class-one_method_class.html
    Ocamlary-class-one_method_class.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-class-two_method_class.html
    Ocamlary-class-two_method_class.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-class-param_class.html Ocamlary-class-param_class.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep1.html Ocamlary-Dep1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep1-module-type-S.html
    Ocamlary-Dep1-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep1-module-type-S-class-c.html
    Ocamlary-Dep1-module-type-S-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep1-X.html Ocamlary-Dep1-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep1-X-Y.html Ocamlary-Dep1-X-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep1-X-Y-class-c.html Ocamlary-Dep1-X-Y-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep2.html Ocamlary-Dep2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep2-argument-1-Arg.html
    Ocamlary-Dep2-argument-1-Arg.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep2-argument-1-Arg-X.html
    Ocamlary-Dep2-argument-1-Arg-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep2-A.html Ocamlary-Dep2-A.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep3.html Ocamlary-Dep3.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep4.html Ocamlary-Dep4.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep4-module-type-T.html
    Ocamlary-Dep4-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep4-module-type-S.html
    Ocamlary-Dep4-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep4-module-type-S-X.html
    Ocamlary-Dep4-module-type-S-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep4-module-type-S-Y.html
    Ocamlary-Dep4-module-type-S-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep4-X.html Ocamlary-Dep4-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep5.html Ocamlary-Dep5.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep5-argument-1-Arg.html
    Ocamlary-Dep5-argument-1-Arg.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep5-argument-1-Arg-module-type-S.html
    Ocamlary-Dep5-argument-1-Arg-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep5-argument-1-Arg-module-type-S-Y.html
    Ocamlary-Dep5-argument-1-Arg-module-type-S-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep5-Z.html Ocamlary-Dep5-Z.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep6.html Ocamlary-Dep6.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep6-module-type-S.html
    Ocamlary-Dep6-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep6-module-type-T.html
    Ocamlary-Dep6-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep6-module-type-T-Y.html
    Ocamlary-Dep6-module-type-T-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep6-X.html Ocamlary-Dep6-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep6-X-Y.html Ocamlary-Dep6-X-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep7.html Ocamlary-Dep7.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep7-argument-1-Arg.html
    Ocamlary-Dep7-argument-1-Arg.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep7-argument-1-Arg-module-type-T.html
    Ocamlary-Dep7-argument-1-Arg-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep7-argument-1-Arg-X.html
    Ocamlary-Dep7-argument-1-Arg-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep7-M.html Ocamlary-Dep7-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep8.html Ocamlary-Dep8.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep8-module-type-T.html
    Ocamlary-Dep8-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep9.html Ocamlary-Dep9.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep9-argument-1-X.html Ocamlary-Dep9-argument-1-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-Dep10.html Ocamlary-module-type-Dep10.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep11.html Ocamlary-Dep11.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep11-module-type-S.html
    Ocamlary-Dep11-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep11-module-type-S-class-c.html
    Ocamlary-Dep11-module-type-S-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep12.html Ocamlary-Dep12.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-Dep12-argument-1-Arg.html
    Ocamlary-Dep12-argument-1-Arg.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep13.html Ocamlary-Dep13.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Dep13-class-c.html Ocamlary-Dep13-class-c.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-With1.html Ocamlary-module-type-With1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-With1-M.html
    Ocamlary-module-type-With1-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With2.html Ocamlary-With2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With2-module-type-S.html
    Ocamlary-With2-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With3.html Ocamlary-With3.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With3-N.html Ocamlary-With3-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With4.html Ocamlary-With4.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With4-N.html Ocamlary-With4-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With5.html Ocamlary-With5.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With5-module-type-S.html
    Ocamlary-With5-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With5-N.html Ocamlary-With5-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With6.html Ocamlary-With6.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With6-module-type-T.html
    Ocamlary-With6-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With6-module-type-T-M.html
    Ocamlary-With6-module-type-T-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With7.html Ocamlary-With7.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With7-argument-1-X.html
    Ocamlary-With7-argument-1-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-With8.html Ocamlary-module-type-With8.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-With8-M.html
    Ocamlary-module-type-With8-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-With8-M-N.html
    Ocamlary-module-type-With8-M-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With9.html Ocamlary-With9.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With9-module-type-S.html
    Ocamlary-With9-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-With10.html Ocamlary-With10.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With10-module-type-T.html
    Ocamlary-With10-module-type-T.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-With10-module-type-T-M.html
    Ocamlary-With10-module-type-T-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-With11.html
    Ocamlary-module-type-With11.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-With11-N.html
    Ocamlary-module-type-With11-N.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-NestedInclude1.html
    Ocamlary-module-type-NestedInclude1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-NestedInclude1-module-type-NestedInclude2.html
    Ocamlary-module-type-NestedInclude1-module-type-NestedInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-NestedInclude2.html
    Ocamlary-module-type-NestedInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-DoubleInclude1.html Ocamlary-DoubleInclude1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-DoubleInclude1-DoubleInclude2.html
    Ocamlary-DoubleInclude1-DoubleInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-DoubleInclude3.html Ocamlary-DoubleInclude3.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-DoubleInclude3-DoubleInclude2.html
    Ocamlary-DoubleInclude3-DoubleInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-IncludeInclude1.html Ocamlary-IncludeInclude1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-IncludeInclude1-module-type-IncludeInclude2.html
    Ocamlary-IncludeInclude1-module-type-IncludeInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-IncludeInclude1-IncludeInclude2_M.html
    Ocamlary-IncludeInclude1-IncludeInclude2_M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-IncludeInclude2.html
    Ocamlary-module-type-IncludeInclude2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-IncludeInclude2_M.html Ocamlary-IncludeInclude2_M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-CanonicalTest.html Ocamlary-CanonicalTest.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CanonicalTest-Base.html
    Ocamlary-CanonicalTest-Base.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CanonicalTest-Base-List.html
    Ocamlary-CanonicalTest-Base-List.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CanonicalTest-Base_Tests.html
    Ocamlary-CanonicalTest-Base_Tests.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CanonicalTest-Base_Tests-C.html
    Ocamlary-CanonicalTest-Base_Tests-C.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-CanonicalTest-List_modif.html
    Ocamlary-CanonicalTest-List_modif.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases.html Ocamlary-Aliases.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo.html Ocamlary-Aliases-Foo.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo-A.html Ocamlary-Aliases-Foo-A.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo-B.html Ocamlary-Aliases-Foo-B.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo-C.html Ocamlary-Aliases-Foo-C.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo-D.html Ocamlary-Aliases-Foo-D.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Foo-E.html Ocamlary-Aliases-Foo-E.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-Std.html Ocamlary-Aliases-Std.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-E.html Ocamlary-Aliases-E.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-P1.html Ocamlary-Aliases-P1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-P1-Y.html Ocamlary-Aliases-P1-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-P2.html Ocamlary-Aliases-P2.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Aliases-P2-Z.html Ocamlary-Aliases-P2-Z.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-module-type-M.html Ocamlary-module-type-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-M.html Ocamlary-M.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary-Only_a_module.html Ocamlary-Only_a_module.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-TypeExt.html
    Ocamlary-module-type-TypeExt.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary-module-type-TypeExtPruned.html
    Ocamlary-module-type-TypeExtPruned.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    ocamlary.targets.gen
    (run odoc html-targets -o . %{dep:../ocamlary.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff ocamlary.targets ocamlary.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

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
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../ocamlary.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.tex Ocamlary.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.ModuleWithSignature.tex
    Ocamlary.ModuleWithSignature.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.ModuleWithSignatureAlias.tex
    Ocamlary.ModuleWithSignatureAlias.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Recollection.tex Ocamlary.Recollection.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.FunctorTypeOf.tex Ocamlary.FunctorTypeOf.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.empty_class.tex Ocamlary.empty_class.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.one_method_class.tex Ocamlary.one_method_class.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.two_method_class.tex Ocamlary.two_method_class.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.param_class.tex Ocamlary.param_class.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep2.tex Ocamlary.Dep2.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep5.tex Ocamlary.Dep5.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep5.Z.tex Ocamlary.Dep5.Z.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep7.tex Ocamlary.Dep7.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep7.M.tex Ocamlary.Dep7.M.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep9.tex Ocamlary.Dep9.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep12.tex Ocamlary.Dep12.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep13.tex Ocamlary.Dep13.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep13.c.tex Ocamlary.Dep13.c.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With3.tex Ocamlary.With3.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With3.N.tex Ocamlary.With3.N.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With4.tex Ocamlary.With4.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With4.N.tex Ocamlary.With4.N.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With7.tex Ocamlary.With7.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    ocamlary.targets.gen
    (run odoc latex-targets -o . %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff ocamlary.targets ocamlary.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

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
   Ocamlary.IncludeInclude1.IncludeInclude2_M.3o.gen
   Ocamlary.IncludeInclude2_M.3o.gen
   Ocamlary.CanonicalTest.3o.gen
   Ocamlary.CanonicalTest.Base.3o.gen
   Ocamlary.CanonicalTest.Base.List.3o.gen
   Ocamlary.CanonicalTest.Base_Tests.3o.gen
   Ocamlary.CanonicalTest.Base_Tests.C.3o.gen
   Ocamlary.CanonicalTest.List_modif.3o.gen
   Ocamlary.Aliases.3o.gen
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
   Ocamlary.Aliases.P2.Z.3o.gen
   Ocamlary.M.3o.gen
   Ocamlary.Only_a_module.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../ocamlary.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.3o Ocamlary.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Empty.3o Ocamlary.Empty.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.ModuleWithSignature.3o Ocamlary.ModuleWithSignature.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.ModuleWithSignatureAlias.3o
    Ocamlary.ModuleWithSignatureAlias.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.One.3o Ocamlary.One.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Buffer.3o Ocamlary.Buffer.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.CollectionModule.3o Ocamlary.CollectionModule.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CollectionModule.InnerModuleA.3o
    Ocamlary.CollectionModule.InnerModuleA.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o
    Ocamlary.CollectionModule.InnerModuleA.InnerModuleA'.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Recollection.3o Ocamlary.Recollection.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.Recollection.InnerModuleA.3o
    Ocamlary.Recollection.InnerModuleA.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o
    Ocamlary.Recollection.InnerModuleA.InnerModuleA'.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.FunctorTypeOf.3o Ocamlary.FunctorTypeOf.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.IncludedA.3o Ocamlary.IncludedA.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.ExtMod.3o Ocamlary.ExtMod.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.empty_class.3o Ocamlary.empty_class.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.one_method_class.3o Ocamlary.one_method_class.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.two_method_class.3o Ocamlary.two_method_class.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.param_class.3o Ocamlary.param_class.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep1.3o Ocamlary.Dep1.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep1.X.3o Ocamlary.Dep1.X.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep1.X.Y.3o Ocamlary.Dep1.X.Y.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep1.X.Y.c.3o Ocamlary.Dep1.X.Y.c.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep2.3o Ocamlary.Dep2.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep2.A.3o Ocamlary.Dep2.A.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep3.3o Ocamlary.Dep3.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep4.3o Ocamlary.Dep4.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep4.X.3o Ocamlary.Dep4.X.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep5.3o Ocamlary.Dep5.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep5.Z.3o Ocamlary.Dep5.Z.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep6.3o Ocamlary.Dep6.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep6.X.3o Ocamlary.Dep6.X.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep6.X.Y.3o Ocamlary.Dep6.X.Y.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep7.3o Ocamlary.Dep7.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep7.M.3o Ocamlary.Dep7.M.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep8.3o Ocamlary.Dep8.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep9.3o Ocamlary.Dep9.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep11.3o Ocamlary.Dep11.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep12.3o Ocamlary.Dep12.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep13.3o Ocamlary.Dep13.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Dep13.c.3o Ocamlary.Dep13.c.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With2.3o Ocamlary.With2.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With3.3o Ocamlary.With3.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With3.N.3o Ocamlary.With3.N.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With4.3o Ocamlary.With4.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With4.N.3o Ocamlary.With4.N.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With5.3o Ocamlary.With5.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With5.N.3o Ocamlary.With5.N.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With6.3o Ocamlary.With6.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With7.3o Ocamlary.With7.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With9.3o Ocamlary.With9.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.With10.3o Ocamlary.With10.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.DoubleInclude1.3o Ocamlary.DoubleInclude1.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.DoubleInclude1.DoubleInclude2.3o
    Ocamlary.DoubleInclude1.DoubleInclude2.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.DoubleInclude3.3o Ocamlary.DoubleInclude3.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.DoubleInclude3.DoubleInclude2.3o
    Ocamlary.DoubleInclude3.DoubleInclude2.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.IncludeInclude1.3o Ocamlary.IncludeInclude1.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.IncludeInclude1.IncludeInclude2_M.3o
    Ocamlary.IncludeInclude1.IncludeInclude2_M.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.IncludeInclude2_M.3o Ocamlary.IncludeInclude2_M.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.CanonicalTest.3o Ocamlary.CanonicalTest.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.CanonicalTest.Base.3o Ocamlary.CanonicalTest.Base.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CanonicalTest.Base.List.3o
    Ocamlary.CanonicalTest.Base.List.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CanonicalTest.Base_Tests.3o
    Ocamlary.CanonicalTest.Base_Tests.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CanonicalTest.Base_Tests.C.3o
    Ocamlary.CanonicalTest.Base_Tests.C.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff
    Ocamlary.CanonicalTest.List_modif.3o
    Ocamlary.CanonicalTest.List_modif.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.3o Ocamlary.Aliases.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.3o Ocamlary.Aliases.Foo.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.A.3o Ocamlary.Aliases.Foo.A.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.B.3o Ocamlary.Aliases.Foo.B.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.C.3o Ocamlary.Aliases.Foo.C.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.D.3o Ocamlary.Aliases.Foo.D.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Foo.E.3o Ocamlary.Aliases.Foo.E.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.Std.3o Ocamlary.Aliases.Std.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.E.3o Ocamlary.Aliases.E.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.P1.3o Ocamlary.Aliases.P1.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.P1.Y.3o Ocamlary.Aliases.P1.Y.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.P2.3o Ocamlary.Aliases.P2.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Aliases.P2.Z.3o Ocamlary.Aliases.P2.Z.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.M.3o Ocamlary.M.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff Ocamlary.Only_a_module.3o Ocamlary.Only_a_module.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    ocamlary.targets.gen
    (run odoc man-targets -o . %{dep:../ocamlary.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.07)))
 (rule
  (alias runtest)
  (action
   (diff ocamlary.targets ocamlary.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.07))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../recent.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.html Recent.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-module-type-S.html Recent-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-module-type-S1.html Recent-module-type-S1.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff
    Recent-module-type-S1-argument-1-_.html
    Recent-module-type-S1-argument-1-_.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-Z.html Recent-Z.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-Z-Y.html Recent-Z-Y.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-Z-Y-X.html Recent-Z-Y-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-X.html Recent-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent-module-type-PolyS.html Recent-module-type-PolyS.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent.targets.gen
    (run odoc html-targets -o . %{dep:../recent.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent.targets recent.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (targets Recent.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../recent.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.tex Recent.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    recent.targets.gen
    (run odoc latex-targets -o . %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent.targets recent.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

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
   (run odoc man-generate -o . --extra-suffix gen %{dep:../recent.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.3o Recent.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.Z.3o Recent.Z.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.Z.Y.3o Recent.Z.Y.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.Z.Y.X.3o Recent.Z.Y.X.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent.X.3o Recent.X.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    recent.targets.gen
    (run odoc man-targets -o . %{dep:../recent.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent.targets recent.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

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
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../recent_impl.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.html Recent_impl.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl-Foo.html Recent_impl-Foo.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl-Foo-A.html Recent_impl-Foo-A.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl-Foo-B.html Recent_impl-Foo-B.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl-B.html Recent_impl-B.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl-module-type-S.html Recent_impl-module-type-S.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff
    Recent_impl-module-type-S-F.html
    Recent_impl-module-type-S-F.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff
    Recent_impl-module-type-S-F-argument-1-_.html
    Recent_impl-module-type-S-F-argument-1-_.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff
    Recent_impl-module-type-S-X.html
    Recent_impl-module-type-S-X.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent_impl.targets.gen
    (run odoc html-targets -o . %{dep:../recent_impl.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent_impl.targets recent_impl.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (targets Recent_impl.tex.gen Recent_impl.B.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../recent_impl.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.tex Recent_impl.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.B.tex Recent_impl.B.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    recent_impl.targets.gen
    (run odoc latex-targets -o . %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent_impl.targets recent_impl.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

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
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../recent_impl.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.3o Recent_impl.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.Foo.3o Recent_impl.Foo.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.Foo.A.3o Recent_impl.Foo.A.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.Foo.B.3o Recent_impl.Foo.B.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff Recent_impl.B.3o Recent_impl.B.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    recent_impl.targets.gen
    (run odoc man-targets -o . %{dep:../recent_impl.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.09)))
 (rule
  (alias runtest)
  (action
   (diff recent_impl.targets recent_impl.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.09))))

(subdir
 html
 (rule
  (targets Section.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../section.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Section.html Section.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    section.targets.gen
    (run odoc html-targets -o . %{dep:../section.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff section.targets section.targets.gen))))

(subdir
 latex
 (rule
  (targets Section.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../section.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Section.tex Section.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    section.targets.gen
    (run odoc latex-targets -o . %{dep:../section.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff section.targets section.targets.gen))))

(subdir
 man
 (rule
  (targets Section.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../section.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Section.3o Section.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    section.targets.gen
    (run odoc man-targets -o . %{dep:../section.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff section.targets section.targets.gen))))

(subdir
 html
 (rule
  (targets Stop.html.gen Stop-N.html.gen Stop-O.html.gen Stop-P.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../stop.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop.html Stop.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop-N.html Stop-N.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop-O.html Stop-O.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop-P.html Stop-P.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    stop.targets.gen
    (run odoc html-targets -o . %{dep:../stop.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff stop.targets stop.targets.gen))))

(subdir
 latex
 (rule
  (targets Stop.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../stop.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop.tex Stop.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    stop.targets.gen
    (run odoc latex-targets -o . %{dep:../stop.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff stop.targets stop.targets.gen))))

(subdir
 man
 (rule
  (targets Stop.3o.gen Stop.N.3o.gen Stop.O.3o.gen Stop.P.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../stop.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop.3o Stop.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop.N.3o Stop.N.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop.O.3o Stop.O.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Stop.P.3o Stop.P.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    stop.targets.gen
    (run odoc man-targets -o . %{dep:../stop.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff stop.targets stop.targets.gen))))

(subdir
 html
 (rule
  (targets Stop_dead_link_doc.html.gen Stop_dead_link_doc-Foo.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../stop_dead_link_doc.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff Stop_dead_link_doc.html Stop_dead_link_doc.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff Stop_dead_link_doc-Foo.html Stop_dead_link_doc-Foo.html.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.targets.gen
    (run odoc html-targets -o . %{dep:../stop_dead_link_doc.odocl} --flat)))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff stop_dead_link_doc.targets stop_dead_link_doc.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 latex
 (rule
  (targets Stop_dead_link_doc.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../stop_dead_link_doc.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff Stop_dead_link_doc.tex Stop_dead_link_doc.tex.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.targets.gen
    (run odoc latex-targets -o . %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff stop_dead_link_doc.targets stop_dead_link_doc.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 man
 (rule
  (targets Stop_dead_link_doc.3o.gen Stop_dead_link_doc.Foo.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../stop_dead_link_doc.odocl}))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff Stop_dead_link_doc.3o Stop_dead_link_doc.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff Stop_dead_link_doc.Foo.3o Stop_dead_link_doc.Foo.3o.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    stop_dead_link_doc.targets.gen
    (run odoc man-targets -o . %{dep:../stop_dead_link_doc.odocl})))
  (enabled_if
   (>= %{ocaml_version} 4.04)))
 (rule
  (alias runtest)
  (action
   (diff stop_dead_link_doc.targets stop_dead_link_doc.targets.gen))
  (enabled_if
   (>= %{ocaml_version} 4.04))))

(subdir
 html
 (rule
  (targets Stop_first_comment.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../stop_first_comment.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop_first_comment.html Stop_first_comment.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    stop_first_comment.targets.gen
    (run odoc html-targets -o . %{dep:../stop_first_comment.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff stop_first_comment.targets stop_first_comment.targets.gen))))

(subdir
 latex
 (rule
  (targets Stop_first_comment.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../stop_first_comment.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop_first_comment.tex Stop_first_comment.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    stop_first_comment.targets.gen
    (run odoc latex-targets -o . %{dep:../stop_first_comment.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff stop_first_comment.targets stop_first_comment.targets.gen))))

(subdir
 man
 (rule
  (targets Stop_first_comment.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../stop_first_comment.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Stop_first_comment.3o Stop_first_comment.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    stop_first_comment.targets.gen
    (run odoc man-targets -o . %{dep:../stop_first_comment.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff stop_first_comment.targets stop_first_comment.targets.gen))))

(subdir
 html
 (rule
  (targets Tag_link.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../tag_link.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Tag_link.html Tag_link.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    tag_link.targets.gen
    (run odoc html-targets -o . %{dep:../tag_link.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff tag_link.targets tag_link.targets.gen))))

(subdir
 latex
 (rule
  (targets Tag_link.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../tag_link.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Tag_link.tex Tag_link.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    tag_link.targets.gen
    (run odoc latex-targets -o . %{dep:../tag_link.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff tag_link.targets tag_link.targets.gen))))

(subdir
 man
 (rule
  (targets Tag_link.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../tag_link.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Tag_link.3o Tag_link.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    tag_link.targets.gen
    (run odoc man-targets -o . %{dep:../tag_link.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff tag_link.targets tag_link.targets.gen))))

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
   Toplevel_comments-Ref_in_synopsis.html.gen
   Toplevel_comments-Comments_on_open.html.gen
   Toplevel_comments-Comments_on_open-M.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../toplevel_comments.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.html Toplevel_comments.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-module-type-T.html
    Toplevel_comments-module-type-T.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-Include_inline.html
    Toplevel_comments-Include_inline.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-Include_inline'.html
    Toplevel_comments-Include_inline'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-module-type-Include_inline_T.html
    Toplevel_comments-module-type-Include_inline_T.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-module-type-Include_inline_T'.html
    Toplevel_comments-module-type-Include_inline_T'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-M.html Toplevel_comments-M.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-M'.html Toplevel_comments-M'.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-M''.html Toplevel_comments-M''.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-Alias.html Toplevel_comments-Alias.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-class-c1.html Toplevel_comments-class-c1.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-class-type-ct.html
    Toplevel_comments-class-type-ct.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments-class-c2.html Toplevel_comments-class-c2.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-Ref_in_synopsis.html
    Toplevel_comments-Ref_in_synopsis.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-Comments_on_open.html
    Toplevel_comments-Comments_on_open.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments-Comments_on_open-M.html
    Toplevel_comments-Comments_on_open-M.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    toplevel_comments.targets.gen
    (run odoc html-targets -o . %{dep:../toplevel_comments.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff toplevel_comments.targets toplevel_comments.targets.gen))))

(subdir
 latex
 (rule
  (targets
   Toplevel_comments.tex.gen
   Toplevel_comments.Alias.tex.gen
   Toplevel_comments.c1.tex.gen
   Toplevel_comments.c2.tex.gen)
  (action
   (run
    odoc
    latex-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../toplevel_comments.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.tex Toplevel_comments.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.Alias.tex Toplevel_comments.Alias.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.c1.tex Toplevel_comments.c1.tex.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.c2.tex Toplevel_comments.c2.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    toplevel_comments.targets.gen
    (run odoc latex-targets -o . %{dep:../toplevel_comments.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff toplevel_comments.targets toplevel_comments.targets.gen))))

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
   Toplevel_comments.Ref_in_synopsis.3o.gen
   Toplevel_comments.Comments_on_open.3o.gen
   Toplevel_comments.Comments_on_open.M.3o.gen)
  (action
   (run
    odoc
    man-generate
    -o
    .
    --extra-suffix
    gen
    %{dep:../toplevel_comments.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.3o Toplevel_comments.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments.Include_inline.3o
    Toplevel_comments.Include_inline.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments.Include_inline'.3o
    Toplevel_comments.Include_inline'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.M.3o Toplevel_comments.M.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.M'.3o Toplevel_comments.M'.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.M''.3o Toplevel_comments.M''.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.Alias.3o Toplevel_comments.Alias.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.c1.3o Toplevel_comments.c1.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff Toplevel_comments.c2.3o Toplevel_comments.c2.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments.Ref_in_synopsis.3o
    Toplevel_comments.Ref_in_synopsis.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments.Comments_on_open.3o
    Toplevel_comments.Comments_on_open.3o.gen)))
 (rule
  (alias runtest)
  (action
   (diff
    Toplevel_comments.Comments_on_open.M.3o
    Toplevel_comments.Comments_on_open.M.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    toplevel_comments.targets.gen
    (run odoc man-targets -o . %{dep:../toplevel_comments.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff toplevel_comments.targets toplevel_comments.targets.gen))))

(subdir
 html
 (rule
  (targets Type.html.gen Type-module-type-X.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../type.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Type.html Type.html.gen)))
 (rule
  (alias runtest)
  (action
   (diff Type-module-type-X.html Type-module-type-X.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    type.targets.gen
    (run odoc html-targets -o . %{dep:../type.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff type.targets type.targets.gen))))

(subdir
 latex
 (rule
  (targets Type.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../type.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Type.tex Type.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    type.targets.gen
    (run odoc latex-targets -o . %{dep:../type.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff type.targets type.targets.gen))))

(subdir
 man
 (rule
  (targets Type.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../type.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Type.3o Type.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    type.targets.gen
    (run odoc man-targets -o . %{dep:../type.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff type.targets type.targets.gen))))

(subdir
 html
 (rule
  (targets Val.html.gen)
  (action
   (run
    odoc
    html-generate
    --indent
    --flat
    --extra-suffix
    gen
    -o
    .
    %{dep:../val.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Val.html Val.html.gen))))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    val.targets.gen
    (run odoc html-targets -o . %{dep:../val.odocl} --flat))))
 (rule
  (alias runtest)
  (action
   (diff val.targets val.targets.gen))))

(subdir
 latex
 (rule
  (targets Val.tex.gen)
  (action
   (run odoc latex-generate -o . --extra-suffix gen %{dep:../val.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Val.tex Val.tex.gen))))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    val.targets.gen
    (run odoc latex-targets -o . %{dep:../val.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff val.targets val.targets.gen))))

(subdir
 man
 (rule
  (targets Val.3o.gen)
  (action
   (run odoc man-generate -o . --extra-suffix gen %{dep:../val.odocl})))
 (rule
  (alias runtest)
  (action
   (diff Val.3o Val.3o.gen))))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    val.targets.gen
    (run odoc man-targets -o . %{dep:../val.odocl}))))
 (rule
  (alias runtest)
  (action
   (diff val.targets val.targets.gen))))
