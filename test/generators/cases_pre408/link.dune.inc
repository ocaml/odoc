(rule
 (target alias.cmt)
 (deps cases/alias.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target alias.odoc)
 (deps alias.cmt)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target bugs.odocl)
 (deps bugs.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target bugs_pre_410.cmt)
 (deps cases/bugs_pre_410.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target bugs_pre_410.odoc)
 (deps bugs_pre_410.cmt)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target bugs_pre_410.odocl)
 (deps bugs_pre_410.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target class.cmti)
 (deps cases/class.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target class.odoc)
 (deps class.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target interlude.odocl)
 (deps interlude.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target markup.cmti)
 (deps cases/markup.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target markup.odoc)
 (deps markup.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target markup.odocl)
 (deps markup.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target page-mld.odoc)
 (deps cases/mld.mld)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target module.odocl)
 (deps module.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target nested.cmti)
 (deps cases/nested.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target nested.odoc)
 (deps nested.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target nested.odocl)
 (deps nested.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target ocamlary.cmti)
 (deps cases/ocamlary.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target ocamlary.odoc)
 (deps ocamlary.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target ocamlary.odocl)
 (deps ocamlary.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target section.cmti)
 (deps cases/section.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target section.odoc)
 (deps section.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target stop.odocl)
 (deps stop.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target stop_dead_link_doc.cmti)
 (deps cases/stop_dead_link_doc.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target stop_dead_link_doc.odoc)
 (deps stop_dead_link_doc.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target stop_dead_link_doc.odocl)
 (deps stop_dead_link_doc.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target toplevel_comments.cmti)
 (deps cases/toplevel_comments.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target toplevel_comments.odoc)
 (deps toplevel_comments.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

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
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target val.odocl)
 (deps val.odoc)
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
     %{dep:../alias.odocl}
     %{dep:../bugs.odocl}
     %{dep:../bugs_pre_410.odocl}
     %{dep:../class.odocl}
     %{dep:../external.odocl}
     %{dep:../functor.odocl}
     %{dep:../functor2.odocl}
     %{dep:../include.odocl}
     %{dep:../include2.odocl}
     %{dep:../include_sections.odocl}
     %{dep:../interlude.odocl}
     %{dep:../markup.odocl}
     %{dep:../page-mld.odocl}
     %{dep:../module.odocl}
     %{dep:../nested.odocl}
     %{dep:../ocamlary.odocl}
     %{dep:../section.odocl}
     %{dep:../stop.odocl}
     %{dep:../stop_dead_link_doc.odocl}
     %{dep:../toplevel_comments.odocl}
     %{dep:../type.odocl}
     %{dep:../val.odocl})
    (run dune format-dune-file)))))

(subdir
 latex
 (rule
  (with-stdout-to
   latex.dune.inc.gen
   (pipe-stdout
    (run
     gen_latex/gen_latex.exe
     %{dep:../alias.odocl}
     %{dep:../bugs.odocl}
     %{dep:../bugs_pre_410.odocl}
     %{dep:../class.odocl}
     %{dep:../external.odocl}
     %{dep:../functor.odocl}
     %{dep:../functor2.odocl}
     %{dep:../include.odocl}
     %{dep:../include2.odocl}
     %{dep:../include_sections.odocl}
     %{dep:../interlude.odocl}
     %{dep:../markup.odocl}
     %{dep:../page-mld.odocl}
     %{dep:../module.odocl}
     %{dep:../nested.odocl}
     %{dep:../ocamlary.odocl}
     %{dep:../section.odocl}
     %{dep:../stop.odocl}
     %{dep:../stop_dead_link_doc.odocl}
     %{dep:../toplevel_comments.odocl}
     %{dep:../type.odocl}
     %{dep:../val.odocl})
    (run dune format-dune-file)))))

(subdir
 man
 (rule
  (with-stdout-to
   man.dune.inc.gen
   (pipe-stdout
    (run
     gen_man/gen_man.exe
     %{dep:../alias.odocl}
     %{dep:../bugs.odocl}
     %{dep:../bugs_pre_410.odocl}
     %{dep:../class.odocl}
     %{dep:../external.odocl}
     %{dep:../functor.odocl}
     %{dep:../functor2.odocl}
     %{dep:../include.odocl}
     %{dep:../include2.odocl}
     %{dep:../include_sections.odocl}
     %{dep:../interlude.odocl}
     %{dep:../markup.odocl}
     %{dep:../page-mld.odocl}
     %{dep:../module.odocl}
     %{dep:../nested.odocl}
     %{dep:../ocamlary.odocl}
     %{dep:../section.odocl}
     %{dep:../stop.odocl}
     %{dep:../stop_dead_link_doc.odocl}
     %{dep:../toplevel_comments.odocl}
     %{dep:../type.odocl}
     %{dep:../val.odocl})
    (run dune format-dune-file)))))

(rule
 (alias runtest)
 (action
  (diff html/html.dune.inc html/html.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff latex/latex.dune.inc latex/latex.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))

(rule
 (alias runtest)
 (action
  (diff man/man.dune.inc man/man.dune.inc.gen))
 (enabled_if
  (>= %{ocaml_version} 4.10)))
