ifeq "$(wildcard dune-local/dune.exe)" ""
  DUNE = dune
	DUNE_BUILD_TARGET =
else
  DUNE = dune-local/dune.exe
	DUNE_BUILD_TARGET = odoc.install
endif
DUNE_ARGS ?=

.PHONY : build
build :
	$(DUNE) build $(DUNE_ARGS) $(DUNE_BUILD_TARGET)

.PHONY : publish-docs
publish-docs:
	dune build @doc
	dune build @docgen || true
	git checkout gh-pages
	rsync -av _build/default/doc/html/odoc/ .

.PHONY : test
test : build
	$(DUNE) runtest $(DUNE_ARGS)

.PHONY : coverage
coverage :
	$(DUNE) build $(DUNE_ARGS) --instrument-with bisect_ppx @test/runtest --no-buffer -j 1 --force || true
	bisect-ppx-report html
	@echo See _coverage/index.html

.PHONY : clean
clean :
	$(DUNE) clean
	rm -rf $(COVERAGE)

DUNIVERSE_DEPS = astring cmdliner cppo fmt fpath ocaml-re result tyxml uutf
ASTRING_VERSION = v0.8.4
CMDLINER_VERSION = v1.0.4
CPPO_VERSION = v1.6.6
DUNE_VERSION = 2.6.2
FMT_VERSION = v0.8.8
FPATH_VERSION = v0.7.2
OCAML_RE_VERSION = 1.9.0
RESULT_VERSION = 1.5
TYXML_VERSION = 4.4.0
UUTF_VERSION = v1.0.2

GIT_CHECKOUT = git -c advice.detachedHead=false checkout

.PHONY : duniverse
duniverse : $(addsuffix /dune-project,$(DUNIVERSE_DEPS)) dune-local/dune.exe

dune-local/dune.exe : dune-local
	cd dune-local ; ocaml bootstrap.ml

astring :
	git clone https://github.com/dbuenzli/astring.git -n -o upstream
	cd astring ; $(GIT_CHECKOUT) $(ASTRING_VERSION)

cmdliner :
	git clone https://github.com/dbuenzli/cmdliner.git -n -o upstream
	cd cmdliner ; $(GIT_CHECKOUT) $(CMDLINER_VERSION)

cppo :
	git clone https://github.com/ocaml-community/cppo.git -n -o upstream
	cd cppo ; $(GIT_CHECKOUT) $(CPPO_VERSION)

dune-local :
	git clone https://github.com/ocaml/dune.git -n -o upstream dune-local
	cd dune-local ; $(GIT_CHECKOUT) $(DUNE_VERSION)

fmt :
	git clone https://github.com/dbuenzli/fmt.git -n -o upstream
	cd fmt ; $(GIT_CHECKOUT) $(FMT_VERSION)

fpath :
	git clone https://github.com/dbuenzli/fpath.git -n -o upstream
	cd fpath ; $(GIT_CHECKOUT) $(FPATH_VERSION)

ocaml-re :
	git clone https://github.com/ocaml/ocaml-re.git -n -o upstream
	cd ocaml-re ; $(GIT_CHECKOUT) $(OCAML_RE_VERSION)

result :
	git clone https://github.com/janestreet/result.git -n -o upstream
	cd result ; $(GIT_CHECKOUT) $(RESULT_VERSION)

tyxml :
	git clone https://github.com/ocsigen/tyxml.git -n -o upstream
	cd tyxml ; $(GIT_CHECKOUT) $(TYXML_VERSION)

uutf :
	git clone https://github.com/dbuenzli/uutf.git -n -o upstream
	cd uutf ; $(GIT_CHECKOUT) $(UUTF_VERSION)

astring/dune-project : astring
	mv $</opam $</$<.opam
	echo '(lang dune 1.0)' > $@; \
  echo '(name $<)' >> $@; \
  echo '(library (name $<)(public_name $<)(modules :standard \ astring_top astring_top_init)(flags :standard -w -27)(wrapped false))' > $</src/dune

cmdliner/dune-project: cmdliner

cppo/dune-project: cppo

fmt/dune-project : fmt
	mv $</opam $</$<.opam
	echo '(lang dune 1.0)' > $@; \
  echo '(name $<)' >> $@; \
  echo '(library (name $<)(public_name $<)(modules fmt)(flags :standard -w -3-6-27-34-50)(wrapped false))' > $</src/dune

fpath/dune-project : fpath
	mv $</opam $</$<.opam
	echo '(lang dune 1.0)' > $@; \
  echo '(name $<)' >> $@; \
  echo '(library (name $<)(public_name $<)(libraries astring result)(modules fpath)(flags :standard -w -6-27-32-33-39)(wrapped false))' > $</src/dune

ocaml-re/dune-project: ocaml-re

result/dune-project: result

tyxml/dune-project: tyxml

uutf/dune-project : uutf
	mv $</opam $</$<.opam
	echo '(lang dune 1.0)' > $@; \
  echo '(name $<)' >> $@; \
  echo '(library (name $<)(public_name $<)(flags (:standard -w -3-27))(wrapped false))' > $</src/dune

.PHONY : distclean
distclean :
	rm -rf $(DUNIVERSE_DEPS) dune-local
