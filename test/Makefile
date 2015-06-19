
.PHONY: all coverage-cmi coverage-cmti coverage-cmt

OCAMLC ?= ocamlc -g
DOC_OCK_BUILD ?= ../_build/lib-doc-ock
DOC_OCK_FLAGS ?= \
   -I  $(DOC_OCK_BUILD) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r compiler-libs.common) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r octavius) \
   $(DOC_OCK_BUILD)/doc-ock.cma

all: coverage-cmi coverage-cmti coverage-cmt

coverage-cmi: coverage.cmi testCmi
	./testCmi coverage.cmi

coverage-cmti: coverage.cmti testCmti
	./testCmti coverage.cmti

coverage-cmt: coverage.cmt testCmt
	./testCmt coverage.cmt

coverage.cmi coverage.cmti: coverage.mli
	$(OCAMLC) -bin-annot -c coverage.mli

coverage.cmt: coverage.ml
	$(OCAMLC) -bin-annot -c coverage.ml

testCmi: testCommon.ml testCmi.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $^ -o $@

testCmti: testCommon.ml testCmti.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $^ -o $@

testCmt: testCommon.ml testCmt.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $^ -o $@

clean:
	rm -f testCmi
	rm -f testCmti
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmti
	rm -f *.cmt
