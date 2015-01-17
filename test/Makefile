
.PHONY: all coverage-cmi coverage-cmti

OCAMLC ?= ocamlc -g
DOC_OCK_BUILD ?= ../_build/lib-doc-ock
DOC_OCK_FLAGS ?= \
   -I  $(DOC_OCK_BUILD) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r compiler-libs.common) \
   $(DOC_OCK_BUILD)/doc-ock.cma

all: coverage-cmi coverage-cmti

coverage-cmi: coverage.cmi testCmi
	./testCmi coverage.cmi

coverage-cmti: coverage.cmti testCmti
	./testCmti coverage.cmti

coverage.cmi coverage.cmti: coverage.mli
	$(OCAMLC) -bin-annot -doc -c coverage.mli

coverage.cmt: coverage.ml
	$(OCAMLC) -bin-annot -doc -c coverage.ml

testCmi: testCommon.ml testCmi.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $^ -o $@

testCmti: testCommon.ml testCmti.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $^ -o $@

clean:
	rm -f testCmi
	rm -f testCmti
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmti
	rm -f *.cmt
