##############################
# SHARED VARIABLES AND RULES #
##############################

ODOC = ../_build/bin/main.native

COMP_FLAGS = --pkg=$(PKG) -I $(PKG)

HTML_OUTPUT_DIR = ./html

%.html: %.odoc
	$(ODOC) html -I $(DIR) -o $(HTML_OUTPUT_DIR) $(DIR)/$<

################################
# DEFAULT VALUE FOR OUTPUT DIR #
################################

PKG = odoc
DIR = ./odoc

all: compile link
	ls $(HTML_OUTPUT_DIR) | xargs $(ODOC) listing --root-dir $(HTML_OUTPUT_DIR)

clean: clean_odoc clean_coverage
	-@rm -R $(HTML_OUTPUT_DIR)
	-@mkdir $(HTML_OUTPUT_DIR)

##########################
# TESTING ON ODOC ITSELF #
##########################

LIB  = ../_build/lib

UNITS = odoc odocFs odocRoot odocUnit odocEnv odocCompile odocHtml odocListing

HTML_FILES = $(UNITS:=.html)
ODOC_FILES = $(UNITS:=.odoc)

compile: PKG = odoc
compile: $(ODOC_FILES)

do-link: DIR = ./odoc
do-link: $(HTML_FILES)
	ls $(HTML_OUTPUT_DIR)/$(PKG) | xargs $(ODOC) listing --root-dir $(HTML_OUTPUT_DIR) --pkg $(PKG)
	@cp odoc.css $(HTML_OUTPUT_DIR)

do-link: DIR = ./odoc
link:
	# $(ODOC) compile -r $(COMP_FLAGS) $(LIB)/odoc.cmt
	$(MAKE) do-link

odoc.odoc:
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odoc.cmt

odocFs.odoc: odoc.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocFs.cmti

odocRoot.odoc: odoc.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocRoot.cmti

odocUnit.odoc: odoc.odoc odocFs.odoc odocRoot.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocUnit.cmti

odocEnv.odoc: odoc.odoc odocFs.odoc odocRoot.odoc odocUnit.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocEnv.cmti

odocCompile.odoc: odoc.odoc odocEnv.odoc odocFs.odoc odocRoot.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocCompile.cmti

odocHtml.odoc: odoc.odoc odocFs.odoc odocEnv.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocHtml.cmti

odocListing.odoc: odocFs.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocListing.cmti

clean_odoc: DIR = ./odoc
clean_odoc:
	-@rm -R $(DIR)
	-@mkdir $(DIR)

.PHONY: all clean clean_odoc clean_coverage compile link
