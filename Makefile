BUILD_SRC = _build/default/src
BUILD_DOC_HTML = _build/default/_doc/_html

DIST = dist
DOC = doc
SRC = src
TEST = test

SOURCES = $(wildcard $(SRC)/*.{ml,mli})
TEST_FILES = $(wildcard $(TEST)/*.{ml,mli})
CMXA_LIBRARY = algolib.cmxa
CMA_LIBRARY = algolib.cma

.PHONY : all build clean compile dirs doc format refresh test

all : compile test

clean :
	rm -fr $(DIST) $(DOC)
	dune clean

refresh : clean all

dirs :
	mkdir -p $(DIST)

build : format all

compile : dirs
	dune build
	cp $(BUILD_SRC)/$(CMXA_LIBRARY) $(DIST)/$(CMXA_LIBRARY)
	cp $(BUILD_SRC)/$(CMA_LIBRARY) $(DIST)/$(CMA_LIBRARY)

test :
	dune runtest

format :
	for F in $(SOURCES) ; do ocamlformat -i $$F ; ocp-indent -i $$F ; done
	for F in $(TEST_FILES) ; do ocamlformat -i $$F ; ocp-indent -i $$F ; done

doc :
	dune build @doc
	cp -r $(BUILD_DOC_HTML) $(DOC)
