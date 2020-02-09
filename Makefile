BUILD_SRC = _build/default/src
BUILD_DOC_HTML = _build/default/_doc/_html

DIST = dist
DOC = doc
SRC = src
TEST = test

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
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
	find $(TEST) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;

doc :
	dune build @doc
	cp -r $(BUILD_DOC_HTML) $(DOC)
