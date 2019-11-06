BUILD_LIB = _build/install/default/lib/algolib
BUILD_DOC_HTML = _build/default/_doc/_html

DIST = dist
DOC = doc
SRC = src
TEST = test

CMXA_DIST = algolib.cmxa
CMA_DIST = algolib.cma

.PHONY : all build clean compile doc format refresh test

all : compile test

clean :
	rm -fr $(DIST) $(DOC)
	dune clean

refresh : clean all

build : format all

compile :
	dune build
	mkdir -p $(DIST)
	ln -sfn ../$(BUILD_LIB)/$(CMXA_DIST) $(DIST)/$(CMXA_DIST)
	ln -sfn ../$(BUILD_LIB)/$(CMA_DIST) $(DIST)/$(CMA_DIST)

test :
	dune runtest

format :
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?') ;\
	  do ocamlformat -i $$F ; ocp-indent -i $$F ; done
	for F in $$(find $(TEST) -regextype egrep -regex '.+\.mli?') ;\
	  do ocamlformat -i $$F ; ocp-indent -i $$F ; done

doc :
	dune build @doc
	ln -sfn $(BUILD_DOC_HTML) $(DOC)
