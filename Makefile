BUILD_SRC = _build/default/src
BUILD_DOC_HTML = _build/default/_doc/_html
DOC = doc
SRC = src
TEST = test
ALGOLIB_CMXA = algolib.cmxa
ALGOLIB_CMA = algolib.cma

.PHONY : all clean compile doc format refresh test

all : format compile test

clean :
	rm -fr $(ALGOLIB_CMXA) $(ALGOLIB_CMA) $(DOC)
	dune clean

refresh : clean all

compile :
	dune build
	ln -sfn $(BUILD_SRC)/$(ALGOLIB_CMXA)
	ln -sfn $(BUILD_SRC)/$(ALGOLIB_CMA)

test :
	dune runtest

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
	for F in $$(find $(TEST) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done

doc :
	dune build @doc
	ln -sfn $(BUILD_DOC_HTML) $(DOC)
