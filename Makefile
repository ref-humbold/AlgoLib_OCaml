BUILD_SRC = _build/default/src
BUILD_DOC = _build/default/_doc/_html
BIN = bin
SRC = src
DOC = doc
CMXA = algolib.cmxa
CMA = algolib.cma

.PHONY : all clean refresh format doc

all : format compile

clean :
	rm -fr $(CMA) $(CMXA) $(DOC)
	dune clean

refresh : clean all

compile :
	dune build
	ln -sfn $(BUILD_SRC)/$(CMXA) $(CMXA)
	ln -sfn $(BUILD_SRC)/$(CMA) $(CMA)

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done

doc :
	dune build @doc
	ln -sfn $(BUILD_DOC) $(DOC)
