OCB = ocamlbuild
OCBFLAGS = -classic-display
BUILD = $(OCB) $(OCBFLAGS)

.PHONY : all clean refresh docs

all : algolib.cma algolib.cmxa

clean :
	$(OCB) -clean

refresh : clean all

docs :
	$(OCB) algolib.docdir/index.html

algolib.cma :
	$(BUILD) algolib.cma

algolib.cmxa :
	$(BUILD) algolib.cmxa
