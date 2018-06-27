OCB = ocamlbuild
OCBFLAGS = -classic-display
BUILD = $(OCB) $(OCBFLAGS)

.PHONY : all clean refresh

all : algolib.cma algolib.cmxa

clean :
	$(OCB) -clean

refresh : clean all

algolib.cma :
	$(BUILD) algolib.cma

algolib.cmxa :
	$(BUILD) algolib.cmxa
