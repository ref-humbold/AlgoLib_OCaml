OCB = ocamlbuild
OCBFLAGS =
BUILD = $(OCB) $(OCBFLAGS)

all : algolib.cma algolib.cmxa

clean :
	$(OCB) -clean

refresh : clean all

algolib.cma :
	$(BUILD) algolib.cma

algolib.cmxa :
	$(BUILD) algolib.cmxa
