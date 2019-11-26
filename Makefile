.PHONY: clean all xmldiff

all: xmldiff

xmldiff:
	dune build xmldiff.exe
