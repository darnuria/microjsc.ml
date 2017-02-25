
TAR=tar

SOURCES= utils.ml parseutils.ml ast.ml parser.mly lexer.mll prim.ml kast.ml expander.ml bytecode.ml compiler.ml ccmain.ml
RESULT= compiler

all: byte-code

-include OCamlMakefile

archive: Makefile utils.ml parseutils.ml ast.ml parser.mly lexer.mll TestParser.ml
	mkdir microjs-${shell date +%F}
	mkdir microjs-${shell date +%F}/tests
	cp -f $? microjs-${shell date +%F}
	cp -f tests/*.js microjs-${shell date +%F}/tests/
	$(TAR) cvzf microjs-${shell date +%F}.tar.gz microjs-${shell date +%F}
	rm -rf microjs-${shell date +%F}

cleanall: clean
	rm -f *~
	rm -f testparser
	rm -f compiler
	rm -f microjs-*.tar.gz
