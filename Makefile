# Makefile
# Part of Micro-JavaScript compiler in ML project
# at Université Pierre et Marie Curie
#
# Copyright 2016 - 2017
#
# 3I018 Compilation Course
# Teachers:
#   - Frederic Peschanski
#   - Lieu Choun Tong
#   - Chailloux Emmanuel
#

TAR=tar

#SOURCES= utils.ml parseutils.ml ast.ml \
#         parser.mly lexer.mll prim.ml \
#         kast.ml expander.ml bytecode.ml \
#         compiler.ml ccmain.ml

all:
	eval `opam config env`
	ocamlbuild -use-ocamlfind -package cmdliner -cflags '-principal' 'ccmain.native'

deps:
	opam update
	opam install cmdliner

# For giving out to student prefer giving an url to the git project.
archiveLastest:
	git archive -o cmicrojsML-${shell git rev-parse HEAD}.zip HEAD

clean:
	ocamlbuild -clean


