#! /bin/bash

rm -rf slides-output.tex
echo '(load "dslides.lisp")(load "clos.lisp")(deck-real-output)' | ~/.emacs.d/ibcl
pdflatex clos.tex


