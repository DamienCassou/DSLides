#! /bin/bash

rm -rf slides-output.tex
echo '(load "dslides.lisp")(load "clos.lisp")(deck-real-output)' | sbcl
pdflatex clos.tex


