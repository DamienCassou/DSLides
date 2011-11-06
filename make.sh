#! /bin/bash

rm -rf slides-output.tex
echo '(load "clos.lisp")' | ~/.emacs.d/ibcl
pdflatex clos.tex
pdflatex clos.tex

