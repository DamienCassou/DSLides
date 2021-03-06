<!-- Copyright (C) 2011, Damien Cassou  -->

<!-- Permission is hereby granted, free of charge, to any person obtaining a  -->
<!-- copy of this software and associated documentation files (the "Software"),  -->
<!-- to deal in the Software without restriction, including without limitation  -->
<!-- the rights to use, copy, modify, merge, publish, distribute, sublicense,  -->
<!-- and/or sell copies of the Software, and to permit persons to whom the  -->
<!-- Software is furnished to do so, subject to the following conditions:  -->

<!-- The above copyright notice and this permission notice shall be included in  -->
<!-- all copies or substantial portions of the Software.  -->

<!-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  -->
<!-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  -->
<!-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL  -->
<!-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  -->
<!-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING  -->
<!-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER  -->
<!-- DEALINGS IN THE SOFTWARE. -->

This project is hosted as a
[github repository](https://github.com/DamienCassou/DSLides).

This project is mainly a demonstration that Common Lisp can be used to
create Domain-Specific Languages (DSL). I created this project for a
lecture (available on
[slideshare](http://www.slideshare.net/DamienCassou/metaprogramming-and-reflection-in-common-lisp))
on
[metaprogramming and reflection](http://www.hpi.uni-potsdam.de/studium/lehrangebot/lehrangebot0/veranstaltung/meta_programming_reflection-3.html?L=1&cHash=e1cc090b265a6a818f89e787323464d8)
in Common Lisp (December 8th, 2011, at the Hasso-Plattner-Institute,
Potsdam, Germany). I'm certainly not an expert in Common Lisp, far
from that, and the code I wrote is also far from being clean.

The DSL included in this project allows to create slides out of Common
Lisp code such as:

	(slide 
	  :title "Creating Lists"
	  :answer t
	  (lisp (cons 3 nil))
	  (lisp (cons 2 (3)) :answer nil  :eval nil)
	  (text "Impossible as \\ct{3} is not a function")
	  (lisp (cons 2 '(3)))
	  (lisp (cons 1 '(2 3)))
	  (lisp (list 1 2 3))
	  (lisp '(1 2 3)))

The DSL evaluates each `lisp` expression and presents, in the
resulting slide, both the expression and its automatically computed
result. Slide definitions are compiled into a pdf by using LaTeX and
Beamer. The previous slide definition results in a slide looking
similar to and much nicer than:

    |-----------------------------------|
    | Creating Lists                    |
    |-----------------------------------|
    | > (cons 3 nil)                    |
    | (3)                               |
    | > (cons 2 (3))                    |
    | Impossible as 3 is not a function |
    | > (cons 2 '(3))                   |
    | (2 3)                             |
    | > (cons 1 '(2 3))                 |
    | (1 2 3)                           |
    | > (list 1 2 3)                    |
    | (1 2 3)                           |
    | > '(1 2 3)                        |
    | (1 2 3)                           |
    |-----------------------------------|



This project mainly consists of the following files:

* `dslides.lisp`: The definition of the DSL to define slides in
  Common Lisp. This file requires [cl-ppcre](http://weitz.de/cl-ppcre/).

* `make.sh`: A Bash script to generate the resulting pdf out of a
  `clos.lisp` and `clos.tex` definition.

* Example slides:
    * `clos.lisp`: defines the slide contents using Common Lisp.
    * `clos.tex`: main Beamer LaTeX file that `\\input` the generated LaTeX slides.
    * `clos.pdf`: result of building the example slides.

* Emacs Lisp configuration

    * `dslides.el`: some functions to make writing slides in Emacs
      easier. Just press `C-c v` while the Emacs cursor is on a slide
      definition to display the resulting slide.
    * `.dir-locals.el`: configure some Emacs variables.

* Beamer configuration: all these files set the theme of the slides
  (logo, colors, header, footer, &hellip;)

    * `resources/hpi.png`
    * `beamercolorthemehpimetaprogramming.sty`
    * `beamerfontthemehpimetaprogramming.sty`
    * `beamerinnerthemehpimetaprogramming.sty`
    * `beamerouterthemehpimetaprogramming.sty`
    * `beamerthemehpimetaprogramming.sty`

* Example files in C and Common Lisp to support the slides 
	* `example-c/compile.sh`
	* `example-c/ex1.c`
	* `example-c/ex2.c`
	* `example-c/macro.c`
	* `example-lisp/oo.lisp`
	* `example-lisp/while.lisp`

<!--  LocalWords:  metaprogramming
 -->
