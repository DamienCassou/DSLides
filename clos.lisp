
(slide
 (plain "\\titlepage"))

(slide :title "Common Lisp History"
 (plain "should talk about lisp history"))

(slide
 :title "Syntax"
 (slisp "(function-name arg1 arg2 ... argn)" :prompt nil)
 (text "")
 (pause)
 (lisp (+ 1 2) :answer t))

(slide 
 :title "Creating lists"
 :pause t
 :answer t
 (lisp (cons 3 nil))
 (lisp (cons 2 '(3)))
 (lisp (cons 1 '(2 3)))
 (lisp (list 1 2 3))
 (lisp '(1 2 3)))

(slide
 :title "Studying lists"
 :answer t
 :pause t
 (lisp (car '(1 2 3)))
 (lisp (cdr '(1 2 3)))
 (lisp (first '(1 2 3)))
 (lisp (last '(1 2 3))))

(slide
 (slisp 
"(defun mult2 (x)
  \"Multiplies x by 2\"
  (* x 2))
" :answer t)
 (pause)
 (lisp (mult2 3) :answer t))

(slide
 (lisp (describe mult2))
 (pause)
 (text "Impossible because \\ct{mult2} is not a variable")
 (pause)
 (lisp (describe #'mult2) :answer 
"(defun mult2 (x)
  \"Multiplies x by 2\"
   (* x 2))
"))

(slide
 (clang "int mult2 (int c) { return c * 2; }")
(pause)
(clang "
int main(void) {
  int (*fmult2) (int) = mult2;
  printf(\"%d\n\", (*fmult2)(3));
}"))
