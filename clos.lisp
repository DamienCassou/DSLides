(slide
 (plain "\\titlepage"))

(slide :title "Common Lisp History"
       (plain "should talk about lisp history"))

(slide
 :title "Syntax"
 :pause nil
 (slisp "(function-name arg1 arg2 ... argn)" :prompt nil)
 (text "")
 (pause)
 (lisp (+ 1 2) :answer t))

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

(slide
 :title "Studying Lists"
 :answer t
 (lisp (car '(1 2 3)))
 (lisp (cdr '(1 2 3)))
 (lisp (first '(1 2 3)))
 (lisp (last '(1 2 3) 2))
 (lisp (last '(1 2 3))))

(slide
 :title "Creating Functions"
 :answer t
 (slisp 
  "(defun mult2 (x)
  \"Multiplies x by 2\"
  (* x 2))
")
 (text "\\ct{defun} is itself a function, it creates functions")
 (lisp (mult2 3)))

(defvar defmult2 "(defun mult2 (x)
   \"Multiplies x by 2\"
   (* x 2))")
(eval (read-from-string defmult2))

(slide
 :title "Studying Functions"
 (lisp (describe mult2))
 (text "Impossible because \\ct{mult2} is not a variable")
 (lisp #'mult2 :answer t)
 (lisp (describe #'mult2) :answer 
       "(defun mult2 (x)
	 \"Multiplies x by 2\"
	 (* x 2))"))

(slide
 :title "Calling Functions"
 :answer t
 (lisp (mult2 3))
 (lisp (funcall #'mult2 3))
 (lisp (defvar fmult2 #'mult2))
 (lisp (funcall fmult2 3)))

(slide
 :title "Summary"
 :pause t
 (text "In Lisp it is possible to:
\\begin{itemize}
\\item define new functions,
\\item retrieve a function by name,
\\item reference a function from a variable,
\\item call a function from a variable.
\\end{itemize}")
 (text "This is very similar to pointer manipulation in C"))

(slide
 :title "Function Pointer Manipulation in C"
 (clang "int mult2 (int c) {
 return c * 2;
}")
 (pause)
 (text "")
 (clang "int main(void) {
  int (*fmult2) (int) = mult2;
  (*fmult2)(3);
}"))

(slide
 :title "Generating new Functions"
 (lisp (get-source 'mult2)
       :answer "(nil nil 
  (defun mult2 (x)
    \"Multiplies x by 2\"
    (* x 2)))")
 (text "don't try this at home!"))

(slide
 :title "Generating new Functions"
 (slisp "(defvar smult2
  (third (get-source 'mult2)))" :answer 'smult2)
 (defvar smult2 (read-from-string defmult2))
 (lisp smult2 :answer defmult2))

(slide
 :title "Generating new Functions"
 :pause t 
 :answer t
 (defvar smult2
   (third (read-from-string defmult2)))
 (lisp (first smult2))
 (lisp (second smult2))
 (lisp (third smult2))
 (lisp (fourth smult2))
 (lisp (fifth smult2)))

(slide
 :title "Generating new Functions"
 :answer t
 (setq smult2
   (third (read-from-string defmult2)))
 (slisp "(defvar smult10
   (copy-list smult2))")
 (lisp (nsubstitute 10 2 (fifth smult10)))
 (lisp smult10 :answer "(defun mult2 (x) 
  \"Multiplies x by 2\"
  (* x 10))"))

(slide
 :title "Generating new Functions"
 (setq smult10 '(defun mult2 (x) "Multiplies x by 2" (* x 10)))
 (lisp smult10  :answer "(defun mult2 (x) 
  \"Multiplies x by 2\"
  (* x 10))")
 (slisp "(nsubstitute 'mult10 'mult2
             smult10)"  :answer "(defun mult10 (x) 
  \"Multiplies x by 2\"
  (* x 10))"))

(slide
 :title "Generating new Functions"
 :answer t
 (setq smult10 '(defun mult10 (x) "Multiplies x by 2" (* x 10)))
 (lisp smult10 :answer "(defun mult10 (x) 
  \"Multiplies x by 2\"
  (* x 10))")
 (slisp "(setf (fourth smult10)
      (cl-ppcre:regex-replace \"2\"
        (fourth smult10) \"10\"))"))

(slide
 :title "Generating new Functions"
 :answer t
 (setq smult10 '(defun mult10 (x) "Multiplies x by 10" (* x 10)))
 (lisp smult10 :answer "(defun mult10 (x)
  \"Multiplies x by 10\"
  (* x 10))")
 (lisp (eval smult10))
 (lisp (mult10 3)))

(slide
 :title "Summary"
 (text "\\begin{itemize}
\\item A function definition in Lisp is a list. 
\\item This list can
be studied like any list.
\\item New functions can be created from a list.
\\end{itemize}"))

(slide
 :title "Beyond Functions"
 :pause nil
 (text "How would you implement \\ct{while} that executes its
  \\ct{body} \\emph{as long as} its \\ct{condition} stays true?")
 (lisp (while condition body) :answer nil))
