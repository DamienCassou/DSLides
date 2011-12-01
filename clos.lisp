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
 (itemize
  :title "In Lisp it is possible to:"
  "define new functions,"
  "retrieve a function by name,"
  "reference a function from a variable,"
  "call a function from a variable.")
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
 (itemize "A function definition in Lisp is a list."
	  "This list can be studied like any list."
	  "New functions can be created from a list."
	  ""))

(slide
 :title "Beyond Functions"
 :pause nil
 (text "How would you implement \\ct{while} that executes its
  \\ct{body} \\emph{as long as} its \\ct{condition} stays true?")
 (lisp (while condition body) :answer nil))

(defvar usingwhile "(while (/= i 0)
  (decf i)
  (format t \"i is now: ~s~%\" i))")

(defvar usingloop "(loop 
  (if (not (/= i 0))
    (return)
    (progn
      (decf i)
      (format t \"i = ~s~%\" i))))")

(slide
 :title "The While Construct"
 :pause nil
 (lisp (setq i 10))
 (slisp usingwhile)
 (pause)
 (answer "i is now: 9
i is now: 8
i is now: 7
...
i is now: 2
i is now: 1
i is now: 0" :notransform t))

(slide
 :title "The While Construct: Using Loop"
 :pause nil
 (slisp usingwhile :answer nil)
 (slisp usingloop :answer nil))

(slide
 :title "The While Construct: Function"
 :pause nil
 (slisp usingwhile)
 (slisp "(defun while (test &rest body)
  (loop
    (if (not test)
        (return)
        (progn body))))")
 (pause)
 (text "doesn't work because parameters are evaluated immediately")
 (slisp "(while nil nil)" :answer nil))

(slide
 :title "The While Construct: Function"
 (slisp "(while '(/= i 0)
  '(decf i)
  '(format t \"i is now: ~s~%\" i))")
 (slisp "(defun while (test &rest body)
  (loop
    (if (not (eval test))
        (return)
        (mapcar #'eval body))))")
 (text "works, but using \\ct{while} is less readable than intended"))

(slide
 :title "Summary"
 (itemize "Arguments of functions are evaluated first."
	  "To prevent evaluation, use \\ct{quote} (or \\ct{'})."
	  "Use \\ct{eval} to evaluate an expression."
	  ""))


;; (slide
;;  :title "Using Macros in C"
;;  (clang "#define MAX(a,b) ((a)>(b)?(a):(b));

;; int
;; main(void)
;; {
;;  printf(\"max(1,5)=%d\\n\", MAX(1,5));
;;  printf(\"max(8,1)=%d\\n\", MAX(8,1));
;; }"))

(slide
 :title "Macros"
 :pause nil
 (itemize
  :title "Macros are programs that write programs"
  "they return lists representing Lisp code."
  "they don't evaluate their arguments."
  "they are evaluated at \\emph{compile time}."
  ""))

(slide
 :title "The While Construct: Macro"
 (slisp usingloop)
 (slisp "(defmacro while (test &body body)
  (list 'loop
    (list 'if (list 'not test)
      (list 'return)
      (cons 'progn body))))"))

(slide
 :title "The While Construct: Macro"
 :pause nil
 (slisp usingloop)
 (slisp "(defmacro while (test &body body)
  `(loop
     (if (not ,test)
       (return)
       (progn ,@body))))"))

(slide
 :title  "Creating an OO language"
 (slisp "(defstruct ooclass
   name
   instVars
   methods)" :answer t))

(slide
 :title "Creating an OO language"
 (slisp "(defstruct ooObject
   aClass
   instValues)" :answer t))

(slide
 :title "Creating an OO language"
 (slisp "(defstruct ooMethod
  name
  lambda)" :answer t))

(slide
 :title "Creating an OO language"
 (lisp (defvar *ooClasses* nil) :answer t)
 (slisp "(defun reset-ooClasses ()
  (setf *ooClasses* nil))" :answer t))

(setf *readtable* *my-readtable*)

(slide
 :title "dsfsdf"
 (slisp [(defun reset-ooClasses ()
	    (setf *ooClasses* nil))]))
