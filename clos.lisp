(setf *readtable* *my-readtable*)

(slide
 :plain t
 :title ""
 (plain "\\titlepage"))

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
  [(defun mult2 (x)
  "Multiplies x by 2"
  (* x 2))])
 (text "\\ct{defun} is itself a function, it creates functions")
 (lisp (mult2 3)))

(defvar defmult2 
  [(defun mult2 (x)
  "Multiplies x by 2"
  (* x 2))])
(eval (read-from-string defmult2))

(slide
 :title "Studying Functions"
 (lisp #'mult2 :answer t)
 (lisp (describe #'mult2) :answer 
       [(defun mult2 (x)
	  "Multiplies x by 2"
	  (* x 2))]))

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
       :answer [(nil nil 
		     (defun mult2 (x)
		       "Multiplies x by 2"
		       (* x 2)))])
 (text "requires ibcl"))

(slide
 :title "Generating new Functions"
 (slisp [(defvar smult2
  (third (get-source 'mult2)))] :answer 'smult2)
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
 (slisp [(defvar smult10
   (copy-list smult2))])
 (lisp (nsubstitute 10 2 (fifth smult10)))
 (lisp smult10 :answer
       [(defun mult2 (x) 
	  "Multiplies x by 2"
	  (* x 10))]))

(slide
 :title "Generating new Functions"
 (setq smult10 '(defun mult2 (x) "Multiplies x by 2" (* x 10)))
 (lisp smult10  :answer [(defun mult2 (x) 
			   "Multiplies x by 2"
  (* x 10))])
 (slisp [(nsubstitute 'mult10 'mult2
             smult10)]  :answer [(defun mult10 (x) 
				   "Multiplies x by 2"
				   (* x 10))]))

(slide
 :title "Generating new Functions"
 :answer t
 (setq smult10 '(defun mult10 (x) "Multiplies x by 2" (* x 10)))
 (lisp smult10 :answer [(defun mult10 (x) 
			  "Multiplies x by 2"
			  (* x 10))])
 (slisp [(setf (fourth smult10)
      (cl-ppcre:regex-replace "2"
        (fourth smult10) "10"))]))

(slide
 :title "Generating new Functions"
 :answer t
 (setq smult10 '(defun mult10 (x) "Multiplies x by 10" (* x 10)))
 (lisp smult10 :answer [(defun mult10 (x)
			  "Multiplies x by 10"
			  (* x 10))])
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

(defvar usingwhile [(while (/= i 0)
  (decf i)
  (format t "i is now: ~s~%" i))])

(defvar usingloop [(loop 
  (if (not (/= i 0))
    (return)
    (progn
      (decf i)
      (format t "i = ~s~%" i))))])

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
 (slisp [(defun while (test &rest body)
  (loop
    (if (not test)
        (return)
        (progn body))))])
 (slisp usingwhile)
 (pause)
 (text "doesn't work because parameters are evaluated immediately")
 (slisp [(while t nil)] :answer nil))

(slide 
 :title "Function Evaluation in C"
 :pause nil
 (clang "int f(int c){printf(\"f\\n\");return c;}
int g(int c){printf(\"g\\n\");return c;}
int h(int c){printf(\"h\\n\");return c;}

int main(void) {
  f(g(h(1)));
}")
 (plain "~")
 (pause)
 (clang "h
g
f"))

(slide
 :title "The While Construct: Function"
 :pause nil
 (slisp [(defun while (test &rest body)
  (loop
    (if (not test)
        (return)
        (progn body))))])
 (slisp usingwhile)
 (text "doesn't work because parameters are evaluated immediately")
 (slisp [(while t nil)] :answer nil))

(slide
 :title "The While Construct: Function"
 (slisp [(while '(/= i 0)
  '(decf i)
  '(format t "i is now: ~s~%" i))])
 (slisp [(defun while (test &rest body)
  (loop
    (if (not (eval test))
        (return)
        (mapcar #'eval body))))])
 (text "works, but using \\ct{while} is less readable than intended"))

(slide
 :title "Summary"
 (itemize "Arguments of functions are evaluated first."
	  "To prevent evaluation, use \\ct{quote} (or \\ct{'})."
	  "Use \\ct{eval} to evaluate an expression."
	  ""))

(slide
 :title "The While Construct: Macro"
 (slisp usingloop)
 (slisp [(defmacro while (test &body body)
  (list 'loop
    (list 'if (list 'not test)
      (list 'return)
      (cons 'progn body))))]))

(slide
 :pause nil
 (slisp usingloop)
 (slisp [(defmacro while (test &body body)
  `(loop
     (if (not ,test)
       (return)
       (progn ,@body))))]))

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
 :title  "Creating an OO language"
 :answer nil
 (slisp [(makeClass Speaker (name)
  (makeMethod speak (sentence)
    (format t
      "Listen all of you: ~s~%"
      sentence)))])
 (slisp [(defvar alex (new 'Speaker "Alex"))])
 (slisp [(call alex 'speak "Hello World!")]
	:answer "Listen all of you: \"Hello World!\"")
 (lisp (getInstVar alex 'name) :answer "Alex"))

(slide
 (slisp [(makeClass Speaker ()
  (makeMethod "..."))])
 (itemize :title "A class is composed of:"
	  "a name,"
	  "some instance variables,"
	  "and some method definitions."
	  "")
 (slisp [(defstruct cls
   name
   vars
   mths)]))

(slide
 :pause nil
 (slisp [(makeClass Speaker ()
  (makeMethod "..."))])
 (plain "~")
 (slisp
  [(defmacro makeClass (name iVars
                     &body meths)
  (push
     (make-cls
       :name ',name
       :vars ',iVars
       :mths
         ',(mapcar #'eval meths))
     *classes*))]))

(slide
 (slisp [(makeMethod speak (sentence)
  (format t "..." sentence))])
 (itemize :title "A method is composed of:"
	  "a name,"
	  "some parameters,"
	  "a body"
	  "")
 (slisp [(defstruct mth
  name
  lmbd)]))

(slide
 (slisp [(makeMethod speak (sentence)
  (format t "..." sentence))])
 (plain "~")
 (slisp
  [(defmacro makeMethod (name 
             argNames &body body)
  `(make-mth
      :name ',name
      :lmbd (lambda ,argNames
                      ,@body)))]))

(slide
 (slisp [(new 'Speaker "Alex")])
 (itemize :title "An object is composed of:"
	  "a reference to its class,"
	  "some values for its instance variables")
 (slisp [(defstruct obj
   cls
   values)]))

(slide
 (slisp [(call alex 'speak "Hello World!")]
	:answer "Listen all of you: \"Hello World!\"")
 (itemize :title "A call is a function with:"
	  "the receiver object,"
	  "a method name,"
	  "and a list of parameters." "")
 (slisp [(defun call (obj name &rest params)
	   "...")] :prompt nil))

(slide
 (slisp [(defun call (obj name &rest params)
  (let* ((cls (obj-cls obj))
     	   (mth (getMethod cls name)))
      (apply (mth-lmbd mth)
             params)))] :prompt nil)
 (plain "~")
 (slisp [(defun getMethod (cls name)
  (find name (cls-mths cls)
        :key #'mth-name))] :prompt nil))

(defvar mappingInstVariables
"
  \\begin{tabular}{l|c|c|c|c|}
    \\cline{2-5}
    class: & $varname_1$ & $varname_2$ & $\\dots$ & $varname_n$ \\\\
    \\cline{2-5}
    \\multicolumn{5}{c}{~}\\\\
    \\cline{2-5}
    object: & $value_1$ & $value_2$ & $\\dots$ & $value_n$ \\\\
    \\cline{2-5}
  \\end{tabular}
")

(slide 
 :pause nil
 (lisp (getInstVar alex 'name) :answer "Alex")
 (pause)
 (itemize :title "Looking for an instance variable value from its name involves:"
	  "getting the position of the name in the list of all instance variables of the class,"
	  "taking the value at this position in the list of all values of the object.")
 (plain mappingInstVariables))

(slide
 (plain mappingInstVariables)
 (slisp
  [(defun getInstVar (obj name)
  (let* ((cls (obj-cls obj))
         (vars (cls-vars cls))
         (pos (position name vars)))
    (nth pos (obj-values obj))))]
  :prompt nil))

(slide
 :title "Handling this"
 (text "An object must be able to get its instance variables and call
 methods by using \\ct{this}.")
 (slisp [(makeClass Speaker (name)
  (makeMethod getName ()
    (getInstVar 'this 'name)))])
 (lisp (call alex 'getName) :answer "Alex")
 (text "This requires the system to keep track of the \\emph{current object}.")
 (lisp (defparameter *cur-obj* nil)))

(slide
 (slisp
  [(defun getInstVar (obj name)
  (let* ((theObj 
            (if (equal obj 'this)
                *cur-obj*
                obj))
         (cls (obj-cls theObj))
         (vars (cls-vars cls))
         (pos (position name vars)))
    (nth pos (obj-values theObj))))]
  :prompt nil)
 (text "When is \\ct{*cur-obj*} updated?
        \\uncover<3->{Before it is \\emph{used}!}\\\\
        \\uncover<4->{As \\ct{this} is only used when a method is executed, the method \\ct{call} needs to do the updating job.}"))

(slide
 :pause nil
 (text "The method \\ct{call} needs to do the updating job:")
 (slisp [(defun call (obj name &rest params)
  (let* ((cls (obj-cls obj))
         (mth (getMethod cls name))
         (*cur-obj* obj))
      (apply (mth-lmbd mth)
             params)))] :prompt nil))

(slide
 :pause nil
 (text "We also want to pass \\ct{this} as first argument to
 \\ct{call}:")
 (slisp [(defun call (obj name &rest params)
  (let* ((theObj 
            (if (equal obj 'this)
                *cur-obj*
                obj))
          (cls (obj-cls theObj))
     	  (mth (getMethod cls name)))
      (setf *cur-obj* theObj)
      (apply (mth-lmbd mth)
             params)))] :prompt nil))

(slide
 :title  "Creating an OO language"
 (itemize :title "Possible improvements:"
	  "setting of instance variables"
	  "inheritance"
	  "constructors"
	  "dedicated syntax"))
 
(slide
 :title "Creating Domain-Specific Languages"
 :pause nil
 (slisp [(makeClass Speaker (name)
  (makeMethod speak (s)
    (format t "I say: ~a" s))	   
  (makeMethod getName ()
    (call 'this 'speak "hi!")
    (getInstVar 'this 'name)))] :prompt nil)
 (plain "~")
 (pause)
 (slisp [(makeMethod getName ()
    {c speak "hi!"}
    {i name})] :prompt nil))

(slide
 :pause nil
 (slisp ";; {c speak \"hi!\"} {i name}" :prompt nil)
 (slisp [(set-macro-character #\{
 (lambda (str)
   (let ((type (read-char str))
         (l (read-delimited-list
                         #\} str)))
     (case type
      (#\c `(call 'this
                  ',(car l)
                  ,@(cdr l)))
      (#\i `(getInstVar 'this
                        ',(car l)))))))] :prompt nil))

(slide
 :title "Summary"
 (itemize
  "Lisp has powerful functions to manipulate lists."
  "Lisp source code is made of lists."
  "As a result, meta-programming is made easy." ""))

(slide
 (itemize :title "Macros"
  "can be used to create source code,"
  "don't evaluate their arguments,"
  "are evaluated at compile time." "")
 (text "Macros are programs that write programs.")
 (text "Macros can also be used to install a new syntax."))

(slide
 :title "A Glimpse at CLOS"
 :answer t
 (slisp [(defclass circle ()
  (radius center))])
 (lisp (make-instance 'circle)))

(slide
 :answer t
 (slisp [(defclass circle ()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))])
 (lisp (setf c (make-instance 'circle)))
 (lisp (setf (circle-radius c) 6))
 (lisp (circle-radius c)))

(slide
 :answer t
 (slisp [(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius)
   (center :accessor circle-center
           :initarg :center)))])
  (slisp [(setf c (make-instance 'circle
                         :radius 6))])
  (lisp (circle-radius c)))

(slide
 :answer t
 (defclass circle ()
   ((radius :accessor circle-radius
	    :initarg :radius)
    (center :accessor circle-center
	    :initarg :center)))
 (slisp [(defmethod area ((c circle))
  (* pi (expt (circle-radius c) 2)))]
 :eval t :answer "#<standard-method area (circle)>")
 (slisp [(setf c (make-instance 'circle
                         :radius 6))])
 (lisp (area c)))

(slide 
 :title "Auxiliary methods"
 :answer nil
 (defclass circle ()
   ((radius :accessor circle-radius
	    :initarg :radius)
    (center :accessor circle-center
	    :initarg :center)))
 (setf c (make-instance 'circle
			  :radius 6))
 (slisp [(defmethod area ((c circle))
  (* pi (expt (circle-radius c) 2)))] :eval t)
 (slisp [(defmethod area :before ((c circle))
  (format t "I'm tired..."))] :eval t)
 (lisp (area c) :answer "I'm tired...
113.09733552923255d0"))

(slide
 :answer nil
 (defclass circle ()
   ((radius :accessor circle-radius
	    :initarg :radius)
    (center :accessor circle-center
	    :initarg :center)))
 (lisp (setf cache nil) :prompt nil)
 (slisp [(defun from-cache (c)
  (find (circle-radius c) cache
        :key #'car))] :prompt nil)
 (slisp [(defun to-cache (c area)
  (push (cons (circle-radius c) area)
        cache)
  area)] :prompt nil))


(slide
 :answer t
 (defclass circle ()
   ((radius :accessor circle-radius
	    :initarg :radius)
    (center :accessor circle-center
	    :initarg :center)))
 (setf c (make-instance 'circle
			  :radius 6))
 (defmethod area ((c circle))
   (* pi (expt (circle-radius c) 2)))
 (defmethod area :before ((c circle))
   (format t "I'm tired..."))
 (setf cache nil)
 (defun to-cache (c area)
   (push (cons (circle-radius c) area) cache)
   area)
 (defun from-cache (c)
   (find (circle-radius c) cache
	 :key #'car))
 (slisp [(defmethod area :around ((c circle))
 (let ((value (from-cache c)))
  (if value
   (progn
     (princ "Using the cache :-)")
     (cdr value))
   (progn
     (princ "So tired...")
     (to-cache c (call-next-method))))))]
	:answer nil :eval t :prompt nil)
 (lisp (area c) :eval t :answer "So tired...I'm tired...
113.09733552923255d0"))

(slide
 :answer t
 (defclass circle ()
   ((radius :accessor circle-radius
	    :initarg :radius)
    (center :accessor circle-center
	    :initarg :center)))
 (setf c (make-instance 'circle
			  :radius 6))
 (defmethod area ((c circle))
   (* pi (expt (circle-radius c) 2)))
 (defmethod area :before ((c circle))
   (format t "I'm tired..."))
 (setf cache nil)
 (defun to-cache (c area)
   (push (cons (circle-radius c) area) cache)
   area)
 (defun from-cache (c)
   (find (circle-radius c) cache
	 :key #'car))
 (defmethod area :around ((c circle))
 (let ((value (from-cache c)))
  (if value
   (progn
     (princ "Using the cache :-)")
     (cdr value))
   (progn
     (princ "So tired...")
     (to-cache c (call-next-method))))))
 (lisp (area c) :eval t :answer "So tired...I'm tired...
113.09733552923255d0")
 (lisp (area c) :eval t :answer "Using the cache :-)
113.09733552923255d0"))


(slide
 :title "Acknowledgments"
 :pause nil
 (itemize
  :title "Thanks to \\ct{#lisp} for all their help:"
  "akovalenko"
  "antifuchs"
  "H4ns"
  "nikodemus"
  "pjb"
  "prxb"
  "ThomasH"
  "")
 (plain "\\vspace{-1em}")
 (text "https://github.com/DamienCassou/DSLides"))
  
