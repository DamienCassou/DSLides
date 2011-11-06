(require 'cl-ppcre)
(defparameter *stream* nil)
(defparameter *deck* nil)
(setf *print-case* :downcase)

(defun deck-reset () (setq *deck* nil))

(defun plain (string)
  (write-line string *stream*))

(defun text (string)
  (plain "\\begin{center}")
  (plain string)
  (plain "\\end{center}"))

(defun answer (obj)
  (plain "\\begin{answer}")
  (plain (format nil "~a" obj))
  (plain "\\end{answer}\\vspace{-1em}"))

(defun slisp (string &key (answer nil) (eval nil))
  "
|--------+------+-------------+---------------+--------------|
| answer | eval | should eval | should answer | answered obj |
|--------+------+-------------+---------------+--------------|
| t      | t    | yes         | yes           | result       |
| nil    | t    | yes         | no            |              |
| o      | t    | yes         | yes           | o            |
| t      | nil  | yes         | yes           | result       |
| nil    | nil  | no          | no            |              |
| o      | nil  | no          | yes           | o            |
|--------+------+-------------+---------------+--------------|
"
  (plain "\\begin{lisp}")
  (plain (format nil "> ~a" (cl-ppcre:regex-replace-all "\\n" string "
  ")))
  (plain "\\end{lisp}\\vspace{-1em}")
  (let ((result (if (or eval (eq answer t))
		    (eval (with-input-from-string (in string) (read in)))
		    answer)))
    (if answer
	(case answer
	  (t (answer result))
	  (otherwise (answer answer))))))

(defmacro lisp (list &rest args)
  `(apply #'slisp (list ,(format nil "~a" list) ,@args)))

(defun clang (string)
  (plain "\\begin{langc}")
  (plain string)
  (plain "\\end{langc}\\vspace{-1em}"))

(defun deck-output (stream)
  (let ((*stream* stream))
    (mapcar (lambda (slide)
	      (plain "\\begin{frame}[fragile]")
	      (mapcar #'eval (substitute '(plain "\\pause") 'pause slide))
	      (plain "\\end{frame}")
	      (plain ""))
	    (reverse *deck*))))

(defun deck-real-output ()
  (with-open-file
      (stream "slides-output.tex"
	      :direction :output
	      :if-exists :overwrite
	      :if-does-not-exist :create)
    (deck-output stream)))

(defun deck-string-output ()
  (with-open-stream (stream (make-string-output-stream))
    (deck-output stream)
    (get-output-stream-string stream)))

(defmacro slide (&rest body)
  `(push (quote ,body) *deck*))

(slide
 (plain "should talk about lisp history"))

(slide
 (plain "should talk about lists 
- creating lists
- studying it (first, car, cdr...)
- adding/removing elements"))

(slide
 (lisp (+ 1 2) :answer t)
 pause
 (lisp '(+ 1 2) :answer t))

(slide
 (slisp 
"(defun mult2 (x)
  \"Multiplies X by 2\"
  (* x 2))
" :answer t)
 pause
 (lisp (mult2 3) :answer t))

(slide
 (lisp (describe mult2))
 pause
 (text "Impossible because \\ct{mult2} is not a variable")
 pause
 (lisp (describe #'mult2) :answer 
"(defun mult2 (x)
  \"Multiplies X by 2\"
   (* x 2))
"))

(slide
 (clang "int mult2 (int c) { return c * 2; }")
pause
(clang "
int main(void) {
  int (*fmult2) (int) = mult2;
  printf(\"%d\n\", (*fmult2)(3));
}"))

(deck-real-output)
