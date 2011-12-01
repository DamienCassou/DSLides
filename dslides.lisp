(require 'cl-ppcre)
(defparameter *stream* nil)
(defparameter *deck* nil)
(setf *print-case* :downcase)

(defun deck-reset () (setq *deck* nil))

(defun plain (string)
  (write-line string *stream*))

(defun text (string &key (action nil))
  (plain (format nil "\\begin{block}~a{}"
		 (if action
		     (format nil "<~a>" action)
		     "")))
  (plain string)
  (plain "\\end{block}"))

(defun answer (obj &key (notransform nil))
  (plain "\\begin{answer}")
  (if (and (not notransform) (stringp obj))
      (plain (format nil "~s" obj))
      (plain (format nil "~a" obj)))
  (plain "\\end{answer}\\vspace{-1em}"))

(defun slisp (string &key (answer nil) (eval nil) (prompt t))
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
  (if prompt
      (plain (format nil "> ~a" 
		     (cl-ppcre:regex-replace-all "\\n" string "
  ")))
      (plain string))
  (plain "\\end{lisp}\\vspace{-1em}")
  (let ((result (if (or eval (eq answer t))
		    (eval (with-input-from-string (in string) (read in)))
		    answer)))
    (if answer
	(case answer
	  (t (answer result))
	  (otherwise (format t "~s~%" string) (answer answer :notransform (stringp answer)))))))

(defmacro lisp (list &rest args)
  `(apply #'slisp (list ,(format nil "~s" list) ,@args)))

(defun pause ()
  (plain "\\pause"))

(defun remove-keyword-params (seq)
  "Useful when a function has both &rest and &key."
  (unless (null seq)
    (let ((head (car seq))
	  (tail (cdr seq)))
      (if (keywordp head)
	  (remove-keyword-params (cdr tail))
	  (cons head (remove-keyword-params tail))))))

(defun itemize (&rest items &key (title "") &allow-other-keys)
  (plain "\\begin{block}{}")
  (plain title)
  (plain "\\begin{itemize}")
  (dolist (item (remove-keyword-params items) nil)
    (unless (equal item "")
      (plain (concatenate 'string "\\item " item))))
  (plain "\\end{itemize}")
  (plain "\\end{block}"))

(defun clang (string)
  (plain "\\begin{langc}")
  (plain string)
  (plain "\\end{langc}\\vspace{-1em}"))

(defun visual-builderp (builder)
  (or (member (car builder) '(slisp lisp))
      (and (member (car builder) '(text itemize))
	   (not (equal (cadr builder) "")))))

(defun answerable-builderp (builder)
  (member (car builder) '(slisp lisp)))

(defun deck-output-slide (slide &key (title nil) (pause t) (answer nil))
  (plain (format nil "\\begin{frame}[fragile,plain]~a"
		 (if title (format nil "{~a}" title) "")))
  (dotimes (i (length slide))
    (let ((builder (nth i slide)))
      (eval (if (and answer (answerable-builderp builder) (not (member ':answer builder)))
		(append builder '(:answer t))
		builder))
      (when (and pause (visual-builderp builder) (< (1+ i) (length slide)))
	(pause))))
  (plain "\\end{frame}")
  (plain ""))

(defun deck-output (stream)
  (let ((*stream* stream))
    (mapcar (lambda (slide)
	      (apply #'deck-output-slide ; (cons (cdr slide) (car slide))))
		     (cons (remove-if-not #'consp slide)
			   (remove-if #'consp slide))))
	    (reverse *deck*))))

(defun deck-real-output ()
  (with-open-file
      (stream "slides-output.tex"
	      :direction :output
	      :if-exists :overwrite
	      :if-does-not-exist :create)
    (deck-output stream)))

(defun slide-real-output (slide)
  (let ((*deck* (cons slide nil)))
    (deck-real-output)))

(defun deck-string-output ()
  (with-open-stream (stream (make-string-output-stream))
    (deck-output stream)
    (get-output-stream-string stream)))

(defmacro slide (&rest body)
  `(push (quote ,body) *deck*))

(defvar *my-readtable* (copy-readtable))

(set-syntax-from-char #\] #\) *my-readtable*)

(set-macro-character
 #\[
 (lambda (stream char)
   (declare (ignore char))
   (let ((reading
	  (with-output-to-string (output)
	    (loop
	      (let ((c (read-char stream nil #\] t)))
		(if (char= c #\])
		    (return)
		    (write-char c output)))))))
     `(string-trim '(#\Space #\Tab #\Newline) ,reading)))
 nil
 *my-readtable*)

(setf *readtable* *my-readtable*)
