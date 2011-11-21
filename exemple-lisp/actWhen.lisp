(defvar events '())

(defun events-reset  ()
    (setf events '()))

(defmacro actWhen (condition &rest body)
  `(push (cons (quote ,condition) (quote ,body)) events))

(defun actAll ()
  (let ((numAct 0))
    (dolist (pair events)
      (when (eval (first pair))
	(incf numAct)
	(eval (second pair))))
    (format t "actAll: ~a action(s) executed" numAct)))



;; (slide
;;  :title "Beyond Functions"
;;  :pause nil
;;  (text "How would you implement \\ct{actWhen} that executes its
;;  \\ct{body} \\emph{as soon as} its \\ct{condition} becomes true?")
;;  (lisp (actWhen condition body) :answer nil))

;; (defvar tryActWhen "(actWhen (null foo)
;;   (format t \"Variable 'foo' 
;;               is null~%\"))")

;; (slide
;;  :answer nil
;;  (lisp (defvar foo) :answer t)
;;  (slisp tryActWhen)
;;  (lisp (setq foo 3) :answer t)
;;  (lisp (setq foo nil)
;;        :answer "Variable 'foo' is null"))

;; (slide
;;  :title "actWhen: With a Function"
;;  (slisp "(defun actWhen (condition body)
;;   (if condition body))" :answer nil :eval t)
;;  (lisp (defvar foo) :answer t)
;;  (lisp (setq foo 3) :answer t)
;;  (slisp tryActWhen :answer "Variable 'foo' is null")
;;  (lisp (setq foo nil) :answer t))

