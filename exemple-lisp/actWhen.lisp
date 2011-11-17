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
