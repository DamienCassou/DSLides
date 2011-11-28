(setq i 10)

(loop 
  (decf i)
  (format t "I is now: ~s~%" i)
  (if (= i 0)
      (return)))

(defmacro while (condition &body body)
  `(loop
     (if (not ,condition)
	 (return)
	 (progn ,@body))))

(setq i 10)
(while (/= i 0)
  (decf i)
  (format t "i is now: ~s~%" i))

(setq i 10)
(loop 
  (if (= i 0)
    (return)
    (progn
      (decf i)
      (format t "i = ~s~%" i))))

(defun fwhile (condition &rest body)
  (loop
    (if (not condition)
	(return)
	(progn
	  body))))
