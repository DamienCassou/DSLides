;; Copyright (C) 2011, Damien Cassou 

;; Permission is hereby granted, free of charge, to any person obtaining a 
;; copy of this software and associated documentation files (the "Software"), 
;; to deal in the Software without restriction, including without limitation 
;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;; and/or sell copies of the Software, and to permit persons to whom the 
;; Software is furnished to do so, subject to the following conditions: 

;; The above copyright notice and this permission notice shall be included in 
;; all copies or substantial portions of the Software. 

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;; DEALINGS IN THE SOFTWARE.

(setq i 10)

(loop 
  (decf i)
  (format t "I is now: ~s~%" i)
  (if (= i 0)
      (return)))

(defmacro lwhile (condition &body body)
  (list 'loop
	(list 'if (list 'not condition)
	 (list 'return)
	 (cons 'progn body))))

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
