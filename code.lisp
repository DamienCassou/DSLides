;; Playing with lists: (very important for meta-programming)
;; - creating lists
;; - studying it (first, car, cdr...)
;; - adding/removing elements

(defun mult2 (x)
  "Multiplies parameter X by 2"
  (* x 2))

(mult2 3) ;; 6

;; Does not work as the mult2 variable is not defined
(describe mult2)

;; Lets create a variable that references the function
(defvar fmult2)

(setq fmult2 (symbol-function 'mult2))

;; Printing it and we see the definition, but without a name (lambda)
fmult2

(describe fmult2)

;; Evaluating the function
(apply fmult2 '(3)) ;; 6

;; Summary: in lisp it is possible to define new functions, retrieve
;; them by name, storing the result in a variable, and executing a
;; function contained in a variable. This is very similar to pointer
;; manipulation in C: see ex1.c (except it is much more readable)

(ibcl:get-source 'mult2)
;; don't try this at home


;; ;; This is a cons
;; (type-of fmult2)

;; ;; Which is another way of saying it is a list
;; (listp fmult2)

;; This means that printing fmult2 does indeed print a list. So, we
;; can get detailed information about the function by studying the
;; list.
(defvar smult2 (third (ibcl:get-source 'mult2)))

smult2

(first smult2) ;; defun
(second smult2) ;; name
(third smult2) ;; parameters
(fourth smult2) ;; comment
(fifth smult2) ;; body
;; let's play with the body a little

(defvar smult10)

(setq smult10 (copy-list smult2))

;; we first change the body
(nsubstitute 10 2 (fifth smult10))

smult10

;; we also need to change the name of the function
(nsubstitute 'mult10 'mult2 smult10)

smult10

;; and the comment
(setf (fourth smult10) (cl-ppcre:regex-replace "2" (fourth smult10) "10"))

smult10

;; defines the new function
(eval smult10)

(apply 'mult10 '(3))

;; Summary: a function in lisp is simply a list. This list can be
;; studied like any list. New functions can be created by modifying a
;; list

(describe 'mult10)
