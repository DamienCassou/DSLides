(proclaim '(optimize debug))

(defstruct ooclass
  name
  instVars
  methods)

(defstruct ooObject
  aClass
  instValues)

(defstruct ooMethod
  name
  source
  lambda)

(defvar *ooClasses* nil)
(defun reset-ooClasses () (setf *ooClasses* nil))

(defmacro makeMethod (name argNames &body body)
  `(make-ooMethod
    :name ',name
    :lambda (lambda ,argNames ,@body)
    :source ',body))

(defmacro makeClass (name instVarNames &body methods)
  `(progn
     (setf *ooClasses* (delete ',name *ooClasses* :key #'ooClass-name))
     (push
      (make-ooclass :name ',name
		    :instvars ',instVarNames
		    :methods ',(mapcar #'eval methods))
      *ooClasses*)))

(defun getClass (className)
  (let ((theClass (find className *ooClasses* :key #'ooClass-name)))
    (if (null theClass)
	(error "~s is not an existing class name" className)
	theClass)))

(defun getMethod (aClass methodName)
  (let ((mth (find methodName (ooClass-methods aClass) :key #'ooMethod-name)))
    (if mth
	mth
	(error "~s is not a method name for class named ~s" 
	       methodName
	       (ooclass-name aClass)))))

(defun new (className &rest instValues)
  (make-ooObject :aClass (getClass className) :instValues instValues))

(defparameter *current-object* nil)

(defun getInstVar (obj instVarName)
  (let* ((theObj (if (equal obj 'this)
		     *current-object*
		     obj))
	 (theClass (ooobject-aClass theObj))
	 (theInstVars (ooclass-instVars theClass))
	 (instVarNum (position instVarName theInstVars)))
    (nth instVarNum (ooobject-instValues theObj))))

(defun call (obj methodName &rest methodParams)
  (let ((theObj (if (equal obj 'this)
		    *current-object*
		    obj)))
    (let* ((theClass (ooObject-aClass theObj))
	   (theMethod (getMethod theClass methodName))
	   (*current-object* theObj))
      (apply (oomethod-lambda theMethod) methodParams))))


(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
	  syms)
     ,@body))

(defvar *save-readtable* *readtable*)

(setf *readtable* *save-readtable*)

(defvar *my-readtable* (copy-readtable))
(set-syntax-from-char #\} #\) *my-readtable*)
(set-macro-character
 #\{
 (lambda (stream char)
   (declare (ignore char))
   (let ((type (read-char stream t nil t))
	 (l (read-delimited-list #\} stream t)))
     (case type
       (#\i ;; get inst var
	  (if (equal (length l) 1)
	      `(getInstVar 'this ',(car l))
	      `(getInstVar ,(car l) ',(cadr l))))
       (#\s `(call ,(car l) ',(cadr l) ,@(cddr l)))
       (#\c `(call 'this ',(car l) ,@(cdr l))))))
		

 nil
 *my-readtable*)

(setf *readtable* *my-readtable*)

;;; instance var of obj name var: #{i obj var}
;;; instance var of this: #{i obj}
;;; call mth with args on obj: #{c obj mth (args)}
;;; call mth with args on this: #{c mth (args)}


;;; Testing the language

(defmacro massert (bool &rest args)
  `(assert ,bool nil ,@args))

(defun massert-equal (expected actual &rest args)
  (massert (equal expected actual) args))


(makeClass Person (name)
  (makeMethod getName ()
    (getInstVar 'this 'name)))

(defvar toto)
(setf toto (new 'Person "Toto"))

(massert (ooobject-p toto))
(massert-equal (getClass 'Person) (ooobject-aClass toto))

(massert-equal "Toto" (call toto 'getName))

(makeClass Speaker ()
  (makeMethod speak (sentence)
    (format nil "Listen all of you: ~s~%" sentence)))

(defvar alexander)
(setf alexander (new 'Speaker))
(massert-equal "Listen all of you: \"Hello World!\"
" (call alexander 'speak "Hello World!"))

(makeClass LocalChain (i)
  (makeMethod add15AndInstVar (n)
    (+ 10 (call 'this 'add5 n) (getInstVar 'this 'i)))
  (makeMethod add5 (n)
    (+ 5 n)))

(massert-equal 25 (call (new 'LocalChain 1) 'add15AndInstVar 9))

(makeClass IntValue (i)
  (makeMethod getValue ()
    (getInstVar 'this 'i)))

(makeClass RemoteChain (intValue)
  (makeMethod add15AndInstVar (anotherIntValue)
    (+ 10
       (call (getInstVar 'this 'intValue) 'getValue)
       (call anotherIntValue 'getValue)
       (call 'this 'add5 0)))
  (makeMethod add5 (n)
    (+ 5 n)))

(massert-equal 25
	       (call (new 'RemoteChain (new 'IntValue 1))
		     'add15AndInstVar
		     (new 'IntValue 9)))

;;; Test syntax
(makeClass Person (name)
  (makeMethod getName ()
    {i name}))

(defvar toto)
(setf toto (new 'Person "Toto"))

(massert-equal "Toto" (call toto 'getName))
(massert-equal "Toto" {i toto name})
(massert-equal "Toto" {s toto getName})


(makeClass IntValue (i)
  (makeMethod getValue ()
    {i i}))

(makeClass RemoteChain (intValue)
  (makeMethod add15AndInstVar (anotherIntValue)
    (+ 10
       {s {i intValue} getValue}
       {s anotherIntValue getValue}
       {c add5 0}))
  (makeMethod add5 (n)
    (+ 5 n)))

(massert-equal 25
	       {s (new 'RemoteChain (new 'IntValue 1))
	          add15AndInstVar
		  (new 'IntValue 9)})
