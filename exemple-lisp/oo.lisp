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
  lambda)

(defvar *ooClasses* nil)
(defun reset-ooClasses () (setf *ooClasses* nil))

(defmacro makeMethod (name argNames &body body)
  `(make-ooMethod :name ',name :lambda (lambda ,argNames ,@body)))

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
  (find methodName (ooClass-methods aClass) :key #'ooMethod-name))

(defun new (className &rest instValues)
  (make-ooObject :aClass (getClass className) :instValues instValues))

(defparameter *current-object* nil)

(defun getInstVar (obj instVarName)
  (let ((theObj (if (equal obj 'this)
		    *current-object*
		    obj)))
    (let* ((theClass (ooobject-aClass theObj))
	   (theInstVars (ooclass-instVars theClass))
	   (instVarNum (position instVarName theInstVars)))
      (nth instVarNum (ooobject-instValues theObj)))))

(defun call (obj methodName &rest methodParams)
  (let ((theObj (if (equal obj 'this)
		    *current-object*
		    obj)))
    (let* ((theClass (ooObject-aClass theObj))
	   (theMethod (getMethod theClass methodName)))
      (setf *current-object* theObj)
      (apply (oomethod-lambda theMethod) methodParams))))

(makeClass Person (name)
  (makeMethod getName ()
    (getInstVar 'this 'name)))

(defvar toto)
(setf toto (new 'Person "Toto"))
(call toto 'getName)

(makeClass Speaker ()
  (makeMethod speak (sentence)
    (format t "Listen all of you: ~s~%" sentence)))

(defvar alexander)
(setf alexander (new 'Speaker))
(call alexander 'speak "Hello World!")
