(in-package :hylas)
(annot:enable-annot-syntax)

(declaim (optimize (debug 3) (speed 0)))

(defparameter +word+ "i64")

@doc "The variable class represents generalized variables, ie both named
variables and registers."
(defclass <variable> ()
  ((type
     :accessor   var-type
     :initarg    :type
     :initform   "")
  (reg-type
    :accessor reg-type
    :initarg :reg-type
    :initform :var)))

(defmethod print-object ((var  <variable>) stream)
  (format stream "~A,~A" (var-type var) (reg-type var)))

(defun make-var (type)
  (make-instance ' <variable> :type type))

(defclass <scope> ()
  ((vars
     :accessor    vars
     :initarg     :vars
     :initform    (make-hash-table :test #'equal))))

(defun print-var (name var)
  (format nil "~S -> ~A" name var))

(defmethod print-object ((scope <scope>) stream)
  (format stream "~{~&  - ~A~}" (or (loop for name being the hash-keys of (vars scope) using (hash-value var)
                                        collecting (print-var name var)) (list "[Empty Scope]"))))

(defclass <code> ()
  ((toplevel
    :accessor   toplevel
    :initarg    :toplevel
    :initform   '()
    :documentation "The toplevel/global code.")
  (entry
    :accessor   entry
    :initarg    :entry
    :initform   '()
    :documentation "The code inside the entry function.")
  (res-version
    :accessor   res-version
    :initarg    :res-version
    :initform   0
    :documentation "The number that is used as the name of result registers.")
  (string-version
    :accessor   string-version
    :initarg    :string-version
    :initform   0)
  (label-version
    :accessor   label-version
    :initarg    :label-version
    :initform   0)
  (stack
    :accessor   stack
    :initarg    :stack
    :initform   (list (make-instance '<scope>))
    :documentation "A list of scopes, the first being the global scope. Scopes are added to the end of the list or removed as new lexical contexts are created and exited.")
  (options
    :accessor   options
    :initarg    :options
    :initform   '(:output plain)
    :documentation "A list of options describing the behavior of the compiler, from the type of output so the optimizations that are enabled.")
  (operators
    :accessor     operators
    :initarg      :operators
    :initform     (make-hash-table :test #'equal)
    :documentation "A list of builtin special forms. These cannot be overloaded, and act as macros on their arguments")
  (core
    :accessor     core
    :initarg      :core
    :initform     (make-hash-table :test #'equal)
    :documentation "A list of builtin core functions. These may be overloaded.")))

(defmethod print-object ((code <code>) stream)
  (format stream "<code reg-version: ~A, stack:~%~{~A~}>" (res-version code) (stack code)))

(defmethod copy-code ((code <code>))
  (make-instance '<code>
    :toplevel (toplevel code)
    :entry (entry code)
    :res-version (res-version code)
    :string-version (string-version code)
    :label-version (label-version code)
    :stack (stack code)
    :options (options code)
    :operators (operators code)
    :core (core code)))

@doc "Just a simplification"
(defmacro emit (ir &rest args)
  `(format nil ,ir ,@args))

; Variables and registers

@doc "Look up a symbol in the scope of a compiler state."
(defun lookup (symbol code &key (lookup-register nil))
  (loop for scope in (reverse (stack code))
    if (and (gethash symbol (vars scope))
      (eql (if lookup-register :reg :var)
       (reg-type (gethash symbol (vars scope)))))
    return
    (values (gethash symbol (vars scope))
      (position scope (stack code)))))

(defun prefix (pos)
  (if (eql pos 0) "@" "%"))

(defmacro var (name &optional variable)
  (if variable
    `(setf (gethash ,name (vars (car (last (stack code))))) ,variable)
    `(gethash ,name (vars (car (last (stack code)))))))

(defmethod res ((code <code>) &optional type)
  (if type
      (let ((num (princ-to-string (incf (res-version code)))))
        (var num (make-var type))
        (emit "%~A" num))
      (emit "%~A" (princ-to-string (res-version code)))))

(defun get-string (num)
  (emit "@__string~A" (princ-to-string num)))

(defmethod new-string ((code <code>))
  (get-string (incf (string-version code))))

(defmethod current-string ((code <code>))
  (get-string (string-version code)))

@doc "Returns the type of the nth register on the current scope, if n is not given,
it returns the type of the last register"
(defun res-type (code &optional (n (res-version code)))
  (aif (gethash (princ-to-string (if n n (res-version code))) (vars (car (last (stack code)))))
       (var-type it)
       (raise code "Could not get type of register ~A~%" n)))

; Core functions

(defmacro append-entry (code &rest ir)
  "Append a piece of IR to the code of the entry function."
  `(let ((code (copy-code ,code)))
     (setf (entry code) (append (entry code) (list ,@ir)))
     code))

@doc "Append of piece of IR to the toplevel (global) code."
(defmacro append-toplevel (code &rest ir)
  `(let ((code (copy-code ,code)))
     (setf (toplevel code) (append (toplevel code) (list ,@ir)))
     code))
