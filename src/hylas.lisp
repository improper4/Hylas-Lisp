(defpackage hylas
  (:use :cl))
(in-package :hylas)

(defclass variable ()
  ((type :accessor   type
   :initarg    :type
   :initform   "")
  (reg-type :accessor reg-type
   :initarg :reg-type
   :initform :var)))

(defun make-var (type)
  (make-instance 'variable :type type))

(defclass scope ()
  ((vars :accessor    vars
   :initarg     :vars
   :initform    (make-hash-table))))

(defclass code ()
  ((top
    :accessor   top
    :initarg    :top
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
  (label-version
    :accessor   label-version
    :initarg    :label-version
    :initform   0)
  (stack
    :accessor   stack
    :initarg    :stack
    :initform   (list (make-instance 'scope))
    :documentation "A list of scopes, the first being the global scope. Scopes are added to the end of the list or removed as new lexical contexts are created and exited.")
  (options
    :accessor   options
    :initarg    :options
    :initform   '(:output plain)
    :documentation "A list of options describing the behavior of the compiler, from the type of output so the optimizations that are enabled.")
  (special
    :accessor     special
    :initarg      :special
    :initform     '()
    :documentation "A list of builtin special forms. These cannot be overloaded, and act as macros on their arguments")
  (core
    :accessor     core
    :initarg      :core
    :initform     '()
    :documentation "A list of builtin core functions. These may be overloaded."
    )))

(defun copy-code (code)
  (make-instance 'code :top (top code)
    :res-version (res-version code)
    :label-version (label-version code)
    :stack (stack code)
    :options (options code)))

(defmacro emit (ir &rest args)
  "Just a simplification"
  `(format nil ,ir ,@args))

; Variables and registers

(defun lookup (symbol code &key (lookup-register nil))
  "Look up a symbol in the scope of a compiler state."
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

;; The following function takes a type, and allocates a new register to store a value of that type. Registers within a scope are named `%[0..+Inf]`.
(defun res (code type)
  (let ((num (princ-to-string (incf (res-version code)))))
    (var num (make-var type))
    (emit "%~a" num)))

(defun res-type (code &optional (n -1))
  "Returns the type of the nth register on the current scope, if n is not given, it returns the type of the last register"
  (type (gethash (princ-to-string (if n n (res-version code))) (vars (car (last (stack code)))))))

; Core functions

(defmacro append-entry (&rest ir)
  "Append a piece of IR to the code of the entry function."
  `(let ((code (copy-code code)))
   (setf (entry code) (append (entry code) (list ,@ir)))
   code))

(defmacro append-toplevel (code &rest ir)
  "Append of piece of IR to the toplevel (global) code.")

(defun assign (left right)
  "LLVM IR assignment."
  (emit "~a = ~a" left right))

(defmacro assign-res (type value)
  `(assign (res code ,type) ,value))

(defun allocate (type)
  "Allocate enough space for a value on the stack."
  (emit "allocate ~a" type))

(defun store (type address value)
  "Store a value of a given type on a given address."
  (emit "store ~a ~a, ~a* ~a" type value type address))

(defun memload (type source)
  "Load a value of a given type from an address."
  (emit "load ~a* ~a" type source))

(defun constant (type value)
  "A dirty little LLVM hack to emit an immediate value of some type."
  (emit "select i1 true, ~a ~a, ~a ~a" (emit-type type) value type value))

;; This function destructively modifies the `code` object that is passed as its argument, so it should only be used within a call to `append-entry`.
;; It returns a copy of the (updated) object, so it can be queried as usual for things like the last register value.
(defun emit-code (form code &optional &key (in-lambda nil))
  (if (atom form)
    (cond
      ((eql t form)
        (append-entry
          (assign-res (int 1) (constant (int 1) "true"))))
      ((null form)
        (append-entry
          (assign-res (int 1) (constant (int 1) "false"))))
      ((integerp form)
        (append-entry
          (assign-res (int 64) (constant (int 64) form))))
      ((floatp form)
        (append-entry
          (assign-res double (constant double form))))
            ;((stringp form)
            ;    ...)
    ((symbolp form)
     (multiple-value-bind (var pos) (lookup form code))
     (if var
       (progn
                     ;; If we're in a lambda, check whether the symbol comes
                     ;; from a lexical context other than the local or global
                     ;; ones
                     (append-entry code
                      (memload (type var) (prefix var))))
       (raise form "Unbound symbol"))))
        ; Input is a list
        (let ((fn (car form)))
          (if (specialp fn code)
            nil
            ;;No? Well, user-defined function then
            (aif (callfn fn (cdr form) code)
              it
                  ;; Not that? Then it's part of the normal core
                  (if (integer-constructorp fn code)
                    (construct-integer (cdr form) code)
                    (aif (corep fn code)
                     (funcall it (cdr form))
                           ;; Since everything above failed, signal an error
                           (raise form "No such function"))))))))

; core

(defmacro defbuiltin (name &rest code)
  `(defun ,name (code form)
    ,@code))
; errors

(defmacro raise (form msg &rest args)
  `(error (format nil ,msg ,@args)))

(defun compile-code (form code)
  "Takes a form. Produces global IR"
  (let ((out (emit-code form code)))
    (format nil "~{~A~%~}~%define ~A @entry(){~%~{    ~A~%~}}"
      (top code) (res-type code) (entry code))))

(defun jit (form code)
  "Takes a string, tries to compile it. Output format:
  fail
  error-type: Normal Error
  error-message: ...
  Alternatively:
  success
  [output of the 'print' part of the repl]
  "
  (compile-code form code))

(defun repl (code)
  (loop (princ (jit (read) code))))

#|(repl (make-instance 'code
  :special
  '(("def" #'def)
    )
  :core
  (list)))|#

