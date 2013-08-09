(in-package :hylas)
(annot:enable-annot-syntax)

@doc "Tests whether `fn` is of the form 'i[bitwidth]'."
(defun integer-constructor? (fn)
  (and (equalp #\i (elt (symbol-name fn) 0))
       (handler-case
         (parse-integer (subseq (symbol-name fn) 1))
         (error () nil))))

(defun core? (fn code)
  (gethash (symbol-name fn) (core code)))

(defun operator? (fn code)
  (gethash (symbol-name fn) (operators code)))

(defmethod callfn (fn form (code <code>))
  (error "Eudoxia hasn't got this far yet 😿"))

@doc "A set of functions sharing the same name that are differentiated by their
prototypes."
(defclass <fn-space> ()
  ((functions :accessor functions :initarg functions :initform (list nil))))

(defclass <function> ()
  ((name :accessor name :initarg :name)
   (base-name :accessor base-name :initarg :base-name)
   (ret-type :accessor ret-type :initarg :ret-type :type <type>)
   (arg-types :accessor arg-types :initarg :arg-types :initform (list nil<))
   (docstring :accessor docstring :initarg :docstring :type string :initform "")
   (tco :accessor tco :initarg :tco :type boolean :initform nil)
   (cconv :accessor cconc :initarg :cconv :initform :ccc)))

(defclass <generic-function> (<function>)
  ((specializations :accessor specializations :initarg :specializations)))

@doc "t if a function `name` with the argument list `proto` and return type
`ret` exists in `code`."
(defmethod fn-exists? (name ret args (code <code>))
  (aif (gethash name (functions code))
       (loop for fn in (functions it)
         if (and (pat-match args (arg-types fn))
                 (match-type ret (ret-type fn))) return t)))

(defmethod mangle-fn (name args (code <code>))
  (let ((n (aif (gethash name (functions code))
                (length (functions it))
                0)))
    (concatenate 'string (mangle name args) (princ-to-string n))))

(defmethod define-function (fn (code <code>)))

(defmethod define-generic-function (fn (code <code>)))
