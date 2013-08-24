(in-package :hylas)
(annot:enable-annot-syntax)

(defun core? (fn code)
  (gethash (symbol-name fn) (core code)))

(defun operator? (fn code)
  (gethash (symbol-name fn) (operators code)))

(defmethod callfn (fn form (code <code>))
  nil)

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
   (cconv :accessor cconv :initarg :cconv :initform :ccc)))

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

(defun parse-function (form)
  (let ((name (car form))
        (ret  (cadr form))
        (args (caddr form))
        (body (cdddr form)))
    (list name ret args body)))

(defmethod add-fn-def (name fn (code <code>))
  (aif (gethash name (functions code))
    (setf it (append it fn))
    (setf it (list fn))))

(defmethod define-function (form (code <code>))
  (destructuring-bind (name ret args body) (parse-function form)
    (if (fn-exists? name ret args code)
        (error form "a function with this name and prototype already exists.")
        (destructuring-bind (opts docs form) (get-meta form)
          (let ((fn (make-instance '<function> :name name
                                    :base-name (mangle-fn name args code)
                                    :ret-type ret
                                    :arg-types args
                                    :docstring docs
                                    :tco (option? "tail" opts)
                                    :cconv (get-cconv opts))))
            (add-fn-def name fn code)
            (format t "~A" body)
            (let ((fn-code (extract-list body code)))
              (append-toplevel fn-code
                (define (base-name fn) :ret ret :args args
                  :tail (tco fn) :body (entry fn-code) :last (res code)))))))))

(defmethod define-generic-function (fn (code <code>)))

;; Some tools for finding specific options

(defun option? (opt options)
  t)

(defun option (opt options)
  )

(defun get-cconv (options)
  "ccc")
