(in-package :hylas)

(defclass hylas-type ()
  ((doc
   :accessor   doc
   :initarg    :doc
   :initform   "<Undocumented>")
  (indirection
   :accessor   indirection
   :initarg    :indirection
   :initform   0
   :documentation "Represents the level of pointer indirection: 0 is a plain old object, 1 is [type]*, 2 is [type]**, et cetera."))
  (:documentation "The base class for all Hylas types."))

(defclass scalar (hylas-type)
  ((type :accessor   type
   :initarg    :type
   :initform   "")))

(defclass func (scalar)
  ((retval :accessor   retval
   :initarg    :retval)
  (args :accessor   args
   :initarg    :args))
  (:documentation "The function pointer type"))

(defun scalar (type)
  (make-instance 'scalar :type type))

(defclass aggregate (hylas-type)
  ((types :accessor   types
    :initarg    :types
    :initform   '()))
  (:documentation "This describes tuples and structures."))

(defun aggregate (types)
  (make-instance 'aggregate :types types))

(defclass struct (aggregate)
  ((names :accessor   names
    :initarg    :names
    :initform   '())))

(defun parse-type (form code)
  "Generate a type object from the form of a type signature."
  (if (atomp form)
    ; Named type
    (case (car form)
      (pointer
        ;Increase the indirection level by one or n (integer constant)
        (let ((type (emit-type (cadr form)))
          (n (if (caddr form)
            (caddr form)
            1)))
        (incf (indirection type) n)
        type))
      (unpointer
        ;Decrease indirection level by one, or n (integer constant)
        ;if object is not a pointer, signal an error
        (let ((type (emit-type (cadr form)))
          (n (if (caddr form)
            (caddr form)
            1)))
        (decf (indirection type) n)
        (if (< (indirection type) 0)
          (raise form "Can't unpointer this object"))))
      (fn
        ;function pointer type: (fn retval type_1 type_2 ... type_3)
        (let ((retval (emit-type (cadr form)))
          (argtypes (mapcar #'emit-type (cddr form))))
        (make-instance 'func :retval retval :types argtypes)))
      (list
        ; anonymous structure type: (list type_1 type_2 ... type_3)
        (let ((types (mapcar #'emit-type (cdr form))))
          (aggregate types)))
      (structure
        ; named structure
        
        )
      (typeof
        ; emit the code for a form, throw away everything by the type
        (res-type (emit-code (cadr form) code)))
      (ret
            ;the return type of a function pointer
            (let ((fn (emit-type (cadr form))))
              (retval fn)))
      (args
        ; return the argument list from a function pointer type  as a list of types
        (let ((fn (emit-type (cadr form))))
          (aggregate (args fn))))
        ;Functions to excise the types of list
        (nth
          
          )
        (first
          
          )
        (last
          
          )
        (tail
          
          )
        (body
          
          ))))

(defmethod print-type ((type scalar))
  (type type))

(defmethod print-type ((type aggregate))
  (format nil "{~{~A~#[~:;, ~]~}}" (mapcar #'emit-type (types type))))

(defmethod print-type ((type func))
  (format nil "~A(~{~A~#[~:;, ~]~})*" retval (mapcar #'emit-type (types type))))

(defmethod emit-type ((type hylas-type))
  (format nil "~A~{~A~}" (print-type type) (loop repeat (indirection type)
    collecting "*")))

(defun match-type (a b)
  (if (equal a b)
    t
    nil))

;; Builtins

(defun int (bit-width)
  (scalar (concatenate 'string "i" (princ-to-string bit-width))))

(defconstant half      (scalar "half"))
(defconstant float     (scalar "float"))
(defconstant double    (scalar "double"))
(defconstant fp128     (scalar "fp128"))
(defconstant x86_fp80  (scalar "x86_f0"))
(defconstant ppc_fp128 (scalar "ppc_fp128"))
