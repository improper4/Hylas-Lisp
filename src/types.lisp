(in-package :hylas)
(annot:enable-annot-syntax)

(defparameter +float-types+
  (list "half" "float" "double" "fp128" "x86_f80" "ppc_fp128"))

(defclass <type> ()
  ((docs
   :accessor   docs
   :initarg    :docs
   :initform   "<Undocumented>")
  (indirection
   :accessor   indirection
   :initarg    :indirection
   :initform   0
   :documentation "Represents the level of pointer indirection: 0 is a plain old object, 1 is [type]*, 2 is [type]**, et cetera."))
  (:documentation "The base class for all Hylas types."))

(defmethod pointer ((type <type>))
  "I'm sorry gods of functional programming ;_;"
  (incf (indirection type))
  type)

(defclass <generic-type> (<type>)
  ((type-var :accessor type-var :initarg :type-var))
  (:documentation "Represents a generic type variable."))

(defun generic (sym)
  (make-instance '<generic-type> :type-var sym))

(defclass <scalar> (<type>)
  ((type
     :accessor   scalar-type
     :initarg    :type
     :initform   "")))

(defun scalar (type)
  (make-instance '<scalar> :type type))

(defclass <integer> (<type>)
  ((width
     :accessor width
     :initarg :width)))

(defun int (bit-width)
  (make-instance '<integer> :width bit-width))

(defclass <func> (<scalar>)
  ((ret
     :accessor   ret
     :initarg    :ret)
  (args
    :accessor   args
    :initarg    :args
    :initform   ()))
  (:documentation "The function pointer type"))

(defclass <aggregate> (<type>)
  ((types :accessor   types
    :initarg    :types
    :initform   '()))
  (:documentation "This describes tuples and structures."))

(defun aggregate (types)
  (make-instance '<aggregate> :types types))

(defclass <struct> (<aggregate>)
  ((names :accessor   names
    :initarg    :names
    :initform   '())))

(defclass <vector> (<type>)
  ((type
     :accessor vector-type
     :initarg :type)
  (size
     :accessor size
     :initarg :size)))

(defmethod vector->intrinsic ((vec <vector>))
  (emit "v~A~A" (size vec) (vector-type vec)))

;; Some functions on types

(defmethod pointer? ((type <type>))
  (> (indirection type) 1))

(defun integer? (type)
  (typep type '<integer>))

(defun boolean? (type)
  (and
    (typep type '<integer>)
    (eql (width type) 1)))

(defun float? (type)
  (and
    (typep type '<scalar>)
    (memeber (scalar-type type) +float-types+)))

(defun func? (type)
  (typep type '<func>))

(defun tuple? (type)
  (and
    (typep type '<aggregate>)
    (not (typep type '<struct>))))

(defun struct? (type)
  (typep type '<struct>))

(defun vector? (type)
  (typep type '<vector>))

(defun vector-or? (alt type)
  (or (typep type alt)
      (and (typep type '<vector>)
           (typep (vector-type type) alt))))

@doc "Generate a type object from the form of a type signature."
(defmethod parse-type (form (code <code>))
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
        (let ((ret (emit-type (cadr form)))
          (argtypes (mapcar #'emit-type (cddr form))))
        (make-instance '<func> :ret ret :args argtypes)))
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
              (ret fn)))
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

(defmethod print-type ((type <scalar>))
  (scalar-type type))

(defmethod print-type ((type <generic-type>))
  (format nil "T~(~A~)" (type-var type)))

(defmethod print-type ((type <integer>))
  (format nil "i~A" (width type)))

(defmethod print-type ((type <aggregate>))
  (format nil "{~{~A~#[~:;, ~]~}}" (mapcar #'emit-type (types type))))

(defmethod print-type ((type <func>))
  (format nil "~A(~{~A~#[~:;, ~]~})*" (ret type)
    (mapcar #'emit-type (args type))))

(defmethod print-type ((type <vector>))
  (format nil "<~A x ~A>" (size type) (type type)))

(defmethod emit-type ((type <type>))
  (format nil "~A~{~A~}" (print-type type) (loop repeat (indirection type)
    collecting "*")))

(defmethod print-object ((type <type>) stream)
  (format stream "~A" (emit-type type)))

(defmethod match ((a <scalar>) (b <scalar>))
  (equal (scalar-type a) (scalar-type b)))

(defmethod match ((a <integer>) (b <integer>))
  (eql (width a) (width b)))

(defmethod match ((a <aggregate>) (b <aggregate>))
  (when (eql (length (types a)) (length (types b)))
    (not
      (member nil
        (loop for i from 0 to (length (types a)) collecting
          (match (nth i (types a)) (nth i (types b))))))))

(defmethod match ((a <vector>) (b <vector>))
  (and (eql (size a) (size b))
       (match (vector-type a) (vector-type b))))

@doc "If all other `match` methods are not appropriate, then the types are not a
match."
(defmethod match ((a <type>) (b <type>))
  nil)

;; Builtins

(defparameter +byte+  (int 8))
(defparameter +cstr+  (pointer (int 8)))

(defparameter +half+      (scalar "half"))
(defparameter +float+     (scalar "float"))
(defparameter +double+    (scalar "double"))
(defparameter +fp128+     (scalar "fp128"))
(defparameter +x86-fp80+  (scalar "x86_f80"))
(defparameter +ppc-fp128+ (scalar "ppc_fp128"))
