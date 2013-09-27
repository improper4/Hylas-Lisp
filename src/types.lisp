(in-package :hylas)
(annot:enable-annot-syntax)

(defparameter +float-types+
  (list '|half| '|uhalf| '|single| '|usingle| '|double| '|udouble| '|quad|
        '|uquad|))

(defparameter +word+ "i64")
(defparameter +word-width+ 64)

(defmacro def-type (name (&rest parents) (&rest slots) &key doc print print-flat
		   match vectorizable?)
  `(progn
     (defclass ,name (,@parents)
       (,@slots)
       (:documentation ,doc))
     
     (defun ,(intern (format nil "~A?"
		       (remove-if #'(lambda (c) (or (char-equal c #\<)
						    (char-equal c #\>)))
		         (symbol-name name))))
       (obj)
       (typep obj ',name))

     (defmethod print-type ((type ,name))
       ,@print)

     (defmethod print-flat ((type ,name))
       ,@print-flat)

     (defmethod match ((a ,name) (b ,name))
       ,@match)))

(defclass <type> ()
  ((docs
   :accessor   docs
   :initarg    :docs
   :initform   "<Undocumented>")
  (indirection
   :accessor   indirection
   :initarg    :indirection
   :initform   0
   :documentation "Represents the level of pointer indirection: 0 is a plain old object, 1 is [type]*, 2 is [type]**, et cetera.")
  (generic :accessor type-generic :initform nil))
  (:documentation "The base class for all Hylas types."))

(defmethod pointer ((type <type>))
  "I'm sorry gods of functional programming ;_;"
  (incf (indirection type))
  type)

(defmethod pointer? ((type <type>))
  (> (indirection type) 1))

(def-type <unit> (<type>)
  ()
  :doc "The unit type ()"
  :print ((format t "i1"))
  :print-flat ("unit")
  :match (t)
  :vectorizable? t)

(defclass <generic-type> (<type>)
  ((type-var :accessor type-var :initarg :type-var)
   (options :accessor options :initarg :options))
  (:documentation "Represents a generic type."))

(defun generic (sym &optional opts)
  (make-instance '<generic-type> :type-var sym :options opts))

(defmethod print-type ((type <generic-type>))
  (format nil "T~(~A~)" (type-var type)))

(def-type <scalar> (<type>)
  ((type
     :accessor   scalar-type
     :initarg    :type
     :initform   "")
   (ordered? :accessor ordered? :initarg :ordered :initform t))
  :doc "Scalar type (integers and floats)."
  :print ((cond
	    ((equal (scalar-type type) "single")
	     "float")
	    ((equal (scalar-type type) "quad")
	     "fp128")
	    (t (scalar-type type))))
  :print-flat ((print-type type))
  :match ((equal (scalar-type a) (scalar-type b)))
  :vectorizable? t)

(defun scalar (type &optional (ordered t))
  (make-instance '<scalar> :type type :ordered ordered))

(defun float? (type)
  (and
    (typep type '<scalar>)
    (member (scalar-type type) +float-types+)))

(defun float-constructor? (fn)
  (aif (member fn +float-types+)
       (scalar (symbol-name fn) (char-equal #\u (aref (symbol-name fn) 0)))))

(def-type <integer> (<type>)
  ((width
     :accessor width
     :initarg :width)
   (signed?
     :accessor signed?
     :initarg :signed?
     :initform t))
  :doc "Integer/bitfield types"
  :print ((format nil "~A~A" (if (signed? type) "i" "ui") (width type)))
  :print-flat ((print-type type))
  :match ((eql (width a) (width b)))
  :vectorizable? t)

@doc "How many bits does it take to represent `int`?"
(defun min-width (int)
  (* +word-width+ (ceiling (integer-length int) +word-width+)))

(defun int (bit-width &optional (signed t))
  (make-instance '<integer> :width bit-width :signed? signed))

(defun boolean? (type)
  (and
    (typep type '<integer>)
    (eql (width type) 1)))

@doc "Tests whether `fn` is of the form 'i[bitwidth]' or 'ui[bitwidth]'."
(defun integer-constructor? (fn)
  (let ((signed t))
    (and (or (char-equal #\i (elt (symbol-name fn) 0))
             (and (char-equal #\u (elt (symbol-name fn) 0))
                  (char-equal #\i (elt (symbol-name fn) 1))
                  (not (setf signed nil))))
         (handler-case
           (if signed
             (int (parse-integer (subseq (symbol-name fn) 1)) t)
             (int (parse-integer (subseq (symbol-name fn) 2)) nil))
           (error () nil)))))

(def-type <func> (<scalar>)
  ((ret
     :accessor   ret
     :initarg    :ret)
  (args
    :accessor   args
    :initarg    :args
    :initform   ()))
  :doc "The function pointer type"
  :print
    ((format nil "~A(~{~A~#[~:;, ~]~})*" (ret type)
       (mapcar #'emit-type (args type))))
  :print-flat
    ((format nil "_bfn_~A_~{~A~#[~:;.~]~}_efn_" (ret type)
       (mapcar #'print-flat (args type)))))

(def-type <aggregate> (<type>)
  ((types :accessor   types
    :initarg    :types
    :initform   '()))
  :doc "This describes tuples and structures."
  :print ((format nil "{~{~A~#[~:;, ~]~}}" (mapcar #'emit-type (types type))))
  :print-flat
    ("The `_ba_` and `_ea` markers here stand for 'begin aggregate' and 'end
aggregate'."
     (format nil "_ba_~{~A.~}_ea_" (types type)))
  :match
    ((when (eql (length (types a)) (length (types b)))
       (not
	 (member nil
	   (loop for i from 0 to (length (types a)) collecting
             (match (nth i (types a)) (nth i (types b)))))))))

;;      body
;;        |
;;   ___________
;;  v           v
;; {S_0,S_1,...,S_n-1,S_n}
;;  ^   ^-------------^
;;  |          |      |
;; first     rest    last

(defun aggregate (types)
  (make-instance '<aggregate> :types types))

(def-type <struct> (<aggregate>)
  ((names :accessor   names
    :initarg    :names
    :initform   '()))
  :doc "Like tuples, only with named fields")

(def-type <abstract> (<struct>)
  ((generic-names :accessor generic-names :initarg :generic-names))
  :doc "Parametric types")

(def-type <vector> (<type>)
  ((type
     :accessor vector-type
     :initarg :type)
  (size
     :accessor size
     :initarg :size))
  :doc "SIMD vectors"
  :print ((format nil "<~A x ~A>" (size type) (type type)))
  :print-flat
    ("This is similar to the way LLVM intrinsics are specialized to take vector
types."
    (format nil "v~Ax~A" (size type) (vector-type type)))
  :match
    ((and (eql (size a) (size b))
	  (match (vector-type a) (vector-type b))))
  :vectorizable? t)

(defmethod vector->intrinsic ((vec <vector>))
  (emit "v~A~A" (size vec) (vector-type vec)))

(defun vector-or? (alt type)
  (or (typep type alt)
      (and (typep type '<vector>)
           (typep (vector-type type) alt))))

;;; Emitting types into IR

(defmethod emit-type ((type <type>))
  (format nil "~A~{~A~}" (print-type type) (loop repeat (indirection type)
    collecting "*")))

(defmethod print-object ((type <type>) stream)
  (format stream "~A" (emit-type type)))

;;; "Flat printing" of types in a way amenable to function mangling

(defmethod print-flat ((type <type>))
  (format nil "~A~{~A~}" (flat-type type) (loop repeat (indirection type)
    collecting ".ptr")))

(defun mangle (fn args)
  (format nil "~A.~{~A~#[~:;. ~]~}" fn (mapcar #'print-flat args)))

;;; Type matching

@doc "If all other `match` methods are not appropriate, then the types are not a
match."
(defmethod match ((a <type>) (b <type>))
  nil)

;;; Parsing forms into type objects

@doc "Can generic type parameters be used within the current context?"
(defmethod generic-context? ((code <code>))
  (or (eq (last-context code) :typedef)
      (eq (last-context code) :fn)))

(defun generic-param? (sym)
  (and (atom sym) (char-equal (first (symbol-name sym)) #\?)))

@export
@doc "Generate a type object from the form of a type signature."
(defmethod parse-type (form (code <code>))
  (cond
    ((null form)
     +unit+)
    ((atom form)
     (cond
       ((integer-constructor? form)
	(integer-constructor? form))
       ((float-constructor? form)
	(scalar (float-constructor? form)))
       ((and (generic-context? code) (generic-param? form))
	;; Universal quantification
	(generic (intern (subseq (symbol-name form) 1))))
       (t
	;; A named type
	(aif (type-exists? form code)
	     it
	     (raise form "Unknown type '~A'." form)))))
    ;; Type function
    (t
     (case (car form)
       (|pointer|
	;Increase the indirection level by one or n (integer constant)
	(let ((type (parse-type (cadr form) code))
	      (n (aif (caddr form) it 1)))
	  (incf (indirection type) n)
	  type))
       (|unpointer|
	;; Decrease indirection level by one, or n (integer constant)
	;; If object is not a pointer, signal an error
	(let ((type (parse-type (cadr form) code))
	      (n (aif (caddr form) it 1)))
	  (decf (indirection type) n)
	  (if (< (indirection type) 0)
              (raise form "Can't (unpointer) this object"))
	  (decf (indirection type))
	  type))
       (|fn|
	;function pointer type: (fn retval type_1 type_2 ... type_3)
	(let ((ret (parse-type (cadr form) code)))
	  (argtypes (mapcar #'(lambda (type) (parse-type type code) (cddr form))))
          (make-instance '<func> :ret ret :args argtypes)))
       (|tup|
	;; (tup type_1 type_2 ... type_3) => {type_1,type_2,...,type_3}
	(let ((types (mapcar #'(lambda (type) (parse-type type code)) (cdr form))))
	  (aggregate types)))
       (|rec|
	;;(rec (name_1 type_1) ... (name_n type_n)) => {type_1,...,type_n}
	(let ((fields
	       (loop for field in (cdr form) collecting
                    (cond
                      ((atom field)
		       (raise form "Fields in a structure must be (name type) lists, but an atom was found."))
                      ((eql (length field) 1)
		       (raise form "Fields in a structure must be (name type) lists, but a single-element list was found."))
                      ((not (symbolp (car field)))
		       (raise form "The name of a structure field must be a symbol."))
                      (t (list (car field) (parse-type (cadr field) code)))))))
	  (make-instance '<struct>
			 :names (loop for field in fields collecting (car field))
			 :types (loop for field in fields collecting (cadr field)))))
       (|type|
	; emit the code for a form, throw away everything by the type
	(res-type (emit-code (cadr form) code)))
       (|ret|
	;the return type of a function pointer
	(let ((fn (parse-type (cadr form) code)))
	  (ret fn)))
       (|args|
	;; return the argument list from a function pointer type  as a list of
	;;types
	(let ((fn (parse-type (cadr form) code)))
	  (aggregate (args fn))))
       ;Functions to excise the types of an aggregate type
       (|nth|
	(let ((type (parse-type (cadr form) code))
	      (n (caddr form)))
	  (nth n (types type))))
       (|first|
	(let ((type (parse-type (cadr form) code)))
	  (first (types type))))
       (|last|
	(let ((type (parse-type (cadr form) code)))
	  (first (last (types type)))))
       (|rest|
	(let ((type (parse-type (cadr form) code)))
	  (rest (types type))))
       (|body|
	(let ((type (parse-type (cadr form) code)))
	  (reverse (rest (reverse (types type))))))
       ;; Abstract types
       (|abstract|

	)
       (t
	(if (and (generic-context? code) (generic-param? (first form)))
	    ;; Generic type parameter, with kinds
	    (let ((tag (first form))
		  (kind (second form)))
	      (generic tag kind))
	    ;; No type function matched: Go find a matching abstract type to
	    ;; specialize
	    +unit+))))))


;; Programmer input

(defmethod type-exists? (name (code <code>))
  ;; Type exists checks for the existence of concrete or generic types, but not
  ;; abstract types
  (aif (gethash name (types code)) (not (abstract? it))))

(defmethod abstract-exists? (name (code <code>)))

(defmethod define-type (name type form (code <code>))
  (if (type-exists? name code)
    (raise form "A type with that name already exists.")
    (let* ((code (copy-code code))
           (type (parse-type type code)))
      (setf (gethash name (types code)) type)
      code)))

(defmethod define-generic-type (fn (code <code>)))

;; Builtins

(defparameter +unit+  (make-instance '<unit>))

(defparameter +byte+  (int 8))
(defparameter +cstr+  (pointer (int 8)))

(defparameter +half+      (scalar '|half|))
(defparameter +float+     (scalar '|float|))
(defparameter +double+    (scalar '|double|))
(defparameter +quad+      (scalar '|quad|))
