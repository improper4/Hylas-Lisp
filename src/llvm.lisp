(in-package :hylas)
(annot:enable-annot-syntax)

@doc "LLVM IR assignment."
(defun assign (left right)
  (emit "~A = ~A" left right))

(defmacro assign-res (type value)
  `(assign (res code ,type) ,value))

@doc "Allocate enough space for a value on the stack."
(defun allocate (type)
  (emit "allocate ~A" (emit-type type)))

@doc "Store a value of a given type on a given address."
(defun store (type address value)
  (emit "store ~A ~A, ~A* ~A" (emit-type type) value type address))

@doc "Load a value of a given type from an address."
(defun memload (type source)
  (emit "load ~A* ~A" (emit-type type) source))

@doc "A dirty little LLVM hack to emit an immediate value of some type."
(defun constant (type value)
  (emit "select i1 true, ~A ~A, ~A ~A" (emit-type type) value type value))

;;; More complex LLVM constructs


;; Flow control

(defun branch (test true-label false-label)
  (emit "br i1 ~A, label ~A, label ~A" test true-label false-label))

(defun goto (label)
  (emit "br label ~A" label))

(defun phi (type true-reg true-label false-reg false-label)
  (emit "phi ~A [~A, ~A], [~A, ~A]" type true-reg true-label false-reg
    false-label))

;; Comparison

(defun cmp (op test type first second)
  (emit "~A ~A ~A ~A, ~A" op test type first second))

(defmacro op (&rest args) `(cmp ,@args))

@doc "An ad-hoc, castrated GetElementPtr"
(defun gep (type ptr &rest indices)
  (emit "getelementptr ~A* ~A ~{~A~#[~:;, ~]~}" type ptr
    (mapcar #'(lambda (idx) (concatenate 'string +word+ " " (princ-to-string idx))) indices)))

;; Bit manipulation

(defun bitop (name type source)
  (emit "tail call ~A @llvm.~A.~A(~A ~A, i1 true)" type name type type source))

(defun bitop-def (name type &optional bool-modifier)
  (if bool-modifier
    (emit "declare ~A @llvm.~A.~A(~A,i1)" type name type type)
    (emit "declare ~A @llvm.~A.~A(~A)" type name type type)))

;; Conversion

(defun conv (op source from-type to-type)
  (emit "~A ~A ~A to ~A" op from-type source to-type))

;; declare

#|@doc "Add a declaration of a function, if it doesn't already exist."
(defmacro with-declare (fn ret-type args &rest code)
  `(let ((code (append-toplevel code (emit "declare ~A ~A(~{~A~#[~:;, ~]~})"
                                       ret-type fn args))))
     ,@code))|#

;; Functions

(defun fn-proto (args)
  (loop for arg in args collecting
    (format nil "~A ~A" (nth 1 arg) (nth 0 arg))))

(defun define (fn &key ret args tail attrs gc body last)
  (emit "define ~A @~A(~A) ~{~A ~} ~A {~&~{    ~A~&~}    ret ~A ~A~&}" ret fn
    (or (fn-proto args) "") (unless attrs (list ""))
    (if gc (concatenate 'string "gc \"" gc "\"") "") body ret last))

(defun call (fn &key ret args (cconv "") tail)
  (emit "~Acall ~A ~A ~A(~{~A~#[~:;, ~]~})"
    (if tail "tail " "") cconv ret fn (fn-proto args)))
