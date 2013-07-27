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

;; More complex LLVM constructs

@doc "An ad-hoc, castrated GetElementPtr"
(defun gep (type ptr &rest indices)
  (emit "getelementptr ~A* ~A ~{~A~#[~:;, ~]~}" type ptr
    (mapcar #'(lambda (idx) (concatenate 'string +word+ " " (princ-to-string idx))) indices)))
