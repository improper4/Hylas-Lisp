(in-package :hylas)
(annot:enable-annot-syntax)

(defmacro aif (test true-branch &optional false-branch)
  `(let ((it ,test))
     (if it ,true-branch ,false-branch)))

(defun pairs (list)
  (if (cdr list)
      (append (list (cons (car list) (cadr list)))
              (pairs (cddr list)))
      nil))

(defun hash (&rest pairs)
  (let ((out (make-hash-table :test #'equal)))
    (loop for pair in (pairs pairs) do
          (setf (gethash (car pair) out) (cdr pair)))
    out))

@export
@doc "Set the `docstring` of *package* to hold the documentation text."
(cl-annot:defannotation document (text)
  (:arity 1 :inline t)
  `(setf (documentation *package* t) ,text))
