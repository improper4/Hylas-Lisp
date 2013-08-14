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

(defun flatten (list)
  (cond
    ((null list)
      nil)
    ((atom list)
      (list list))
    (t
      (loop for item in list appending (flatten item)))))

(defun get-from-list (name list)
  (loop for form in list do
    (if (and (listp form) (equal (car form) name))
        (return-from get-from-list
          (list
            form
            (remove-if #'(lambda (form)
                          (and (listp form)
                               (equal (car form) name))) list))))))

(defun get-opts (form)
  (or (get-from-list '|opt| form) (list nil form)))

(defun get-docs (form)
  (or (get-from-list '|doc| form) (list nil form)))

(defun get-meta (form)
  (destructuring-bind (opts form) (get-opts form)
    (let ((final (get-docs form)))
      (list opts (car final) (cadr final)))))
