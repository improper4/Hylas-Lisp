(in-package :hylas)
(annot:enable-annot-syntax)

(defun create-package (name)
  (handler-case
    (make-package name)
    (error () (find-package name))))
