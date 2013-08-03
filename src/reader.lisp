(in-package :hylas)
(annot:enable-annot-syntax)

(defun safe-read () (read))

(defun safe-read-from-string (str) (read-from-string str))
