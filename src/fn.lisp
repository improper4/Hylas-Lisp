(in-package :hylas)

(defun integer-constructorp (fn)
    (and (equalp #\i (elt (symbol-name fn) 0))
        (handler-case
            (parse-integer (subseq (symbol-name fn) 1))
        (error () nil))))

(defun corep (fn code)
    (gethash (symbol-name fn) (core code)))

(defun specialp (fn code)
    (gethash (symbol-name fn) (special code)))
