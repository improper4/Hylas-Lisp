(in-package :hylas)

(defun integer-constructorp (fn)
  (and (equalp #\i (elt (symbol-name fn) 0))
       (handler-case
         (parse-integer (subseq (symbol-name fn) 1))
         (error () nil))))

(defun corep (fn code)
  (gethash (symbol-name fn) (core code)))

(defun operatorp (fn code)
  (gethash (symbol-name fn) (operators code)))

(defmethod callfn (fn form (code <code>))
  (error "Eudoxia hasn't got this far yet 😿"))
