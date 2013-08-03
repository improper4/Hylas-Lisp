(in-package :hylas)

(defun integer-constructor? (fn)
  (and (equalp #\i (elt (symbol-name fn) 0))
       (handler-case
         (parse-integer (subseq (symbol-name fn) 1))
         (error () nil))))

(defun core? (fn code)
  (gethash (symbol-name fn) (core code)))

(defun operator? (fn code)
  (gethash (symbol-name fn) (operators code)))

(defmethod callfn (fn form (code <code>))
  (error "Eudoxia hasn't got this far yet 😿"))
