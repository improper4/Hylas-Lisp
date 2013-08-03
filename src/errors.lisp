(in-package :hylas)

(defmacro raise (code msg &rest args)
  `(error (format nil ,(concatenate 'string msg "~%Code state:~%~a") ,@args ,code)))

(defun bad-input-type (code name expected-type pos given-type)
  (error code "(~A) expected ~A as its ~A argument, but got ~A"
    expected-type pos given-type))
